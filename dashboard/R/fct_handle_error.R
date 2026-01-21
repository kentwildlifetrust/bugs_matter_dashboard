#' handle_error
#'
#' @description Handle an error caught with tryCatch
#' This function prints the error message and context to the console, as well as sending an error log to Azure when `!golem::app_dev()`.
#' @param e Error passed from tryCatch.
#' @param script_name Name of the script in R/ to help locate error source.
#' @param alert_tite Title of popup alerting user to error. Defaults to "Error".
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'      tryCatch({
#'          stop("This is an error")
#'      }, error = function(e) {
#'          handle_error(e, "test_script.R", "test_app")
#'      })
#' }
#'
#' #Console Output
#'
#' #---------------------------------Script---------------------------------
#' #R/test_script.R
#' #---------------------------------Context--------------------------------
#' #tryCatch({
#' #    stop("This is an error")
#' #}, error = function(e) {
#' #    handle_error(e, "test_script.R", "test_app")
#' #})
#' #
#' #---------------------------------Message--------------------------------
#' #Error: This is an error
#'
#'
handle_error <- function(e, script_name, alert_title = "Error") {
    # Get the call stack
    call_stack <- sys.calls()
    call_stack <- call_stack[max(1, length(call_stack) - 7):length(call_stack)] |>
        lapply(deparse) |>
        lapply(function(x) paste0(x, collapse = "\n")) |>
        lapply(function(x) paste0(x, "\n\n"))

    #show some helpful messages in the console
    cat("---------------------------------Script---------------------------------\n")
    cat(paste0("R/", script_name, "\n"))
    cat("---------------------------------Context--------------------------------\n")
    cat(call_stack[grepl("tryCatch({", call_stack, fixed = TRUE)][[1]])
    cat("---------------------------------Message--------------------------------\n")
    message("Error: ", e$message)

    #don't try to show notification if not being called from a server (i.e. in global script)
    if (!is.null(shiny::getDefaultReactiveDomain())) {
        shinyalert::shinyalert(
            title = alert_title,
            text = "Please try again later",
            type = "error"
        )
    }

    call_stack <- call_stack |>
        lapply(htmltools::htmlEscape) |>
        lapply(paste, collapse = "<br>")

    #write a log message and save to azure
    log <- list(
        app = golem::pkg_name(),
        event = "error",
        script = script_name,
        call_stack = call_stack,
        message = htmltools::htmlEscape(e$message),
        timestamp = Sys.time()
    )

    #write to azure blob storage. This triggers Power Automate flow to send email to digitaldevelopment@kentwildlife.org.uk
    if (!golem::app_dev()) {
        if (nchar(Sys.getenv("SHINY_LOGS_SAS")) == 0) {
            stop("environment variable SHINY_LOGS_SAS not found. Please get this from Kia or Euan.")
        }
        container <- AzureStor::storage_endpoint(
            "https://kwtwebappstorage.blob.core.windows.net/",
            sas = Sys.getenv("SHINY_LOGS_SAS")
        ) |>
            AzureStor::storage_container("shiny-logs")

        #write log file to tempfiles
        filepath <- tempfile(fileext = ".json")
        log <- jsonlite::toJSON(log)
        write(log, filepath)
        dest <- paste0(golem::pkg_name(), "_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), "_error.json")
        AzureStor::storage_upload(container, src = filepath, dest = dest)
        cat("Error log written\n")
    }
}
