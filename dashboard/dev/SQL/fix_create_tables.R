path <- "dev/SQL/create_tables.sql"
sql <- readLines(path)

# FK_op.  ->  FK_op_
# FK_ref.  ->  FK_ref_
# TO_op. ->  TO_op_
# TO_ref.  ->  TO_ref_

sql <- sql |>
    stringr::str_replace_all(
        stringr::fixed("FK_op."),
        "FK_op_"
    ) |>
    stringr::str_replace_all(
        stringr::fixed("FK_ref."),
        "FK_ref_"
    ) |>
    stringr::str_replace_all(
        stringr::fixed("TO_op."),
        "TO_op_"
    ) |>
    stringr::str_replace_all(
        stringr::fixed("TO_ref."),
        "TO_ref_"
    )

#use serial for ids
index <- which(stringr::str_detect(sql, "GENERATED ALWAYS AS IDENTITY"))
sql[index] <- "id     SERIAL NOT NULL,"

#on delete cascade
index <- which(sql %in% "ALTER TABLE op.likes") + 3
sql[index] <- stringr::str_replace_all(sql[index], ";", " ON DELETE CASCADE;")

file.remove(path)
writeLines(sql, path)

