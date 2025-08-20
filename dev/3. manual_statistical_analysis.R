library(sf)
library(dplyr)
library(tidyr)
library(pool)
library(RPostgres)
library(ggplot2)
library(ggtext)
library(osmdata)
library(stringr)
library(terra)
library(MASS)
library(lubridate)
library(performance)
library(sjPlot)
library(marginaleffects)
library(showtext)
library(fontawesome)
library(scales)
# Automatically download and load Rubik font
font_add_google("Rubik", "Rubik")
sysfonts::font_add(family = "Font Awesome 6 Solid",
                   regular = "C:/Users/lawrenceb/Downloads/fontawesome-free-6.7.2-desktop/fontawesome-free-6.7.2-desktop/otfs/Font Awesome 6 Free-Solid-900.otf")
showtext::showtext_auto()

colorBlind8  <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey")
colorBlind15 <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
                  "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
                  "#920000","#924900","#db6d00","#24ff24","#ffff6d")

setwd("C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis 2025/outputs")

# set up pool for postgis connections
pool <- dbPool(drv = RPostgres::Postgres(),
               host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
               port = 5432, dbname = "shared",
               user = "lawrenceball",
               password = "pgiskwt54171",
               sslmode = "prefer")

#kent <- st_read(pool, query = "SELECT * from admin_boundaries.kent_county") %>% st_transform(4326) #euan
journeys <- st_read(pool, query = "SELECT * from bugs_matter.journeys9") #euan
#journeys <- journeys[st_within(journeys, kent, sparse = FALSE), ]

journeys_raw <- st_read(pool, query = "SELECT * from bugs_matter.journeys")
#journeys_raw <- journeys_raw[st_within(journeys_raw, kent, sparse = FALSE), ]

summary(as.factor(journeys_raw$year))

users <- st_read(pool, query = "SELECT * from bugs_matter.user_data3")
#users <- users[st_within(users, kent, sparse = FALSE), ]

users <- subset(users, year(sign_up_date) > "2020" & year(sign_up_date) < "2025")
users$sign_up_year <- year(users$sign_up_date)
users_summary <- users %>%
  count(sign_up_year) %>%
  arrange(sign_up_year) %>%
  mutate(cumsum = cumsum(n))  # Compute cumulative sum
usersicon <- "&#xf0c0"
usersicon1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Solid\";'>{usersicon};</span>")
usersicon1

users_summary$icon <- usersicon1

# Create cumsum bar plot
ggplot(users_summary, aes(x = as.factor(sign_up_year), y = cumsum)) +
  geom_bar(stat = "identity", width=0.7) +
  geom_richtext(aes(label = icon), vjust = 1.4, size = 5, fill = NA, label.color = NA, text.color="white") +
  labs(title = "", x = "Year", y = "Total number of users") +
  theme_classic() +
  theme(text = element_text(family = "Rubik"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("Barplot of total users.pdf", width = 1800, height = 1200, units = "px", dpi=300)

journeys <- subset(journeys, year != "2020") #euan

nrow(subset(journeys, year == "2023"))
nrow(subset(journeys, year == "2024"))
sum(journeys$splat_count)

#euan
journeys$vehicle_cl <- as.factor(journeys$vehicle_cl)
journeys$year <- as.factor(journeys$year)
journeys$region <- as.factor(journeys$region)
journeys$country <- as.factor(journeys$country)
journeys$timeofday <- as.numeric(hms(journeys$timeofday))
journeys$midpoint_time <- as.Date(journeys$midpoint_time)
journeys$month <- month(journeys$midpoint_time)

a <- summary(journeys$year)
journeys_summary <- data.frame(year = c(2021,2022,2023,2024),
                               journeys = a)

journeys_summary$journeys_cumsum <- cumsum(journeys_summary$journeys)

caricon <- "&#xf5e4"
caricon1 <- glue::glue("<span style='font-family:\"Font Awesome 6 Solid\";'>{caricon};</span>")
caricon1

journeys_summary$icon <- caricon1

# Create cumsum bar plot
ggplot(journeys_summary, aes(x = as.factor(year), y = journeys_cumsum)) +
  geom_bar(stat = "identity", width=0.7) +
  geom_richtext(aes(label = icon), vjust = 1.4, size = 5, fill = NA, label.color = NA, text.color="white") +
  labs(title = "", x = "Year", y = "Total number of journeys") +
  theme_classic() +
  theme(text = element_text(family = "Rubik"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("Barplot of total journeys_Kent.pdf", width = 1800, height = 1200, units = "px", dpi=300)

summary(journeys$region)
journeys1 <- subset(journeys, !is.na(region)) #euan
journeys1$country <- droplevels(journeys1$country)
summary(journeys1$country)
journeys1 <- subset(journeys1, country %in% c("England", "Scotland", "Wales", "Northern Ireland"))
journeys1$country <- droplevels(journeys1$country)
summary(journeys1$country)

journeys1_summary <- aggregate(journeys1$year,
                               by = list(country = journeys1$country, year = journeys1$year),
                               FUN = length)  # Count journeys per (country, year)
# Rename column 'x' to 'journey_count' for clarity
colnames(journeys1_summary)[3] <- "journey_count"
journeys1_summary

journeys1_summary_region <- aggregate(journeys1$year,
                                      by = list(region = journeys1$region),
                               FUN = length)  # Count journeys per (region, year)
colnames(journeys1_summary_region)[3] <- "journey_count"
journeys1_summary_region

# Compute cumulative sum per country
journeys1_summary <- journeys1_summary[order(journeys1_summary$country, journeys1_summary$year), ]
journeys1_summary$cumulative_journeys <- ave(journeys1_summary$journey_count,
                                             journeys1_summary$country,
                                             FUN = cumsum)

ggplot(journeys1_summary, aes(x = as.factor(year), y = cumulative_journeys, fill = country)) +
  scale_fill_manual(values = colorBlind8) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Total number of journeys",
       fill = "Country") +
  theme_classic() +
  theme(text = element_text(family = "Rubik"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("Barplot of total journeys by country.pdf", width = 1800, height = 1200, units = "px", dpi=300)

# Compute cumulative sum per country
journeys1_summary_region <- journeys1_summary_region[order(journeys1_summary_region$region, journeys1_summary_region$year), ]
journeys1_summary_region$cumulative_journeys <- ave(journeys1_summary_region$journey_count,
                                             journeys1_summary_region$region,
                                             FUN = cumsum)

ggplot(journeys1_summary_region, aes(x = as.factor(year), y = cumulative_journeys, fill = region)) +
  scale_fill_manual(values = colorBlind15) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year",
       y = "Total number of journeys",
       fill = "Region") +
  theme_classic() +
  theme(text = element_text(family = "Rubik"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

ggsave("Barplot of total journeys by region.pdf", width = 1800, height = 1200, units = "px", dpi=300)

# # Subset the data for a specific month (e.g., March)
# june_data <- subset(journeys2, month %in% c(6,7,8))

###############################

summary(journeys1)

journeys2 <- journeys1 %>% rename(
  "Year" = year,
  "Distance" = distance,
  "Average.speed" = avg_speed,
  "Vehicle.type" = vehicle_cl,
  "Vehicle.height" = vehicle_he,
  "Time.of.day" = timeofday,
  "Day.of.year" = dayofyear,
  "Elevation" = elevation,
  "Temperature" = temp,
  "Longitude" = X,
  "Latitude" = Y,
  "Proportion.forest" = forest,
  "Proportion.shrubland" = shrubland,
  "Proportion.grassland" = grassland,
  "Proportion.wetland" = wetland,
  "Proportion.marine" = marine,
  "Proportion.arable" = arable,
  "Proportion.urban" = urban)

# Run the model
NB <- glm.nb(splat_count ~ Year +
               Distance +
               Average.speed +
               Vehicle.type +
               #Vehicle.height +
               Time.of.day +
               Day.of.year +
               Elevation +
               Temperature +
               #Longitude +
               #Latitude +
               Proportion.forest +
               #Proportion.shrubland +
               Proportion.grassland +
               Proportion.wetland +
               #Proportion.marine +
               Proportion.arable +
               #Proportion.pastureland +
               Proportion.urban +
               offset(log_cm_miles_offset),
             #data = subset(journeys2, country == "Scotland")
             #data = subset(journeys2, country == "Northern Ireland")
             #data = subset(journeys2, country == "England")
             #data = subset(journeys2, country == "Wales")
             data = subset(journeys2, region == "Yorkshire and the Humber")
             #data = journeys2
             )

summary(NB)

VIFtable <- performance::check_collinearity(NB, component = "count")
VIFtable

# estimates
est <- cbind(Estimate = coef(NB), confint(NB))
est
est1 <- round((1 - exp(est)) * 100, 3) # to show as decrease
format(est1, scientific=F) #this one for reporting
est2 <- round(exp(est) * 100, 3) #
est2
est3 <- round(exp(est), 3) #
est3

plot_model(NB,
           vline.color = "grey",
           type="est",
           show.values = T,
           show.p = T,
           sort.est=T,
           title="",
           ci.lvl=0.95,
           line.size=0.3,
           show.zeroinf = F,
           digits = 3,
           value.offset = 0.35,
           value.size = 2.5,
           dot.size=0.8) +
  theme_classic() +
  labs(x = "Explanatory variable") +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        text = element_text(family = "Rubik"))

ggsave("Forest plot_Kent.pdf", width = 1800, height = 1600, units = "px", dpi=300)

?plot_model
plot_model(NB, type="pred", terms="Year", colors="black") +
  theme_classic() +
  labs(y = "Splat count", x = "Year", title = NULL) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        text = element_text(family = "Rubik"))
ggsave("Predictions of splat count_Kent.pdf", width = 1800, height = 1200, units = "px", dpi=300)

#Splat rate plots
ggplot(data = journeys2, aes(x = Day.of.year)) +
  geom_line(lwd=0.5, data = subset(journeys2, Year == "2021"), aes(y=cummean(splat_rate), color="2021")) +
  geom_line(lwd=0.5, data = subset(journeys2, Year == "2022"), aes(y=cummean(splat_rate), color="2022")) +
  geom_line(lwd=0.5, data = subset(journeys2, Year == "2023"), aes(y=cummean(splat_rate), color="2023")) +
  geom_line(lwd=0.5, data = subset(journeys2, Year == "2024"), aes(y=cummean(splat_rate), color="2024")) +
  scale_color_manual(values=c(colorBlind8), name="Year") +
  theme_minimal() +
  labs(x = "Journey date", y = "Splat rate",
       #title = "Insect splats",
       #subtitle = "Cumulative mean of insect splat rate in each survey year",
       #caption = paste("Data fetched on:", filedate)
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) +
  #scale_y_continuous(labels=comma) +
  scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                     minor_breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
ggsave("Figures/Cumulative mean of insect splat rate in each survey year.pdf", width = 20, height = 10, units = "cm")
ggsave("Figures/Cumulative mean of insect splat rate in each survey year.png", width = 20, height = 10, units = "cm")


#Regional analysis - a function to run models for each region and store estimates and CI.
regions <- unique(as.character(subset(journeys2, country == "England")$region))
regions

regional_models <- function(x){
  NB_region <- glm.nb(splat_count ~ Year +
                        Distance +
                        Average.speed +
                        Vehicle.type +
                        #Vehicle.height +
                        Time.of.day +
                        Day.of.year +
                        Elevation +
                        Temperature +
                        #Longitude +
                        #Latitude +
                        Proportion.forest +
                        #Proportion.shrubland +
                        Proportion.grassland +
                        Proportion.wetland +
                        #Proportion.marine +
                        Proportion.arable +
                        #Proportion.pastureland +
                        Proportion.urban +
                        offset(log_cm_miles_offset),
                      data=journeys2, region == x)
  est <- cbind(Estimate = coef(NB_region), confint(NB_region))
  est
  p <- scales::pvalue(summary(NB_region)$coefficients[,4])
  p
  mainresult_2122_region <- c(round((1 - exp(est)[2, ]) * 100, 1), p[2])
  mainresult_2123_region <- c(round((1 - exp(est)[3, ]) * 100, 1), p[3])
  mainresult_2124_region <- c(round((1 - exp(est)[4, ]) * 100, 1), p[4])
  NB_region22 <- glm.nb(splat_count ~ Year +
                          Distance +
                          Average.speed +
                          Vehicle.type +
                          #Vehicle.height +
                          Time.of.day +
                          Day.of.year +
                          Elevation +
                          Temperature +
                          #Longitude +
                          #Latitude +
                          Proportion.forest +
                          #Proportion.shrubland +
                          Proportion.grassland +
                          Proportion.wetland +
                          #Proportion.marine +
                          Proportion.arable +
                          #Proportion.pastureland +
                          Proportion.urban +
                          offset(log_cm_miles_offset),
                        data=journeys2, region == x)
  est22 <- cbind(Estimate = coef(NB_region22), confint(NB_region22))
  est22
  p22 <- scales::pvalue(summary(NB_region22)$coefficients[,4])
  p22
  mainresult_2223_region <- c(round((1 - exp(est22)[3, ]) * 100, 1), p22[3])
  NB_region23 <- glm.nb(splat_count ~ Year +
                        Distance +
                        Average.speed +
                        Vehicle.type +
                        #Vehicle.height +
                        Time.of.day +
                        Day.of.year +
                        Elevation +
                        Temperature +
                        #Longitude +
                        #Latitude +
                        Proportion.forest +
                        #Proportion.shrubland +
                        Proportion.grassland +
                        Proportion.wetland +
                        #Proportion.marine +
                        Proportion.arable +
                        #Proportion.pastureland +
                        Proportion.urban +
                        offset(log_cm_miles_offset),
                      data=journeys2, region == x)
  est23 <- cbind(Estimate = coef(NB_region23), confint(NB_region23))
  est23
  p23 <- scales::pvalue(summary(NB_region23)$coefficients[,4])
  p23
  mainresult_2324_region <- c(round((1 - exp(est23)[4, ]) * 100, 1), p23[4])
  out <- data.frame(region = x,
                    "2021-2022" = mainresult_2122_region,
                    "2021-2023" = mainresult_2123_region,
                    "2021-2024" = mainresult_2124_region,
                    "2022-2023" = mainresult_2223_region,
                    "2023-2024" = mainresult_2324_region)
  return(out)
}

regions
regional_results <- do.call("rbind", lapply(regions[c(1:7,9)], regional_models))
regional_results

write.csv(regional_results, "regional_results.csv")
write.csv(journeys1_summary_region, "regional_journey_counts.csv")


regional_results$decade <- ((as.numeric(regional_results$X2021.2022)/(2022-2021))*10 +
                               (as.numeric(regional_results$X2021.2023)/(2023-2021))*10 +
                               (as.numeric(regional_results$X2021.2024)/(2024-2021))*10) / 3
