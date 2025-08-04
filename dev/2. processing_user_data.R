library(sf)
library(dplyr)
library(tidyr)
library(pool)
library(RPostgres)
library(httr)

# PARTICIPATION ANALYSIS
# set up pool for postgis connections
pool <- dbPool(drv = RPostgres::Postgres(),
               host = "kwt-postgresql-azdb-1.postgres.database.azure.com",
               port = 5432, dbname = "shared",
               user = Sys.getenv("kwt_user"),
               password = Sys.getenv("kwt_password"),
               sslmode = "prefer")

# read in postcodes with coords in df.
pcodes <- read.csv("C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis 2025/ONSPD_FEB_2024_UK/Data/ONSPD_FEB_2024_UK.csv", header=T)
head(pcodes)
pcodes1 <- pcodes[c("pcd", "lat", "long", "oseast1m", "osnrth1m")]
pcodes1$pcd <- gsub(" ", "", pcodes1$pcd, fixed = TRUE)

#st_write(obj = pcodes1, dsn = pool, Id(schema="bugs_matter", table = "postcodes"), drop=TRUE)

# read in user data (latest version) and spreadsheet with some users that are missing postcodes from the main version.
userdata <- read.csv("C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis 2025/bugsMatterDashboard-users-7bbba7a3-92c8-449f-9f70-a465bfc8ae76.csv")
userdata1 <- userdata %>% mutate(postcode = na_if(postcode, ""))
userdata_w_postcodes <- read.csv("C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis 2025/signups.w.postcodes.csv", header=T)
userdata_w_postcodes1 <- userdata_w_postcodes %>%
  rename(User.ID = user_id, w_postcode = postcode) %>%
  select(User.ID, w_postcode) %>%
  mutate(w_postcode = na_if(w_postcode, ""))
userdata2 <- left_join(userdata1, userdata_w_postcodes1, by = "User.ID") %>%
  mutate(postcode_final = ifelse(is.na(postcode), w_postcode, postcode)) %>%
  dplyr::select(-w_postcode, -postcode) %>%
  rename(postcode = postcode_final)

# users_manual_postcodes <- subset(userdata2, is.na(postcode))
# write.csv(users_manual_postcodes, "users_manual_postcodes.csv")
users_manual_postcodes <- read.csv("users_manual_postcodes.csv")
users_manual_postcodes1 <- users_manual_postcodes %>% select(User.ID, postcode)
users_manual_postcodes1 <- users_manual_postcodes1 %>% rename(man_postcode = postcode)
userdata3 <- left_join(userdata2, users_manual_postcodes1, by = "User.ID") %>%
  mutate(postcode_final = ifelse(is.na(postcode), man_postcode, postcode)) %>%
  dplyr::select(-man_postcode, -postcode) %>%
  rename(postcode = postcode_final)

userdata3$Sign.Up.Date <- as.Date(userdata3$Sign.Up.Date)
userdata3 <- userdata3[order(as.Date(userdata3$Sign.Up.Date, format="%m/%d/%Y")),]
userdata3$postcode <- toupper(gsub(" ", "", userdata3$postcode, fixed = TRUE))
sum(is.na(userdata3$postcode))
userdata3 <- userdata3 %>% mutate(address = na_if(address, ""))

# create list of postcodes
list <- as.list(unique(userdata3$postcode))
datamap <- subset(pcodes1, pcodes1$pcd %in% list, select=c("lat","long","pcd", "oseast1m", "osnrth1m"))
names(datamap)[names(datamap) == "pcd"] <- "postcode"
userdata4 <- merge(userdata3, datamap, by="postcode", all.x=T)
userdata4[which(userdata4$postcode == "GY46QJ"), "lat"] <- 49.4448
userdata4[which(userdata4$postcode == "GY46QJ"), "long"] <- 2.5557
userdata4[which(userdata4$postcode == "IM44BG"), "lat"] <- 54.1778
userdata4[which(userdata4$postcode == "IM44BG"), "long"] <- 4.5559
userdata4[which(userdata4$postcode == "IM73EB"), "lat"] <- 54.3876
userdata4[which(userdata4$postcode == "IM73EB"), "long"] <- 4.4367
userdata4[which(userdata4$postcode == "JE37JU"), "lat"] <- 49.2154
userdata4[which(userdata4$postcode == "JE37JU"), "long"] <- 2.1894
userdata4[which(userdata4$User.ID == "157309"), "postcode"] <- "KW96LX"
userdata4[which(userdata4$postcode == "KW96LX"), "lat"] <- 58.0634
userdata4[which(userdata4$postcode == "KW96LX"), "long"] <- 3.9956
userdata5 <- st_as_sf(userdata4, coords=c("long", "lat"), crs=4326, na.fail=F)
userdata5 <-  userdata5[order(userdata5$Sign.Up.Date), ]
summary(userdata5)
plot(userdata5[7])
userdata5a <- userdata5 %>% st_transform(27700)

url <- parse_url("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services")
url$path <- paste(url$path, "Counties_and_Unitary_Authorities_December_2024_Boundaries_UK_BUC/FeatureServer/0/query", sep = "/")
url$query <- list(where = "FID > 0",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)
counties <- st_read(request) %>% st_transform(27700)
plot(counties[3])

url <- parse_url("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services")
url$path <- paste(url$path, "Regions_December_2023_Boundaries_EN_BSC/FeatureServer/0/query", sep = "/")
url$query <- list(where = "FID > 0",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)
regions_en <- st_read(request) %>% st_transform(27700)
plot(regions_en[3])

url <- parse_url("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services")
url$path <- paste(url$path, "Countries_December_2023_Boundaries_UK_BUC/FeatureServer/0/query", sep = "/")
url$query <- list(where = "FID > 0",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)
countries <- st_read(request) %>% st_transform(27700)
plot(countries[3])

userdata5a$county <- NULL

userdata6 <- userdata5a %>%
  st_join(counties["CTYUA24NM"] %>% rename(county = CTYUA24NM)) %>%
  st_join(regions_en["RGN23NM"] %>% rename(region = RGN23NM)) %>%
  st_join(countries["CTRY23NM"] %>% rename(country = CTRY23NM))

userdata6$country <- as.factor(userdata6$country)
summary(userdata6$country)

userdata6$county <- as.factor(userdata6$county)
userdata6$region <- as.factor(userdata6$region)

journeys_raw <- st_read(pool, query = "SELECT * from bugs_matter.journeys2")

#Add a column for the count of journeys for each user.
user_journey_count <- journeys_raw %>%
  st_drop_geometry() %>%
  dplyr::select(userId) %>%
  mutate(journeys = 1) %>%
  group_by(userId) %>%
  summarise(journeys = sum(journeys)) %>%
  ungroup() %>%
  mutate(userID1 = as.integer(userId)) %>%
  dplyr::select(-userId)

userdata7 <- left_join(userdata6, user_journey_count, by=c("User.ID" = "userID1"))

userdata8 <- userdata7 %>% rename(sign_up_date = Sign.Up.Date,
                                  user_id = User.ID,
                                  marketing_consent = Marketing.Consent,
                                  name = Name,
                                  username = Username,
                                  email = Email)

st_write(obj = userdata8, dsn = pool, Id(schema="bugs_matter", table = "user_data2"), append=FALSE)

dbExecute(pool, 'GRANT SELECT, INSERT, UPDATE, DELETE ON bugs_matter.user_data2 TO "bugsMatterDashboardReadOnly";')

###################################################







### sign up did/did not journeys analysis
didnojourneys <- nrow(userdata6 %>% filter(is.na(journeys)))
didjourneys <- nrow(userdata6[which(!is.na(userdata6$journeys)), ])
conversion_rate_overall <- round(didjourneys/nrow(userdata6) * 100, 2)
mean_journeys_per_participant <- round(mean(userdata6[which(!is.na(userdata6$journeys)), ]$journeys), 2)

#create a temporary subset, (temporary such that it doesnt have coordinates yet).
userdata21_temp <- subset(userdata6, Sign.Up.Date >= "2021-01-01" & Sign.Up.Date <= "2021-12-31")
userdata21_temp$doy <- as.numeric(format(as.Date(userdata21_temp$Sign.Up.Date), "%j"))
userdata22_temp <- subset(userdata6, Sign.Up.Date >= "2022-01-01"& Sign.Up.Date <= "2022-12-31")
userdata22_temp$doy <- as.numeric(format(as.Date(userdata22_temp$Sign.Up.Date), "%j"))
userdata23_temp <- subset(userdata6, Sign.Up.Date >= "2023-01-01"& Sign.Up.Date <= "2023-12-31")
userdata23_temp$doy <- as.numeric(format(as.Date(userdata23_temp$Sign.Up.Date), "%j"))
userdata24_temp <- subset(userdata6, Sign.Up.Date >= "2024-01-01"& Sign.Up.Date <= "2024-12-31")
userdata24_temp$doy <- as.numeric(format(as.Date(userdata24_temp$Sign.Up.Date), "%j"))

userdata21_temp_didnojourneys <- nrow(userdata21_temp %>% filter(is.na(journeys)))
userdata21_temp_didjourneys <- nrow(userdata21_temp[which(!is.na(userdata21_temp$journeys)), ])
userdata22_temp_didnojourneys <- nrow(userdata22_temp %>% filter(is.na(journeys)))
userdata22_temp_didjourneys <- nrow(userdata22_temp[which(!is.na(userdata22_temp$journeys)), ])
userdata23_temp_didnojourneys <- nrow(userdata23_temp %>% filter(is.na(journeys)))
userdata23_temp_didjourneys <- nrow(userdata23_temp[which(!is.na(userdata23_temp$journeys)), ])
userdata24_temp_didnojourneys <- nrow(userdata24_temp %>% filter(is.na(journeys)))
userdata24_temp_didjourneys <- nrow(userdata24_temp[which(!is.na(userdata24_temp$journeys)), ])

participants_21 <- unique(subset(journeys1, year == "2021")$userId)
participants_22 <- unique(subset(journeys1, year == "2022")$userId)
participants_23 <- unique(subset(journeys1, year == "2023")$userId)
participants_24 <- unique(subset(journeys1, year == "2024")$userId)

participants_21_1 <- subset(userdata6, User.ID %in% participants_21)
participants_22_1 <- subset(userdata6, User.ID %in% participants_22)
participants_23_1 <- subset(userdata6, User.ID %in% participants_23)
participants_24_1 <- subset(userdata6, User.ID %in% participants_24)

participants_21_1$signupyear <- format(participants_21_1$Sign.Up.Date, format="%Y")
participants_22_1$signupyear <- format(participants_22_1$Sign.Up.Date, format="%Y")
participants_23_1$signupyear <- format(participants_23_1$Sign.Up.Date, format="%Y")
participants_24_1$signupyear <- format(participants_24_1$Sign.Up.Date, format="%Y")

mean_journeys_per_user_21 <- round(mean(participants_21_1$journeys), 2)
mean_journeys_per_user_22 <- round(mean(participants_22_1$journeys), 2)
mean_journeys_per_user_23 <- round(mean(participants_23_1$journeys), 2)
mean_journeys_per_user_24 <- round(mean(participants_24_1$journeys), 2)

#Make a table of participation results.
participation_table <- data.frame("step" = "Number of new sign-ups",
                                  "Signed up in 2021" = nrow(userdata21_temp),
                                  "Signed up in 2022" = nrow(userdata22_temp),
                                  "Signed up in 2023" = nrow(userdata23_temp),
                                  "Signed up in 2024" = nrow(userdata24_temp))
participation_table
participation_table1 <- rbind(participation_table,
                              data.frame("step" = "Number of participants in 2021 survey",
                                         "Signed up in 2021" = nrow(participants_21_1),
                                         "Signed up in 2022" = "NA",
                                         "Signed up in 2023" = "NA",
                                         "Signed up in 2024" = "NA"))
participation_table1
participation_table2 <- rbind(participation_table1,
                              data.frame("step" = "Number of participants in 2022 survey",
                                         "Signed up in 2021" = nrow(subset(participants_22_1, signupyear == "2021")),
                                         "Signed up in 2022" = nrow(subset(participants_22_1, signupyear == "2022")),
                                         "Signed up in 2023" = "NA",
                                         "Signed up in 2024" = "NA"))
participation_table2
participation_table3 <- rbind(participation_table2,
                              data.frame("step" = "Number of participants in 2023 survey",
                                         "Signed up in 2021" = nrow(subset(participants_23_1, signupyear == "2021")),
                                         "Signed up in 2022" = nrow(subset(participants_23_1, signupyear == "2022")),
                                         "Signed up in 2023" = nrow(subset(participants_23_1, signupyear == "2023")),
                                         "Signed up in 2024" = "NA"))
participation_table3
participation_table4 <- rbind(participation_table3,
                              data.frame("step" = "Number of participants in 2024 survey",
                                         "Signed up in 2021" = nrow(subset(participants_24_1, signupyear == "2021")),
                                         "Signed up in 2022" = nrow(subset(participants_24_1, signupyear == "2022")),
                                         "Signed up in 2023" = nrow(subset(participants_24_1, signupyear == "2023")),
                                         "Signed up in 2024" = nrow(subset(participants_24_1, signupyear == "2024"))))

participation_table4
participation_table4$Signed.up.in.2022 <- as.integer(participation_table4$Signed.up.in.2022)
participation_table4$Signed.up.in.2023 <- as.integer(participation_table4$Signed.up.in.2023)
participation_table4$Signed.up.in.2024 <- as.integer(participation_table4$Signed.up.in.2024)
participation_table4

participation_table5 <- rbind(participation_table4,
                              data.frame("step" = "Conversion rate (proportion of users that signed up and did one or more journeys in the same year)",
                                         "Signed up in 2021" = paste0(round(participation_table4[2,2] / participation_table4[1,2] * 100, 1), "%"),
                                         "Signed up in 2022" = paste0(round(participation_table4[3,3] / participation_table4[1,3] * 100, 1), "%"),
                                         "Signed up in 2023" = paste0(round(participation_table4[4,4] / participation_table4[1,4] * 100, 1), "%"),
                                         "Signed up in 2024" = paste0(round(participation_table4[5,5] / participation_table4[1,5] * 100, 1), "%")
                              ))
participation_table5
write.csv(participation_table5, "C:/Users/lawrenceb/Kent Wildlife Trust/EVD - Bugs Matter - General/10. Reporting/1. Analysis/Analysis 2025/outputs/kent_participation_table.csv")











# Summary stats about time between sign-up and first journey.
journeys212223_9a <- journeys212223_9
journeys212223_9a$User.ID <- journeys212223_9a$userId
journeys212223_9b <- merge(journeys212223_9a,
                           userdata2[c("User.ID", "Sign.Up.Date")])
signupstart <- aggregate(journeys212223_9b$starttime ~ journeys212223_9b$User.ID , FUN = min, simplify=T, drop=F)
signupstart$User.ID <- signupstart$`journeys212223_9b$User.ID`
signupstart$starttime <- signupstart$`journeys212223_9b$starttime`
signupstart1 <- merge(signupstart, userdata2[c("User.ID", "Sign.Up.Date")])
signupstart1$datediff <- as.Date(signupstart1$starttime) - as.Date(signupstart1$Sign.Up.Date)
signupstart1
signupstart2 <- subset(signupstart1, Sign.Up.Date >= "2021-01-01" & Sign.Up.Date <= "2023-12-31")
hist(as.numeric(signupstart2$datediff))

##############why are there negative numbers of days here? Because some people have recorded journeys before signing up.
#Plot histogram of Time between sign-up and first journey
ggplot(signupstart2, aes(x=datediff)) + geom_histogram(fill = "#72DB41", color=1, binwidth = 10) +
  theme_minimal() +
  xlim(0,900) +
  labs(x = "Time between sign-up and first journey (days)", y = "Count of users",
       #title = "Participation",
       #subtitle = "Histogram showing counts of users in bins of time between sign-up and recording a first journey"
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
ggsave("Figures/Histogram showing counts of users in bins of time between sign-up and recording a first journey.pdf", width = 20, height = 10, units = "cm")
ggsave("Figures/Histogram showing counts of users in bins of time between sign-up and recording a first journey.png", width = 20, height = 10, units = "cm")


###############################







userdata7$county <- as.factor(userdata7$county)
hertsusers_all <- subset(userdata7, county == "Hertfordshire")
hertsusers_mc <- subset(userdata7, county == "Hertfordshire" & Marketing.Consent == "TRUE")
write.csv(hertsusers_mc[c("postcode",
                          "District",
                          "county",
                          "Name.x",
                          "Email",
                          "Sign.Up.Date",
                          "Marketing.Consent",
                          "address",
                          "journeys")], "hertsusers.csv")

kentusers_all <- subset(userdata7, county == "Kent")
kentusers_mc <- subset(userdata7, county == "Kent" & Marketing.Consent == "TRUE")
write.csv(kentusers_mc[c("postcode",
                         "District",
                         "county",
                         "Name.x",
                         "Email",
                         "Sign.Up.Date",
                         "Marketing.Consent",
                         "address",
                         "journeys")], "kentusers.csv")

#subset data by years.
userdata21 <- subset(userdata7, Sign.Up.Date >= "2021-01-01" & Sign.Up.Date <= "2021-12-31")
userdata21$doy <- as.numeric(format(as.Date(userdata21$Sign.Up.Date), "%j"))
userdata22 <- subset(userdata7, Sign.Up.Date >= "2022-01-01"& Sign.Up.Date <= "2022-12-31")
userdata22$doy <- as.numeric(format(as.Date(userdata22$Sign.Up.Date), "%j"))
userdata23 <- subset(userdata7, Sign.Up.Date >= "2023-01-01"& Sign.Up.Date <= "2023-12-31")
userdata23$doy <- as.numeric(format(as.Date(userdata23$Sign.Up.Date), "%j"))

#userdata21_county <- st_intersection(userdata21, subset(admin_areas, County == params$county))
#userdata22_county <- st_intersection(userdata22, subset(admin_areas, County == params$county))

#userdata22_england <- st_intersection(userdata22, subset(countryboundaries2, country == "England"))
#userdata22_scotland <- st_intersection(userdata22, subset(countryboundaries2, country == "Scotland"))
#userdata22_wales <- st_intersection(userdata22, subset(countryboundaries2, country == "Wales"))
#userdata22_northerni <- st_intersection(userdata22, subset(countryboundaries2, country == "Northern I"))

participant_count21 <- length(unique(subset(journeys212223_9, year == "2021")$userId))
participant_count22 <- length(unique(subset(journeys212223_9, year == "2022")$userId))
participant_count23 <- length(unique(subset(journeys212223_9, year == "2023")$userId))

#Plots

#Sort journeys by date and create cumsum column.
userdata8 <- userdata7 %>% arrange(Sign.Up.Date)
userdata8$row1 <- 1
userdata8$cumsum <- cumsum(userdata8$row1)

userdata9 <- userdata8 %>% group_by(country) %>% mutate(cumsum = cumsum(row1))
#userdata7 <- ddply(userdata6, .(country), transform, cumsum = cumsum(row1))
userdata8a <- userdata8
userdata8a$country <- "UK"
userdata9a <- rbind(userdata9, userdata8a)

mean_journeys_per_user_england <- round(mean(subset(userdata8, !is.na(userdata8$journeys) & country == "England")$journeys), 2)
mean_journeys_per_user_scotland <- round(mean(subset(userdata8, !is.na(userdata8$journeys) & country == "Scotland")$journeys), 2)
mean_journeys_per_user_northernireland <- round(mean(subset(userdata8, !is.na(userdata8$journeys) & country == "Northern Ireland")$journeys), 2)
mean_journeys_per_user_wales <- round(mean(subset(userdata8, !is.na(userdata8$journeys) & country == "Wales")$journeys), 2)

x_dates <- dmy(c("01/01/21","01/06/21","01/09/21","01/01/22","01/06/22","01/09/22","01/01/23","01/06/23","01/09/23"))

ggplot(subset(userdata9a, Sign.Up.Date >= "2021-01-01" & Sign.Up.Date <= "2023-12-31"), aes(x=Sign.Up.Date, y=cumsum, colour=country, group=country)) +
  annotate("rect", xmin=as.Date("2021-06-01"), xmax=as.Date("2021-09-01"), ymin=0, ymax=Inf, alpha=0.2, fill="#72DB41") +
  annotate("rect", xmin=as.Date("2022-06-01"), xmax=as.Date("2022-09-01"), ymin=0, ymax=Inf, alpha=0.2, fill="#72DB41") +
  annotate("rect", xmin=as.Date("2023-06-01"), xmax=as.Date("2023-09-01"), ymin=0, ymax=Inf, alpha=0.2, fill="#72DB41") +
  geom_line(lwd=0.5) +
  scale_color_manual(values=c(colorBlind7[1:4], "black"), name="Country", na.translate = F) +
  theme_minimal() +
  labs(x = "Date", y = "Count of sign-ups",
       #title = "Participation",
       #subtitle = "Cumulative count of sign-ups in each country over the lifetime of the Bugs Matter app",
       #caption = paste("Data fetched on:", filedate)
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust=0)) +
  scale_x_date(breaks = x_dates, minor_breaks = "1 month", date_labels = "%b %Y")

ggsave("Figures/Cumulative count of sign-ups in each country over the lifetime of the Bugs Matter app.pdf", width = 20, height = 10, units = "cm")
ggsave("Figures/Cumulative count of sign-ups in each country over the lifetime of the Bugs Matter app.png", width = 20, height = 10, units = "cm")

userdata10 <- ddply(userdata9, .(region), transform, cumsum = cumsum(row1))

ggplot(subset(userdata10, Sign.Up.Date >= "2021-01-01" & Sign.Up.Date <= "2023-12-31" & country == "England"), aes(x=Sign.Up.Date, y=cumsum, colour=region, group=region)) +
  annotate("rect", xmin=as.Date("2021-06-01"), xmax=as.Date("2021-09-01"), ymin=0, ymax=Inf, alpha=0.2, fill="#72DB41") +
  annotate("rect", xmin=as.Date("2022-06-01"), xmax=as.Date("2022-09-01"), ymin=0, ymax=Inf, alpha=0.2, fill="#72DB41") +
  annotate("rect", xmin=as.Date("2023-06-01"), xmax=as.Date("2023-09-01"), ymin=0, ymax=Inf, alpha=0.2, fill="#72DB41") +
  geom_line(lwd=0.5) +
  scale_color_manual(values=c(colorBlind7, "grey", "black"), name="Region", na.translate = F) +
  theme_minimal() +
  labs(x = "Date", y = "Count of sign-ups",
       #title = "Participation",
       #subtitle = "Cumulative count of sign-ups in each region in England over the lifetime of the Bugs Matter app",
       #caption = paste("Data fetched on:", filedate)
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust=0)) +
  scale_x_date(breaks = x_dates, minor_breaks = "1 month", date_labels = "%b %Y")
ggsave("Figures/Cumulative count of sign-ups in each region in England over the lifetime of the Bugs Matter app.pdf", width = 20, height = 10, units = "cm")
ggsave("Figures/Cumulative count of sign-ups in each region in England over the lifetime of the Bugs Matter app.png", width = 20, height = 10, units = "cm")

user_county <- aggregate(userdata10$row1 ~ userdata10$county, FUN=sum, simplify=T, drop=F)
countyboundaries1 <- countyboundaries %>% arrange(Name)
names(countyboundaries1)[names(countyboundaries1) == "Name"] <- "county"
names(user_county)[1] <- "county"
names(user_county)[2] <- "signups"
user_county
missingcounty <- data.frame(county = "City and County of the City of London",
                            signups = "NA")
missingcounty
user_county_1 <- rbind(user_county, missingcounty)
countyboundaries2 <- merge(countyboundaries1, user_county_1)
countyboundaries3 <- st_simplify(countyboundaries2, preserveTopology = FALSE, dTolerance = 1000)
countyboundaries3$signups <- as.numeric(countyboundaries3$signups)
write.csv(as.data.frame(st_drop_geometry(countyboundaries3)), "Figures/users_by_county.csv")

ggplot() +
  geom_sf(data = countyboundaries3, aes(fill = signups), colour="black", size=0.05, inherit.aes = FALSE) +
  theme_minimal() +
  labs(x = NULL, y = NULL,
       #title = "Participation",
       #subtitle = "Heat map of total sign-ups for each county over the lifetime of the Bugs Matter app"
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) +
  theme(legend.margin = margin(1, 1, 1, 4),
        legend.key=element_rect(colour="red")) +
  labs(fill="Count of sign-ups") +
  scale_fill_viridis_c(limits=c(0,max(countyboundaries3$signups)),
                       #breaks=c(0,100,200,300,400,500,600),
                       guide=guide_colorbar(frame.colour="black",
                                            draw.ulim=T,
                                            barwidth = 0.6,
                                            barheight = 10,
                                            title.vjust = 3,
                                            ticks.colour = "black",
                                            draw.llim=T))
ggsave("Figures/Heat map of total sign-ups for each county over the lifetime of the Bugs Matter app.pdf", width = 20, height = 18, units = "cm")
ggsave("Figures/Heat map of total sign-ups for each county over the lifetime of the Bugs Matter app.png", width = 20, height = 18, units = "cm")
