# This script will load country level data and save it into a country-level
# dataset that will get merged in code_variables.R

#Currently we are only looking at the Human Development Index

# Load Libraries ----------------------------------------------------------

library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","pewdata_combined.RData"))

#get the names of all countries in our sample from the pew data
sample_countries <- as.character(unique(pew$country))


# HDI Data from UNDP ------------------------------------------------------

# why cant these international organizations design machine-readable data?
hdi <- read_csv(here("analysis","input","undp","hdi_undp.csv"),
                skip=1, na="..")[,c(2,seq(from=3,to=59, by=2))]


#how many missing values in 2010
summary(hdi$`2010`)

#fix some country names for matching
hdi$Country[hdi$Country=="Guinea-Bissau"] <- "Guinea Bissau"
hdi$Country[hdi$Country=="Palestine, State of"] <- "Palestinian Territories"
hdi$Country[hdi$Country=="Russian Federation"] <- "Russia"
hdi$Country[hdi$Country=="Tanzania (United Republic of)"] <- "Tanzania"
#use Serbia's value for Kosovo
hdi$Country[hdi$Country=="Serbia"] <- "Kosovo"

#check to make sure we have all countries
sample_countries[!(sample_countries %in% hdi$Country)]

#trim the data to just the countries I need, rename, etc
hdi <- subset(hdi, Country %in% sample_countries)
summary(hdi$`2010`)
hdi <- subset(hdi, select=c("Country","2010"))
colnames(hdi) <- c("country","hdi")

hdi$hdi_scaled <- scaleIndVariable(hdi$hdi)

# WDI Data from World Bank ------------------------------------------------

wdi <- read_csv(here("analysis","input","wdi",
                     "2cafb383-199a-4c4b-938a-f4df8e52b362_Data.csv"),
                n_max=1085, na="..")
colnames(wdi) <- c("year","year_code", "country", "country_code", "gdpcap",
                   "oil_rent_pct")

#check to make sure we have all countries
sample_countries[!(sample_countries %in% wdi$country)]

#fix names
wdi$country[wdi$country=="Egypt, Arab Rep."] <- "Egypt"
wdi$country[wdi$country=="Guinea-Bissau"] <- "Guinea Bissau"
wdi$country[wdi$country=="Kyrgyz Republic"] <- "Kyrgyzstan"
wdi$country[wdi$country=="Russian Federation"] <- "Russia"
wdi$country[wdi$country=="West Bank and Gaza"] <- "Palestinian Territories"

wdi <- subset(wdi, country %in% sample_countries)

#what am I looking at in terms of missing values in each year
tapply(is.na(wdi$gdpcap), wdi$year, sum)
tapply(is.na(wdi$oil_rent_pct), wdi$year, sum)

#2010 has no missing data so lets take 2010
wdi <- subset(wdi, year==2010,
              select=c("country","gdpcap","oil_rent_pct"))

wdi$lgdpcap <- log(wdi$gdpcap)
wdi$lgdpcap_scaled <- scaleIndVariable(wdi$lgdpcap)
wdi$oil_rent_pct_scaled <- scaleIndVariable(wdi$oil_rent_pct)

# Save data ---------------------------------------------------------------

country_data <- merge(hdi, wdi, all.x=TRUE, all.y=TRUE)

save(country_data, file=here("analysis","output","country_data.RData"))
