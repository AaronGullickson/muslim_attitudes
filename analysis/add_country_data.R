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
#standardize scale of hdi by country
hdi$hdi_scaled <- (hdi$hdi-mean(hdi$hdi))/(2*sd(hdi$hdi))
summary(hdi$hdi_scaled)



# Save data ---------------------------------------------------------------

country_data <- hdi

save(country_data, file=here("analysis","output","country_data.RData"))
