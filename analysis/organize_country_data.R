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

# Religious Distributions -------------------------------------------------

# The raw data come from the Pew Report on the Global Religious Landscape.
# The raw distributions of religious affiliation by country were extracted
# from the PDF report using Tabula. Tabula did a good job but there is 
# still considerable cleaning to be done from within R.
#
# http://www.pewforum.org/2012/12/18/global-religious-landscape-exec/


# extract table from first page of example PDF
#the tabulizer package is awesome. The stream method here solved my problem
#with cutting off country names
extract <- extract_tables(here("analysis","input","pew_data_orig",
                               "global-religion-full.pdf"), 
                          pages = 45:50, method="stream")
#this comes back as a list with a table per page. Lets check the dimensions
lapply(extract, dim)

#looks good and interactive exploration looked good as well, so lets combine
global_relig <- NULL
for(page in extract) {
  global_relig <- rbind(global_relig, page)
}
#remove header rows that are either "" or "COUNTRY" in first column
global_relig <- global_relig[global_relig[,1]!="" 
                             & global_relig[,1]!="COUNTRY",]

#now variable names
colnames(global_relig) <- c("country","population","christian","muslim",
                            "unaffiliated","hindu","buddhist","folk","other",
                            "jewish")


#now clean up the strings so I can convert to numeric values
global_relig[,"population"] <- gsub("< 10,000","10000",
                                    global_relig[,"population"])
global_relig[,"population"] <- gsub(",","",
                                    global_relig[,"population"])
global_relig[,3:10] <-  gsub(" %","", global_relig[,3:10])
global_relig[,3:10] <-  gsub("< 0.1","0", global_relig[,3:10])
global_relig[,3:10] <-  gsub(">99.0","100", global_relig[,3:10])
global_relig <- as_tibble(global_relig)
global_relig[,2:10] <- lapply(global_relig[,2:10], as.numeric)

#check for missing values
lapply(global_relig, function(x) {sum(is.na(x))})

#How do country names match up?
sample_countries[!(sample_countries %in% global_relig$country)]

#A few name changes are necessary for matching
global_relig$country[global_relig$country=="Bosnia-Herzegovina"] <- "Bosnia and Herzegovina"
global_relig$country[global_relig$country=="Palestinian territories"] <- "Palestinian Territories"

#drop regions
global_relig <- subset(global_relig,
                       country!="Asia-Pacific" & country!="Europe" &
                         country!="Latin America-Caribbean" & 
                         country!= "Middle East-North Africa" &
                         country!="North America" &
                         country!="Sub-Saharan Africa" & 
                         country!="World")

#get key variable
global_relig$pct_muslim <- global_relig$muslim
global_relig$muslim_pop <- (global_relig$pct_muslim/100)*global_relig$population

#what percent of the global Muslim population is in our dataset
sum(global_relig$muslim_pop[global_relig$country %in% sample_countries])/
  sum(global_relig$muslim_pop)

global_relig <- subset(global_relig, country %in% sample_countries,
                       select=c("country", "pct_muslim"))

# Save data ---------------------------------------------------------------

country_data <- merge(hdi, wdi, all.x=TRUE, all.y=TRUE)
country_data <- merge(country_data, global_relig, all.x=TRUE, all.y=TRUE)


save(country_data, file=here("analysis","output","country_data.RData"))
