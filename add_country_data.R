library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","pewdata_combined.RData"))

sample_countries <- as.character(unique(pew$country))

#Human Development Index
#jesus christ, why cant these international organizations design machine-readable
#data
hdi <- read_csv(here("analysis","input","undp","hdi_undp.csv"),
                skip=1, na="..")[,c(2,seq(from=3,to=59, by=2))]


#how many missing values in 2010
summary(hdi$`2010`)

hdi$Country[hdi$Country=="Guinea-Bissau"] <- "Guinea Bissau"
hdi$Country[hdi$Country=="Palestine, State of"] <- "Palestinian Territories"
hdi$Country[hdi$Country=="Russian Federation"] <- "Russia"
hdi$Country[hdi$Country=="Tanzania (United Republic of)"] <- "Tanzania"
#use Serbia's value for Kosovo
hdi$Country[hdi$Country=="Serbia"] <- "Kosovo"


sample_countries[!(sample_countries %in% hdi$Country)]

hdi <- subset(hdi, Country %in% sample_countries)
summary(hdi$`2010`)
hdi <- subset(hdi, select=c("Country","2010"))
colnames(hdi) <- c("country","hdi")
#standardize scale of hdi by country
hdi$hdi_scaled <- (hdi$hdi-mean(hdi$hdi))/(2*sd(hdi$hdi))

load(here("analysis","output","analytic_samples.RData"))

for(i in 1:5) {
  asample <- analytic_samples[[i]]
  asample <- merge(asample, hdi, all.x=TRUE, all.y=FALSE)
  analytic_samples[[i]] <- asample
}


save(analytic_samples, file=here("analysis","output","analytic_samples.RData"))
