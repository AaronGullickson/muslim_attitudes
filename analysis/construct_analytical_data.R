#source this document to read all raw data in from original datasets (main and
#SS Africa), clean it, and merge it. Data will be kept in output folder. 
#html log files of the process will also be kept in the log folder.

library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))

#read in the two main datasets and recode/organize all variables
rmarkdown::render(here("analysis","organize_world_muslim_pew_data.R"),
                  output_file="organize_world_muslim_pew_data.html",
                  output_dir=here("analysis","logs"))
rmarkdown::render(here("analysis","organize_african_pew_data.R"),
                  output_file="organize_african_pew_data.html",
                  output_dir=here("analysis","logs"))

#combine them
pew_africa$country <- as.character(pew_africa$country)
pew_global_muslims$country <- as.character(pew_global_muslims$country)
pew <- rbind(pew_global_muslims, pew_africa)
pew$country <- as.factor(pew$country)
summary(pew)

save(pew, file=here("analysis","output","pewdata_combined.RData"))

#diagnostics for missing country data
missingCountries <- lapply(pew, getCountriesMissing)
sink(file=here("analysis","logs","summarystats_data.txt"))
cat("SUMMARY\n--------------------------------------------------------------------------------\n")
summary(pew, maxsum=10)
cat("COUNTRIES WITH MISSING DATA\n--------------------------------------------------------------------------------\n")
missingCountries
sink()

#run the imputation
rmarkdown::render(here("analysis","imputation.Rmd"))

#final coding of variables for analysis
source("code_variables.R")
