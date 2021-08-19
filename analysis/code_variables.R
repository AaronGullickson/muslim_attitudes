# code_variables.R

library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))
load(here("analysis","output","imputed_samples.RData"))
load(here("analysis","output","country_data.RData"))


# Introduction ------------------------------------------------------------

# This script will take the imputed complete datasets and code some final
# variables that will be needed for the analysis. This script will output the
# final analytical datasets for all parts of the analysis. 

# Create Scales -----------------------------------------------------------

# create scales based on results from factoranalysis.Rmd. 
# All quantitative independent variables will be re-scaled by centering on the 
# mean and dividing by twice the SD. Dependent variables scaled by the SD.

analytic_samples <- lapply(analytic_samples, function(asample) {
  
  #Dependent Variable
  asample$violence <- scale(as.numeric(asample$death_apostasy=="Favor") 
                            +as.numeric(asample$stone_adultery=="Favor")
                            +as.numeric(asample$severe_corporal=="Favor"))[,1]
  
  #convert morality questions into two constructs
  asample$social_cons_death <- scaleIndVariable(
    apply(asample[,c("moral_euthanasia","moral_suicide","moral_abortion")]=="Morally wrong",1,sum))
  asample$social_cons_sex <- scaleIndVariable(
    apply(asample[,c("moral_prostitution","moral_premar_sex","moral_gay")]=="Morally wrong",1,sum))
  
  
  #convert religiosity variables into scale variables
  asample$attend.scale <- scaleIndVariable(as.numeric(asample$attend))
  asample$prayer.scale <- scaleIndVariable(as.numeric(asample$prayer))
  asample$relig_important.scale <- scaleIndVariable(as.numeric(asample$relig_important))
  #also create a single religiosity scale, even though we probably will no longer use this
  asample$religiosity <- scaleIndVariable(scale(as.numeric(asample$prayer))[,1]
                                          +scale(as.numeric(asample$relig_important))[,1]
                                          +scale(as.numeric(asample$attend))[,1])
  
  #also re-scale income and education quantiles
  asample$incomeqq_scl <- scaleIndVariable(asample$incomeqq)
  asample$educqq_scl <- scaleIndVariable(asample$educqq)
  
  #turn western variables into booleans 
  asample$western_culture <- asample$western_culture=="I dislike western music, movies and television"
  asample$western_immoral <- asample$western_immoral=="Western culture has hurt morality"

  return(asample)
})


# Add country level variables ---------------------------------------------

#I want each country variable to be scaled by country level standard 
#deviation but re-centered by individual level mean
scale_country_var <- function(variable, country) {
  sd.country <- sd(variable[!duplicated(country)])
  return((variable-mean(variable))/(2*sd.country))
}

analytic_samples <- lapply(analytic_samples, function(asample) {
 
  #merge country data
  asample <- merge(asample, country_data, all.x=TRUE, all.y=FALSE)
  asample$hdi_scaled <- scale_country_var(asample$hdi, asample$country)
  asample$lgdpcap_scaled <- scale_country_var(asample$lgdpcap, asample$country)
  asample$oil_rent_pct_scaled <- scale_country_var(asample$oil_rent_pct, 
                                                   asample$country)
  
  return(asample) 
})


# Grand Mean Centering ----------------------------------------------------

# Grand mean center all individual level variables for the random effects 
# variables. 

analytic_samples <- lapply(analytic_samples, function(x) {
  # factor variables
  x <- createDummiesGrandMeanCentered(x$age, x, "age")
  x <- createDummiesGrandMeanCentered(x$gender, x)
  x <- createDummiesGrandMeanCentered(x$urbanicity, x)
  x <- createDummiesGrandMeanCentered(x$denom, x)
  x <- createDummiesGrandMeanCentered(x$believe_moral, x)
  x <- createDummiesGrandMeanCentered(x$edattain.simple, x)
  #booleans
  x$sufi_ctr <- x$sufi - mean(x$sufi, na.rm=TRUE)
  x$islam_truth_ctr <- (as.numeric(x$islam_truth)==3) - 
    mean(as.numeric(x$islam_truth)==3, na.rm=TRUE)
  x$islam_oneway_ctr <- (as.numeric(x$islam_oneway)==3) - 
    mean(as.numeric(x$islam_oneway)==3, na.rm=TRUE)
  x$conflict_modern_ctr <- (as.numeric(x$conflict_modern)==2) - 
    mean(as.numeric(x$conflict_modern)==2, na.rm=TRUE)
  x$anti_democratic_ctr <- (as.numeric(x$democracy)==1) -
    mean(as.numeric(x$democracy)==1, na.rm=TRUE)
  x$western_culture_ctr <- (as.numeric(x$western_culture)==1) -
    mean(as.numeric(x$western_culture)==1, na.rm=TRUE)
  x$western_immoral_ctr <- (as.numeric(x$western_immoral)==1) -
    mean(as.numeric(x$western_immoral)==1, na.rm=TRUE)
  return(x)
})

# Save Results ------------------------------------------------------------

save(analytic_samples, file=here("analysis","output","analytic_samples.RData"))
