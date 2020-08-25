#' ---
#' title: "organize_world_muslim_pew_data.R"
#' author: ""
#' ---

library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))

# Introduction ------------------------------------------------------------
# this code will import the original pew data on the Worlds Muslims into R and 
# re-code a variety of variables for the later analysis.

# Load Data and Prelims ---------------------------------------------------

#pew data is in SPSS format (sigh)
pew <- as.data.frame(read.spss(here("analysis","input","pew_data_orig",
                                    "The-Worlds-Muslims-Dataset",
                                    "2012-Pew-Religion-Worlds-Muslims_dataset.sav"),
                               add.undeclared.levels = "no",
                               duplicated.value.labels = "condense"))

#why do people capitalize variable names? WHY? 
pew$country <- pew$COUNTRY
pew$caseid <- pew$CASEID
pew$weight <- pew$WEIGHT

#A variety of questions were asked differently in each country and then recorded
#as separate questions with a country suffix, so it will be useful to have those
#suffixes

country_suffix <- c("AFG","ALB","ALG","AZE","BAN","BOS","EGY","IDN","IRN","IRQ","JOR","KAZ","KOS","KYR",
			              "LEB","MAL","MOR","NIG","PAK","PAL","RUS","TAJ","THA","TUN","TUR","UZB")


# Demographic Variables ---------------------------------------------------
# demographic variables are recorded at the end starting with Q95

#GENDER: no missing values
pew$gender <- pew$Q95

#AGE: missing values for 98 and 99, also a few cases of less than 18
pew$age <- pew$Q96
pew$age[pew$age<18 | pew$age>97] <- NA
summary(pew$age)
#UPDATE: we need to cut these into categories to be consistent with African dataset
pew$age <- cut(pew$age, breaks=c(0,24,29,34,39,44,49,54,59,100),
               labels=c("Age 18-24","25-29","30-34","35-39","40-44",
                     "45-49","50-54","55-59","Age 60 or over"))
table(pew$Q96, pew$age, exclude=NULL)

#MARITAL STATUS: asked differently in Russia, so we need to combine. Also will
#combine Divorced and Separated
pew$marital <- NA
pew$marital[(!is.na(pew$Q97) & pew$Q97=="Married") | 
              (!is.na(pew$Q97RUS) & pew$Q97RUS=="Married or living with a partner")] <- "Married"
pew$marital[(!is.na(pew$Q97) & (pew$Q97=="Divorced" | pew$Q97=="Separated")) | 
              (!is.na(pew$Q97RUS) & (pew$Q97RUS=="Divorced" | pew$Q97RUS=="Separated"))] <- "Divorced/Separated"
pew$marital[(!is.na(pew$Q97) & pew$Q97=="Widowed") | 
              (!is.na(pew$Q97RUS) & pew$Q97RUS=="Widowed")] <- "Widowed"
pew$marital[(!is.na(pew$Q97) & pew$Q97=="Never been married") | 
              (!is.na(pew$Q97RUS) & pew$Q97RUS=="Never been married")] <- "Never married"
pew$marital <- factor(pew$marital, levels=c("Never married","Married","Divorced/Separated","Widowed"))
table(pew$marital, pew$Q97)
table(pew$marital, pew$Q97RUS)

#NUMBER OF CHILDREN: not asked of never married individuals so we are assuming
#zero in that case. Also asked slightly differently in Russia and Uzbekistan, so
#need to integrate those responses.
pew$nchild <- pew$Q110
#add Russia and UZB
pew$nchild[!is.na(pew$Q110RUS)] <- pew$Q110RUS[!is.na(pew$Q110RUS)]
pew$nchild[!is.na(pew$Q110UZB)] <- pew$Q110UZB[!is.na(pew$Q110UZB)]
#missing values at this point should be never married
summary(pew$nchild[pew$marital=="Never married"])
pew$nchild[is.na(pew$nchild)] <- 0
#now I can put in the proper missing values
pew$nchild[pew$nchild>=98] <- NA
#IMPORTANT NOTE: top-coding at 6 to be consistent with SS African data
pew$nchild[!is.na(pew$nchild) & pew$nchild>6] <- 6
summary(pew$nchild)
table(pew$nchild, pew$Q110)
table(pew$nchild, pew$Q110RUS)
table(pew$nchild, pew$Q110UZB)

#IMMIGRANT: is respondent a migrant to current country?
pew$immigrant <- pew$Q116=="Another country"
pew$immigrant[pew$Q116=="Don't Know (VOL.)" | pew$Q116=="Refused (VOL.)"] <- NA
summary(pew$immigrant)
table(pew$Q116, pew$immigrant)

#URBANICITY - not urbanity dammit
pew$urbanicity <- relevel(pew$Q133, "Rural")

# Technology Use ----------------------------------------------------------------
#there are a couple of interesting questions about technological use that may be useful

#USE INTERNET
pew$use_internet <- as.character(pew$Q111)
pew$use_internet[pew$use_internet=="Don't know" | pew$use_internet=="Refused"] <- NA
pew$use_internet <- factor(pew$use_internet, levels=c("No","Yes"))
table(pew$Q111, pew$use_internet, exclude=NULL)

#SOCIAL NETWORKING: this question is only asked if they use the internet and was not asked in Thailand (?)
#we will code non internet users as NO
pew$social_networking <- as.character(pew$Q112)
pew$social_networking[!is.na(pew$use_internet) & pew$use_internet=="No"] <- "No"
pew$social_networking[pew$social_networking=="Don't know" | pew$social_networking=="Refused"] <- NA
pew$social_networking <- factor(pew$social_networking, levels=c("No","Yes"))
table(pew$social_networking, pew$Q112, exclude=NULL)

#CELL PHONE
pew$own_cellphone <- as.character(pew$Q114)
pew$own_cellphone[pew$own_cellphone=="Don't know" | pew$own_cellphone=="Refused"] <- NA
pew$own_cellphone <- factor(pew$own_cellphone, levels=c("No","Yes"))
table(pew$Q114, pew$own_cellphone, exclude=NULL)


# Income ------------------------------------------------------------------
# Income was asked as a set of income backets that are specific to each country. 
# The full distribution for each country is shown by the code below. 

#look at the categories of income for each country
summary(pew[,paste("Q102",country_suffix,sep="")])

#The number of categories varies substantially by country and the distribution
#of respondents across categories also varies. This represents a challenge for
#trying to create a measure of income that can be compared across countries. Our
#technique is to code each respondent based on the midpoint of the quantiles
#associated with each category. This will give us a measure of relative income
#in each country. We are collapsing variation within categories but this is no
#different than using the midpoint of income intervals as is often done in
#research. The other issue is that we will get a much "chunkier" measure in 
#countries with fewer categories, and may not capture the distribution well if
#the categories were chosen poorly (e.g. 95% of respondents are in the bottom
#category)

pew$incomeqq <- NA
for(country in country_suffix) {	
  income <- pew[,paste("Q102",country,sep="")]
  #put in missing values
  income[!is.na(income) & (income=="Don't know" 
                           | income=="Don’t know"
                           | income=="Refused")] <- NA
  income <- droplevels(income)
  temp <- table(income)
  temp <- temp/sum(temp)
  #code the midpoint of each category
  quantile <- c(0,cumsum(temp[1:(length(temp)-1)]))+temp/2
  pew$incomeqq[!is.na(income)] <- quantile[as.numeric(income[!is.na(income)])]
}

summary(pew$incomeqq)
plot(as.numeric(pew$country), pew$incomeqq, pch=21, bg="red")
abline(v=as.numeric(pew$country), lty=2)
par(mar=c(5,6,1,1), cex.axis=0.5)
boxplot(pew$incomeqq~pew$country, col="red", horizontal=TRUE, las=1, xlab="income quantile")
tapply(pew$incomeqq, pew$country, mean, na.rm=TRUE)
par(mar=c(5,4,1,1), cex.axis=1)

#SUBJECTIVE WELLBEING. Lets also use a measure of subjective econoimc situation
pew$subj_econ <- pew$Q7
pew$subj_econ[pew$subj_econ=="Don’t know" | pew$subj_econ=="Refused"] <- NA
pew$subj_econ <- factor(pew$subj_econ, levels=c("Very bad","Somewhat bad","Somewhat good","Very good"))
table(pew$subj_econ, pew$Q7, exclude=NULL)

boxplot(pew$incomeqq~pew$subj_econ, col="red", ylab="income quantile")

# Education ---------------------------------------------------------------

# We have a similar problem with education as income. The categories used are
# country-specific. This makes sense given that given the variance in
# educational systems, but we need something that allows us to make comparisons
# across countries. The code below will show the different categories used by country

summary(pew[,paste("Q101",country_suffix[country_suffix!="MOR"],sep="")], maxsum=100)

# We have chosen to standardize these measures using the same simple categories used by 
# IPUMS international: 
# - less than primary
# - primary complete
# - secondary complete
# - University comlete
# In cases, where IPUMS international includes one of the countries in our dataset, we use
# their description of comparability to construct categories. In other cases, we attempt to 
# determine best codes based on our own research of educational systems. WE made particular use
# of World Education Services reports from WNER

# https://international.ipums.org/international-action/variables/EDATTAIN#comparability_section
# http://wenr.wes.org/category/education-system-profiles

#When I have a division between primary and intermediate, we code completion of
#primary by the lower level.

#UPDATE: All of the countries in the African dataset use a simple Primary or
#less, Secondary, Post-secondary set of categories. In order to harmonize the
#data, we will also create that breakdown here and call it "edattain.simple"

pew$edattain <- NA
pew$edattain.simple <- NA

#Afghanistan
# No data in IPUMS international
# http://wenr.wes.org/2016/09/education-afghanistan
# According to link the education system is a 6+3+3 withprimary education  up to
# grade 6, and secondary is complete with grade 12. However, that system was
# only introduced in 1990, before it was 8+4. We will code based on 6+3+3
temp <- pew$Q101AFG
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=2] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>2 & temp<=4] <- "Primary"
pew$edattain[!is.na(temp) & temp>=5 & temp<=6] <- "Secondary"
pew$edattain[!is.na(temp) & temp>=7] <- "University"
table(pew$Q101AFG, pew$edattain, exclude=NULL)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=6] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=6] <- "Post-Secondary"
table(pew$Q101AFG, pew$edattain.simple)

#Albania: Albania's categories are straightforward
temp <- pew$Q101ALB
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=3] <- "Primary"
pew$edattain[!is.na(temp) & temp>=4 & temp<=6] <- "Secondary"
pew$edattain[!is.na(temp) & temp>=7] <- "University"
table(pew$Q101ALB, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=3 & temp<=5] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=6] <- "Post-Secondary"
table(pew$Q101ALB, pew$edattain.simple)

#Algeria. Pretty straighforward except for the "vocational" and "religious" education variables. 
# I am going to treat those as secondary
#http://wenr.wes.org/2006/04/wenr-apr-2006-education-in-algeria
temp <- pew$Q101ALG
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=3] <- "Primary"
pew$edattain[!is.na(temp) & (temp==4 | temp==7 | temp==8)] <- "Secondary"
pew$edattain[!is.na(temp) & (temp==5 | temp==6)] <- "University"
table(pew$Q101ALG, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & (temp==3 | temp==4 | temp==7 | temp==8)] <- "Secondary"
pew$edattain.simple[!is.na(temp) & (temp==5 | temp==6)] <- "Post-Secondary"
table(pew$Q101ALG, pew$edattain.simple)

#Azerbaijan. A bit of a high bar for primary compared to others.
temp <- pew$Q101AZE
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp==2] <- "Primary"
pew$edattain[!is.na(temp) & temp>2 & temp<5] <- "Secondary"
pew$edattain[!is.na(temp) & temp==5] <- "University"
table(pew$Q101AZE, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=3 & temp<=4] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=5] <- "Post-Secondary"
table(pew$Q101AZE, pew$edattain.simple)

#Bangladesh  - a little unclear where the 1 vocational goes, put in secondary
# We also have two levels of secondary, but will assume higher secondary is secondary
temp <- pew$Q101BAN
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<3] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=3 & temp<=6] <- "Primary"
pew$edattain[!is.na(temp) & (temp==7 | temp==8 | temp==11)] <- "Secondary"
pew$edattain[!is.na(temp) & (temp==9 | temp==10)] <- "University"
table(pew$Q101BAN, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & ((temp>=4 & temp<=7) | temp==11)] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>7 & temp!=11] <- "Post-Secondary"
table(pew$Q101BAN, pew$edattain.simple)

#Bosnia - Until 2004, Bosnia had an 8 year primary education system, so we will use 8 years as the 
#cut mark
temp <- pew$Q101BOS
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=3] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=4 & temp<=5] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=6 & temp<=9)] <- "Secondary"
pew$edattain[!is.na(temp) & temp>=10] <- "University"
table(pew$Q101BOS, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=4] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=5 & temp<=8] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=9] <- "Post-Secondary"
table(pew$Q101BOS, pew$edattain.simple)

#Egypt
temp <- pew$Q101EGY
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=2] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=3 & temp<=6] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=7 & temp<=8)] <- "Secondary"
pew$edattain[!is.na(temp) & temp==9] <- "University"
table(pew$Q101EGY, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=7] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=8] <- "Post-Secondary"
table(pew$Q101EGY, pew$edattain.simple)


#Indonesia
temp <- pew$Q101IDN
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=2] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=3 & temp<=6] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=7 & temp<=8)] <- "Secondary"
pew$edattain[!is.na(temp) & temp==9] <- "University"
table(pew$Q101IDN, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=7] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=8] <- "Post-Secondary"
table(pew$Q101IDN, pew$edattain.simple)


#Iran
temp <- pew$Q101IRN
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=6] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=7 & temp<=10)] <- "Secondary"
pew$edattain[!is.na(temp) & temp>=11] <- "University"
table(pew$Q101IRN, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=7] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=8] <- "Post-Secondary"
table(pew$Q101IRN, pew$edattain.simple)

#Iraq - a little unclear what "diploma" is. I will assume more than secondary less than university. 
temp <- pew$Q101IRQ
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=3] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=4 & temp<=5)] <- "Secondary"
pew$edattain[!is.na(temp) & temp>5] <- "University"
table(pew$Q101IRQ, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=3 & temp<=4] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=5] <- "Post-Secondary"
table(pew$Q101IRQ, pew$edattain.simple)

#Jordan
temp <- pew$Q101JOR
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=2] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=3 & temp<=4] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=5 & temp<=6)] <- "Secondary"
pew$edattain[!is.na(temp) & temp==7] <- "University"
table(pew$Q101JOR, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=5] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=6] <- "Post-Secondary"
table(pew$Q101JOR, pew$edattain.simple)


#Kazhakstan - A little ambiguity between primary and "base-general". This seems
#to be a feature of the central asian countries, generally. I will stick with my
#standard rule, but in some of these Central Asian countries only "base-general"
#is provided, so some inconsistencies here. 
temp <- pew$Q101KAZ
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=3] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=4 & temp<=7)] <- "Secondary"
pew$edattain[!is.na(temp) & temp==8] <- "University"
table(pew$Q101KAZ, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=6] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=7] <- "Post-Secondary"
table(pew$Q101KAZ, pew$edattain.simple)


#Kosovo - the primary code in Kosovo goes up to 8 yers of education.
temp <- pew$Q101KOS
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=4] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=5 & temp<=6] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=7 & temp<=9)] <- "Secondary"
pew$edattain[!is.na(temp) & temp==10] <- "University"
table(pew$Q101KOS, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=5] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=6 & temp<=8] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=9] <- "Post-Secondary"
table(pew$Q101KOS, pew$edattain.simple)

#Kyrgyzstan - base education issue
temp <- pew$Q101KYR
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<3] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=3 & temp<=4] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=5 & temp<=7)] <- "Secondary"
pew$edattain[!is.na(temp) & temp==8] <- "University"
table(pew$Q101KYR, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=4] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=5 & temp<=6] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=7] <- "Post-Secondary"
table(pew$Q101KYR, pew$edattain.simple)

#Lebanon
temp <- pew$Q101LEB
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=5] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=6 & temp<=7)] <- "Secondary"
pew$edattain[!is.na(temp) & temp==8] <- "University"
table(pew$Q101LEB, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=3 & temp<=6] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=7] <- "Post-Secondary"
table(pew$Q101LEB, pew$edattain.simple)

#Malaysia -WTF, Malaysia. everything is coded as "began or completed." Kind of 
#big difference don't you think? This one will be very rough.
temp <- pew$Q101MAL
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=3] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=4 & temp<=7)] <- "Secondary"
pew$edattain[!is.na(temp) & temp>=8] <- "University"
table(pew$Q101MAL, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=3 & temp<=6] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=7] <- "Post-Secondary"
table(pew$Q101MAL, pew$edattain.simple)

#Morocco appears to be missing

#Niger
temp <- pew$Q101NIG
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=2] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=3 & temp<=6] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=7 & temp<=9)] <- "Secondary"
pew$edattain[!is.na(temp) & temp>=10] <- "University"
table(pew$Q101NIG, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=7] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=8] <- "Post-Secondary"
table(pew$Q101NIG, pew$edattain.simple)

#Pakistan: matriculation is equivalent to High School, and Graduate to BA degree. 
temp <- pew$Q101PAK
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=3] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=4 & temp<=7] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=8 & temp<=9)] <- "Secondary"
pew$edattain[!is.na(temp) & temp>=10] <- "University"
table(pew$Q101PAK, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=4] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=5 & temp<=8] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=9] <- "Post-Secondary"
table(pew$Q101PAK, pew$edattain.simple)

#Palestine
temp <- pew$Q101PAL
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=2] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=3 & temp<=4] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=5 & temp<=6)] <- "Secondary"
pew$edattain[!is.na(temp) & temp>=7] <- "University"
table(pew$Q101PAL, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=5] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=6] <- "Post-Secondary"
table(pew$Q101PAL, pew$edattain.simple)

#Russia
temp <- pew$Q101RUS
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=3] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=4 & temp<=7)] <- "Secondary"
pew$edattain[!is.na(temp) & temp>=8] <- "University"
table(pew$Q101RUS, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=3 & temp<=6] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=7] <- "Post-Secondary"
table(pew$Q101RUS, pew$edattain.simple)

#Tajikistan - base-general business again
temp <- pew$Q101TAJ
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp==2] <- "Primary"
pew$edattain[!is.na(temp) & temp>2 & temp<=5] <- "Secondary"
pew$edattain[!is.na(temp) & temp>5] <- "University"
table(pew$Q101TAJ, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=3 & temp<=4] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=5] <- "Post-Secondary"
table(pew$Q101TAJ, pew$edattain.simple)

#Thailand
temp <- pew$Q101THA
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=3] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=4 & temp<=5)] <- "Secondary"
pew$edattain[!is.na(temp) & temp==6] <- "University"
table(pew$Q101THA, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=3 & temp<=4] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=5] <- "Post-Secondary"
table(pew$Q101THA, pew$edattain.simple)

#Tunisia
temp <- pew$Q101TUN
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp==1] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=2 & temp<=3] <- "Primary"
pew$edattain[!is.na(temp) & temp==4] <- "Secondary"
pew$edattain[!is.na(temp) & temp==5] <- "University"
table(pew$Q101TUN, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=2] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=3 & temp<=4] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=5] <- "Post-Secondary"
table(pew$Q101TUN, pew$edattain.simple)

#Turkey
temp <- pew$Q101TUR
temp[!is.na(temp) & (temp=="DK" | temp=="Don't know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=2] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=3 & temp<=5] <- "Primary"
pew$edattain[!is.na(temp) & (temp==6)] <- "Secondary"
pew$edattain[!is.na(temp) & temp>6] <- "University"
table(pew$Q101TUR, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=6] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=7] <- "Post-Secondary"
table(pew$Q101TUR, pew$edattain.simple)

#Uzbekistan - base-general thing again
temp <- pew$Q101UZB
temp[!is.na(temp) & (temp=="DK" | temp=="Don’t know" | temp=="Refused" | temp=="Refusal")] <- NA
temp <- as.numeric(droplevels(temp))
pew$edattain[!is.na(temp) & temp<=2] <- "Less than primary"
pew$edattain[!is.na(temp) & temp>=3 & temp<=5] <- "Primary"
pew$edattain[!is.na(temp) & (temp>=6 & temp<=8)] <- "Secondary"
pew$edattain[!is.na(temp) & temp==9] <- "University"
table(pew$Q101UZB, pew$edattain)
pew$edattain.simple[!is.na(temp) & temp<=3] <- "Primary or less"
pew$edattain.simple[!is.na(temp) & temp>=4 & temp<=7] <- "Secondary"
pew$edattain.simple[!is.na(temp) & temp>=8] <- "Post-Secondary"
table(pew$Q101UZB, pew$edattain.simple)

pew$edattain <- factor(pew$edattain,
                       levels=c("Less than primary","Primary","Secondary","University"))
pew$edattain.simple <- factor(pew$edattain.simple,
                              levels=c("Primary or less","Secondary","Post-Secondary"))

round(100*prop.table(table(pew$country, pew$edattain),1),1)
round(100*prop.table(table(pew$country, pew$edattain.simple),1),1)

#Do a quantile based educational variable to just like income I will need to
#eliminate a few categories that don't fit the ordinal structure
pew$educqq <- NA
for(country in country_suffix[country_suffix!="MOR"]) {	
  educ <- pew[,paste("Q101",country,sep="")]
  #put in missing values
  educ[!is.na(educ) & (educ=="DK" | educ=="Don’t know" | educ=="Don't know"
                       | educ=="Refused" | educ=="Refusal"
                       | educ=="Vocational/Diploma" | educ=="Vocational Education" |
                         educ=="Religious Education")] <- NA
  educ <- droplevels(educ)
  temp <- table(educ)
  temp <- temp/sum(temp)
  #code the midpoint of each category
  quantile <- c(0,cumsum(temp[1:(length(temp)-1)]))+temp/2
  pew$educqq[!is.na(educ)] <- quantile[as.numeric(educ[!is.na(educ)])]
}

summary(pew$educqq)
plot(as.numeric(pew$country), pew$educqq, pch=21, bg="red")
abline(v=as.numeric(pew$country), lty=2)
par(mar=c(4,6,1,1), cex.axis=0.5)
boxplot(pew$educqq~pew$country, col="red", horizontal=TRUE, las=1, xlab="income quantile")
tapply(pew$educqq, pew$country, mean, na.rm=TRUE)

# Islamic Denomination ----------------------------------------------------

#It looks like the Amhadiyya quesion was either asked only in the SS African 
#survey, or is supressed for confidentiality reasons. Alevi and Bektashi are
#more or less country-specific, so I think we want a four category system here.
# - Sunni
# - Shia
# - Other (Something Else, Other, Alevi, Bektashi)
# - Nothing in particular (includes JaM -  Just a Muslim)
pew$denom <- NA
pew$denom[pew$Q31rec=="Sunni (for example, Hanafi, Maliki, Shafi, Hanbali)"] <- "Sunni"
pew$denom[pew$Q31rec=="Shi'a (for example, Ithnashari/Twelver, Ismaili/Sevener)"] <- "Shia"
pew$denom[pew$Q31rec=="Alevi (VOL.)" | pew$Q31rec=="Bektashi (VOL.)" |
            pew$Q31rec=="Other (Categories collapsed to protect respondent confidentiality)" |
            pew$Q31rec=="Something else"] <- "Other"
pew$denom[pew$Q31rec=="None in particular (VOL.)" | 
            pew$Q31rec=="Just a Muslim, not further specified (VOL.)"] <- "Nothing in particular"
pew$denom <- factor(pew$denom, levels=c("Sunni","Shia","Other","Nothing in particular"))
table(pew$Q31rec, pew$denom, exclude=NULL)
round(100*prop.table(table(pew$country, pew$denom),1),1)


#For the moment we are going to treat Sufism as another dimension of belief
#assume DK people aren't Sufi's
pew$sufi <- (!is.na(pew$Q32CEN) & (pew$Q32CEN!="No, do not identify with any Sufi orders" & 
  pew$Q32CEN!="Don't know if identify with Sufi order (VOL.)")) |
  (!is.na(pew$Q32MENA) & (pew$Q32MENA!="No, do not identify with any Sufi orders" &
                           pew$Q32MENA!="Don't know if identify with Sufi order (VOL.)")) |
  (!is.na(pew$Q32NIG) & (pew$Q32NIG!="No, do not identify with any Sufi orders" & 
                           pew$Q32NIG!="Don't know if identify with Sufi order")) | 
  (!is.na(pew$Q32RUS) & (pew$Q32RUS!="No, do not identify with any Sufi orders" & 
                           pew$Q32RUS!="Don't know if identify with Sufi order")) | 
  (!is.na(pew$Q32SEA) & (pew$Q32SEA!="No, do not identify with any Sufi orders" & 
                           pew$Q32SEA!="Don't know if identify with Sufi order (VOL.)")) | 
  (!is.na(pew$Q32SEE) & (pew$Q32SEE!="No, do not identify with any Sufi orders" & 
                           pew$Q32SEE!="Don't know if identify with Sufi order (VOL.)")) | 
  (!is.na(pew$Q32SOA) & (pew$Q32SOA!="No, do not identify with any Sufi orders" & 
                           pew$Q32SOA!="Don't know if identify with Sufi order (VOL.)")) | 
  (!is.na(pew$Q32TUR) & (pew$Q32TUR!="No, do not identify with any Sufi orders" & 
                           pew$Q32TUR!="Don't know if identify with Sufi order (VOL.)"))
#missing values
pew$sufi[(!is.na(pew$Q32CEN) & pew$Q32CEN=="Refused to say if identify with Sufi order (VOL.)") |
           (!is.na(pew$Q32MENA) & pew$Q32MENA=="Refused to say if identify with Sufi order (VOL.)") |
           (!is.na(pew$Q32NIG) & pew$Q32NIG=="Refused to say if identify with Sufi order") |
           (!is.na(pew$Q32RUS) & pew$Q32RUS=="Refused to say if identify with Sufi order") |
           (!is.na(pew$Q32SEA) & pew$Q32SEA=="Refused to say if identify with Sufi order (VOL.)") |
           (!is.na(pew$Q32SEE) & pew$Q32SEE=="Refused to say if identify with Sufi order (VOL.)") |
           (!is.na(pew$Q32SOA) & pew$Q32SOA=="Refused to say if identify with Sufi order (VOL.)") |
           (!is.na(pew$Q32TUR) & pew$Q32TUR=="Refused to say if identify with Sufi order (VOL.)")] <- NA

#some checks
table(pew$Q32CEN,pew$sufi,exclude=NULL)
table(pew$Q32MENA,pew$sufi,exclude=NULL)
table(pew$Q32NIG,pew$sufi,exclude=NULL)
table(pew$Q32CEN,pew$sufi,exclude=NULL)
table(pew$Q32RUS,pew$sufi,exclude=NULL)
table(pew$Q32SEA,pew$sufi,exclude=NULL)
table(pew$Q32SEE,pew$sufi,exclude=NULL)
table(pew$Q32SOA,pew$sufi,exclude=NULL)
table(pew$Q32TUR,pew$sufi,exclude=NULL)

summary(pew$sufi)
round(100*prop.table(table(pew$denom, pew$sufi),1),1)
round(100*prop.table(table(pew$country, pew$sufi),1),1)

# Religiosity -----------------------------------------------------------
# We want measures here the correspond to classical notions of religiousity in terms
# of its importance and centrality in life, not about specific practices and beliefs. 

#MOSQUE ATTENDANCE
pew$attend <- pew$Q34
pew$attend[pew$attend=="Don’t know" | pew$attend=="Refused"]<- NA
pew$attend <- gsub(" for Jum’ah prayer|, especially for the Eid","",pew$attend)
#reverse for high == more religious
pew$attend <- factor(pew$attend, levels=c("Never","Seldom","A few times a year",
                                          "Once or twice a month","Once a week",
                                          "More than once a week"))
table(pew$attend, pew$Q34, exclude=NULL)

##IMPORTANCE OF RELIGION
pew$relig_important <- pew$Q36
pew$relig_important[pew$relig_important=="Don’t know" | pew$relig_important=="Refused"]<- NA
pew$relig_important <- factor(pew$relig_important, levels=c("Not at all important","Not too important",
                                                            "Somewhat important","Very important"))
table(pew$relig_important, pew$Q36, exclude=NULL)

#PRAYER FREQUENCY
pew$prayer <- pew$Q61
pew$prayer[pew$prayer=="Don’t know" | pew$prayer=="Refused"]<- NA
pew$prayer <- factor(pew$prayer, levels=c("Never","Seldom","A few times a month","Once a week",
                                          "A few times a week","Once a day","Several times a day"))
table(pew$prayer, pew$Q61, exclude=NULL)

#READ KORAN
pew$read_koran <- pew$Q65
pew$read_koran[pew$read_koran=="Don’t know" | pew$read_koran=="Refused"]<- NA
pew$read_koran <- factor(pew$read_koran, levels=c("Never","A few times a year","Once or twice a month",
                                                  "At least once a week","Every day"))
table(pew$read_koran, pew$Q65, exclude=NULL)


# Theological Conservatism ------------------------------------------------
# measures of theologically conservative beliefs, try to find things 
# that are consistent with Olson and Carroll definition from GSS: literalism, 
# importance of conforming to orthodox religious norms, centrality of religious
# scripture to personal life, and certainty of faith

#BElIEVE MORAL: Is it necessary to believe in god to be moral and have good values, Q16
pew$believe_moral <- pew$Q16
pew$believe_moral[pew$believe_moral=="Don’t know" | pew$believe_moral=="Refused"]<- NA
pew$believe_moral <- factor(pew$believe_moral, 
                            levels=c("Number 1 – It is not necessary to believe in God in order to be moral and have good values",
                                    "Number 2 – It is necessary to believe in God in order to be moral and have good values"),
                            labels=c("Not necessary to believe in God to be moral",
                                     "Necessary to believe in God to be moral"))
table(pew$believe_moral, pew$Q16, exclude=NULL)

#ISLAM ONE TRUE FAITH: Q55 islam is the one true faith
pew$islam_truth <- pew$Q55
pew$islam_truth[pew$islam_truth=="Depends (VOL.)" 
                  | pew$islam_truth=="Don’t know (VOL.)" 
                  | pew$islam_truth=="Refused (VOL.)"]<- NA
pew$islam_truth <- factor(pew$islam_truth, 
                          levels=c("2 - Many religions can lead to eternal life in heaven",
                                   "Neither/Both equally (VOL.)",
                                   "1 – Islam is the one, true faith leading to eternal life in heaven"),
                          labels=c("Truth in many religions","Both/Neither","Islam is the one true faith"))
table(pew$islam_truth, pew$Q55, exclude=NULL)

#LITERALISM: Q56 only one true way to interpret teachings
pew$islam_oneway <- pew$Q57
pew$islam_oneway[pew$islam_oneway=="Don’t know" 
                | pew$islam_oneway=="Refused"]<- NA
pew$islam_oneway <- factor(pew$islam_oneway, 
                          levels=c("There is MORE than one true way to interpret the teachings of my religion",
                                   "Neither/Both equally (Volunteered)",
                                   "There is only ONE true way to interpret the teachings of my religion"),
                          labels=c("Many ways to interpret","Neither/Both","One way to interpret"))
table(pew$islam_oneway, pew$Q57, exclude=NULL)

#FOLLOW LIFE OF PROPHET
pew$follow_prophet <- pew$Q59
pew$follow_prophet[pew$follow_prophet=="Don’t know" | pew$follow_prophet=="Refused"]<- NA
pew$follow_prophet <- factor(pew$follow_prophet, levels=c("Not at all","Not too much","A little",
                                                          "A lot"))
table(pew$follow_prophet, pew$Q59, exclude=NULL)


#SHARIA IS WORD OF GOD: Q66, sharia is revealed word of go
pew$sharia_god <- pew$Q66
pew$sharia_god[pew$sharia_god=="Don’t know" | pew$sharia_god=="Refused"]<- NA
pew$sharia_god <- factor(pew$sharia_god, 
                           levels=c("Shar’ia is developed by men, based on the word of God",
                                    "Both (Volunteered)",
                                    "The Shar’ia is the revealed word of God"),
                           labels=c("Developed by men","Both","Word of God"))
table(pew$sharia_god, pew$Q66, exclude=NULL)

#SHARIA INTERPRETATION: Q67, sharia is open to multiple interpretations or not
pew$sharia_oneway <- pew$Q67
pew$sharia_oneway[pew$sharia_oneway=="Don’t know" | pew$sharia_god=="Refused"]<- NA
pew$sharia_oneway <- factor(pew$sharia_oneway, 
                         levels=c("Should be open to multiple interpretations",
                                  "Neither (Volunteered)",
                                  "One true understanding"),
                         labels=c("Multiple Interpretations",
                                  "Neither",
                                  "One true Interpretation"))
table(pew$sharia_oneway, pew$Q67, exclude=NULL)

#Some other options that I have initially decided not to include:
#   Q19: religion conflict with science. 
#   Q37 and Q38: Okay for son/daughter to marry a christian/buddhist?
#   Q43: variety of questions about the appropriateness of a variety of beliefs
#   Q52: The necessity of poselytizing as a part of being Muslim.
#   Q64f: fasting during Ramadan?
#   Q84: variety of responses about the moral acceptability of certain behaviors. I am not sure it goes here
#        but seems like it should go somewhere


#Lets also look at attitudes toward morality for a range of issues and see how well these correlate

morality <- data.frame(country=pew$country)
for(subs in c("a","b","c","d","e","f","g","h","i","j")) {
  question <- as.character(pew[,paste("Q84",subs,sep="")])
  question[question=="Don’t know" | question=="Refused"]<- NA
  question[question=="Not a moral issue" 
           | question=="Depends on situation (Volunteered)"] <- "Depends/Not a moral issue"
  question <- factor(question, 
                     levels=c("Morally acceptable","Depends/Not a moral issue","Morally wrong"))
  morality <- cbind(morality,question)
}
morality <- morality[,-1]
colnames(morality) <- paste("moral",
                            c("divorce","polygamy","fertility","alcohol","euthanasia","suicide","abortion",
                              "prostitution","premar_sex","gay"),
                            sep="_")
pew <- cbind(pew,morality)

# Anti-Secularism ----------------------------------------------------------------
# Measure the preference for Islam being a strong component of
# government/civil society

#ISLAMIC PARTIES - better or worse? (not asked in five countries)
pew$islamic_parties <- pew$Q13
pew$islamic_parties[pew$islamic_parties=="Don’t know" | pew$islamic_parties=="Refused"]<- NA
pew$islamic_parties <- factor(pew$islamic_parties, levels=c("Worse","About the same","Better"))
table(pew$islamic_parties, pew$Q13, exclude=NULL)

#INFLUENCE OF RELIGIOUS LEADERS IN POLITICS - Q15
pew$leaders_influence <- pew$Q15
pew$leaders_influence[pew$leaders_influence=="Don’t know" | pew$leaders_influence=="Refused"]<- NA
pew$leaders_influence <- factor(pew$leaders_influence, 
                                levels=c("No influence at all","Not too much influence","Some influence",
                                         "Large influence"))
table(pew$leaders_influence, pew$Q15, exclude=NULL)

#We also need a simplified binary of this variable that can come close to the way the African variable is split.
#I am going to split it at Not too much influcence/Some influence. 
pew$leaders_influence_binary <- NA
pew$leaders_influence_binary[pew$leaders_influence=="Large influence" |
                               pew$leaders_influence=="Some influence"] <- "Should express views"
pew$leaders_influence_binary[pew$leaders_influence=="Not too much influence" |
                               pew$leaders_influence=="No influence at all"] <- "Should keep out"
pew$leaders_influence_binary <- factor(pew$leaders_influence_binary,
                                       levels=c("Should keep out","Should express views"))


#GOOD/BAD THAT LAWS FOLLOW SHARIA - This is based on two questions. Do laws in 
#your country follow sharia closely and is that a good or bad thing?. We will
#reverse the direction of this variable if the respondent said bad thing.
#Neither will be NA.
# We are making some assumptions that make me a little uncomfortable with this variable.
# UPDATE: I am getting rid of this - just too crude and too many assumptions. The later question on Sharia
# already gets at this more directly and this variable seems to have poor correlations with everything.
# pew$sharia_good <- pew$Q68
# pew$sharia_good[pew$sharia_good=="Don’t know" | pew$sharia_good=="Refused"]<- NA
# pew$sharia_good <- as.numeric(pew$sharia_good)
# summary(pew$sharia_good)
# #reverse the order if they thought this was good, because it is coded in the wrong direction already
# pew$sharia_good[!is.na(pew$Q69) & pew$Q69=="Good thing"] <- 5-pew$sharia_good[!is.na(pew$Q69) & pew$Q69=="Good thing"]
# summary(pew$sharia_good)

#SHARIA AS LAW: Two questions Q79a and Q81. Q81 asked those who said they favored
#sharia whether it should be applied to muslims and non-muslims. We will create
#a compound ordinal variable which splits favor into universal sharia and
#muslim-only sharia
#also need to adjust for special questions for Russia, Thailand, and Iran
pew$sharia_law <- NA
pew$sharia_law[!is.na(pew$Q79a) & pew$Q79a=="Oppose"] <- 1
pew$sharia_law[!is.na(pew$Q79a) & pew$Q79a=="Favor"]  <- 2
pew$sharia_law[!is.na(pew$Q79aRUS) & pew$Q79aRUS=="Oppose"] <- 1
pew$sharia_law[!is.na(pew$Q79aRUS) & pew$Q79aRUS=="Favor"]  <- 2
pew$sharia_law[!is.na(pew$Q79aTHA) & pew$Q79aTHA=="Oppose"] <- 1
pew$sharia_law[!is.na(pew$Q79aTHA) & pew$Q79aTHA=="Favor"]  <- 2
pew$sharia_law[!is.na(pew$Q80) & pew$Q80=="Oppose"] <- 1
pew$sharia_law[!is.na(pew$Q80) & pew$Q80=="Favor"]  <- 2
pew$sharia_law2 <- factor(pew$sharia_law, labels=c("Oppose","Favor"))
#now adjust for those who want universal
pew$sharia_law[!is.na(pew$Q81) & pew$Q81=="Both Muslims and non-Muslims"] <- 3
pew$sharia_law[!is.na(pew$Q81RUS) & pew$Q81RUS=="Both Muslims and non-Muslims"] <- 3
pew$sharia_law[!is.na(pew$Q81THA) & pew$Q81THA=="Both Muslims and non-Muslims"] <- 3
pew$sharia_law3 <- factor(pew$sharia_law,
                         labels=c("Oppose","Favor for muslims only","Favor universal Sharia"))
table(pew$sharia_law3, pew$Q79a, exclude=NULL)
table(pew$sharia_law3, pew$Q81, exclude=NULL)
table(pew$sharia_law3, pew$Q79aRUS, exclude=NULL)
table(pew$sharia_law3, pew$Q81RUS, exclude=NULL)
table(pew$sharia_law3, pew$Q79aTHA, exclude=NULL)
table(pew$sharia_law3, pew$Q81THA, exclude=NULL)
table(pew$sharia_law3, pew$Q80, exclude=NULL)

#RELIGIOUS JUDGES: Q92a, give muslim leaders and religious judges power to
#decide family and property disputes.
pew$relig_judge <- pew$Q92a
pew$relig_judge[!is.na(pew$Q92aRUS)] <- pew$Q92aRUS[!is.na(pew$Q92aRUS)]
pew$relig_judge[!is.na(pew$Q92aTHA)] <- pew$Q92aTHA[!is.na(pew$Q92aTHA)]
pew$relig_judge[!is.na(pew$Q92aTUR)] <- pew$Q92aTUR[!is.na(pew$Q92aTUR)]
pew$relig_judge[pew$relig_judge=="Don’t know" | pew$relig_judge=="Refused"]<- NA
pew$relig_judge <- droplevels(relevel(pew$relig_judge, "Oppose"))
table(pew$relig_judge, pew$Q92a, exclude=NULL)
table(pew$relig_judge, pew$Q92aRUS, exclude=NULL)
table(pew$relig_judge, pew$Q92aTHA, exclude=NULL)
table(pew$relig_judge, pew$Q92aTUR, exclude=NULL)

#ISLAM AND MODERNITY: Q75 and Q75IRN, conflict with religion and modernity. I
#don't think this is actually a good measure of islamism, but it might be useful
#to code it and see the correlations, because it gets at the issue of whether
#islamism is modernist/pre-modernist in orientation.
pew$conflict_modern <- as.character(pew$Q75)
pew$conflict_modern[!is.na(pew$Q75IRN)] <- as.character(pew$Q75IRN[!is.na(pew$Q75IRN)])
#stupid
pew$conflict_modern <- gsub("'","’",pew$conflict_modern)
pew$conflict_modern <- gsub("there is conflict","there is a conflict",pew$conflict_modern)
pew$conflict_modern[pew$conflict_modern=="Don’t know" | pew$conflict_modern=="Refused"]<- NA
pew$conflict_modern <- factor(pew$conflict_modern, 
                              levels=c("No, don’t think so","Yes, there is a conflict"))
table(pew$conflict_modern, pew$Q75, exclude=NULL)
table(pew$conflict_modern, pew$Q75IRN, exclude=NULL)

#Other variables considered:
#   Q76: blasphemy laws, but only in Pakistan
#   Q85: asks about concern over extremist religious groups in country, but I
#        worry that this may be conflated with how prevalent these groups actually are
#        in the country, rather than implicit level of support.


# Womens Rights -----------------------------------------------------------
# attitudes toward women's rights and equality

#RIGHT TO VEIL: Q58, women should have the right to decide if they veil. We will
#code neither/both equally as the middle and husband/family decide as women do
#not have right
pew$right_veil <- as.character(pew$Q58)
pew$right_veil[pew$right_veil=="Don’t know" | pew$right_veil=="Refused"]<- NA
pew$right_veil[pew$right_veil=="Husband/family should decide (Volunteered)"] <- 
  "2 - Women should not have the right to decide whether to wear a veil"
pew$right_veil <- factor(pew$right_veil, 
                         levels=c("1 - Women should have the right to decide if they wear a veil",
                                  "Neither/Both equally (Volunteered)",
                                  "2 - Women should not have the right to decide whether to wear a veil"),
                         labels=c("Should have right to decide","Neither/Both","Should not have right to decide"))
table(pew$right_veil, pew$Q58, exclude=NULL)

#RIGHT TO DIVORCE: Q77
pew$right_divorce <- pew$Q77
pew$right_divorce[pew$right_divorce=="Don’t know" | pew$right_divorce=="Refused"]<- NA
pew$right_divorce <- factor(pew$right_divorce, 
                            levels=c("1—A wife should have the right to divorce her husband",
                                     "Neither (Volunteered)",
                                     "2—A wife should not have the right to divorce her husband"),
                            labels=c("Right to divorce","Neither","No right to divorce"))
table(pew$right_divorce, pew$Q77, exclude=NULL)

#OBEY HUSBAND: Q78
pew$obey_husband <- pew$Q78
pew$obey_husband[pew$obey_husband=="Don’t know" | pew$obey_husband=="Refused"]<- NA
pew$obey_husband <- factor(pew$obey_husband, 
                           levels=c("Completely disagree","Mostly disagree","Mostly agree","Completely agree"))
table(pew$obey_husband, pew$Q78, exclude=NULL)

#SONS SHOULD INHERIT: Q83, very few peopl said daughters, so lets just code sons vs all others
pew$sons_inherit <- NA
pew$sons_inherit[!is.na(pew$Q83) & (pew$Q83=="Daughters" 
                                    | pew$Q83=="Both should have equal rights"
                                    | pew$Q83=="Neither (Volunteered)")] <- "Does not prefer sons"
pew$sons_inherit[!is.na(pew$Q83) & pew$Q83=="Sons"] <- "Prefers sons"
pew$sons_inherit <- factor(pew$sons_inherit, levels=c("Does not prefer sons","Prefers sons"))
table(pew$sons_inherit, pew$Q83, exclude=NULL)

# Support for Violent Practices ------------------------------------------------------
# support for harsh and violent responses to violations of religious norms. Note
# this is not mutually exclusive of women's rights.

#HONOR KILLING: Q53, Q53AIU, Q54, Q54AIU, separate question for men and women.
#Responses are pretty close, but when there is a difference people prefer to
#kill women
pew$honorkill_man <- pew$Q53
pew$honorkill_man[!is.na(pew$Q53AIU)] <- pew$Q53AIU[!is.na(pew$Q53AIU)]
pew$honorkill_man[pew$honorkill_man=="Don’t know" | pew$honorkill_man=="Refused"]<- NA
pew$honorkill_man <- factor(pew$honorkill_man,
                            levels=c("Never justified","Rarely justified","Sometimes justified","Often justified"))
table(pew$honorkill_man, pew$Q53, exclude=NULL)
table(pew$honorkill_man, pew$Q53AIU, exclude=NULL)

pew$honorkill_woman <- pew$Q54
pew$honorkill_woman[!is.na(pew$Q54AIU)] <- pew$Q54AIU[!is.na(pew$Q54AIU)]
pew$honorkill_woman[pew$honorkill_woman=="Don’t know" | pew$honorkill_woman=="Refused"]<- NA
pew$honorkill_woman <- factor(pew$honorkill_woman,
                            levels=c("Never justified","Rarely justified","Sometimes justified","Often justified"))
table(pew$honorkill_woman, pew$Q54, exclude=NULL)
table(pew$honorkill_woman, pew$Q54AIU, exclude=NULL)

sum(as.numeric(pew$honorkill_man)>as.numeric(pew$honorkill_woman), na.rm=TRUE)
sum(as.numeric(pew$honorkill_man)<as.numeric(pew$honorkill_woman), na.rm=TRUE)

#DEATH FOR APOSTASY: Q92b
pew$death_apostasy <- pew$Q92b
pew$death_apostasy[pew$death_apostasy=="Don’t know" | pew$death_apostasy=="Refused"]<- NA
pew$death_apostasy <- droplevels(relevel(pew$death_apostasy, "Oppose"))
table(pew$death_apostasy, pew$Q92b, exclude=NULL)

#SEVERE CORPORAL PUNISHMENT: Q92c - punishments like whipping/cutting off hands for crimes
pew$severe_corporal <- pew$Q92c
pew$severe_corporal[pew$severe_corporal=="Don’t know" | pew$severe_corporal=="Refused"]<- NA
pew$severe_corporal <- droplevels(relevel(pew$severe_corporal, "Oppose"))
table(pew$severe_corporal, pew$Q92c, exclude=NULL)

#STONING FOR ADULTERY
pew$stone_adultery <- pew$Q92d
pew$stone_adultery[pew$stone_adultery=="Don’t know" | pew$stone_adultery=="Refused"]<- NA
pew$stone_adultery <- droplevels(relevel(pew$stone_adultery, "Oppose"))
table(pew$stone_adultery, pew$Q92d, exclude=NULL)

# Other Attitudes ---------------------------------------------------------
#Non-religious attitudes that may be good to have on hand

#DEMOCRACY GOOD
pew$democracy <- pew$Q14
pew$democracy[pew$democracy=="Don’t know" | pew$democracy=="Refused"]<- NA
pew$democracy <- factor(pew$democracy, 
                        levels=c("Strong leader","Democratic form of government"),
                        labels=c("Strong leader","Democracy"))
table(pew$democracy, pew$Q14, exclude=NULL)

#SAY IN GOVERNMENT -  (missing Uzbekistan)
pew$say_govern <- pew$Q12
pew$say_govern[pew$say_govern=="Don’t know" | pew$say_govern=="Refused"]<- NA
pew$say_govern <- factor(pew$say_govern, levels=c("Completely disagree","Mostly disagree",
                                                  "Mostly agree", "Completely agree"))
table(pew$say_govern, pew$Q12, exclude=NULL)

#LIKE WESTERN CULTURE, Q17
pew$western_culture <- pew$Q17
pew$western_culture[pew$western_culture=="Don’t know" | pew$western_culture=="Refused"]<- NA
pew$western_culture <- droplevels(relevel(pew$western_culture, "I dislike western music, movies and television"))
table(pew$western_culture, pew$Q17, exclude=NULL)

#WESTERN CULTURE MORALLY BAD, Q26
pew$western_immoral <- pew$Q26
pew$western_immoral[pew$western_immoral=="Don’t know" | pew$western_immoral=="Refused"]<- NA
pew$western_immoral <- factor(pew$western_immoral,
                              levels=c("Western music, movies and television have NOT hurt morality in our country",
                                       "Western music, movies and television have hurt morality in our country"),
                              labels=c("Western culture has NOT hurt morality","Western culture has hurt morality"))
table(pew$western_immoral, pew$Q26, exclude=NULL)
table(pew$western_culture, pew$western_immoral)

# Orphans -----------------------------------------------------------------
#stuff I am not sure where to put in other places

# Q85 - concern about conflict between reliigous and not religious muslims in country. This is really
# interesting but its hard to tease out attitudes toward islamism from it. 

# Q89 - VIOLENCE AGAINST CIVILIAN TARGETS. 
pew$civilian_target <- pew$Q89
pew$civilian_target[pew$civilian_target=="Don’t know" | pew$civilian_target=="Refused"]<- NA
pew$civilian_target <- factor(pew$civilian_target,
                              levels=c("Never justified","Rarely justified","Sometimes justified","Often justified"))
table(pew$civilian_target, pew$Q89, exclude=NULL)


# Create Final Subset -------------------------------------------

#Iran needs to go because it is missing on almost all of the ideational variables we care about.
#Uzbekistan isn't missing quite as much, but is missing for most of what we care about as well.
#Morocco is missing education variable and also a lot of ideational variables. 
#Thailand is only a sample of five southern provinces

pew_global_muslims <- subset(pew, 
                             country!="Iran" & country!="Uzbekistan" & country!="Morocco" & 
                               country!="Thailand",
                             select=c("country","caseid","weight",
                                      "gender","age","marital","nchild","urbanicity","immigrant",
                                      "use_internet","social_networking","own_cellphone",
                                      "incomeqq","subj_econ","edattain","edattain.simple","educqq",
                                      "denom","sufi",
                                      "attend","relig_important","prayer","read_koran",
                                      "believe_moral","follow_prophet","islam_truth","islam_oneway","sharia_god","sharia_oneway",
                                      "moral_divorce","moral_polygamy","moral_fertility","moral_alcohol","moral_euthanasia",
                                      "moral_suicide","moral_abortion","moral_prostitution","moral_premar_sex","moral_gay",
                                      "islamic_parties","leaders_influence","leaders_influence_binary","sharia_law2","sharia_law3","relig_judge",
                                      "conflict_modern",
                                      "right_veil","right_divorce","obey_husband","sons_inherit",
                                      "honorkill_man","honorkill_woman","death_apostasy","severe_corporal","stone_adultery",
                                      "civilian_target",
                                      "democracy","say_govern","western_culture","western_immoral"))

#Save as RData file to preserve the ordering of ordinal variables
save(pew_global_muslims, file=here("analysis","output","pew_world_muslim_data.RData"))