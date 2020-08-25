#' ---
#' title: "organize_africa_pew_data.R"
#' author: ""
#' ---

library(here)
source(here("analysis","check_packages.R"))
source(here("analysis","useful_functions.R"))


# Introduction ------------------------------------------------------------
# this code will organize the data from the 2009-2010 survey of 15 Sub-Saharan African
# countries.

# Load Data and Prelims ---------------------------------------------------

#pew data is in SPSS format (sigh)
library(foreign)
pew <- as.data.frame(read.spss(here("analysis","input","pew_data_orig",
                                    "Sub-Saharan-Africa-Religion-Dataset",
                                    "Dataset - Pew Forum on Religion & Public Life 2009 Africa Survey.sav"),
                               add.undeclared.levels = "no",
                               duplicated.value.labels = "condense"))

pew$country <- pew$COUNTRY
pew$caseid <- pew$PSRAID
pew$weight <- pew$WEIGHT

# Demographic Variables ---------------------------------------------------
# demographic variables are recorded at the end starting with Q95

#GENDER: no missing values
pew$gender <- pew$Q96

#AGE: in the African dataset, age was only recorded in brackets. Adjusted other
#dataset for consistency
pew$age <- pew$q97rec
pew$age[pew$age=="Don't know/refused"] <- NA
pew$age <- droplevels(pew$age)
summary(pew$age)

#MARITAL STATUS: Recode to be consisten with other dataset. Treat cohab as married.
pew$marital <- NA
pew$marital[pew$Q79=="Married" | pew$Q79=="Living with a partner"] <- "Married"
pew$marital[pew$Q79=="Divorced" | pew$Q79=="Separated"] <- "Divorced/Separated"
pew$marital[pew$Q79=="Widowed"] <- "Widowed"
pew$marital[pew$Q79=="Never been married"] <- "Never married"
pew$marital <- factor(pew$marital, levels=c("Never married","Married","Divorced/Separated","Widowed"))
table(pew$marital, pew$Q79, exclude=NULL)

#NUMBER OF CHILDREN: Assume NA's are zero. This variable is top-coded at six.
pew$nchild <- pew$q104arec
TF <- is.na(pew$nchild)
pew$nchild[pew$nchild=="Don't know" | pew$nchild=="Refused"] <- NA
pew$nchild <- as.numeric(droplevels(pew$nchild))
pew$nchild[TF] <- 0
table(pew$nchild,pew$q104arec, exclude=NULL)

#IMMIGRANT: is respondent a migrant to current country?
pew$immigrant <- as.character(pew$q109rec)!=as.character(pew$country)
pew$immigrant[pew$q109rec=="Other/Don't know/Refused"] <- NA
summary(pew$immigrant)

#URBANICITY - not urbanity dammit. Code Semi-urban to urban for consistency with other dataset
pew$urbanicity <- as.character(pew$q115rec)
pew$urbanicity[pew$urbanicity=="Semi-urban"] <- "Urban"
pew$urbanicity <- factor(pew$urbanicity, 
                         levels=c("Rural","Urban"))
table(pew$urbanicity, pew$q115rec, exclude=NULL)

# Technology Use ----------------------------------------------------------------

#USE INTERNET
pew$use_internet <- as.character(pew$Q106)
pew$use_internet[pew$use_internet=="Don't know" | pew$use_internet=="Refused"] <- NA
pew$use_internet <- factor(pew$use_internet, levels=c("No","Yes"))
table(pew$use_internet, pew$Q106, exclude=NULL)

#NO SOCIAL NETWORKING VARIABLE
pew$social_networking <- NA

#CELL PHONE
pew$own_cellphone <- as.character(pew$Q108B)
pew$own_cellphone[pew$own_cellphone=="Don't know" | pew$own_cellphone=="Refused"] <- NA
pew$own_cellphone <- factor(pew$own_cellphone, levels=c("No","Yes"))
table(pew$own_cellphone, pew$Q108B, exclude=NULL)


# Income ------------------------------------------------------------------
# God damn it. They don't give brackets here. Instead they just give four 
# categories that are considered "rough" quartiles. Following logic of 
# previous approach, we can code incomeqq based on the midpoint of each
# quartile. 

pew$incomeqq <- pew$income
pew$incomeqq[pew$incomeqq==99] <- NA
pew$incomeqq <- as.numeric(pew$incomeqq)/4-0.125
summary(pew$incomeqq)
table(pew$incomeqq)

#what if I do it instead by treating each one as a bracket and calculating
#actual quantiles within each
pew$incomeqq2 <- NA
for(country in levels(pew$country)) {	
  TF <- pew$country==country & pew$income!=99
  income <- pew$income[TF]
  temp <- table(income)
  temp <- temp/sum(temp)
  #code the midpoint of each category
  quantile <- c(0,cumsum(temp[1:(length(temp)-1)]))+temp/2
  pew$incomeqq2[TF] <- quantile[income]
}
summary(pew$incomeqq2)
tapply(pew$incomeqq2, pew$country, mean, na.rm=TRUE)
plot(as.numeric(pew$country), pew$incomeqq2, pch=21, bg="red")
abline(v=as.numeric(pew$country), lty=2)
par(mar=c(5,6,1,1), cex.axis=0.5)
boxplot(pew$incomeqq~pew$country, col="red", horizontal=TRUE, las=1, xlab="income quantile")

plot(pew$incomeqq, pew$incomeqq2)
abline(h=1:4/4-.125, lty=2)
#a fair bit of variation within country in how this is understood. I think using
#the actual quantiles is probably more consistent
pew$incomeqq <- pew$incomeqq2

#SUBJECTIVE WELLBEING. Lets also use a measure of subjective econoimc situation
pew$subj_econ <- pew$Q7
pew$subj_econ[pew$subj_econ=="Don’t know" | pew$subj_econ=="Refused"] <- NA
pew$subj_econ <- factor(pew$subj_econ, levels=c("Very bad","Somewhat bad","Somewhat good","Very good"))
table(pew$subj_econ, pew$Q7, exclude=NULL)

boxplot(pew$incomeqq~pew$subj_econ, col="red", ylab="income quantile")
tapply(pew$incomeqq,pew$subj_econ, mean, na.rm=TRUE)

# Education ---------------------------------------------------------------

#The education measures mixes up less than primary with those who completed it
#and some secondary with those who completed it. WTF, Pew??!!!

#I will probably need to collapse my categories for the other dataset for the
#less than primary category, but I am not sure about how to handle the some
#secondary/completed secondary. That would take a more significant reworking.
pew$edattain <- NA
pew$edattain.simple <- pew$educ
pew$edattain.simple[pew$edattain.simple=="Don't know/refused/other"] <- NA
pew$edattain.simple <- droplevels(pew$edattain.simple)
pew$edattain.simple <- factor(pew$edattain.simple, 
                       levels=c("Completed Primary or less",
                                "Some Secondary/Completed Secondary",
                                "Post-Secondary and up"),
                       labels=c("Primary or less","Secondary","Post-Secondary"))
table(pew$edattain.simple, pew$educ, exclude=NULL)

round(100*prop.table(table(pew$country, pew$edattain.simple),1),1)

pew$educqq <- NA
for(country in unique(pew$country)) {	
  educ <- pew$edattain.simple[pew$country==country]
  temp <- table(educ)
  temp <- temp/sum(temp)
  #code the midpoint of each category
  quantile <- c(0,cumsum(temp[1:(length(temp)-1)]))+temp/2
  pew$educqq[pew$country==country & !is.na(pew$edattain.simple)] <- quantile[as.numeric(pew$edattain.simple[pew$country==country 
                                                                                              & !is.na(pew$edattain.simple)])]
}

summary(pew$educqq)
plot(as.numeric(pew$country), pew$educqq, pch=21, bg="red")
abline(v=as.numeric(pew$country), lty=2)
par(mar=c(4,6,1,1), cex.axis=0.5)
boxplot(pew$educqq~pew$country, col="red", horizontal=TRUE, las=1, xlab="income quantile")
tapply(pew$educqq, pew$country, mean, na.rm=TRUE)

# Islamic Denomination ----------------------------------------------------

#also keep relig question for African data
pew$relig <- factor(as.character(pew$RELIGrec),
                    levels=c("Christian","Muslim",
                             "Ancestral, tribal, animist, or other traditional African religion",
                             "Unaffiliated"),
                    labels=c("Christian","Muslim","Traditional","Unaffiliated"))
table(pew$RELIGrec, pew$relig, exclude=NULL)

pew$denom <- NA
pew$denom[pew$q76rec=="Sunni (for example, Hanafi, Maliki, Shafi, Hanbali)"] <- "Sunni"
pew$denom[pew$q76rec=="Shi'a (for example, Ithnashari/Twelver, Ismaili/Sevener)"] <- "Shia"
pew$denom[pew$q76rec=="Ahmadiyya (VOLUNTEERED)"  | pew$q76rec=="Something else"] <- "Other"
pew$denom[pew$q76rec=="None in particular (VOLUNTEERED)" | 
            pew$q76rec=="Just a Muslim, not further specified (VOLUNTEERED)"] <- "Nothing in particular"
pew$denom <- factor(pew$denom, levels=c("Sunni","Shia","Other","Nothing in particular"))
table(pew$q76rec, pew$denom, exclude=NULL)
round(100*prop.table(table(pew$country, pew$denom),1),1)

#sufi order
pew$sufi <- pew$q77rec!="No, did not identify with any Sufi orders"
#They don't break up don't know/Refused here, so I will treat them as non-Sufis - even without them numbers are very high.
table(pew$q77rec, pew$sufi, exclude=NULL)

summary(pew$sufi)
round(100*prop.table(table(pew$denom, pew$sufi),1),1)
round(100*prop.table(table(pew$country, pew$sufi),1),1)


# Religiosity -----------------------------------------------------------

#MOSQUE ATTENDANCE
pew$attend <- as.character(pew$Q41)
pew$attend <- gsub(" for Jum'ah prayer|, especially for the Eid","",pew$attend)
pew$attend[is.na(pew$attend)] <- as.character(pew$Q40[is.na(pew$attend)])
pew$attend[pew$attend=="Don’t know" | pew$attend=="Refused"]<- NA
#reverse for high == more religious
pew$attend <- factor(pew$attend, levels=c("Never","Seldom","A few times a year",
                                          "Once or twice a month","Once a week",
                                          "More than once a week"))
table(pew$attend, pew$Q41, exclude=NULL)

##IMPORTANCE OF RELIGION
pew$relig_important <- pew$Q42
pew$relig_important[pew$relig_important=="Don’t know" | pew$relig_important=="Refused"]<- NA
pew$relig_important <- factor(pew$relig_important, levels=c("Not at all important","Not too important",
                                                            "Somewhat important","Very important"))
table(pew$relig_important, pew$Q42, exclude=NULL)

#PRAYER FREQUENCY
pew$prayer <- pew$Q64
pew$prayer[pew$prayer=="Don’t know" | pew$prayer=="Refused"]<- NA
pew$prayer <- factor(pew$prayer, levels=c("Never","Seldom","A few times a month","Once a week",
                                          "A few times a week","Once a day","Several times a day"))
table(pew$prayer, pew$Q64, exclude=NULL)

#READ KORAN - not asked in this survey
pew$read_koran <- NA

# Theological Conservatism ------------------------------------------------

#BElIEVE MORAL: Is it necessary to believe in god to be moral and have good values, Q16
pew$believe_moral <- pew$Q16
pew$believe_moral[pew$believe_moral=="Don’t know" | pew$believe_moral=="Refused"]<- NA
pew$believe_moral <- factor(pew$believe_moral, 
                            levels=c("Number 1 - It is not necessary to believe in God in order to be moral and have good values",
                                     "Number 2 - It is necessary to believe in God in order to be moral and have good values"),
                            labels=c("Not necessary to believe in God to be moral",
                                     "Necessary to believe in God to be moral"))
table(pew$believe_moral, pew$Q16, exclude=NULL)

pew$islam_truth <- pew$Q59A
pew$islam_truth[pew$islam_truth=="Don't know" 
                | pew$islam_truth=="Refused"]<- NA
pew$islam_truth <- factor(pew$islam_truth, 
                          levels=c("Many religions can lead to eternal life",
                                   "Neither/Both equally (VOLUNTEERED)",
                                   "My religion is the one, true faith leading to eternal life"),
                          labels=c("Truth in many religions","Both/Neither","Islam is the one true faith"))
table(pew$islam_truth, pew$Q59A, exclude=NULL)

#LITERALISM: Q56 only one true way to interpret teachings
pew$islam_oneway <- pew$Q59B
pew$islam_oneway[pew$islam_oneway=="Don't know" 
                 | pew$islam_oneway=="Refused"]<- NA
pew$islam_oneway <- factor(pew$islam_oneway, 
                           levels=c("There is MORE than one true way to interpret the teachings of my religion",
                                    "Neither/Both equally (VOLUNTEERED)",
                                    "There is only ONE true way to interpret the teachings of my religion"),
                           labels=c("Many ways to interpret","Neither/Both","One way to interpret"))
table(pew$islam_oneway, pew$Q59B, exclude=NULL)

#FOLLOW LIFE OF PROPHET - missing in this survey
pew$follow_prophet <- NA

#The African data asks about the literalism (Qof the Koran but not Sharia, in
#comparison to the other dataset
pew$sharia_god <- NA
pew$sharia_oneway <- NA

#Lets also look at attitudes toward morality for a range of issues and see how well these correlate
morality <- data.frame(country=pew$country)
for(subs in c("A","B","C","D","E","F","G","H","I")) {
  question <- as.character(pew[,paste("Q85",subs,sep="")])
  question[question=="Don't know" | question=="Refused"]<- NA
  question[question=="Not a moral issue" 
           | question=="Depends on situation (VOLUNTEERED)"] <- "Depends/Not a moral issue"
  question <- factor(question, 
                     levels=c("Morally acceptable","Depends/Not a moral issue","Morally wrong"))
  morality <- cbind(morality,question)
}
morality <- morality[,-1]
colnames(morality) <- paste("moral",
                            c("divorce","prostitution","euthanasia","suicide","alcohol","premar_sex","polygamy",
                              "abortion","gay"),
                            sep="_")
pew <- cbind(pew,morality)
#fertility wasnt asked
pew$moral_fertility <- NA


# Islamism ----------------------------------------------------------------

#ISLAMIC PARTIES - was not asked
pew$islamic_parties <- NA

#INFLUENCE OF RELIGIOUS LEADERS IN POLITICS - Q12a, asked in a slightly
#different way and given a binary option which is different from other survey
pew$leaders_influence <- NA
pew$leaders_influence_binary <- pew$Q12A
pew$leaders_influence_binary[pew$leaders_influence_binary=="Don't know" | pew$leaders_influence_binary=="Refused"]<- NA
pew$leaders_influence_binary <- droplevels(relevel(pew$leaders_influence_binary, "Should keep out"))
table(pew$leaders_influence_binary, pew$Q12A, exclude=NULL)

#SHARIA AS LAW: this was only given as a favor d onot favor. Respondents who said favor were not asked follow up question 
#regarding whether it should be universal or only for non-muslims, as they were in the other dataset.
pew$sharia_law2 <- pew$Q95A
pew$sharia_law2[is.na(pew$sharia_law2)] <- pew$Q94A[is.na(pew$sharia_law2)]
pew$sharia_law2[pew$sharia_law2=="Don't know" | pew$sharia_law2=="Refused"]<- NA
pew$sharia_law2 <- droplevels(relevel(pew$sharia_law2, "Oppose"))
table(pew$sharia_law2, pew$Q95A, exclude=NULL)

pew$sharia_law3 <- NA

#RELIGIOUS JUDGES
pew$relig_judge <- pew$Q95B
pew$relig_judge[is.na(pew$relig_judge)] <- pew$Q94B[is.na(pew$relig_judge)]
pew$relig_judge[pew$relig_judge=="Don't know" | pew$relig_judge=="Refused"]<- NA
pew$relig_judge <- droplevels(relevel(pew$relig_judge, "Oppose"))
table(pew$relig_judge, pew$Q95B, exclude=NULL)

#ISLAM AND MODERNITY: see organizedata.R for notes about why this variable
pew$conflict_modern <- pew$Q84
pew$conflict_modern[pew$conflict_modern=="Don't know" | pew$conflict_modern=="Refused"]<- NA
pew$conflict_modern <- factor(pew$conflict_modern, 
                              levels=c("No, don't think so","Yes, there is conflict"),
                              labels=c("No, don’t think so","Yes, there is a conflict"))
table(pew$conflict_modern, pew$Q84, exclude=NULL)

# Womens Rights -----------------------------------------------------------
# attitudes toward women's rights and equality

#RIGHT TO VEIL: Q58, women should have the right to decide if they veil. We will
#code neither/both equally as the middle and husband/family decide as women do
#not have right
pew$right_veil <- pew$Q59D
pew$right_veil[pew$right_veil=="Don't know" | pew$right_veil=="Refused"]<- NA
pew$right_veil <- factor(pew$right_veil, 
                         levels=c("Women should have the right to decide if they wear a veil",
                                  "Neither/Both equally (VOLUNTEERED)",
                                  "Women should not have the right to decide whether to wear a veil - society should decide"),
                         labels=c("Should have right to decide","Neither/Both","Should not have right to decide"))
table(pew$right_veil, pew$Q59D, exclude=NULL)

#RIGHT TO DIVORCE - missing in this survey
pew$right_divorce <- NA

#OBEY HUSBAND - missing in this survey
pew$obey_husband <- NA

#SONS SHOULD INHERIT - missing in this survey
pew$sons_inherit <- NA

# Violent Practices -----------------------------------------------------------
# support for harsh and violent responses to violations of religious norms. Note
# this is not mutually exclusive of women's rights.

#HONOR KILLING - not in this survey
pew$honorkill_man <- NA
pew$honorkill_woman <- NA

#DEATH FOR APOSTASY
pew$death_apostasy <- pew$Q95C
pew$death_apostasy[pew$death_apostasy=="Don't know" | pew$death_apostasy=="Refused"]<- NA
pew$death_apostasy <- droplevels(relevel(pew$death_apostasy, "Oppose"))
table(pew$death_apostasy, pew$Q95C, exclude=NULL)

#SEVERE CORPORAL PUNISHMENT
pew$severe_corporal <- pew$Q95D
pew$severe_corporal[is.na(pew$severe_corporal)] <- pew$Q94C[is.na(pew$severe_corporal)]
pew$severe_corporal[pew$severe_corporal=="Don't know" | pew$severe_corporal=="Refused"]<- NA
pew$severe_corporal <- droplevels(relevel(pew$severe_corporal, "Oppose"))
table(pew$severe_corporal, pew$Q95D, exclude=NULL)

#STONING FOR ADULTERY
pew$stone_adultery <- pew$Q95E
pew$stone_adultery[is.na(pew$stone_adultery)] <- pew$Q94D[is.na(pew$stone_adultery)]
pew$stone_adultery[pew$stone_adultery=="Don't know" | pew$stone_adultery=="Refused"]<- NA
pew$stone_adultery <- droplevels(relevel(pew$stone_adultery, "Oppose"))
table(pew$stone_adultery, pew$Q95E, exclude=NULL)


# Other Attitudes ---------------------------------------------------------
#Non-religious attitudes that may be good to have on hand

#DEMOCRACY GOOD
pew$democracy <- pew$Q12
pew$democracy[pew$democracy=="Don't know" | pew$democracy=="Refused"]<- NA
pew$democracy <- factor(pew$democracy, 
                        levels=c("Strong leader","Democratic form of government"),
                        labels=c("Strong leader","Democracy"))
table(pew$democracy, pew$Q12, exclude=NULL)

#SAY IN GOVERNMENT
pew$say_govern <- pew$Q11B
pew$say_govern[pew$say_govern=="Don't know" | pew$say_govern=="Refused"]<- NA
pew$say_govern <- factor(pew$say_govern, levels=c("Completely disagree","Mostly disagree",
                                                  "Mostly agree", "Completely agree"))
table(pew$say_govern, pew$Q11B, exclude=NULL)

#LIKE WESTERN CULTURE
pew$western_culture <- pew$Q13
pew$western_culture[pew$western_culture=="Don't know" | pew$western_culture=="Refused"]<- NA
pew$western_culture <- droplevels(relevel(pew$western_culture, "I dislike western music, movies and television"))
table(pew$western_culture, pew$Q13, exclude=NULL)

#WESTERN CULTURE MORALLY BAD, Q26
pew$western_immoral <- pew$Q30
pew$western_immoral[pew$western_immoral=="Don't know" | pew$western_immoral=="Refused"]<- NA
pew$western_immoral <- factor(pew$western_immoral,
                              levels=c("Western music, movies and television have NOT hurt morality in our country",
                                       "Western music, movies and television have hurt morality in our country"),
                              labels=c("Western culture has NOT hurt morality","Western culture has hurt morality"))
table(pew$western_immoral, pew$Q30, exclude=NULL)

# Orphans -----------------------------------------------------------------

# Q89 - VIOLENCE AGAINST CIVILIAN TARGETS. 
pew$civilian_target <- pew$Q88
pew$civilian_target[pew$civilian_target=="Don’t know" | pew$civilian_target=="Refused"]<- NA
pew$civilian_target <- factor(pew$civilian_target,
                              levels=c("Never justified","Rarely justified","Sometimes justified","Often justified"))
table(pew$civilian_target, pew$Q88, exclude=NULL)


# Create Final Subset -------------------------------------------

#Now organize the data so I can merge it with the rest of the Pew data for
#Muslims. keep only muslims. Get rid of DR Congo, Botswana, Rwanda, South Africa, and
#Zambia (low numbers of muslims in sample and countries) Note that the report
#also left out Botswana, Rwanda, South Africa, and Zambia, because of small
#numbers of Muslims there. denomination and sufi questions were not asked in
#Mozambique

pew_africa <- subset(pew, !is.na(relig) & 
                       relig=="Muslim" &
                       country!="Botswana" & 
                       country!="South Africa" & 
                       country!="Rwanda" & 
                       country!="Zambia" & 
                       country!="Mozambique" & 
                       country!="Democratic Republic of the Congo",
                     select=c("country","caseid","weight",
                              "gender","age","marital","nchild","urbanicity","immigrant",
                              "use_internet","social_networking","own_cellphone",
                              "incomeqq","subj_econ","edattain","edattain.simple","educqq",
                              "relig","denom","sufi",
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
#drop relig variable
pew_africa <- pew_africa[,!(colnames(pew_africa) %in% "relig")]

save(pew_africa, file=here("analysis","output","pew_african_data.RData"))
