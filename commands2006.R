# For future reference in installing library from downloaded zip file in Windows
# install.packages("C:\\Users\\nacdacon\\Desktop\\data.table_1.9.4.zip",repos=NULL)
# Unfortunately you have to take care of dependencies manually this way.

library(foreign)
library(Hmisc)
library(data.table)
library(dplyr)

setwd("O:\\NACD\\NACD\\Data\\Data\\Population Survey 2006-7\\WorkingJOB")

prev <- read.spss("nacd_drugs_survey_2006_ROI.sav", to.data.frame=TRUE)

# Extract (and clean) the number of dependant children.
children <- prev$c5b
children[prev$c5b < 0] <- NA
# Calculate the Audit-C score.
# Scores taken from http://www.hepatitis.va.gov/provider/tools/audit-c.asp
# Q1: How often did you have a drink containing alcohol in the past year?
# Answer	                Points
# Never	                    0
# Monthly or less	          1
# Two to four times a month	2
# Two to three times a week	3
# Four or more times a week	4


# Need to combine a couple of questions from the survey to build the score.
# Q11 Any alcohol in last 12 months

#auditc_q1 <- as.character(prev$q11)
#auditc_q1[prev$q11=="No"] <- 0    # People who haven't had a drink in 12 months.


# Q12 Have you had a drink in the last 30 days.
# Nobody who said "No" to Q11 said "Yes" to this one - table(prev$Q11,prev$Q12)
#auditc_q1[prev$q11=="Yes" & prev$q12=="No"] <- 1 # Monthly or less.

# Q13 How many days have you had a drink on in last 30 days.
# No-one who said "No" to Q11 or Q12 has given a number here.
# Go through the possibilities, higher values get over-written at each stage.
#auditc_q1[prev$q13 == 1] <- 1  # Monthly or less
#auditc_q1[prev$q13 >  1] <- 2  # 2-4 times per month
#auditc_q1[prev$q13 >  4] <- 3  # 2-3 times per week
#auditc_q1[prev$q13 > 12] <- 4  # 4+ times per week

# Take any "Yes" values left as representing drinking less than once per month.
#auditc_q1[auditc_q1=="Yes"] <- 1
#auditc_q1 <- as.numeric(auditc_q1)

# AUDIT-C question.
# Q2: How many drinks did you have on a typical day when you were drinking in 
# the past year?
# Answer	              Points
# None, I do not drink	0
#  1 or 2	              0
#  3 or 4	              1
#  5 or 6	              2
#  7 to 9	              3
# 10 or more	          4

# No matching variable in data set.
#auditc_q2 <- rep_len(NA, nrow(prev))


# Next AUDIT-C question.
# Q3: How often did you have six or more drinks on one occasion in the past 
# year?
# Answer	              Points
# Never	                0
# Less than monthly	    1
# Monthly	              2
# Weekly	              3
# Daily or almost daily	4

# Survey question
# Q14 How often did you drink 6+ drinks on same occasion.
# 1  - Daily/almost daily
# 2  - 2/3 times/week
# 3  - Every week
# 4  - 2/3 times/month
# 5  - Every month
# 6  - Less than monthly
# 7  - Never
# 7+ - Don't know/No answer
#auditc_q3 <- rep_len(0,nrow(prev))

#auditc_q3[prev$q14 == 1] <- 4
#auditc_q3[prev$q14 == 2] <- 3   # *** Questionable ***
#auditc_q3[prev$q14 == 3] <- 3
#auditc_q3[prev$q14 == 4] <- 1
#auditc_q3[prev$q14 == 5] <- 2
#auditc_q3[prev$q14 == 6] <- 1
# == 7 is already catered for
#auditc_q3[prev$q14 >  7] <- NA

# Sum the three scores. They'll all be NA until we find a source for Q2
#auditc <- auditc_q1 + auditc_q2 + auditc_q3


# Calculate Audit-C cut-offs. A search of the literature shows no clear
# consensus on cut-off values. We have defined a US and a European cutoff here.
# The US cut-off is >=4 for men, >= 3 for women.
# The European cut-off is >= 5.
#us_ac_high <- rep_len(NA,length(auditc))
#us_ac_high[auditc >= 4 & prev$GENDER=="Male"]   <- "Yes"
#us_ac_high[auditc <  4 & prev$GENDER=="Male"]   <- "No"
#us_ac_high[auditc >= 3 & prev$GENDER=="Female"] <- "Yes"
#us_ac_high[auditc <  3 & prev$GENDER=="Female"] <- "No"

#eu_ac_high <- rep_len(NA,length(auditc))
#eu_ac_high[auditc >= 5] <- "Yes"
#eu_ac_high[auditc <  5] <- "No"

# Create a table holding the drug/alcohol-usage variables.
# Question numbers identified from folder in NACDA office.
# This is valid for the 2006/07 survey
drug_data <- data.frame(
  amphet_ever   = prev$q55,  amphet_year   = prev$q58,  amphet_30d   = prev$q59,
  antidep_ever  = prev$q27,  antidep_year  = prev$q29,  antidep_30d  = prev$q30,
  cannabis_ever = prev$q36,  cannabis_year = prev$q39,  cannabis_30d = prev$q40,
  cocaine_ever  = prev$q72,  cocaine_year  = prev$q75,  cocaine_30d  = prev$q76,
  crack_ever    = prev$q64,  crack_year    = prev$q67,  crack_30d    = prev$q68,
  ecstasy_ever  = prev$q47,  ecstasy_year  = prev$q50,  ecstasy_30d  = prev$q51,
  heroin_ever   = prev$q81,  heroin_year   = prev$q84,  heroin_30d   = prev$q85,
  lsd_ever      = prev$q90,  lsd_year      = prev$q93,  lsd_30d      = prev$q94,
  methadone_ever= prev$q123, methadone_year= prev$q125, methadone_30d= prev$q126,
  mushroom_ever = prev$q114, mushroom_year = prev$q117, mushroom_30d = prev$q118,
  opiates_ever  = prev$q140, opiates_year  = prev$q142, opiates_30d  = prev$q143,
  poppers_ever  = prev$q106, poppers_year  = prev$q109, poppers_30d  = prev$q110,
  sedative_ever = prev$q18,  sedative_year = prev$q20,  sedative_30d = prev$q21,
  solvents_ever = prev$q97,  solvents_year = prev$q101, solvents_30d = prev$q102,
  steroids_ever = prev$q148, steroids_year = prev$q150, steroids_30d = prev$q151
)
#  audc_us = us_ac_high,
#  audc_eu = eu_ac_high

adults <- apply( drug_data, 2, function(x) sum(x=="Yes", na.rm=TRUE) )
kids   <- apply( drug_data, 2, function(x) sum( children[x=="Yes"], na.rm=TRUE))
tot_adults <- length(children)
tot_kids   <- sum(children, na.rm=TRUE)

x <- data.frame(
  adults=adults,
  prop_adults=adults/tot_adults,
  kids=kids,
  prop_kids=kids/tot_kids
)

write.table(x,"Prevalence_2006_Summary.txt",sep="\t")


