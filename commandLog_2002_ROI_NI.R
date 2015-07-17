# For future reference in installing library from downloaded zip file in Windows
# install.packages("C:\\Users\\nacdacon\\Desktop\\data.table_1.9.4.zip",repos=NULL)
# Unfortunately you have to take care of dependencies manually this way.

library(foreign)
library(Hmisc)
library(data.table)
library(dplyr)

setwd("O:\\NACD\\NACD\\Data\\Data\\Population Survey 2002-3\\WorkingJOB")


prev <- read.spss("NACD drug survey 2002_ROI_NI.sav", to.data.frame=TRUE)



# Extract (and clean) the number of dependant children.
# Two variables CHILDA and CHILDB. No question on survey from concerning
# children.
#children <- prev$c5b
#children[prev$c5b < 0] <- NA

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
#auditc_q1 <- as.character(prev$Q11)
#auditc_q1[prev$Q11=="No"] <- 0    # People who haven't had a drink in 12 months.


# Q12 Have you had a drink in the last 30 days.
# Nobody who said "No" to Q11 said "Yes" to this one - table(prev$Q11,prev$Q12)
#auditc_q1[prev$Q11=="Yes" & prev$Q12=="No"] <- 1 # Monthly or less.

# Q13 How many days have you had a drink on in last 30 days.
# No-one who said "No" to Q11 or Q12 has given a number here.
# Go through the possibilities, higher values get over-written at each stage.
#auditc_q1[prev$Q13 == 1] <- 1  # Monthly or less
#auditc_q1[prev$Q13 >  1] <- 2  # 2-4 times per month
#auditc_q1[prev$Q13 >  4] <- 3  # 2-3 times per week
#auditc_q1[prev$Q13 > 12] <- 4  # 4+ times per week

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

#auditc_q3[prev$Q14 == 1] <- 4
#auditc_q3[prev$Q14 == 2] <- 3   # *** Questionable ***
#auditc_q3[prev$Q14 == 3] <- 3
#auditc_q3[prev$Q14 == 4] <- 1
#auditc_q3[prev$Q14 == 5] <- 2
#auditc_q3[prev$Q14 == 6] <- 1
# == 7 is already catered for
#auditc_q3[prev$Q14 >  7] <- NA

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
# This is valid for the 2002/03 survey
drug_data <- data.frame(
  amphet_ever   = prev$Q42,  amphet_year   = prev$Q44,  amphet_30d   = prev$Q45,
  cannabis_ever = prev$Q26,  cannabis_year = prev$Q28,  cannabis_30d = prev$Q29,
  cocaine_ever  = prev$Q56,  cocaine_year  = prev$Q58,  cocaine_30d  = prev$Q59,
  crack_ever    = prev$Q49,  crack_year    = prev$Q51,  crack_30d    = prev$Q52,
  ecstasy_ever  = prev$Q35,  ecstasy_year  = prev$Q37,  ecstasy_30d  = prev$Q38,
  heroin_ever   = prev$Q64,  heroin_year   = prev$Q66,  heroin_30d   = prev$Q67,
  lsd_ever      = prev$Q72,  lsd_year      = prev$Q74,  lsd_30d      = prev$Q75,
  methadone_ever= prev$Q100, methadone_year= prev$Q102, methadone_30d= prev$Q103,
  mushroom_ever = prev$Q93,  mushroom_year = prev$Q95,  mushroom_30d = prev$Q96,
  opiates_ever  = prev$Q114, opiates_year  = prev$Q116, opiates_30d  = prev$Q117,
  poppers_ever  = prev$Q86,  poppers_year  = prev$Q88,  poppers_30d  = prev$Q89,
  sedative_ever = prev$Q17,  sedative_year = prev$Q19,  sedative_30d = prev$Q20,
  solvents_ever = prev$Q79,  solvents_year = prev$Q81,  solvents_30d = prev$Q82
)
#  audc_us = us_ac_high,
#  audc_eu = eu_ac_high


# anti-depressants grouped with sedatives/tranquillisers.
# no entry for anabolic steroids.
# 

# For some reason read.dta has read some of the above columns as codes rather
# than labels.
# By inspection, here is the mapping between them.
#
# -1  Blank
#  1  Yes
#  2  No
#  3  Don't know
#  4  Refused
# Where the codes are saved as strings there is a single left-padding space.

regex <- "^(Yes| 1)$"
adults <- apply( drug_data, 2, function(x) sum( grepl(regex,x), na.rm=TRUE) )
#kids   <- apply(
#  drug_data, 2, function(x) sum( children[grepl(regex,x)], na.rm=TRUE)
#)

tot_adults <- nrow(drug_data)
#tot_kids   <- sum(children, na.rm=TRUE)

x <- data.frame(
  adults=adults,
  prop_adults=adults/tot_adults
)
#,
#  kids=kids,
#  prop_kids=kids/tot_kids
#)

write.table(x,"Prevalence_2002_ROI_NI_Summary.txt",sep="\t")
