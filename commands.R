# For future reference in installing library from downloaded zip file in Windows
# install.packages("C:\\Users\\nacdacon\\Desktop\\data.table_1.9.4.zip",repos=NULL)
# Unfortunately you have to take care of dependencies manually this way.

library(foreign)
library(Hmisc)
library(data.table)
library(dplyr)

setwd(
  paste0("O:\\NACD\\NACD\\Data\\Data\\General Population survey 2010-11",
         "\\Stata files\\WorkingJOB")
)

prev <- data.table( read.dta("masternacd2010.dta") )


# To export the variable labels from Stata (v12) use these commands:
# describe, replace
# export excel name varlab using filename
# (From within Excel save as tab-limited file to save installing more libraries)
labels <- fread("PrevVarLabs.txt")

# The default column labels are awkward so replace them.
setnames(labels, names(labels), c("Var", "Question") )


# Define the regular expressions used to find questions relating to each drug.
spatterns <- matrix(
  data=c(
    "cocaine",          "cocaine",
    "heroin",           "heroin",
    "cannabis",         "cannabis|marijuana",
    "ecstasy",          "ecstasy",
    "lsd",              "lsd",
    "mushroom",         "mushroom",
    "opiates",          "opiates",
    "crack",            "crack",
    "amphetamines",     "amphetamine",
    "alcohol",          "alcohol|beer|wine|spirit|alcopop|cider|drink",
    "sedative",         "sedative|tranquil",
    "anti-depressants", "anti-depressant",
    "solvents",         "solvent",
    "poppers",          "poppers",
    "methadone",        "methadone",
    "steroids",         "anabolic|steroid"
  ),
  ncol=2, byrow=TRUE
)

# Make a list of the questions concerning alcohol or each drug.
for (r in 1:nrow(spatterns)) {
  # Make a logical vector to select out all the entries related to X.
  log_vec <- grepl(spatterns[r,2], labels$Question, ignore.case=TRUE)
  
  # Save the list of questions (labels).
  q_list <- labels$Question[log_vec]
  
  # Associate the drug name with the list.
  assign( spatterns[r,1], q_list)
}

# Tidy up.
rm(list=c("r", "q_list", "log_vec", "labels"))

# Extract (and clean) the number of dependant children.
children <- prev$c5b
children[prev$c5b==99] <- NA

# Calculate the Audit-C score.
# First code in the criteria.
# Scores taken from http://www.hepatitis.va.gov/provider/tools/audit-c.asp
audit_q1 <- matrix(
  data=c(
    "Never",                        "0",
    "Less often than once a month", "1",
    "Once a month",                 "1",
    "2-3 times a month",            "2",
    "Once a week",                  "2",
    "2/3 times a week",             "3",
    "4/5 times a week",             "4",
    "Daily",                        "4",
    "Don't know",                   "0",
    "Refused",                      "0"
  ),
  ncol=2, byrow=TRUE
)
audit_q1_score <- as.numeric(audit_q1[match(prev$q11b, audit_q1[,1]), 2])

audit_q2_data  <- prev$q11c
audit_q2_data[prev$q11c > 95] <- NA

audit_q2_class <- cut2(audit_q2_data,c(0,3,5,7,10))
audit_q2_score <- as.numeric(audit_q2_class) - 1

audit_q3 <- matrix(
  data=c(
    "Never",                        "0",
    "Less often than once a month", "1",
    "Once a month",                 "2",
    "2-3 times a month",            "3",
    "Once a week",                  "3",
    "2/3 times a week",             "3",
    "4/5 times a week",             "4",
    "Daily",                        "4",
    "Don't know",                   "0",
    "Refused",                      "0"
  ),
  ncol=2, byrow=TRUE
)
audit_q3_score <- as.numeric(audit_q3[match(prev$q11d, audit_q3[,1]), 2])

audit_c <- audit_q1_score + audit_q2_score + audit_q3_score

# For troubleshooting. Change 400 to anything <= length(audit_q1_score)
#tail(head(data.frame(audit_c,
#                     prev$q11b, audit_q1_score,
#                     prev$q11c, audit_q2_score,
#                     prev$q11d, audit_q3_score),
#        400))

rm(list=c(
  "audit_q2_class", "audit_q1_score", "audit_q2_score", "audit_q3_score", 
  "audit_q2_data", "audit_q1", "audit_q3")
)


# Calculate Audit-C cut-offs. A search of the literature shows no clear
# consensus on cut-off values. We have defined a US and a European cutoff here.
# The US cut-off is >=4 for men, >= 3 for women.
# The European cut-off is >= 5.
us_ac_high <- rep_len("No",length(audit_c))
us_ac_high[audit_c >= 4 & prev$sex=="Male"]   <- "Yes"
us_ac_high[audit_c >= 4 & prev$sex=="Female"] <- "Yes"

eu_ac_high <- rep_len("No",length(audit_c))
eu_ac_high[audit_c >= 5] <- "Yes"

# Create a table holding the drug/alcohol-usage variables.
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
  solvents_ever = prev$q98,  solvents_year = prev$q101, solvents_30d = prev$q102,
  steroids_ever = prev$q148, steroids_year = prev$q150, steroids_30d = prev$q151,
  audc_us = us_ac_high,
  audc_eu = eu_ac_high
)


# We only used spatterns for searching through the question texts.
rm("spatterns")

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
kids   <- apply(
  drug_data, 2, function(x) sum( children[grepl(regex,x)], na.rm=TRUE)
)

tot_adults <- length(children)
tot_kids   <- sum(children, na.rm=TRUE)

x <- data.frame(
  adults=adults,
  prop_adults=adults/tot_adults,
  kids=kids,
  prop_kids=kids/tot_kids
)

write.table(x,"Prevalence_2010_11_Summary.txt",sep="\t")
