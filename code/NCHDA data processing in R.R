#########################data processing in NCHDA############################################
## process diagnosis/procedure/previous procedure/comorbidity filed
## Specific procedure algorithm and activity algorithm allocation 
#####################################################################
rm(list = setdiff(ls(), "con"))

library(odbc)
library(DBI)
library(dplyr)
library(lubridate)
library(tidyr)
library("tibble")
library(openxlsx)

# first load the NCHDA dataset db

db$patient_identifier <- db$NHS_NUMBER_DEID

# set the format
db$type_procedure <- as.integer(as.character(gsub("[^Q0-9]", "", db[, "3_07_TYPE_OF_PROCEDURE"])))
db$procedure_date <- as_date(db[, "3_01_DATE_OF_VISIT"])
db$discharge_date <- as_date(db[, "4_01_DATE_OF_DISCHARGE"])

# rename the diagnosis filed, comorbidity fields, procedure filed and previous procedure field
db$alldiagnosis <- db[, "2_01_DIAGNOSIS1"]
db$allcomorbidity <- db[, "2_07_COMORBID_CONDITIONS"]
db$allproc <- db[, "3_09_OPERATION_PERFORMED"]
db$allprevproc <- db[, "2_02_PREVIOUS_PROCEDURE"]

######################################################################################
## process procedure  field
## number of separated fields for diagnosis, comorbidity, previous procedure and procedure codes
procedure_fields <- sprintf("procedure_%d", seq(1:8))

## create new fields for the codes
db <- separate(db, col = allproc, procedure_fields, sep = ";", convert = FALSE, extra = "drop", remove = FALSE)

db <- db %>%
  ## ensure any other characters are removed and only the first 6 characters are kept
  mutate(proccode1 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_1)), 6))) %>%
  mutate(proccode2 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_2)), 6))) %>%
  mutate(proccode3 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_3)), 6))) %>%
  mutate(proccode4 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_4)), 6))) %>%
  mutate(proccode5 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_5)), 6))) %>%
  mutate(proccode6 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_6)), 6))) %>%
  mutate(proccode7 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_7)), 6))) %>%
  mutate(proccode8 = (strtrim(as.character(gsub("[^Q0-9]", "", procedure_8)), 6))) 

######################################################################################
## process diagnosis  field
diagnosis_fields <- sprintf("diagnosis_%d", seq(1:10))
db <- separate(db, col = alldiagnosis, diagnosis_fields, sep = ";", convert = FALSE, extra = "drop", remove = FALSE)

db <- db %>%
  ## ensure any other characters are removed and only the first 6 characters are kept
  mutate(diagcode1 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_1)), 6))) %>%
  mutate(diagcode2 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_2)), 6))) %>%
  mutate(diagcode3 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_3)), 6))) %>%
  mutate(diagcode4 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_4)), 6))) %>%
  mutate(diagcode5 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_5)), 6))) %>%
  mutate(diagcode6 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_6)), 6))) %>%
  mutate(diagcode7 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_7)), 6))) %>%
  mutate(diagcode8 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_8)), 6))) %>%
  mutate(diagcode9 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_9)), 6))) %>%
  mutate(diagcode10 = (strtrim(as.character(gsub("[^Q0-9]", "", diagnosis_10)), 6)))


######################################################################################
## process previous procedure field

num_prevproc <- 20
prevproc_fields <- sprintf("prevproc_%d", seq(1:num_prevproc))
prevproccodes_fields <- sprintf("prevproccode%d", seq(1:num_prevproc))

db[, c(prevproc_fields, prevproccodes_fields)] <- NULL
db <- separate(db, col = allprevproc, prevproc_fields, sep = ";", convert = FALSE, extra = "drop", remove = FALSE)

length(which(!is.na(db$prevproc_20))) # up to 20

## create new fields for the codes
db <- db %>%
  ## ensure any other characters are removed and only the first 6 characters are kept
  mutate(prevproccode1 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_1)), 6))) %>%
  mutate(prevproccode2 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_2)), 6))) %>%
  mutate(prevproccode3 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_3)), 6))) %>%
  mutate(prevproccode4 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_4)), 6))) %>%
  mutate(prevproccode5 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_5)), 6))) %>%
  mutate(prevproccode6 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_6)), 6))) %>%
  mutate(prevproccode7 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_7)), 6))) %>%
  mutate(prevproccode8 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_8)), 6))) %>%
  mutate(prevproccode9 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_9)), 6))) %>%
  mutate(prevproccode10 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_10)), 6))) %>%
  mutate(prevproccode11 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_11)), 6))) %>%
  mutate(prevproccode12 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_12)), 6))) %>%
  mutate(prevproccode13 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_13)), 6))) %>%
  mutate(prevproccode14 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_14)), 6))) %>%
  mutate(prevproccode15 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_15)), 6))) %>%  
  mutate(prevproccode16 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_16)), 6))) %>%
  mutate(prevproccode17 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_17)), 6))) %>%
  mutate(prevproccode18 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_18)), 6))) %>%  
  mutate(prevproccode19 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_19)), 6))) %>%
  mutate(prevproccode20 = (strtrim(as.character(gsub("[^Q0-9]", "", prevproc_20)), 6))) 

######################################################################################
## process comorbidity field
num_comorbidity <- 12
comorbidity_fields <- sprintf("comorbid_%d", seq(1:num_comorbidity))
comorbiditycodes_fields <- sprintf("comorbidity%d", seq(1:num_comorbidity))

db[, c(comorbidity_fields, comorbiditycodes_fields)] <- NULL
db <- separate(db, col = allcomorbidity, comorbidity_fields, sep = ";", convert = FALSE, extra = "drop", remove = FALSE)

length(which(!is.na(db$comorbid_12))) # up to 12

length(which(!is.na(db$proccode8)))
## create new fields for the codes
db <- db %>%
  ## ensure any other characters are removed and only the first 6 characters are kept
  mutate(comorbidity1 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_1)), 6))) %>%
  mutate(comorbidity2 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_2)), 6))) %>%
  mutate(comorbidity3 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_3)), 6))) %>%
  mutate(comorbidity4 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_4)), 6))) %>%
  mutate(comorbidity5 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_5)), 6))) %>%
  mutate(comorbidity6 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_6)), 6))) %>%
  mutate(comorbidity7 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_7)), 6))) %>%
  mutate(comorbidity8 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_8)), 6))) %>%
  mutate(comorbidity9 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_9)), 6))) %>%
  mutate(comorbidity10 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_10)), 6))) %>%
  mutate(comorbidity11 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_11)), 6))) %>%
  mutate(comorbidity12 = (strtrim(as.character(gsub("[^Q0-9]", "", comorbid_12)), 6)))


source("02.nchda.aa_sp_shared_codes_v8.05.R")
###################################################################################################
## Activity algorithm (AA) allocation 
#The algorithm is developed and used by National Institute for Cardiovascular Outcomes Research (NICOR) (version 8.03)
source("04.activity_analysis_algorithm_v8.03_ QH.R")
db <- db %>%
  mutate(aa_allocation = "") %>%
  mutate(fuvh = "") %>%
  ## sort file > important for activity allocation (ecmo)
  arrange(patient_identifier, procedure_date) %>%
  ## allocate row_id to a named column
  rowid_to_column()


db <- aa_allocation(db)
aa_allocation <- db$aa_allocation
###################################################################################################
## run and update primary ecmo
## subgroup of records - 1,2,4,6,7,11 (3 added for 2016-19 analysis)
nchda_ecmo <- subset(db, ((type_procedure %in% c(1, 2, 3, 4, 6, 7, 11) & (aa_allocation != "unallocated") & (aa_allocation != "0:no_valid_codes"))))
nchda_ecmo <- ecmo_allocation(nchda_ecmo)

ecmo_records <- nchda_ecmo %>%
  filter(aa_allocation == "12:primary_ecmo") %>%
  select(rowid, primary_ecmo = aa_allocation)

db <- db %>%
  left_join(ecmo_records, by = c("rowid" = "rowid")) %>%
  mutate(aa_allocation = replace(aa_allocation, primary_ecmo == "12:primary_ecmo", "12:primary_ecmo"))

######################################### end of AA##########################################################



###################################################################################################
## Specific procedure (SP) allocation 
#The algorithm is developed and used by National Institute for Cardiovascular Outcomes Research (NICOR) (version 8.05)

## run sp allocation processes
source("05.specific_procedure_algorithm_v8.05_QH.R")
db[1:nrow(db), "sp_allocation"] <- sapply(1:nrow(db), function(i) sp_algorithm(db[i, ]))
summary(db$sp_allocation)

######################################### end of SP##########################################################

