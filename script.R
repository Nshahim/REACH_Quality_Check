rm(list=ls())
source("R/functions/functions.R")

# Libraries --------------------------------------------------------------------
library(dplyr)
library(readxl)
library(readr)
library(googlesheets4)
library(tidyr)
library(data.table)
library(janitor)
# Loading Datasets -------------------------------------------------
`%notin%` <- Negate(`%in%`)

DO <- read_excel("input/raw_data/REACH Direct Observation Form.xlsx", guess_max = 100000) 
form1 <- read_excel("input/raw_data/REACH PRE-DISTRIBUTION FORM 1.xlsx", guess_max = 100000)
form2 <- read_excel("input/raw_data/REACH PRE-DISTRIBUTION FORM 2.xlsx", guess_max = 100000)

DO <- DO %>% 
  filter(Status %notin% "Rejected") 
form1 <- form1 %>% 
  filter(Status %notin% "Rejected")
form2 <- form2 %>% 
  filter(Status %notin% "Rejected")

#date filter
# data <- data %>%
#   filter(Startdate < as.Date("2021-06-01"))

#renaming form1 (for the sake of simplicity in the rest of the codes)
form1 <- form1 %>% 
  rename(Village_Cdc_Ccdc_Gozar_Name = CDC_CCDC_Gozar_Name,
         Village_Cdc_Ccdc_Gozar_Name_ID = CDC_CCDC_Gozar_ID)
#renaming form2 
form2 <- form2 %>% 
  rename(Village_Cdc_Ccdc_Gozar_Name_ID = Cdc_Ccdc_Gozar_Id)



# weekly report -------------------------------------------------
# c("Approved", "Pending", NA)
status <- c("Approved", "Pending", NA)
cutoff_date <- c("2021-07-27", "2021-08-02")

DO_filtered <- week_labeler(DO, status, cutoff_date)
form1_filtered <- week_labeler(form1, status, cutoff_date)
form2_filtered <- week_labeler(form2, status, cutoff_date)


# for weekly checks
# DO_filtered <- DO %>% 
#   filter(!is.na(weekly_reporting_round))
# form1_filtered <- form1 %>% 
#   filter(!is.na(weekly_reporting_round))
# form2_filtered <- form2 %>% 
#   filter(!is.na(weekly_reporting_round))

# Ad Hoc Check (Line_Ministy_Name -------------------------------------------------
DO_ministry_log <- ministry_check(DO_filtered)
form1_ministry_log <- ministry_check(form1_filtered)
form2_ministry_log <- ministry_check(form2_filtered)

writexl::write_xlsx(DO_ministry_log, "output/line_ministry_issue/DO_Line_ministry_issue.xlsx")
writexl::write_xlsx(form1_ministry_log, "output/line_ministry_issue/form1_Line_ministry_issue.xlsx")
writexl::write_xlsx(form2_ministry_log, "output/line_ministry_issue/form2_Line_ministry_issue.xlsx")



# Quick checks -------------------------------------------------
reach_mis <- read_excel("input/MIS/MRRD_Reach_MIS.xlsx")
relief_mis <- read_excel("input/MIS/MRRD_Relief_MIS.xlsx")
idlg_mis <- read_excel("input/MIS/IDLG_MIS.xlsx")
kochi_mis <- read_excel("input/MIS/MRRD_Relief_kochi_MIS.xlsx")
casa <- read_excel("input/MIS/CASA_MIS.xlsx")
kabul_mis <- read_excel("input/MIS/Kabul Reach MIS (translated).xlsx")
sig_mis <- read_excel("input/MIS/SIG_mis.xlsx")

DO_mis_log <- mis_check(DO_filtered)
form1_mis_log <- mis_check(form1_filtered)
form2_mis_log <- mis_check(form2_filtered)

writexl::write_xlsx(DO_mis_log, "output/mis_log/DO_mis_log.xlsx")
writexl::write_xlsx(form1_mis_log, "output/mis_log/form1_mis_log.xlsx")
writexl::write_xlsx(form2_mis_log, "output/mis_log/form2_mis_log.xlsx")



# CDC cleaning log -------------------------------------------------
complete_mis <- rbind(
  reach_mis %>% select(CDCCode, CDCName),
  relief_mis %>% select(CDCCode, CDCName),
  kochi_mis %>% select(CDCCode, CDCName),
  idlg_mis %>% select(CDCCode, CDCName),
  casa %>% select(CDCCode, CDCName),
  kabul_mis %>% select(CDCCode, CDCName),
  sig_mis %>% select(CDCCode, CDCName)
)

DO_uncleaned_CDC <- CDC_check(DO_filtered)
form1_uncleaned_CDC <- CDC_check(form1_filtered)
form2_uncleaned_CDC <- CDC_check(form2_filtered)

writexl::write_xlsx(DO_uncleaned_CDC, "output/uncleaned_cdc/DO_uncleaned_CDC.xlsx")
writexl::write_xlsx(form1_uncleaned_CDC, "output/uncleaned_cdc/form1_uncleaned_CDC.xlsx")
writexl::write_xlsx(form2_uncleaned_CDC, "output/uncleaned_cdc/form2_uncleaned_CDC.xlsx")

#ad hoc
DO_filtered <- DO_filtered %>% 
  filter(KEY %notin% DO_uncleaned_CDC$KEY)
form1_filtered <- form1_filtered %>% 
  filter(KEY %notin% form1_uncleaned_CDC$KEY)
form2_filtered <- form2_filtered %>% 
  filter(KEY %notin% form2_uncleaned_CDC$KEY)



# Inconsistency across data -------------------------------------------------
DO_incon <- check_inconsistency(DO_filtered)
form1_incon <- check_inconsistency(form1_filtered)
form2_incon <- check_inconsistency(form2_filtered)

writexl::write_xlsx(DO_incon, "output/inconsistenct_data/DO_inconsistent_data.xlsx")
writexl::write_xlsx(form1_incon, "output/inconsistenct_data/form1_inconsistent_data.xlsx")
writexl::write_xlsx(form2_incon, "output/inconsistenct_data/form2_inconsistent_data.xlsx")



# Inconsistency across forms -------------------------------------------------
vars <- c(
  "Province", 
  "District", 
  "Village_Cdc_Ccdc_Gozar_Name_ID", 
  "Village_Cdc_Ccdc_Gozar_Name", 
  "Project_Name", 
  "Line_Ministry_Name"
)
#form1
form1_unique_vals <- form1_filtered %>% 
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% form2$Village_Cdc_Ccdc_Gozar_Name_ID) %>% 
  select(all_of(vars)) %>% 
  unique() %>% 
  arrange(Village_Cdc_Ccdc_Gozar_Name_ID)
# Form2
form2_unique_vals <- form2_filtered %>% 
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% form1$Village_Cdc_Ccdc_Gozar_Name_ID) %>% 
  select(all_of(vars)) %>% 
  unique() %>% 
  arrange(Village_Cdc_Ccdc_Gozar_Name_ID)

#Cross checking each value
form1_crosscheck_log <- cross_check(form1_unique_vals, form2)
form2_crosscheck_log <- cross_check(form2_unique_vals, form1)
#combining the key 
form1_crosscheck_log <- left_join(form1_crosscheck_log, select(form1, all_of(vars), KEY, weekly_reporting_round), by=vars)
form2_crosscheck_log <- left_join(form2_crosscheck_log, select(form2, all_of(vars), KEY, weekly_reporting_round), by=vars)

writexl::write_xlsx(form1_crosscheck_log, "output/cross_check/form1_crosscheck_log.xlsx")
writexl::write_xlsx(form2_crosscheck_log, "output/cross_check/form2_crosscheck_log.xlsx")


# Tracker update -------------------------------------------------
gs4_deauth()
tracker_sheet <- readr::read_csv("")

# Direct observation \ Pre-distribution form 1
DO_tracker <- tracker_sheet %>% 
  filter(`Form Type` %in% "Direct observation")
form1_tracker <- tracker_sheet %>% 
  filter(`Form Type` %in% "Pre-distribution form 1")

DO_clerk_log <- clerk_check(DO_filtered, DO_tracker)
form1_clerk_log <- clerk_check(form1_filtered, form1_tracker)

progress_report <- list("DO"=DO_clerk_log, "Form1"=form1_clerk_log)
writexl::write_xlsx(progress_report, "output/uncleaned_data_entry.xlsx")



# Data Clerk Check -------------------------------------------------
DO_questions <- c(
  "Staff_Wearing_Masks",
  "According_To_Hand_Washing_Sanitizer_Spray_For_Beneficiaries",
  "Photo_hand_Washing_show_soap",
  "According_Pictures_Evidence_Social_Distancing",
  "Picture_Confirm_Complaints_Information_Visible",
  "Picture_Clear_Form_1",
  "Picture_Clear_Form_1_No_Why",
  "Form1_Female_Headed",
  "Form1_A_Disability",
  "Form1_65_And_Above",
  "Hh_Entered_Under_Details_Not_Shown_Above_CCAP",
  "New_Idp_Hh",
  "New_Returnee_Hh",
  "Economic_Migrants_Hh",
  "New_Kuchis_Hh",
  "Covid1_Migrant_Hh",
  "Form_1_Signed_All_Required",
  "Picture_Clear_Form_2",
  "If_Form_2_No_Why",
  "Form2_Signed_Required",
  "Picture_Clear_Form3",
  "Picture_Clear_Form3_No_Why",
  "Form3_Signed_Required",
  "Picture_Clear_Form_4A",
  "Picture_Clear_Form_4A_No_Why",
  "Form_4A_Signed_Required",
  "Picture_Clear_Of_The_Entire_Form_4B",
  "Picture_Clear_Of_4B_No_Why",
  "Form_4B_Signed_Required_Signatories",
  "Rice_Kg_Per_Hh_Form_4B",
  "Flour_Kg_Per_Hh_Form_4B",
  "Quantity_Oil_Per_Hh_Form_4B",
  "Beans_Kg_Per_Hh_Form_4B",
  "Kg_Hh_Per_Hh_Form_4B",
  "Soap_Pieces_Per_Hh_Form_4B",
  "Picture_Clear_Copy_Form_5A_Or_5B",
  "Picture_Clear_Copy_Form_5A_Or_5B_No_Why",
  "Is_Form_5A_Or_5B_Signed_By_All_Required_Signatories",
  "Rice_Kg_Per_Hh_Form_5",
  "Flour_Kg_Per_Hh_Form_5",
  "Quantity_Oil_Litres_Per_Hh_Form_5",
  "Beans_Kg_Per_Hh_Form_5",
  "Kg_Per_Hh_Per_Form_5",
  "Soap_Bar_Per_Hh_Per_Form_5",
  "Money_Per_HH_Per_Form_5",
  "Other_Amount_Integer_Per_Form_5"
)
# form1 questions
form1_questions <- c(
  "Is_The_Picture_A_Clear_Copy_Of_The_Entire_Form",
  "Reasons_For_Why_The_Copy_Of_The_Form_Is_Not_Clear",
  "Election_unit_number",
  "Number_eligible_HH_form1",
  "Number_ineligible_HH_form1",
  "Number_Of_Female_Headed_Household",
  "Number_Of_HH_Headed_By_A_Person_With_A_Disability",
  "Number_Of_HH_Headed_By_Elderly",
  "Number_Of_HH_Entered_Under_Details_To_Be_Added_For_HHs_Not_Shown_Above_From_The_Time_Of_Ccap_Mobilization_Of_This_Community",
  "New_Idp_HH",
  "New_Returnee_HH",
  "Economic_Migrants_HH",
  "New_Kuchis_HH",
  "Covid_19_Migrant_HH",
  "Where_These_Households_Came_From_Province",
  "Where_These_Households_Came_From_District",
  "Where_These_Households_Came_From_Village"
)

# DO (check for any abnormal value)
DO_question_vals <- data.frame(question=character(), uniq_val=character())
for(i in DO_questions){
  DO_question_vals <- rbind(DO_question_vals, c(i, DO_filtered[[i]] %>% unique() %>% paste(., collapse = " -- ")))
}
View(DO_question_vals)
#form1
form1_question_vals <- data.frame(question=character(), uniq_val=character())
for(i in form1_questions){
  form1_question_vals <- rbind(form1_question_vals, c(i, form1_filtered[[i]] %>% unique() %>% paste(., collapse = " -- ")))
}
View(form1_question_vals)



# Cross Checking form1 and form2  -------------------------------------------------
# CDCs in form2 that are not in form1 and vice versa
form1_approved <- form1 %>% filter(Status %in% "Approved") %>% select(Village_Cdc_Ccdc_Gozar_Name_ID)
not_in_form1 <- form2_filtered %>% 
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %notin% form1_approved$Village_Cdc_Ccdc_Gozar_Name_ID) %>%
  select(KEY, Starttime, Province, District, Village_Cdc_Ccdc_Gozar_Name_ID, Village_Cdc_Ccdc_Gozar_Name, Village, weekly_reporting_round, Status) %>% 
  mutate(Issue = "CDC Id not in Form1") %>% arrange(Village_Cdc_Ccdc_Gozar_Name_ID)

form2_approved <- form2 %>% filter(Status %in% "Approved") %>% select(Village_Cdc_Ccdc_Gozar_Name_ID)
not_in_form2 <- form1_filtered %>% 
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %notin% form2_approved$Village_Cdc_Ccdc_Gozar_Name_ID) %>%
  select(KEY, Starttime, Province, District, Village_Cdc_Ccdc_Gozar_Name_ID, Village_Cdc_Ccdc_Gozar_Name, Village, weekly_reporting_round, Status) %>% 
  mutate(Issue = "CDC Id not in Form2") %>% arrange(Village_Cdc_Ccdc_Gozar_Name_ID)

writexl::write_xlsx(not_in_form1, "output/not_in_form1.xlsx")
writexl::write_xlsx(not_in_form2, "output/not_in_form2.xlsx")


#CDCs that exist in each form but are not QAed yet
not_QAed_form1 <- form1 %>% 
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% form2_filtered$Village_Cdc_Ccdc_Gozar_Name_ID & Status %notin% c("Approved", "Rejected")) %>% 
  select(KEY, Starttime, Province, District, Village_Cdc_Ccdc_Gozar_Name_ID, Village_Cdc_Ccdc_Gozar_Name, Village, Status) %>% 
  arrange(Village_Cdc_Ccdc_Gozar_Name_ID)
not_QAed_form2 <- form2 %>% 
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% form1_filtered$Village_Cdc_Ccdc_Gozar_Name_ID & Status %notin%  c("Approved", "Rejected")) %>% 
  select(KEY, Starttime, Province, District, Village_Cdc_Ccdc_Gozar_Name_ID, Village_Cdc_Ccdc_Gozar_Name, Village, Status) %>% 
  arrange(Village_Cdc_Ccdc_Gozar_Name_ID)

not_QAed_list <- list("not_QAed_form1" = not_QAed_form1, "not_QAed_form2"=not_QAed_form2)
writexl::write_xlsx(not_QAed_list, "output/not_QAed_list.xlsx")




# Security Incidents  -------------------------------------------------
DO_security_incidents <- DO_filtered %>%
  # filter(weekly_reporting_round %in% 24) %>%
  filter(Security_Incidents_Observed %in% "Yes") %>%
  select(Province, District, Village_Cdc_Ccdc_Gozar_Name, Village, Village_Cdc_Ccdc_Gozar_Name_ID, Security_Incidents_Observed)
writexl::write_xlsx(DO_security_incidents, "output/security_incidents_observed.xlsx")


# Exporting Data   -------------------------------------------------
# filtering non-existing CDCs from the weekly report
#for form1
form1_filtered <- form1_filtered %>%
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %notin% not_in_form2$Village_Cdc_Ccdc_Gozar_Name_ID)
#for form2
form2_filtered <- form2_filtered %>%
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %notin% not_in_form1$Village_Cdc_Ccdc_Gozar_Name_ID)

#Renaming form1 and form2 for the export
form1_filtered <- form1_filtered %>% 
  rename(CDC_CCDC_Gozar_Name = Village_Cdc_Ccdc_Gozar_Name,
         CDC_CCDC_Gozar_ID = Village_Cdc_Ccdc_Gozar_Name_ID)
form2_filtered <- form2_filtered %>% 
  rename(Cdc_Ccdc_Gozar_Id = Village_Cdc_Ccdc_Gozar_Name_ID)

writexl::write_xlsx(DO_filtered, "output/week_specific_data/REACH Direct Observation Form.xlsx")
writexl::write_xlsx(form1_filtered, "output/week_specific_data/REACH PRE-DISTRIBUTION FORM 1.xlsx")
writexl::write_xlsx(form2_filtered, "output/week_specific_data/REACH PRE-DISTRIBUTION FORM 2.xlsx")


# Labeling unlabeled data (weekly)  ---------------------------------------------
form1_filtered <- form1 %>% 
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% form2$Village_Cdc_Ccdc_Gozar_Name_ID[form2$Status %in% "Approved"])
form2_filtered <- form2 %>% 
  filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% form1$Village_Cdc_Ccdc_Gozar_Name_ID[form1$Status %in% "Approved"])

# c("Approved", "Pending", NA)
status <- c("Approved")
cutoff_date <- as.Date("2021-07-27")

DO_filtered <- week_labeler(DO, status, cutoff_date, weekly_check = T)
form1_filtered <- week_labeler(form1_filtered, status, cutoff_date, weekly_check = T)
form2_filtered <- week_labeler(form2_filtered, status, cutoff_date, weekly_check = T)

# form1_filtered <- form1_filtered %>% 
#   filter(Project_Name %notin% "KM")
# form2_filtered <- form2_filtered %>% 
#   filter(Project_Name %notin% "KM")


writexl::write_xlsx(DO_filtered, "output/unlabeled_data/REACH Direct Observation Form.xlsx")
writexl::write_xlsx(form1_filtered, "output/unlabeled_data/REACH PRE-DISTRIBUTION FORM 1.xlsx")
writexl::write_xlsx(form2_filtered, "output/unlabeled_data/REACH PRE-DISTRIBUTION FORM 2.xlsx")


# Ad Hoc Requests ---------------------------------------------
####LOG ####
raw_data <- read_excel("respondent_fixed.xlsx", sheet = "HH_Not_found")
cleaned_data <- read_excel("respondent_fixed.xlsx", sheet = "Respondent_fixed")

log <- create_log(raw_data, cleaned_data, c("Name_Of_Respondent", "Phone_Number_Of_Respondent"), "KEY")
log <- log %>% mutate(uuid = gsub("/.*", "", uuid))


writexl::write_xlsx(log, "output/log.xlsx")

# CASA CSP filter  ####
data <- data %>% 
  # filter(weekly_reporting_round %in% c(12:16)) %>% 
  filter(Project_Name %in% c("CASA CSP Relif", "CASA CSP REACH")) %>% 
  select(KEY, SubmissionDate, Startdate, Project_Name, Province, District, Village_Cdc_Ccdc_Gozar_Name, Village, Village_Cdc_Ccdc_Gozar_Name_ID, weekly_reporting_round)

Visit_per_week <- table(data$Project_Name) %>% 
  data.frame()

#renaming form1
data <- data %>% 
  rename(CDC_CCDC_Gozar_Name = Village_Cdc_Ccdc_Gozar_Name,
         CDC_CCDC_Gozar_ID = Village_Cdc_Ccdc_Gozar_Name_ID)
#renaming form2 
data <- data %>% 
  rename(Village_Cdc_Ccdc_Gozar_Name = Village_Cdc_Ccdc_Gozar_Name,
         Cdc_Ccdc_Gozar_Id= Village_Cdc_Ccdc_Gozar_Name_ID)

CASA_data <- list("DO"=DO, "Form1"=form1, "Form2"=form2)
CASA_data <- list("Visits_per_week"=Visit_per_week, "data"=data)
writexl::write_xlsx(CASA_data, "output/CASA data/Form2_CASA.xlsx")


# creating log for the clerks to clean ####
#filtering repeated uuids from the log
cleaned_link <- ""
DO_log <- read_sheet(cleaned_link, sheet = "DO_log")
form1_log <- read_sheet(cleaned_link, sheet = "Form1_log") %>% select(-check)
form2_log <- read_sheet(cleaned_link, sheet = "Form2_log")%>% select(-check)

#Renaming form1 and form2 for the export
form1 <- form1 %>% 
  rename(CDC_CCDC_Gozar_Name = Village_Cdc_Ccdc_Gozar_Name,
         CDC_CCDC_Gozar_ID = Village_Cdc_Ccdc_Gozar_Name_ID)
form2 <- form2 %>% 
  rename(Cdc_Ccdc_Gozar_Id = Village_Cdc_Ccdc_Gozar_Name_ID)

for (rowi in 1:nrow(DO_log)){
  
  uuid_i <- DO_log$uuid[rowi]
  var_i <- DO_log$question[rowi]
  old_i <- DO_log$old_value[rowi]
  new_i <- DO_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  DO[DO$KEY == uuid_i, var_i] <- new_i
}

for (rowi in 1:nrow(form1_log)){
  
  uuid_i <- form1_log$uuid[rowi]
  var_i <- form1_log$question[rowi]
  old_i <- form1_log$old_value[rowi]
  new_i <- form1_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  form1[form1$KEY == uuid_i, var_i] <- new_i
}

for (rowi in 1:nrow(form2_log)){
  
  uuid_i <- form2_log$uuid[rowi]
  var_i <- form2_log$question[rowi]
  old_i <- form2_log$old_value[rowi]
  new_i <- form2_log$new_value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  form2[form2$KEY == uuid_i, var_i] <- new_i
}

writexl::write_xlsx(DO, "input/raw_data/REACH Direct Observation Form.xlsx")
writexl::write_xlsx(form1, "input/raw_data/REACH PRE-DISTRIBUTION FORM 1.xlsx")
writexl::write_xlsx(form2, "input/raw_data/REACH PRE-DISTRIBUTION FORM 2.xlsx")

#non-existent uuids
not_in_do <- DO_log %>% 
  filter(uuid %notin% DO$KEY)
not_in_form1 <- form1_log %>% 
  filter(uuid %notin% form1$KEY)
not_in_form2 <- form2_log %>% 
  filter(uuid %notin% form2$KEY)

# repeated uuids
DO_repeated <- DO_log %>% 
  get_dupes(question, uuid)
form1_repeated <- form1_log %>% 
  get_dupes(question, uuid)
form2_repeated <- form2_log %>% 
  get_dupes(question, uuid)

repeated_list <- list(DO_repeated=DO_repeated, form1_repeated=form1_repeated, form2_repeated=form2_repeated)
writexl::write_xlsx(repeated_list, "output/log/repeated_logs.xlsx")

#logs of data not changed on CTO
DO_new_log <- clerk_log(DO, DO_log)
form1_new_log <- clerk_log(form1, form1_log)
form2_new_log <- clerk_log(form2, form2_log)

writexl::write_xlsx(DO_new_log, "output/log/DO_unchanged_data.xlsx")
writexl::write_xlsx(form1_new_log, "output/log/form1_unchanged_data.xlsx")
writexl::write_xlsx(form2_new_log, "output/log/form2_unchanged_data.xlsx")

#combining IDLG list ####

Badghis <- read_excel("input/IDLG Lists_14 June/provinces/Badghis.xlsx")
balkh <- read_excel("input/IDLG Lists_14 June/provinces/Balkh.xlsx")
kandahar <- read_excel("input/IDLG Lists_14 June/provinces/Kandahar.xlsx")
kapisa <- read_excel("input/IDLG Lists_14 June/provinces/Kapisa.xlsx")
nangarhar <- read_excel("input/IDLG Lists_14 June/provinces/Nangarhar.xlsx")
nimroz <- read_excel("input/IDLG Lists_14 June/provinces/Nimroz.xlsx")
paktia <- read_excel("input/IDLG Lists_14 June/provinces/Paktia.xlsx")
panjshir <- read_excel("input/IDLG Lists_14 June/provinces/Panjshir.xlsx")
parwan <- read_excel("input/IDLG Lists_14 June/provinces/Parwan.xlsx")

IDLG_list <- rbind(Badghis, balkh, kandahar, kapisa, nangarhar, nimroz, paktia, panjshir, parwan)
writexl::write_xlsx(IDLG_list, "input/IDLG Lists_14 June/IDLG_list_5July.xlsx")

#joing the translated ones
idlg <- read_excel("input/IDLG Lists_14 June/IDLG_list_5July.xlsx")
updated <- read_excel("input/IDLG Lists_14 June/IDLG English updated.xlsx")

idlg <- idlg %>% 
  select(Region, Province, District, `Community Code`, `Community Name`) %>% 
  unique()

idlg <- idlg %>% 
  mutate(`Community Name` = case_when(
    `Community Code` %in% updated$`Community Code` ~ updated$`Community Name`[updated$`Community Code` %in% `Community Code`],
    TRUE ~ `Community Name`
  ))

test <- left_join(idlg, select(updated, `Community Code`, `Community Name`), by="Community Code")


######
# Beneficiary (Fahima Ad Hoc request) ####
rm(list=ls())
`%notin%` <- Negate(`%in%`)

data <- read_excel("input/raw_data/fahima/REACH Direct Observation Form.xlsx", guess_max = 100000) 

data$Hh_Observed_Receiving_Aid_Package_Per_Team_Surveyor <- as.numeric(data$Hh_Observed_Receiving_Aid_Package_Per_Team_Surveyor) 
data$Hh_Observed_Complina_Not_Receiving_Aid_Package_Per_Team_Surveyor <- as.numeric(data$Hh_Observed_Complina_Not_Receiving_Aid_Package_Per_Team_Surveyor)

filtered_data <- data %>% 
  # filter(QA_status %in% "app") %>% 
  # filter(If_rejected_reason_for_the_rejection. %notin% c("test","Test")) %>% 
  group_by(Village_Cdc_Ccdc_Gozar_Name_ID) %>% 
  mutate(Beneficiary = sum(Hh_Observed_Receiving_Aid_Package_Per_Team_Surveyor, Hh_Observed_Complina_Not_Receiving_Aid_Package_Per_Team_Surveyor, na.rm = T)) %>% 
  select(Date_And_Time, Province, District, Village_Cdc_Ccdc_Gozar_Name, 
         Village_Cdc_Ccdc_Gozar_Name_ID, Hh_Observed_Receiving_Aid_Package_Per_Team_Surveyor,Hh_Observed_Complina_Not_Receiving_Aid_Package_Per_Team_Surveyor, Beneficiary) %>% 
  arrange(Village_Cdc_Ccdc_Gozar_Name_ID)

filtered <- test %>% 
  mutate(count = if_else(!is.na(Beneficiary), 1, 0)) %>% 
  group_by(Village_Cdc_Ccdc_Gozar_Name_ID) %>%
  filter(sum(count)>1)

writexl::write_xlsx(filtered_data, "output/fahima/DO_Beneficiary_list_more_than_one (all data).xlsx")


data <- read_excel("input/raw_data/fahima/form2_HH not found.xlsx", guess_max = 100000) 

#form2
test <- data %>% 
  # filter(If_rejected_reason_for_the_rejection. %notin% c("test","Test", "it was a test")) %>% 
  # filter(QA_status %in% "app") %>% 
  group_by(Cdc_Ccdc_Gozar_Id) %>% 
  mutate(Total = sum(Number_Of_Hh_Not_Found_Through_Door_To_Door)) %>% 
  # select(Date_And_Time, Line_Ministry_Name, Province, District, Village_Cdc_Ccdc_Gozar_Name, 
  #        Cdc_Ccdc_Gozar_Id, Number_Of_Hh_Not_Found_Through_Door_To_Door, Total, QA_status) %>% 
  arrange(Cdc_Ccdc_Gozar_Id)


writexl::write_xlsx(test,"output/fahima/Form2_Number_of_hh_filtered (All data).xlsx")

#Nik -----------------------------
data <- read_excel("input/REACH JUNE LOG.xlsx")

test <- data %>% 
  filter(Name %in% "")
  
data$`Clock In` <- format(strptime(data$`Clock In`, format = "%H:%M"), "%H:%M:%S")

data$`Clock Out` <- format(strptime(data$`Clock Out`, format = "%H:%M"), "%H:%M:%S")



difftime(data$`Clock Out`[43],data$`Clock In`[43], units = "hours")

data <- data %>% 
  mutate(
    `Clock In` = format(as.POSIXct(data$`Clock In`, format = "%H:%M"), "%H:%M:%S"),
    `Clock Out`= format(as.POSIXct(data$`Clock Out`, format = "%H:%M"), "%H:%M:%S"),
    hours_perday = round(as.numeric(times(`Clock Out`) - times(`Clock In`)) * 24, 1)
  )

june_timesheet <- data %>%   
  group_by(`AC-No.`, Name, Department = "REACH") %>% 
  summarize(
    total_days = n(),
    days_absent = sum(is.na(`Clock In`) & is.na(`Clock Out`)),
    partial = sum((!is.na(`Clock In`) & is.na(`Clock Out`)) | is.na(`Clock In`) & !is.na(`Clock Out`)),
    days_present = total_days - days_absent - partial,
    arrived_on_time = sum(`Clock In` <= times("08:15:00"), na.rm = T),
    arrived_late = sum(`Clock In` > times("08:15:00"), na.rm = T),
    left_on_time = sum(`Clock Out` >= times("16:15:00"), na.rm = T),
    left_early = sum(`Clock Out` < times("16:15:00"), na.rm = T),
    total_hours = round(sum(hours_perday, na.rm = T), 2),
    average_hours = round(total_hours/days_present,2)
  ) %>% 
  ungroup() %>%
  mutate(average_hours = case_when(
    is.na(average_hours) & days_present == 0 ~ 0,
    TRUE ~ average_hours
  ))

data <- read_excel("input/CCAP Revised Social InPerson Tool.xlsx")
test2 <- data %>% select(Surveyor_Id, Surveyor_Name) %>% unique() %>% get_dupes(Surveyor_Id)