rm(list=ls())
# Libraries  ------------------------------
library(readxl)
library(dplyr)
library(data.table)
library(lubridate)
`%notin%` <- Negate(`%in%`)

date = "2021-08-04"
#MRRD REACH MIS ------------------------------
files <- list.files("MIS/MRRD_Reach/", pattern = "*.xlsx", full.names =T)
MRRD_reach_MIS = rbindlist(lapply(files, read_excel))

MRRD_reach_MIS <- MRRD_reach_MIS %>% 
  filter(SN %notin% "Total") %>% 
  unique()

#changes
path = "input/MIS/MIS_changes_log.xlsx"
sheets <- readxl::excel_sheets(path)
changes_log <- lapply(sheets, function(X) read_excel(path, sheet = X))
names(changes_log) = sheets
changes_log[["reach_log"]] <- rbind(changes_log[["reach_log"]], c(length(unique(MRRD_reach_MIS$Province)), 
                                              length(unique(MRRD_reach_MIS$District)), 
                                              length(unique(MRRD_reach_MIS$CDCCode)), 
                                              Date=date))

writexl::write_xlsx(MRRD_reach_MIS, "input/MIS/MRRD_Reach_MIS.xlsx")
writexl::write_xlsx(MRRD_reach_MIS, paste0("MIS/backup/MRRD_Reach/MRRD_Reach_MIS_", today(),".xlsx"))
writexl::write_xlsx(changes_log, "input/MIS/MIS_changes_log.xlsx")

#MRRD Relief MIS ------------------------------
files <- list.files("MIS/MRRD_Relief/", pattern = "*.xlsx", full.names =T)
MRRD_relief_MIS = rbindlist(lapply(files, read_excel))

MRRD_relief_MIS <- MRRD_relief_MIS %>% 
  filter(SN %notin% "Total") %>% 
  unique()

#changes
path = "input/MIS/MIS_changes_log.xlsx"
sheets <- readxl::excel_sheets(path)
changes_log <- lapply(sheets, function(X) read_excel(path, sheet = X))
names(changes_log) = sheets
changes_log[["relief_log"]] <- rbind(changes_log[["relief_log"]], 
                                     c(length(unique(MRRD_relief_MIS$Province)), 
                                              length(unique(MRRD_relief_MIS$District)), 
                                              length(unique(MRRD_relief_MIS$CDCCode)), 
                                              Date=date))

writexl::write_xlsx(MRRD_relief_MIS, "input/MIS/MRRD_Relief_MIS.xlsx")
writexl::write_xlsx(MRRD_relief_MIS, paste0("MIS/backup/MRRD_Relief/MRRD_Relief_MIS_", today(),".xlsx"))
writexl::write_xlsx(changes_log, "input/MIS/MIS_changes_log.xlsx")

#MRRD Relief Kochi MIS ------------------------------
files <- list.files("MIS/MRRD_Relief_kochi/", pattern = "*.xlsx", full.names =T)
MRRD_relief_kochi_MIS = rbindlist(lapply(files, read_excel))

MRRD_relief_kochi_MIS <- MRRD_relief_kochi_MIS %>% 
  filter(SN %notin% "Total") %>% 
  unique()

#changes
path = "input/MIS/MIS_changes_log.xlsx"
sheets <- readxl::excel_sheets(path)
changes_log <- lapply(sheets, function(X) read_excel(path, sheet = X))
names(changes_log) = sheets
changes_log[["relief_kochi_log"]] <- rbind(changes_log[["relief_kochi_log"]], 
                                           c(length(unique(MRRD_relief_kochi_MIS$Province)), 
                                              length(unique(MRRD_relief_kochi_MIS$District)), 
                                              length(unique(MRRD_relief_kochi_MIS$CDCCode)), 
                                              Date=date))

writexl::write_xlsx(MRRD_relief_kochi_MIS, "input/MIS/MRRD_Relief_kochi_MIS.xlsx")
writexl::write_xlsx(MRRD_relief_kochi_MIS, paste0("MIS/backup/MRRD_Relief_kochi/MRRD_Relief_kochi_MIS_", today(),".xlsx"))
writexl::write_xlsx(changes_log, "input/MIS/MIS_changes_log.xlsx")

#CASA MIS ------------------------------
#CASA Reach
files <- list.files("MIS/CASA_Reach/", pattern = "*.xlsx", full.names =T)
CASA_Reach = rbindlist(lapply(files, read_excel))

CASA_Reach <- CASA_Reach %>% 
  filter(SN %notin% "Total") %>% 
  unique() %>% 
  mutate(`Type of Areas` = "CASA_REACH") %>% 
  select(1:5, `Type of Areas`, 6:16)

#changes
path = "input/MIS/MIS_changes_log.xlsx"
sheets <- readxl::excel_sheets(path)
changes_log <- lapply(sheets, function(X) read_excel(path, sheet = X))
names(changes_log) = sheets
changes_log[["casa_reach_log"]] <- rbind(changes_log[["casa_reach_log"]], 
                                         c(length(unique(CASA_Reach$Province)), 
                                              length(unique(CASA_Reach$District)), 
                                              length(unique(CASA_Reach$CDCCode)), 
                                              Date=date))

#CASA Relief
files <- list.files("MIS/CASA_Relief/", pattern = "*.xlsx", full.names =T)
CASA_Relief = rbindlist(lapply(files, read_excel))

CASA_Relief <- CASA_Relief %>% 
  filter(SN %notin% "Total") %>% 
  unique() %>% 
  mutate(`Type of Areas` = "CASA_CCAP") %>% 
  select(1:5, `Type of Areas`, 6:16)
#changes
changes_log[["casa_relief_log"]] <- rbind(changes_log[["casa_relief_log"]], 
                                         c(length(unique(CASA_Relief$Province)), 
                                           length(unique(CASA_Relief$District)), 
                                           length(unique(CASA_Relief$CDCCode)), 
                                           Date=date))

CASA_MIS <- rbind(CASA_Reach, CASA_Relief)
writexl::write_xlsx(CASA_MIS, "input/MIS/CASA_MIS.xlsx")
writexl::write_xlsx(CASA_MIS, paste0("MIS/backup/CASA_Reach_Relief/CASA_MIS_", today(),".xlsx"))
writexl::write_xlsx(changes_log, "input/MIS/MIS_changes_log.xlsx")

#SIG MIS  ------------------------------
sig_mis <- read_excel("MIS/SIG_MIS/SIG Report.xlsx")

sig_mis <- sig_mis %>% 
  select(Province, District, `CDC Code`, `CDC Name`) %>% 
  rename(CDCCode = `CDC Code`, CDCName = `CDC Name`)

#changes
path = "input/MIS/MIS_changes_log.xlsx"
sheets <- readxl::excel_sheets(path)
changes_log <- lapply(sheets, function(X) read_excel(path, sheet = X))
names(changes_log) = sheets
changes_log[["sig_log"]] <- rbind(changes_log[["sig_log"]], 
                                         c(length(unique(sig_mis$Province)), 
                                           length(unique(sig_mis$District)), 
                                           length(unique(sig_mis$CDCCode)), 
                                           Date=date))
writexl::write_xlsx(sig_mis, "input/MIS/SIG_mis.xlsx")
writexl::write_xlsx(sig_mis, paste0("MIS/backup/SIG_MIS/SIG_mis_", today(),".xlsx"))
writexl::write_xlsx(changes_log, "input/MIS/MIS_changes_log.xlsx")

#IDLG ------------------------------
idlg_translation <- read_excel("input/IDLG Lists_14 June/IDLG English updated_8thJuly.xlsx")
files <- list.files("MIS/IDLG/", pattern = "*.xlsx", full.names =T)
idlg_MIS = rbindlist(lapply(files, read_excel))

idlg_MIS <- idlg_MIS %>% 
  select(Region, Province, District, `Community Code`, `Community Name`) %>% 
  rename(CDCCode = `Community Code`, CDCName = `Community Name`) %>%
  unique()

idlg_MIS <- left_join(idlg_MIS, select(idlg_translation, CDCCode, CDCName), by="CDCCode") %>% 
  mutate(CDCName.x = case_when(
    !is.na(CDCName.y) ~ CDCName.y,
    TRUE ~ CDCName.x
  )) %>% 
  rename(CDCName = CDCName.x) %>% 
  select(-CDCName.y)


#changes
path = "input/MIS/MIS_changes_log.xlsx"
sheets <- readxl::excel_sheets(path)
changes_log <- lapply(sheets, function(X) read_excel(path, sheet = X))
names(changes_log) = sheets
changes_log[["idlg_log"]] <- rbind(changes_log[["idlg_log"]], c(length(unique(idlg_MIS$Province)), 
                                                                  length(unique(idlg_MIS$District)), 
                                                                  length(unique(idlg_MIS$CDCCode)), 
                                                                  Date=date))

writexl::write_xlsx(idlg_MIS, "input/MIS/IDLG_MIS.xlsx")
writexl::write_xlsx(idlg_MIS, paste0("MIS/backup/IDLG/IDLG_MIS_", today(),".xlsx"))
writexl::write_xlsx(changes_log, "input/MIS/MIS_changes_log.xlsx")


# Compiling all the MIS in one file for the clerks  ------------------------------
reach_mis <- read_excel("input/MIS/MRRD_Reach_MIS.xlsx")
relief_mis <- read_excel("input/MIS/MRRD_Relief_MIS.xlsx")
idlg_mis <- read_excel("input/MIS/IDLG_MIS.xlsx")
kochi_mis <- read_excel("input/MIS/MRRD_Relief_kochi_MIS.xlsx")
casa <- read_excel("input/MIS/CASA_MIS.xlsx")
kabul_mis <- read_excel("input/MIS/Kabul Reach MIS (translated).xlsx")
sig_mis <- read_excel("input/MIS/SIG_mis.xlsx")

complete_mis <- rbind(
  reach_mis %>% select(Province, District, CDCCode, CDCName) %>% mutate(MIS = "MRRD REACH"),
  relief_mis %>% select(Province, District, CDCCode, CDCName) %>% mutate(MIS = "MRRD Relief"),
  kochi_mis %>% select(Province, District, CDCCode, CDCName) %>% mutate(MIS = "MRRD Relief Kochi"),
  idlg_mis %>% select(Province, District, CDCCode, CDCName) %>% mutate(MIS = "IDLG"),
  casa %>% select(Province, District, CDCCode, CDCName) %>% mutate(MIS = "CASA"),
  kabul_mis %>% mutate(Province="Kabul", District="Kabul") %>% select(Province, District, CDCCode, CDCName) %>% mutate(MIS = "Kabul"),
  sig_mis %>% select(Province, District, CDCCode, CDCName) %>% mutate(MIS = "SIG")
)
writexl::write_xlsx(complete_mis, "output/MIS for clerks/Complete_MIS_for_clerks.xlsx")


# reachlog <- read_excel("input/MIS/MRRD_Reach_MIS.xlsx")
# reachlog %>% filter(District %notin% MRRD_reach_MIS$District) %>% select(District) %>% unique()