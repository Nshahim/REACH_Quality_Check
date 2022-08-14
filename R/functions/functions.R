#label weekly data
week_labeler <- function(data, qa_status, cutoff_date, weekly_check=FALSE){
  
  if(weekly_check){
    cdc_id <- data %>% 
      filter(Status %in% qa_status) %>%
      filter(is.na(weekly_reporting_round)) %>% 
      filter(Startdate < cutoff_date) %>% 
      select(Village_Cdc_Ccdc_Gozar_Name_ID)
  } else {
    cdc_id <- data %>% 
      filter(Status %in% qa_status) %>%
      filter(Startdate %between% cutoff_date) %>% 
      select(Village_Cdc_Ccdc_Gozar_Name_ID)
  }

  data <- data %>% 
    filter(Status %in% qa_status) %>%
    filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% cdc_id$Village_Cdc_Ccdc_Gozar_Name_ID) %>% 
    group_by(Village_Cdc_Ccdc_Gozar_Name_ID) %>% 
    mutate(new_weekly_round = paste0(weekly_reporting_round, collapse = " ")) %>% 
    mutate(new_weekly_round = strsplit(new_weekly_round, " ") %>% unlist() %>% unique() %>% paste0(collapse = ", "))
  
  return(data)
}

#checking inconsistency among variables
check_inconsistency <- function(data){
  
  #Fixing inconsistencies across columns
  unique_vals <- data %>% 
    select(Province, District, Village_Cdc_Ccdc_Gozar_Name_ID, Village_Cdc_Ccdc_Gozar_Name, Project_Name, Line_Ministry_Name) %>% 
    unique() %>% 
    filter(duplicated(Village_Cdc_Ccdc_Gozar_Name_ID))
  
  unique_vals <- data %>% 
    filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% unique_vals$Village_Cdc_Ccdc_Gozar_Name_ID) %>% 
    select(Province, District, Village_Cdc_Ccdc_Gozar_Name_ID, Village_Cdc_Ccdc_Gozar_Name, Project_Name, Line_Ministry_Name, KEY) %>% 
    arrange(Village_Cdc_Ccdc_Gozar_Name_ID)
  
  return(unique_vals)
}

#Checks whether the CDC is in CASA area
CASA_check <- function(data, casa){
  subset <- data %>% 
    filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% casa$CDCCode)
  
  if(nrow(subset)  %in% 0){
    print("No CASA CDCs found!")
  } else{
    log <- data.frame("Key"=NA, "Village_Cdc_Ccdc_Gozar_Name"=NA, "Village_Cdc_Ccdc_Gozar_Name_ID"= NA, "Project_Name"=NA, "correct_project"=NA)
    for(i in 1:nrow(subset)){
      found_row <- casa %>% 
        filter(CDCCode %in% subset$Village_Cdc_Ccdc_Gozar_Name_ID[i])
      
      if(found_row$`Type of Areas` %in% "CASA_CCAP" & subset$Project_Name[i] %notin% "CASA CSP Relif"){
        log <- rbind(log, c(subset$KEY[i], subset$Village_Cdc_Ccdc_Gozar_Name_ID[i], subset$Village_Cdc_Ccdc_Gozar_Name[i], subset$Project_Name[i], "CASA CSP Relif"))
      }
      if(found_row$`Type of Areas` %in% "CASA_REACH" & subset$Project_Name[i] %notin% "CASA CSP REACH"){
        log <- rbind(log, c(subset$KEY[i], subset$Village_Cdc_Ccdc_Gozar_Name_ID[i], subset$Village_Cdc_Ccdc_Gozar_Name[i], subset$Project_Name[i], "CASA CSP REACH"))
      }
    }
    log <- log[-1,] %>% 
      arrange(Village_Cdc_Ccdc_Gozar_Name_ID)
    
    return(log)
  }
}

ministry_check <- function(data){
  #for processed raw_data
  log <- data.frame("Key"=NA, "Province"=NA, "District"=NA, "Village_Cdc_Ccdc_Gozar_Name_ID"=NA, "Village_Cdc_Ccdc_Gozar_Name"=NA,  "Line_Ministry_Name"=NA, "Project_Name"=NA, "Issue"=NA)
  for(i in 1:nrow(data)){
    if(grepl("M", data$Village_Cdc_Ccdc_Gozar_Name_ID[i]) & data$District[i] %notin% "Kabul" & data$Line_Ministry_Name[i] %notin% "MRRD (Rural)"){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Belongs to MRRD"))
    }
    
    if(grepl("I", data$Village_Cdc_Ccdc_Gozar_Name_ID[i]) & !grepl("R", data$Village_Cdc_Ccdc_Gozar_Name_ID[i]) & (data$Line_Ministry_Name[i] %notin% "IDLG (Urban)" | data$Project_Name[i] %notin% "CCAP Covid-19 Relief Response")){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Belongs to IDLG / CCAP"))
    }
    
    if(grepl("RI", data$Village_Cdc_Ccdc_Gozar_Name_ID[i], ignore.case = T) & (data$Line_Ministry_Name[i] %notin% "IDLG (Urban)" | data$Project_Name[i] %notin% "REACH")){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Belongs to IDLG / REACH"))
    }
    
    if(grepl("R", data$Village_Cdc_Ccdc_Gozar_Name_ID[i], ignore.case = T) & 
       !grepl("I", data$Village_Cdc_Ccdc_Gozar_Name_ID[i], ignore.case = T) & 
       (data$Line_Ministry_Name[i] %notin% "IDLG (Urban)" | data$Project_Name[i] %notin% "REACH") & data$District[i] %notin% "Kabul"){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Belongs to IDLG / REACH"))
    }
    
    if(grepl("K", data$Village_Cdc_Ccdc_Gozar_Name_ID[i], ignore.case = T) & (data$Line_Ministry_Name[i] %notin% "MRRD (Rural)" | data$Project_Name[i] %notin% "CCAP Covid-19 Relief Response")){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Belongs to MRRD / CCAP"))
    }
    
    if(data$District[i] %in% "Kabul" & (data$Line_Ministry_Name[i] %notin% "KM (Kabul)" | data$Project_Name[i] %notin% "KM")){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Belongs to KM"))
    }
    
    if((data$Line_Ministry_Name[i] %in% "KM (Kabul)" | data$Project_Name[i] %in% "KM") & data$District[i] %notin% "Kabul"){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Doesn't Belong to KM"))
    }
  }
  log <- log[-1,] %>% 
    arrange(Village_Cdc_Ccdc_Gozar_Name_ID)
  return(log)
}

clerk_check <- function(data, tracker){
  
  uuid <- vector()
  # status <- vector()
  completed <- vector()
  clerk_name <- vector()
  QA_status_of_data_clerk <- vector()
  fixed_by_clerk <- vector()
  
  for(i in 1:nrow(data)){
    
    found_row <- tracker %>% 
      filter(UUID %in% data$KEY[i])
    
    uuid <- c(uuid, found_row$UUID)
    # status <- c(status, found_row$Status)
    completed <- c(completed, found_row$Completed)
    clerk_name <- c(clerk_name, found_row$`Data Clerk Name`)
    QA_status_of_data_clerk <- c(QA_status_of_data_clerk, found_row$`QA Status of Data Clerk`)
    fixed_by_clerk <- c(fixed_by_clerk, found_row$`Is data clerk Fixed the Rejected Fill` )
  }
  
  report <- data.frame(uuid, completed, clerk_name, QA_status_of_data_clerk, fixed_by_clerk) %>% 
    mutate(completed = case_when(
      is.na(completed) ~ "Pending",
      TRUE ~ completed
    ),
    QA_status_of_data_clerk = case_when(
      is.na(QA_status_of_data_clerk) ~ "Pending",
      TRUE ~ QA_status_of_data_clerk
    ))
  
  uncleaned_list <- report %>% 
    filter(QA_status_of_data_clerk %notin% "Approved") %>% 
    arrange(QA_status_of_data_clerk)
  
  return(uncleaned_list)
}

CDC_check <- function(data){
  reach_sampling <- read_excel("input/reach_sampling (provinces, districts).xlsx")
  #level 1
  incorrect_id <- data %>% 
    filter(nchar(Village_Cdc_Ccdc_Gozar_Name_ID) %notin% c(13,14) | 
             (grepl("[^a-zA-Z0-9]+", Village_Cdc_Ccdc_Gozar_Name_ID) & !grepl("-", Village_Cdc_Ccdc_Gozar_Name_ID))) %>% 
    mutate(Issue = "Incorrect CDC Id")
  
  spelling_issue <- data %>% 
    filter(Encoding(Village_Cdc_Ccdc_Gozar_Name) %in% "UTF-8") %>% 
    filter(KEY %notin% incorrect_id$KEY) %>%
    mutate(Issue = "CDC spelling")
  
  small_letters <- data %>% 
    filter(toupper(Village_Cdc_Ccdc_Gozar_Name_ID) %notin% Village_Cdc_Ccdc_Gozar_Name_ID) %>% 
    filter(KEY %notin% c(incorrect_id$KEY, spelling_issue$KEY)) %>% 
    mutate(Issue = "Small letters in CDC ID")
  
  district_issue <- data %>% 
    filter(District %notin% reach_sampling$District) %>% 
    mutate(Issue = "District not in the sample")
  
  cdc_spelling <- data %>% 
    filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% complete_mis$CDCCode & 
             Village_Cdc_Ccdc_Gozar_Name %notin% complete_mis$CDCName[complete_mis$CDCCode %in% Village_Cdc_Ccdc_Gozar_Name_ID]) %>% 
    filter(KEY %notin% c(incorrect_id$KEY, spelling_issue$KEY, small_letters$KEY)) %>% 
    left_join(., complete_mis, by=c("Village_Cdc_Ccdc_Gozar_Name_ID"="CDCCode")) %>% 
    mutate(Issue = paste0("CDC name in MIS is: ", CDCName)) %>% 
    select(-CDCName)
  
  uncleaned_data <- rbind(spelling_issue, incorrect_id, small_letters, district_issue, cdc_spelling) %>% 
    select(KEY, Starttime, Province, District, Village_Cdc_Ccdc_Gozar_Name_ID, Village_Cdc_Ccdc_Gozar_Name, Issue)
    
  return(uncleaned_data)
}

create_log <- function(raw_data, cleaned_data, col_vec, identifier){
  uuid <- vector()
  question.name <- vector()
  old_val <- vector()
  new_val <- vector()
  
  for(index in 1:length(col_vec)){
    col_name <- col_vec[index]
    
    for(i in 1:length(cleaned_data[[col_name]])) {
      id <- cleaned_data[[identifier]][i]
      newVal <- cleaned_data[[col_name]][i]
      oldVal <- raw_data[raw_data[[identifier]] %in% id, col_name]
      
      if(!(newVal %in% oldVal[[1]])){
        uuid <- c(uuid, id)
        question.name <- c(question.name, col_name)
        old_val <- c(old_val, oldVal[[1]])
        new_val <- c(new_val, newVal)
      }  
    }
  }
  logVal <- data.frame(question.name, old_val, new_val, uuid)
  return(logVal)
}



##to compare each row of the tab with the main tab 
cross_check <- function(data, checkAgainst){
  inconsistent_data <- data.frame()
  
  for(i in 1:nrow(data)){
    cdc_id <- data$Village_Cdc_Ccdc_Gozar_Name_ID[i]
    
    found_row <- checkAgainst %>% 
      filter(Village_Cdc_Ccdc_Gozar_Name_ID %in% cdc_id) %>% 
      select(Province, District, Village_Cdc_Ccdc_Gozar_Name_ID, Village_Cdc_Ccdc_Gozar_Name, Project_Name, Line_Ministry_Name) %>% 
      unique()
    
    incon_columns <- vector()
    other_value <- vector()
    
    col_check <- 0
    
    for(j in 2:ncol(data)){
      column_value <- data[[j]][i]
      
      if(column_value %notin% found_row[[j]][1]){
        col_check <- col_check + 1
        incon_columns <- c(incon_columns, names(data[j]))
        other_value <- c(other_value, found_row[[j]][1])
      }
    }
    
    if(col_check > 0){
      inconsistent_data <- data[i,] %>% 
        mutate(inconsistent_Columns = paste0(incon_columns, collapse = " - "),
               other_form_value = paste0(other_value, collapse = " - ")) %>% 
        rbind(inconsistent_data)
    }
  }
  
  return(inconsistent_data)
}

clerk_log <- function(data, log){
  
  new_log <- data.frame(question=NA, old_value=NA, new_value=NA, uuid=NA)
  for(i in 1:nrow(log)){
    key <- log$uuid[i]
    question <- log$question[i]
    new_value <- log$new_value[i]
    
    if(data[[question]][data$KEY %in% key] %notin% new_value){
      new_log <- rbind(new_log, log[i,])
    }
  }
  new_log <- new_log[-1,]
  
  return(new_log)
}

mis_check <- function(data){
  log <- data.frame("Key"=NA, "Province"=NA, "District"=NA, "Village_Cdc_Ccdc_Gozar_Name_ID"=NA, "Village_Cdc_Ccdc_Gozar_Name"=NA,  "Line_Ministry_Name"=NA, "Project_Name"=NA, "Issue"=NA)
  
  for(i in 1:nrow(data)){
    cdc_id <- data$Village_Cdc_Ccdc_Gozar_Name_ID[i]
    cdc_name <- data$Village_Cdc_Ccdc_Gozar_Name[i]
    proj_name <- data$Project_Name[i]
    minis_name <- data$Line_Ministry_Name[i]
    
    #CDC in Reach MIS but project Name not "REACH"
    if(cdc_id %in% reach_mis$CDCCode & cdc_id %notin% casa$CDCCode & proj_name %notin% "REACH"){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "REACH"))
    }
    
    #CDC in CCAP MIS but project Name not CCAP
    if(cdc_id %in% c(relief_mis$CDCCode, kochi_mis$CDCCode, sig_mis$CDCCode) & cdc_id %notin% casa$CDCCode & proj_name %notin% "CCAP Covid-19 Relief Response"){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "CCAP Covid-19 Relief Response"))
    }
    
    #CDC in IDLG MIS but project Name not REACH
    if(cdc_id %in% idlg_mis$CDCCode & proj_name %notin% "REACH"){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "REACH"))
    }
    
    #CDC in CASA relief but project name not CASA CSP Relief
    if(cdc_id %in% casa$CDCCode[casa$`Type of Areas` %in% "CASA_CCAP"] & proj_name %notin%  "CASA CSP Relif"){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "CASA CSP Relif"))
    }
    
    #CDC in CASA reach but project name not CASA CSP REACH
    if(cdc_id %in% casa$CDCCode[casa$`Type of Areas` %in% "CASA_REACH"] & proj_name %notin%  "CASA CSP REACH"){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "CASA CSP REACH"))
    }
    
    #Project name in CASA but cdc not in CASA MIS
    if(proj_name %in% c("CASA CSP Relif", "CASA CSP REACH") & data$Village_Cdc_Ccdc_Gozar_Name_ID[i] %notin% casa$CDCCode){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Not CASA"))
    }
    
    #Project in REACH but cdc not in reach or idlg MIS
    if(proj_name %in% "REACH" & cdc_id %notin% reach_mis$CDCCode & cdc_id %notin% idlg_mis$CDCCode){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Not Reach"))
    }
    
    #Project in CCAP but cdc not in Relief MIS
    if(proj_name %in% "CCAP Covid-19 Relief Response" & cdc_id %notin% c(relief_mis$CDCCode, kochi_mis$CDCCode, sig_mis$CDCCode)){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "Not CCAP"))
    }
    
    #CDC in reach or relief MIS but ministry not MRRD
    if(cdc_id %in%  c(reach_mis$CDCCode, relief_mis$CDCCode) & minis_name %notin% "MRRD (Rural)"){
      log <- rbind(log, c(data$KEY[i], data$Province[i], data$District[i], data$Village_Cdc_Ccdc_Gozar_Name_ID[i], data$Village_Cdc_Ccdc_Gozar_Name[i], data$Line_Ministry_Name[i], data$Project_Name[i], "MRRD (Rural)"))
    }
    
  }
  
  log <- log[-1,] %>% 
    arrange(Issue)
  
  return(log)
}
