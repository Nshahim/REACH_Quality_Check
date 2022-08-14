rm(list=ls())
if(!require("shinyjs")) install.packages("shinyjs")
library(httr)
library(dplyr)
library(readxl)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyjs)
source("functions/functions.R")

`%notin%` <- Negate(`%in%`)

# Offline Data Inputs -----------------------------------------------------
covid <- read_excel("input/Covid_response_MIS_merged_data.xlsx")
ccap <- read_excel("input/CCAP_MIS_IDLG_011120.xlsx")

MIS_data <- rbind(covid %>% 
                    select(Province, District, CDCCode, CDCName),
                  ccap %>%
                    rename(CDCCode = `CDC Code`,
                           CDCName = `CDC Name`) %>% 
                    select(Province, District, CDCCode, CDCName))

ui <- dashboardPage(
  
  dashboardHeader(title = "Uncleaned Data Extraction"),
  
  dashboardSidebar(
    selectInput("form_type", "Choose a dataset:",
                choices = c("Direct Observation", "Form 1", "Form 2")),
    radioButtons("date_radio", "Do you want to filter by date?", choices = c("Yes", "No"), selected = "No"),
    uiOutput("display_date")
  ),
  
  dashboardBody(
    textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
    passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
    actionButton("extract", "Extract", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
    disabled(downloadButton("download", "Download", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;")),
    dataTableOutput("filtered_data")
  )
)

server <- function(input, output, session) {
  
  output$display_date <- renderUI({
    validate(
      need(!is.null(input$date_radio), "Please select an answer!")
    )
    if(input$date_radio %in% "Yes"){
      dateInput("date", "Download all data before: ")
    }
  })
  
  formID <- reactive({
    switch(input$form_type,
           "Direct Observation" = "TPMA_REACH_Direct_Observation_FORM",
           "Form 1" = "TPMA-REACH_PRE-DISTRIBUTION_FORM_1",
           "Form 2" = "TPMA_Reach_Predistribution_Monitoring_Approach_Tools_Form2")
  })
  
  date_radio <- reactive({
    input$date_radio
  })
  
  observe({
    toggleState("extract", !is.null(input$userName) && input$name != "")
  })
  
  observeEvent(input$extract, {
    # Getting credentials
    cto_user <- input$userName
    cto_pass <- input$passwd
    cto_link <- paste0("")
    
    tryCatch(
      expr = {
        
        print("Downloading Data!")
        # Get data ----------------------------------------------------
        withProgress(message = "Donwloading Data!", value =0, {
          response_long <- GET(cto_link,
                               authenticate(cto_user, cto_pass),
                               write_memory(), timeout(1000),
                               progress(type = "down"))
          data <- readr::read_csv(content(response_long, as = "text", type = "text/csv" , encoding = "UTF-8"))
        })
        
        # Process Date and Time ---------------------------------------
        SubmissionDate_temp <- as.POSIXlt(data$SubmissionDate, format='%B %d, %Y %I:%M:%S') + hours(4) + minutes(30)
        data$SubmissionDate <- strftime(SubmissionDate_temp)
        
        # Filter data if Date is provided
        if(date_radio() %in% "Yes") {
          data <- data %>%
            filter(SubmissionDate < as.Date(input$date))
        }
        
        print("Filtering Data!")
        #level 1
        uncleaned_data <- data %>% 
          filter(CDC_CCDC_Gozar_ID %notin% MIS_data$CDCCode) %>%
          select(KEY, SubmissionDate, Province, District, CDC_CCDC_Gozar_ID, CDC_CCDC_Gozar_Name, Village) %>% 
          mutate(Issue = "Gozar ID not in MIS")
        
        #level 2
        uncleaned_data <- rbind(uncleaned_data, 
                                data %>% 
                                  filter(CDC_CCDC_Gozar_Name %notin% Village & KEY %notin% uncleaned_data$KEY) %>% 
                                  select(KEY, SubmissionDate, Province, District, CDC_CCDC_Gozar_ID, CDC_CCDC_Gozar_Name, Village) %>% 
                                  mutate(Issue = "CDC and Village are different")) %>% 
          arrange(District)
        
        #level 3
        filtered_data <- data %>% 
          filter(KEY %notin% uncleaned_data$KEY)
        
        for(i in 1:nrow(filtered_data)){
          found_row <- MIS_data %>% 
            filter(CDCCode %in% filtered_data$CDC_CCDC_Gozar_ID[i])
          
          if(filtered_data$CDC_CCDC_Gozar_Name[i] %notin% found_row$CDCName){
            uncleaned_data <- rbind(uncleaned_data, filtered_data[i,] %>% 
                                      select(KEY, SubmissionDate, Province, District, CDC_CCDC_Gozar_ID, CDC_CCDC_Gozar_Name, Village) %>%
                                      mutate(Issue = "ID and Gozar Name mismatch")) %>% 
              arrange(District)
          }
          if(filtered_data$District[i] %notin% found_row$District){
            uncleaned_data <- rbind(uncleaned_data, filtered_data[i,] %>% 
                                      select(KEY, SubmissionDate, Province, District, CDC_CCDC_Gozar_ID, CDC_CCDC_Gozar_Name, Village) %>%
                                      mutate(Issue = "District mismatch")) %>% 
              arrange(District)
          }
        }
        
        # Displaying the table
        output$filtered_data <- renderDataTable(
          uncleaned_data,
          table_data <<- reactive(uncleaned_data)
        )
        
        print("All Went Well! ")
      },
      
      error = function(e){
        print(e)
      }
    )
  })
  
  # dataset_input <- reactive(DO_filtered)
  output$download <- downloadHandler(
    filename = function() {
      paste(formID(),"_filtered", ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(table_data(), file)
    }
  )
}

shinyApp(ui, server)

# 
# # for test -------------------------------------------------------
