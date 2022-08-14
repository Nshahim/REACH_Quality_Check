library(dplyr)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

`%notin%` <- Negate(`%in%`)

# Offline Data Inputs -----------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "DO & Form2 Analysis"),
    
    dashboardSidebar(
        
    ),
    
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-size: 18;
      }
    "))),
        
        fileInput(
            "uploaded_data",
            "Please upload the dataset",
            accept = c("xlsx", ".xlsx"),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
        ),
        
        selectInput("Form_Type", "Please choose the form Type:",
                    choices = c("Choose One:"="", "Direct Observatoin", "Form2")),
        uiOutput("error_form"),
        
        pickerInput("vars_id", "Please select the output Columns: ", choices=NULL, options = list(`actions-box` = T), multiple = T),
        uiOutput("error_vars"),
        selectInput("qa_status", "Please select the QA status:", choices = "All"),
        
        shinyjs::hidden(actionButton("analyze", "Analyze", icon("chart-bar"))),
        shinyjs::hidden(downloadButton("download", "Download")),
        DT::dataTableOutput("data")
    )
)

server <- function(input, output, session){
    options(shiny.maxRequestSize=30*1024^2)
    
    observeEvent(input$uploaded_data, {
        shinyjs::show("analyze")
        data <- read_excel(input$uploaded_data$datapath, guess_max = 100000)
        updatePickerInput(
            session,
            "vars_id",
            choices = names(data),
        )
        updateSelectInput(
            session,
            "qa_status",
            choices = c("All", data$QA_status),
            selected = "All"
        )
    })
    
    observeEvent(input$analyze,{
        
        data <- read_excel(input$uploaded_data$datapath, guess_max = 100000)
        
        output$error_form <- renderUI(
            validate(
                need(input$Form_Type != "", "Please select the form Type!")
            ))
        req(input$Form_Type)
        
        output$error_vars <- renderUI(
            validate(
                need(!is.null(input$vars_id), "Please select the output columns!")
            )
        )
        req(input$vars_id)
        
        
        qa_status_exists <- F
        if("QA_status" %in% names(data)){
            qa_status_exists <- T
            
            status <- unique(data$QA_status)
            
            if(input$qa_status %notin% "All"){
                status <- input$qa_status
            }
            
        }
        
        
        if(input$Form_Type %in% "Direct Observatoin"){
            data$Hh_Observed_Receiving_Aid_Package_Per_Team_Surveyor <- as.numeric(data$Hh_Observed_Receiving_Aid_Package_Per_Team_Surveyor)
            data$Hh_Observed_Complina_Not_Receiving_Aid_Package_Per_Team_Surveyor <- as.numeric(data$Hh_Observed_Complina_Not_Receiving_Aid_Package_Per_Team_Surveyor)
            
            if(qa_status_exists){
                data <- data %>%
                    filter(QA_status %in% status)
            }
            
            filtered_data <- data %>%
                group_by(Village_Cdc_Ccdc_Gozar_Name_ID) %>%
                mutate(Beneficiary = sum(Hh_Observed_Receiving_Aid_Package_Per_Team_Surveyor, Hh_Observed_Complina_Not_Receiving_Aid_Package_Per_Team_Surveyor, na.rm = T)) %>%
                select(input$vars_id, Hh_Observed_Receiving_Aid_Package_Per_Team_Surveyor,
                       Hh_Observed_Complina_Not_Receiving_Aid_Package_Per_Team_Surveyor,
                       Beneficiary) %>%
                arrange(Village_Cdc_Ccdc_Gozar_Name_ID)
            
            
            output$data <- DT::renderDataTable(
                DT::datatable(
                    filtered_data,
                    extensions = 'Buttons',
                    
                    options = list(
                        paging = TRUE,
                        searching = TRUE,
                        fixedColumns = TRUE,
                        autoWidth = TRUE,
                        ordering = TRUE,
                        dom = 'tBp',
                        buttons = c('copy', 'csv', 'excel')
                        
                    )
                ),
                server = F
            )
            
            
            
        } else if (input$Form_Type %in% "Form2"){
            
            if(qa_status_exists){
                data <- data %>%
                    filter(QA_status %in% status)
            }
            #form2
            filtered_data <- data %>%
                group_by(Cdc_Ccdc_Gozar_Id) %>%
                mutate(Total = sum(Number_Of_Hh_Not_Found_Through_Door_To_Door)) %>%
                select(input$vars_id ,Total) %>%
                arrange(Cdc_Ccdc_Gozar_Id)
            
            output$data <- DT::renderDataTable(
                DT::datatable(
                    filtered_data,
                    extensions = 'Buttons',
                    
                    options = list(
                        paging = TRUE,
                        searching = TRUE,
                        fixedColumns = TRUE,
                        autoWidth = TRUE,
                        ordering = TRUE,
                        dom = 'tBp',
                        buttons = c('copy', 'csv', 'excel')
                        
                    )
                ),
                server = F
            )
        }
    })
}


shinyApp(ui, server)

