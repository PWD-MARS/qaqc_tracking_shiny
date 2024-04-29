#QA/QC tracking shiny app
# Author: Farshad Ebrahimi
# Last changed: 04/04/2024

# SET UP
#0.0: load libraries --------------
library(shiny)
#pool for database connections
library(pool)
#odbc for database connections
library(odbc)
#tidyverse for data manipulations
library(tidyverse)
#shinythemes for colors
library(shinythemes)
#lubridate to work with dates
library(lubridate)
#shinyjs() to use easy java script functions
library(shinyjs)
#DT for datatables
library(DT)
#reactable
library(reactable)
#reactable for reactable tables
library(reactable)
#excel download
library(xlsx)
library(DBI)
# managing long notes in UI
library(shinipsum)
#Not in logical
`%!in%` <- Negate(`%in%`)

#0.1: database connection and global options --------

#set default page length for datatables
options(DT.options = list(pageLength = 25))


#set db connection
#using a pool connection so separate connnections are unified
#gets environmental variables saved in local or pwdrstudio environment
poolConn <- dbPool(odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

data <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from external.mat_assets")) %>% 
  dplyr::arrange(smp_id)
# vector  of all systems ids with wics 
names(data) <- "SMP ID"
# Fiscal Quarters 
fq <- dbGetQuery(poolConn, "SELECT * FROM admin.tbl_fiscal_quarter_lookup")


# filter out to more recent quarters
q_list <- fq %>%
  arrange(fiscal_quarter_lookup_uid) %>%
  select(fiscal_quarter) %>%
  pull


js <- "
function(cell) {
  var $cell = $(cell);
  $cell.contents().wrapAll('<div class=\\\"content\\\"></div>');
  var $content = $cell.find('.content');
  $cell.append($('<button>Read more</button>'));
  $btn = $cell.find('button');
  $content.css({
    height: '50px',
    overflow: 'hidden'
  });
  $cell.data('isLess', true);
  $btn.click(function () {
    var isLess = $cell.data('isLess');
    $content.css('height', isLess ? 'auto' : '50px');
    $(this).text(isLess ? 'Read less' : 'Read more');
    $cell.data('isLess', !isLess);
  });
}
"
# Define UI
ui <- tagList(useShinyjs(), navbarPage("QA/QC Tracking App", id = "TabPanelID", theme = shinytheme("cerulean"),
                                       #1.1 Unmonitored Active SMPs -------
                                       tabPanel("Deployments QA/QC Status", value = "deployment_value", 
                                                titlePanel("Level Sensor Deployments Table"),
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    selectInput("smp_id", "SMP ID", choices = c("All", data), selected = "All"),
                                                    selectInput("property_type", "Property Type", choices = c("All" = .5, "Public" = 1, "Private" = 0)),
                                                    selectInput("interval_filter", "Interval", choices = c("All" = 10, "5" = 5, "15" = 15)),
                                                    #selectInput("purpose_filter", "Sensor Purpose", choices = c("All" = 1.5, "BARO" = 1, "LEVEL" = 2, "DATALOGGER" = 3), selected = 2),
                                                    selectInput("term_filter", "Term", choices = c("All" = 1.5, "Short" = 1, "Long"  = 2), selected = 1.5),
                                                    selectInput("f_q", "Collected/Expected Fiscal Quarter", choices = c("All", q_list), selected = "FY24Q3"),
                                                    selectInput("status", "QA/QC Status", c("","Complete", "Needs Edit/Check", "Unresolved Issue", "Partially Complete"), selected = NULL),
                                                    #textAreaInput("qaqc_note", "Comments", height = '85px'),
                                                    conditionalPanel("input.deployments_rows_selected != 0",
                                                                     textAreaInput("qaqc_comments", "Additional Comments:", height = '85px')),
                                                    actionButton("update_button", "Update"),
                                                    width = 3
                                                    
                                                    
                                                ),
                                                mainPanel(
                                                  #DTOutput("deployments"),
                                                  reactableOutput("deployments"),
                                                  width = 9
                                                  
                                                )
                                       )
                                       )
)
)
                                  

# Server logic
server <- function(input, output, session) {
  
  rv <- reactiveValues()  
  #toggle state for the update button
  observe(toggleState(id = "update_button", condition = !is.null(input$deployments_rows_selected)))
  
  selected_row <- reactiveVal()
  
  
  # Update the values in the drop-down menus when a row is selected
  observeEvent(input$deployments_rows_selected, {
    # Get the selected row
    row <- input$deployments_rows_selected
    
    # Update the reactive value
    if (!is.null(row)) {
      selected_row(row)
      
      updateTextAreaInput(session, "qaqc_comments", value = rv$collect_table_filter()$qaqc_notes[row])
      updateSelectInput(session, "status", selected = rv$collect_table_filter()$status[row])
      
 
    }
  })
  
  #query the collection calendar and arrange by deployment_uid
  collect_query <- "select *, data.fun_date_to_fiscal_quarter(cast(date_100percent AS DATE)) as expected_fiscal_quarter, data.fun_date_to_fiscal_quarter(cast(collection_dtime_est AS DATE)) as collected_fiscal_quarter from fieldwork.viw_qaqc_deployments"
  
  
  # pull notes and status
  rv$status_notes <- reactive(odbc::dbGetQuery(poolConn, "SELECT * FROM fieldwork.tbl_qaqc_status"))
  
  # Data quarters for level data-populating the next fiscal quarter for a data point to compare with collection quarter
  level_data_quarter <- odbc::dbGetQuery(poolConn, "SELECT * FROM data.mat_level_data_quarter") %>%
    inner_join(fq, by = c("level_data_quarter" = "fiscal_quarter")) %>%
    mutate(next_quarter_uid = fiscal_quarter_lookup_uid + 1) %>%
    inner_join(fq, by = c("next_quarter_uid" = "fiscal_quarter_lookup_uid")) 
    
  # Data quarters for GW data-populating the next fiscal quarter for a data point to compare with collection quarter
  gw_data_quarter <- odbc::dbGetQuery(poolConn,"SELECT * FROM data.mat_gw_data_quarter") %>%
    inner_join(fq, by = c("gw_data_quarter" = "fiscal_quarter")) %>%
    mutate(next_quarter_uid = fiscal_quarter_lookup_uid + 1) %>%
    inner_join(fq, by = c("next_quarter_uid" = "fiscal_quarter_lookup_uid")) 
  
  # If sensor is collected, fiscal_quarter column is the quarter it was collected. If not collected, fiscal_quarter is the the quarter associated with the 100%-full date. 
  # If OW suffix is GW- or CW1, the app looks at gw_data_quarter to find at least one data point in the past quarter
  # if NOT  GW- or CW1, the app looks at level_data_quarter to find at least one data point in the past quarter
  # qa_qc column looks at collection date; if no date, "No". If there is a date, looks at the ow suffix and data points in gw- or level- tables
  # the deployments are limited to level data for short- and long-term monitoring of level data (including green inlets and any other suffix in data.tbl_ow_leveldata_raw) and groundwater data in data.tbl_gw_depthdata_raw
  rv$collect_table_db <- reactive(odbc::dbGetQuery(poolConn, collect_query) %>%
    mutate(fiscal_quarter = ifelse(collected_fiscal_quarter == "", expected_fiscal_quarter, collected_fiscal_quarter)) %>%
    inner_join(fq, by = "fiscal_quarter") %>%
    left_join(level_data_quarter, by = c("ow_uid","fiscal_quarter")) %>%
    left_join(gw_data_quarter, by = c("ow_uid","fiscal_quarter")) %>%
    mutate(gw = ifelse(ow_suffix == "GW1" | ow_suffix == "GW2" | ow_suffix == "GW3" | ow_suffix == "GW4" | ow_suffix == "GW5" | ow_suffix == "CW1", "Yes","No")) %>%
    mutate(qa_qc = case_when(is.na(collection_dtime_est) ~ "No",
                             gw == "Yes" ~ ifelse(is.na(gw_data_quarter),"No","Yes"),
                             gw == "No" ~ ifelse(is.na(level_data_quarter),"No","Yes"))) %>%
    mutate(collection_status = ifelse(is.na(collection_dtime_est), "Not Collected", as.character(collection_dtime_est))) %>%
    left_join(rv$status_notes(), by = "deployment_uid") %>%
    filter(sensor_purpose == 2 & long_term_lookup_uid %in% c(1, 2)) %>%
    arrange(ow_suffix) %>%
    arrange(desc(fiscal_quarter_lookup_uid)) %>%
    arrange(desc(collection_dtime_est)) %>%
    arrange(qa_qc)
  )
  

  rv$term_filter <- reactive(
    if(input$term_filter == 1.5){
      c(1, 2)
    } else {
      input$term_filter
    }
  )
  
  
  
  rv$smp_filter <- reactive(
    if(input$smp_id == "All"){
      data$'SMP ID'
    } else {
      input$smp_id
    }
  )
  
  #rv$purpose_filter <- reactive(if(input$purpose_filter == 1.5){c(0, 1, 2, 3)} else {input$purpose_filter})
  rv$quarter <- reactive(if(input$f_q == "All"){q_list} else {input$f_q})
  
  
  #arrange and filtered the collection calendar
  rv$collect_table_filter <- reactive(rv$collect_table_db() %>% 
                                        mutate(collection_dtime_est = collection_dtime_est %>% lubridate::ymd()) %>%
                                        #use 1 or 0 for public or private, respectively, and 0.5 for both, with a tolerance of .51 
                                        #so if .5 is selected, 0 and 1 are picked up
                                        dplyr::filter(near(as.numeric(public), as.numeric(input$property_type), tol = 0.51) &
                                                        #use 5 or 15 minute intervals, with 10 for both, with a tolerance of 5.1 
                                                        #so if 10 is selected, 5 and 15 are picked up
                                                        near(interval_min, as.numeric(input$interval_filter), tol = 5.1) &
                                                        #sensor_purpose %in% rv$purpose_filter() &
                                                        long_term_lookup_uid %in% rv$term_filter() & 
                                                        fiscal_quarter %in% rv$quarter() &
                                                        smp_id %in% rv$smp_filter()) %>%
                                                        #ifelse(input$smp_id == "All", TRUE, smp_id == input$smp_id)) %>%
                                        mutate(across("sensor_purpose",
                                                      ~ case_when(. == 1 ~ "Baro",
                                                                  . == 2 ~ "Level",
                                                                  . == 3 ~ "Data Logger")))
                                      
                                        ) 
  
  
  #select and rename columns to show in app
  rv$collect_table <- reactive(rv$collect_table_filter() %>%
                                 select(`SMP ID` = smp_id, `OW Suffix`= ow_suffix, `Project Name` = project_name, Term = term, `Collection Date` = collection_status, `Collected/Expected Quarter` = fiscal_quarter, `QAed Data in DB?` = qa_qc, Status = status, deployment_uid) #, Notes =  qaqc_notes)
  )
                                  

  # Update the values in the drop-down menus when a row is selected
  observeEvent(input$deployments_rows_selected, {
    # Get the selected row
    row <- input$deployments_rows_selected
    
    # Update the reactive value
    if (!is.null(row) && length(row) > 0) {
    
    }
  })
  
  
  
  
  observeEvent(input$update_button, {
    row <- input$deployments_rows_selected
    #if(is.na(rv$collect_table_filter()$qaqc_notes[row]) & is.na(rv$collect_table_filter()$status[row])) {
    
      if((input$status != "" | input$qaqc_comments != "") & rv$collect_table_filter()$deployment_uid[row] %!in% rv$status_notes()$deployment_uid) {
      new_note_status <- data.frame(deployment_uid = rv$collect_table_filter()$deployment_uid[row],
                                    status = input$status,
                                    qaqc_notes = input$qaqc_comments)
      
      odbc::dbWriteTable(poolConn, Id(schema = "fieldwork", table = "tbl_qaqc_status"), new_note_status, append= TRUE, row.names = FALSE )
      
      } else if ((input$status != "" | input$qaqc_comments != "") & rv$collect_table_filter()$deployment_uid[row] %in% rv$status_notes()$deployment_uid) {
    
        edit_query <- paste0(
          "Update fieldwork.tbl_qaqc_status SET status ='", input$status,"', qaqc_notes = '", input$qaqc_comments, "' where deployment_uid = ", rv$collect_table_filter()$deployment_uid[row])
        odbc::dbGetQuery(poolConn, edit_query)
        
        
      } else {
        delete_query <- paste0(
          "delete from fieldwork.tbl_qaqc_status where deployment_uid = ", rv$collect_table_filter()$deployment_uid[row])
        odbc::dbGetQuery(poolConn, delete_query)
        
      }
    
    
    # re-run notes and status
    rv$status_notes <- reactive(odbc::dbGetQuery(poolConn, "SELECT * FROM fieldwork.tbl_qaqc_status"))
    
    reset("status")
    reset("qaqc_comments")
    
    
    
    
  }
  )
    
  
  output$deployments <- renderReactable(
    reactable(rv$collect_table() %>%
                select(-deployment_uid), 
              fullWidth = TRUE,
              selection = "single",
              searchable = TRUE,
              onClick = "select",
              selectionId = "deployments_rows_selected",
              #searchable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(25, 50, 100),
              defaultPageSize = 25,
              height = 1050,
              columns = list(
                #`System ID` = colDef(width = 90),
                `SMP ID` = colDef(width = 75),
                `OW Suffix` = colDef(width = 100),
                `Project Name` = colDef(width = 400),
                 Term = colDef(width = 75),
                `Collected/Expected Quarter` = colDef(width = 200),
                `Collection Date` = colDef(width = 125, style = function(value){
                  if(value == "Not Collected"){
                    color = "yellow"
                    textColor = "black"
                  }else{
                    color = "green"
                    textColor = "white"
                  }
                  list(backgroundColor = color, color = textColor, fontweight = "bold")
                }),
                `Status` = colDef(width = 250, style = function(value){
                  if(!is.na(value) & value == "Complete"){
                    color = "green"
                    textColor = "white"
                  }else if(is.na(value)){
                    color = "white"
                    textColor = "black"
                  }else{
                    color = "yellow"
                    textColor = "black"
                  }
                  list(backgroundColor = color, color = textColor, fontweight = "bold")
                }),
                `QAed Data in DB?` = colDef(width = 150, style = function(value){
                  if(value == "No"){
                    color = "yellow"
                    textColor = "black"
                  }else{
                    color = "green"
                    textColor = "white"
                  }
                  list(backgroundColor = color, color = textColor, fontweight = "bold")
                })),
              details = function(index) {
                nested_notes <- rv$status_notes()[rv$status_notes()$deployment_uid == rv$collect_table()$deployment_uid[index], ] %>%
                  select(Notes = qaqc_notes)
                htmltools::div(style = "padding: 1rem",
                               reactable(nested_notes, columns = list(
                                 Notes = colDef(width = 950)
                               ), outlined = TRUE)
                )
              }
  
              )
    )

  
  
  
}

# Complete app with UI and server components
shinyApp(ui, server)