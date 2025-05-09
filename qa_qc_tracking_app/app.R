# QA/QC tracking shiny app
# Author: Farshad Ebrahimi
# Last changed: 04/04/2024

# SET UP
# 0.0: load libraries --------------
library(shiny)
# pool for database connections
library(pool)
# odbc for database connections
library(odbc)
# tidyverse for data manipulations
library(tidyverse)
# shinythemes for colors
library(shinythemes)
# lubridate to work with dates
library(lubridate)
# shinyjs() to use easy java script functions
library(shinyjs)
# DT for datatables
library(DT)
# reactable
library(reactable)
# reactable for reactable tables
library(reactablefmtr)
# excel download
library(xlsx)
library(DBI)
# connection
library(RPostgres)
# managing long notes in UI
library(shinipsum)
# package versioning
library(renv)
# css
library(shinycssloaders)
# Not in logical
`%!in%` <- Negate(`%in%`)

# 0.1: database connection and global options

# set default page length for datatables
options(DT.options = list(pageLength = 25))

# set db connection
# using a pool connection so separate connnections are unified
# gets environmental variables saved in local or pwdrstudio environment
# poolConn <- dbPool(odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
poolConn <- dbPool(RPostgres::Postgres(),
  dbname = "mars_data",
  host = "PWDMARSDBS1",
  port = 5434,
  user = Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd")
)

data <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from external.mat_assets")) %>%
  dplyr::arrange(smp_id)
# vector  of all systems ids with wics
names(data) <- "SMP ID"
# Fiscal Quarters
fq <- dbGetQuery(poolConn, "SELECT * FROM admin.tbl_fiscal_quarter_lookup")

current_fq_query <- paste("select data.fun_date_to_fiscal_quarter('", Sys.Date(), "')", sep = "")
current_fq <- odbc::dbGetQuery(poolConn, current_fq_query) %>%
  pull()

# filter out to more recent quarters
q_list <- fq %>%
  arrange(fiscal_quarter_lookup_uid) %>%
  select(fiscal_quarter) %>%
  pull()

# #replace special characters with friendlier characters
special_char_replace <- function(note) {
  note_fix <- note %>%
    str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', "”" = '"'))
  return(note_fix)
}

# Define UI
ui <- tagList(
  useShinyjs(), navbarPage("QA/QC Tracking App",
    id = "TabPanelID", theme = shinytheme("cyborg"),
    tabPanel("Deployments QA/QC Status",
      value = "deployment_value",
      sidebarLayout(
        sidebarPanel(
          selectInput("smp_id", "SMP ID", choices = c("All", data), selected = "All"),
          selectInput("property_type", "Property Type", choices = c("All" = .5, "Public" = 1, "Private" = 0)),
          selectInput("interval_filter", "Interval", choices = c("All" = 10, "5" = 5, "15" = 15)),
          # selectInput("purpose_filter", "Sensor Purpose", choices = c("All" = 1.5, "BARO" = 1, "LEVEL" = 2, "DATALOGGER" = 3), selected = 2),
          selectInput("term_filter", "Term", choices = c("All" = 1.5, "Short" = 1, "Long" = 2), selected = 1.5),
          selectInput("f_q", "Collected/Expected Fiscal Quarter", choices = c("All", q_list), selected = current_fq),
          conditionalPanel(
            "input.deployments_rows_selected != 0",
            selectInput("status", "Add/Edit QA/QC Status:", c("", "Complete", "Needs Edit/Check", "Unresolved Issue with Data", "Missing/Stolen Sensor", "Partially Complete"), selected = NULL)
          ),
          conditionalPanel(
            "input.deployments_rows_selected != 0",
            selectInput("flagged", "Is the System Flagged for Issues?", c("", "Yes", "No"), selected = NULL)
          ),
          # textAreaInput("qaqc_note", "Comments", height = '85px'),
          conditionalPanel(
            "input.deployments_rows_selected != 0",
            textAreaInput("qaqc_comments", "Add/Edit Additional Comments:", height = "120px")
          ),
          conditionalPanel(
            "input.deployments_rows_selected != 0",
            h6("You Must Pick a QA/QC & Flag Status for the Deployment to be Able to Edit!")
          ),
          actionButton("update_button", "Update", icon("paper-plane"),
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          ),
          width = 3
        ),
        mainPanel(
          strong(span(textOutput("table_name"), style = "font-size:22px")),
          reactableOutput("deployments") %>% withSpinner(color = "#0dc5c1"),
          width = 9
        )
      )
    )
  ),
  tags$style(HTML("
  #qaqc_comments {
    color: black !important;  /* Ensures text color is black */
    font-weight: bold !important;  /* Makes the text bold */
  }
"))
)

# Server logic
server <- function(input, output, session) {
  rv <- reactiveValues()

  # get quarters as dates
  rv$start_quarter <- reactive(case_when(
    str_sub(input$f_q, 5, 7) == "Q3" ~ "1/1",
    str_sub(input$f_q, 5, 7) == "Q4" ~ "4/1",
    str_sub(input$f_q, 5, 7) == "Q1" ~ "7/1",
    str_sub(input$f_q, 5, 7) == "Q2" ~ "10/1"
  ))

  rv$end_quarter <- reactive(case_when(
    str_sub(input$f_q, 5, 7) == "Q3" ~ "3/31",
    str_sub(input$f_q, 5, 7) == "Q4" ~ "6/30",
    str_sub(input$f_q, 5, 7) == "Q1" ~ "9/30",
    str_sub(input$f_q, 5, 7) == "Q2" ~ "12/31"
  ))

  # parse the year component from this format "FY24Q2"
  rv$year <- reactive(str_sub(input$f_q, 3, 4))



  # convert FY/Quarter to a real date
  rv$start_date <- reactive(lubridate::mdy(paste0(rv$start_quarter(), "/", ifelse(str_sub(input$f_q, 5, 7) == "Q1" | str_sub(input$f_q, 5, 7) == "Q2", as.numeric(rv$year()) - 1, rv$year()))))
  rv$end_date <- reactive(lubridate::mdy(paste0(rv$end_quarter(), "/", ifelse(str_sub(input$f_q, 5, 7) == "Q1" | str_sub(input$f_q, 5, 7) == "Q2", as.numeric(rv$year()) - 1, rv$year()))))

  output$table_name <- renderText(ifelse(input$f_q == "All", "Full List of Short/Long-Term Deployments of Water level Sensors Across Every Fiscal Quarter:", paste("Short/Long-Term Deployments of Water level Sensors Collected or Expected to be Collected in ", input$f_q, " (", rv$start_date(), " to ", rv$end_date(), ")", ": ", sep = "")))

  # get row number
  selected_row <- reactive(getReactableState("deployments", "selected"))

  # process text field to prevent sql injection
  rv$reason_step <- reactive(gsub("'", "''", input$qaqc_comments))
  rv$input_note <- reactive(special_char_replace(rv$reason_step()))

  # toggle state for the update button
  observe(toggleState(id = "update_button", condition = !is.null(input$deployments_rows_selected) & input$status != "" & input$flagged != ""))

  # Update the values in the drop-down menus when a row is selected
  observeEvent(input$deployments_rows_selected, {
    # Update the reactive value
    if (!is.null(selected_row())) {

      rv$textbox_notes <- reactive(rv$status_notes()[rv$status_notes()$deployment_uid == rv$collect_table_filter()$deployment_uid[selected_row()], ] %>%
        select(qaqc_notes) %>%
        pull())

      updateTextAreaInput(session, "qaqc_comments", value = rv$textbox_notes())
      updateSelectInput(session, "status", selected = rv$collect_table_filter()$status[selected_row()])
      updateSelectInput(session, "flagged", selected = rv$collect_table_filter()$flagged[selected_row()])
    }
  })


  # postcon table
  postcon_table <- odbc::dbGetQuery(poolConn, "SELECT * FROM fieldwork.tbl_postcon_status") %>%
    mutate(postcon_exist = "Yes")

  # query the collection calendar and arrange by deployment_uid
  collect_query <- "select *, admin.fun_smp_to_system(smp_id) as system_id ,data.fun_date_to_fiscal_quarter(cast(date_100percent AS DATE)) as expected_fiscal_quarter, data.fun_date_to_fiscal_quarter(cast(collection_dtime_est AS DATE)) as collected_fiscal_quarter from fieldwork.viw_qaqc_deployments"

  # pull notes and status
  rv$status_notes <- reactive(odbc::dbGetQuery(poolConn, "SELECT * FROM fieldwork.tbl_qaqc_status"))

  # pull flagged systems
  rv$flagged_system <- reactive(odbc::dbGetQuery(poolConn, "SELECT * FROM fieldwork.tbl_flagged_systems"))

  # Data day for level
  level_data_day <- odbc::dbGetQuery(poolConn, "SELECT * FROM data.mat_level_data_day") %>%
    mutate(level_data_exist = "Yes")
  # Data day for gw
  gw_data_day <- odbc::dbGetQuery(poolConn, "SELECT * FROM data.mat_gw_data_day") %>%
    mutate(gw_data_exist = "Yes")


  # data gaps
  datagaps <- odbc::dbGetQuery(poolConn, "SELECT * FROM data.tbl_datagaps")

  # populating the main table
  rv$collect_table_db <- reactive(odbc::dbGetQuery(poolConn, collect_query) %>%
    mutate(fiscal_quarter = ifelse(collected_fiscal_quarter == "", expected_fiscal_quarter, collected_fiscal_quarter)) %>%
    inner_join(fq, by = "fiscal_quarter") %>%
    mutate(after_deployment_day = deployment_dtime_est + days(1)) %>%
    mutate(gw = ifelse(ow_suffix == "GW1" | ow_suffix == "GW2" | ow_suffix == "GW3" | ow_suffix == "GW4" | ow_suffix == "GW5" | ow_suffix == "CW1", "Yes", "No")) %>%
    left_join(level_data_day, by = c("ow_uid" = "ow_uid", "after_deployment_day" = "level_data_day")) %>%
    left_join(gw_data_day, by = c("ow_uid" = "ow_uid", "after_deployment_day" = "gw_data_day")) %>%
    mutate(qa_qc = case_when(
      is.na(collection_dtime_est) ~ "No",
      gw == "Yes" ~ ifelse(is.na(gw_data_exist), "No", "Yes"),
      gw == "No" ~ ifelse(is.na(level_data_exist), "No", "Yes")
    )) %>%
    mutate(collection_status = ifelse(is.na(collection_dtime_est), "Not Collected", as.character(collection_dtime_est))) %>%
    left_join(rv$status_notes(), by = "deployment_uid") %>%
    left_join(rv$flagged_system(), by = "system_id") %>%
    mutate(flagged = ifelse(flagged == 1, "Yes", "No")) %>%
    filter(sensor_purpose == 2 & long_term_lookup_uid %in% c(1, 2)) %>%
    arrange(ow_suffix) %>%
    arrange(desc(fiscal_quarter_lookup_uid)) %>%
    arrange(desc(collection_dtime_est)) %>%
    arrange(qa_qc))

  rv$term_filter <- reactive(
    if (input$term_filter == 1.5) {
      c(1, 2)
    } else {
      input$term_filter
    }
  )

  rv$smp_filter <- reactive(
    if (input$smp_id == "All") {
      data$"SMP ID"
    } else {
      input$smp_id
    }
  )

  # rv$purpose_filter <- reactive(if(input$purpose_filter == 1.5){c(0, 1, 2, 3)} else {input$purpose_filter})
  rv$quarter <- reactive(if (input$f_q == "All") {
    q_list
  } else {
    input$f_q
  })

  # arrange and filtered the collection calendar
  rv$collect_table_filter <- reactive(rv$collect_table_db() %>%
    left_join(datagaps, by = "deployment_uid") %>%
    left_join(postcon_table, by = "system_id") %>%
    mutate(collection_dtime_est = collection_dtime_est %>% lubridate::ymd()) %>%
    # use 1 or 0 for public or private, respectively, and 0.5 for both, with a tolerance of .51
    # so if .5 is selected, 0 and 1 are picked up
    dplyr::filter(near(as.numeric(public), as.numeric(input$property_type), tol = 0.51) &
      # use 5 or 15 minute intervals, with 10 for both, with a tolerance of 5.1
      # so if 10 is selected, 5 and 15 are picked up
      near(interval_min, as.numeric(input$interval_filter), tol = 5.1) &
      # sensor_purpose %in% rv$purpose_filter() &
      long_term_lookup_uid %in% rv$term_filter() &
      fiscal_quarter %in% rv$quarter() &
      smp_id %in% rv$smp_filter()) %>%
    # ifelse(input$smp_id == "All", TRUE, smp_id == input$smp_id)) %>%
    mutate(across(
      "sensor_purpose",
      ~ case_when(
        . == 1 ~ "Baro",
        . == 2 ~ "Level",
        . == 3 ~ "Data Logger"
      )
    )) %>%
    replace_na(list(postcon_exist = "No")) %>%
    select(smp_id, system_id, ow_suffix, project_name, term, collection_status, qa_qc, status, postcon_exist, flagged, datagap_days, deployment_uid) %>%
    distinct())

  observeEvent(input$update_button, {

    if ((input$status != "" | rv$input_note() != "") & rv$collect_table_filter()$deployment_uid[selected_row()] %!in% rv$status_notes()$deployment_uid) {
      new_note_status <- data.frame(
        deployment_uid = rv$collect_table_filter()$deployment_uid[selected_row()],
        status = input$status,
        qaqc_notes = rv$input_note()
      )

      odbc::dbWriteTable(poolConn, Id(schema = "fieldwork", table = "tbl_qaqc_status"), new_note_status, append = TRUE, row.names = FALSE)
    } else if ((input$status != "" | rv$input_note() != "") & rv$collect_table_filter()$deployment_uid[selected_row()] %in% rv$status_notes()$deployment_uid) {
      edit_query <- paste0(
        "Update fieldwork.tbl_qaqc_status SET status ='", input$status, "', qaqc_notes = '", rv$input_note(), "' where deployment_uid = ", rv$collect_table_filter()$deployment_uid[selected_row()]
      )
      odbc::dbGetQuery(poolConn, edit_query)
    } else {
      delete_query <- paste0(
        "delete from fieldwork.tbl_qaqc_status where deployment_uid = ", rv$collect_table_filter()$deployment_uid[selected_row()]
      )
      odbc::dbGetQuery(poolConn, delete_query)
    }

    # update flagged status based on rv$flagged_system()

    if (rv$collect_table_filter()$system_id[selected_row()] %!in% rv$flagged_system()$system_id) {
      new_flag <- data.frame(
        system_id = rv$collect_table_filter()$system_id[selected_row()],
        flagged = ifelse(input$flagged == "Yes", TRUE, FALSE)
      )

      odbc::dbWriteTable(poolConn, Id(schema = "fieldwork", table = "tbl_flagged_systems"), new_flag, append = TRUE, row.names = FALSE)
    } else {
      edit_flag <- paste0(
        "Update fieldwork.tbl_flagged_systems SET flagged = ", ifelse(input$flagged == "Yes", TRUE, FALSE), " where system_id = '", rv$collect_table_filter()$system_id[selected_row()], "'"
      )
      odbc::dbGetQuery(poolConn, edit_flag)
    }


    # re-run notes and status, and flags
    rv$status_notes <- reactive(odbc::dbGetQuery(poolConn, "SELECT * FROM fieldwork.tbl_qaqc_status"))
    rv$flagged_system <- reactive(odbc::dbGetQuery(poolConn, "SELECT * FROM fieldwork.tbl_flagged_systems"))


    reset("status")
    reset("qaqc_comments")
    reset("flagged")
  })

  output$deployments <- renderReactable(
    reactable(
      rv$collect_table_filter() %>%
        select(`SMP ID` = smp_id, `OW Suffix` = ow_suffix, `Project Name` = project_name, Term = term, `Collection Date` = collection_status, `Data in DB?` = qa_qc, `QA/QC Status` = status, `Post-Con Status?` = postcon_exist, `Flagged?` = flagged, `Gap Days` = datagap_days),
      theme = darkly(),
      fullWidth = TRUE,
      selection = "single",
      searchable = TRUE,
      onClick = "select",
      selectionId = "deployments_rows_selected",
      # searchable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25, 50, 100),
      defaultPageSize = 25,
      columns = list(
        # `System ID` = colDef(width = 90),
        `SMP ID` = colDef(width = 95),
        `OW Suffix` = colDef(width = 100),
        `Project Name` = colDef(width = 250),
        Term = colDef(width = 75),
        # `Collected/Expected Quarter` = colDef(width = 225),
        `Collection Date` = colDef(width = 125, style = function(value) {
          if (value == "Not Collected") {
            color <- "yellow"
            textColor <- "black"
          } else {
            color <- "green"
            textColor <- "white"
          }
          list(backgroundColor = color, color = textColor, fontweight = "bold")
        }),
        `QA/QC Status` = colDef(width = 220, style = function(value) {
          if (!is.na(value) & value == "Complete") {
            color <- "green"
            textColor <- "white"
          } else if (is.na(value)) {
            color <- NULL
            textColor <- NULL
          } else if (value == "Needs Edit/Check") {
            color <- "lightgreen"
            textColor <- "black"
          } else if (value == "Unresolved Issue with Data") {
            color <- "red"
            textColor <- "white"
          } else if (value == "Missing/Stolen Sensor") {
            color <- "purple"
            textColor <- "white"
          } else {
            color <- "orange"
            textColor <- "black"
          }
          list(backgroundColor = color, color = textColor, fontweight = "bold")
        }),
        `Data in DB?` = colDef(width = 100, style = function(value) {
          if (value == "No") {
            color <- "yellow"
            textColor <- "black"
          } else {
            color <- "green"
            textColor <- "white"
          }
          list(backgroundColor = color, color = textColor, fontweight = "bold")
        }),
        `Flagged?` = colDef(width = 85, style = function(value) {
          if (!is.na(value) & value == "No") {
            color <- "green"
            textColor <- "white"
          } else if (is.na(value)) {
            color <- NULL
            textColor <- NULL
          } else {
            color <- "red"
            textColor <- "white"
          }
          list(backgroundColor = color, color = textColor, fontweight = "bold")
        })
      ),
      details = function(index) {
        nested_notes <- rv$status_notes()[rv$status_notes()$deployment_uid == rv$collect_table_filter()$deployment_uid[index], ] %>%
          select(Notes = qaqc_notes)
        htmltools::div(
          style = "padding: 1rem",
          reactable(nested_notes,
            theme = darkly(),
            outlined = TRUE
          )
        )
      }
    )
  )
}

# Complete app with UI and server components
shinyApp(ui, server)
