#QA/QC tracking shiny app - script to check for data gaps
# Author: Farshad Ebrahimi
# Last changed: 07/19/2024

# SET UP

library(pool)
#odbc for database connections
library(odbc)
#tidyverse for data manipulations
library(tidyverse)
#lubridate to work with dates
library(lubridate)
library(DBI)
# preserve the type and class of variable
library(data.table)
#Not in logical
`%!in%` <- Negate(`%in%`)

#0.1: database connection and global options --------

#set default page length for datatables
options(DT.options = list(pageLength = 25))

#set db connection
#using a pool connection so separate connnections are unified
#gets environmental variables saved in local or pwdrstudio environment
con <- dbConnect(odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

data_gaps_current <- odbc::dbGetQuery(con, "SELECT * FROM data.tbl_datagaps")

#query the collection calendar and arrange by deployment_uid
collect_query <- "select *, admin.fun_smp_to_system(smp_id) as system_id ,data.fun_date_to_fiscal_quarter(cast(date_100percent AS DATE)) as expected_fiscal_quarter, data.fun_date_to_fiscal_quarter(cast(collection_dtime_est AS DATE)) as collected_fiscal_quarter from fieldwork.viw_qaqc_deployments"
deployments_df <- odbc::dbGetQuery(con, collect_query) %>%
  select(smp_id, deployment_uid ,ow_uid, ow_suffix, deployment_dtime_est, collection_dtime_est, date_100percent, type, term) %>%
  filter(type == "LEVEL" & (term == "Short" | term == "Long")) %>%
  na.omit() %>%
  mutate(dif = as.numeric(collection_dtime_est - date_100percent)) %>%
  mutate(reference_date = fifelse(dif > 0, as.POSIXct(date_100percent), as.POSIXct(collection_dtime_est)))




# Data day for level
level_data_day <- odbc::dbGetQuery(con, "SELECT * FROM data.mat_level_data_day") %>%
  mutate(level_data_exist = "Yes")
# Data day for gw
gw_data_day <- odbc::dbGetQuery(con,"SELECT * FROM data.mat_gw_data_day") %>%
  mutate(gw_data_exist = "Yes")


# checking the data contunity for collected sensors. long/short level data sensors
deployments_df <- deployments_df %>%
                                select(deployment_uid ,ow_uid, ow_suffix, deployment_dtime_est, collection_dtime_est, reference_date, type, term) %>%
                                filter(type == "LEVEL" & (term == "Short" | term == "Long")) %>%
                                na.omit() %>%
                                mutate(datagap_days = NA)

# loop through deployments and assign a boolean datagap status using left-join tool. To do this, a squence of days are created from deployment to collection and is
# left-joined by the data-day time-series to expose gaps
  for (i in 1:nrow(deployments_df)) {
    
    dates <- seq(from=as.Date(deployments_df$deployment_dtime_est[i]), to=as.Date(deployments_df$reference_date[i]), by = "days")
    dates <- as.data.frame(dates)
    
    if (deployments_df$ow_suffix[i] == "GW1" | deployments_df$ow_suffix[i] == "GW2" | deployments_df$ow_suffix[i] == "GW3" | deployments_df$ow_suffix[i] == "GW4" | deployments_df$ow_suffix[i] == "GW5" | deployments_df$ow_suffix[i] == "CW1"){
      
      data_df <- gw_data_day %>%
        filter(ow_uid == deployments_df$ow_uid[i]) 
      data_check <- dates %>%
        left_join(data_df, by = c("dates" = "gw_data_day")) 
      deployments_df[i,"datagap_days"] <- sum(is.na(data_check$gw_data_exist))
      
    } else {
      
      data_df <- level_data_day %>%
        filter(ow_uid == deployments_df$ow_uid[i]) 
      data_check <- dates %>%
        left_join(data_df, by = c("dates" = "level_data_day")) 
      # only flag of there is more than 2 days of data gaps (excluding deployment and collection dates)
      deployments_df[i,"datagap_days"] <- sum(is.na(data_check$level_data_exist))
      
    }
  }


# prep and write to db, look for new deployments and those with updated gapdays
datagaps <- deployments_df %>%
  select(deployment_uid, datagap_days) %>%
  anti_join(data_gaps_current, by = c("deployment_uid","datagap_days"))


# delete values that have changed 
sql_string <- "delete from data.tbl_datagaps WHERE deployment_uid = %s;"
dbSendStatement(con, paste(sprintf(sql_string, datagaps$deployment_uid), collapse=""))


# write the new data and data with updated values to DB
dbWriteTable(con, Id(schema = "data", table = "tbl_datagaps"), datagaps, append= TRUE, row.names = FALSE )
