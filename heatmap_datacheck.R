# Author: Farshad Ebrahimi
# Last changed: 07/12/2024

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
library(reactablefmtr)
#excel download
library(xlsx)
library(DBI)
library(padr)
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

# Data day for level
level_data_day <- odbc::dbGetQuery(poolConn, "SELECT * FROM data.mat_level_data_day")

# Deploymenyt
deployment <- odbc::dbGetQuery(poolConn, "SELECT * FROM fieldwork.viw_deployment_full")

# Creating an example heat map for ow_uid = 1235, smp_id = 1025-1-1

dep_1235 <- deployment %>% 
  filter(ow_uid == 1235) %>%
  mutate(deployment_date = as.Date(deployment_dtime_est)) %>%
  select(deployment_date) %>%
  mutate(deployment = TRUE)

  
coll_1235 <- deployment %>% 
  filter(ow_uid == 1235) %>%
  mutate(collection_date = as.Date(collection_dtime_est)) %>%
  select(collection_date) %>%
  mutate(collection = TRUE)


# 
# data_day_1235 <- level_data_day %>%
#   filter(ow_uid == 1235) %>%
#   mutate(data = TRUE) %>%
#   pad() %>%
#   fill_by_value(data, value = FALSE) %>%
#   fill_by_prevalent(ow_uid) %>%
#   left_join(dep_1235, by= c("level_data_day" = "deployment_date")) %>%
#   left_join(coll_1235, by= c("level_data_day" = "collection_date")) 

nodata_day_1235 <- level_data_day %>%
  filter(ow_uid == 1235) %>%
  mutate(data = TRUE) %>%
  pad() %>%
  fill_by_value(data, value = FALSE) %>%
  fill_by_prevalent(ow_uid) %>%
  filter(data == FALSE) %>%
  mutate(data = 1) 



data_day_1235 <- level_data_day %>%
  filter(ow_uid == 1235) %>%
  mutate(data = 1) 



example_plot <- ggplot(data_day_1235, aes(x = level_data_day, y= data)) +
  geom_point(size = 5) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), axis.text.y=element_blank(), legend.position ="none" ) + 
  labs(x = "Date", y = "Data Exist?") +
  geom_vline(xintercept= dep_1235$deployment_date, color = "green", size=2) +
  geom_vline(xintercept= coll_1235$collection_date, color = "red", size=2, linetype="dashed")+
  geom_point(data = nodata_day_1235, mapping = aes(x = level_data_day, y = data, color = "purple", size = 5)) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) 
  

  
  


