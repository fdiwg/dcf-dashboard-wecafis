#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)
options(shiny.maxRequestSize=200*1024^2) #increase upload size to 200Mb

#packages
#---------------------------------------------------------------------------------------
library(shiny)
library(pool)
library(RPostgres)
library(dplyr)
library(DBI)
library(lubridate)
library(tidyr)
library(plotly)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(openxlsx)
library(shinyWidgets)

#utils
#---------------------------------------------------------------------------------------
scripts <- as.list(list.files("assets/scripts", recursive = T, full.names = TRUE))
invisible(lapply(scripts, source))

#env
#---------------------------------------------------------------------------------------
try(dotenv::load_dot_env(file = ".REnviron"), silent = TRUE)

#tasks
#---------------------------------------------------------------------------------------
tasks<-c(
  "Nominal catches"="task-I.2",
  "Catch"="task-II.1",
  "Effort"="task-II.2",
  "Fleet engagement"="task-III.1"
)

#db
#---------------------------------------------------------------------------------------
pool <- try(pool::dbPool(
  drv = DBI::dbDriver(Sys.getenv("DB_DRV")),
  dbname = Sys.getenv("DB_DBNAME"),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
))

#datasets
#---------------------------------------------------------------------------------------
dt_reporting_entities<-readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/regional/wecafc/cl_flagstate.csv")

#main Shiny scripts
#---------------------------------------------------------------------------------------
source("ui.R")
source("server.R")

