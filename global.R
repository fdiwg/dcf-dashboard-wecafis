#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)
options(shiny.maxRequestSize=200*1024^2) #increase upload size to 200Mb

#packages
#---------------------------------------------------------------------------------------
library(shiny)
library(config)
library(DBI)
library(pool)
library(RPostgres)
library(dplyr)
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

#main Shiny scripts
#---------------------------------------------------------------------------------------
source("ui.R")
source("server.R")

