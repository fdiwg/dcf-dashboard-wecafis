ui <- fluidPage(
  useShinyjs(),
  useShinydashboard(),
  #TODO header of infoboxes
  #   #of tasks | timestamp  
  #   #of flagstates | #of species
  
  #tabs
  #tab1 Statistics
  uiOutput("menu"),
)