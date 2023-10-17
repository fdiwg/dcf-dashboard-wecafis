
library(shiny)

ui <- fluidPage(
	
	#TODO header of infoboxes
	#   #of tasks | timestamp  
	#   #of flagstates | #of species

	#tabs
	#tab1 Statistics
	#TODO 
	#tab2 map viewer
    shiny::htmlOutput("frame")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$frame <- renderUI({
    my_iframe <- tags$iframe(src="https://wecafc-firms.d4science.org/data-viewer", height="700px", width="100%")
    my_iframe
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
