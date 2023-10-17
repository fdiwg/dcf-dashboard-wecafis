
library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("WECAFIS Dashboard"),

    
    shiny::htmlOutput("frame")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$frame <- renderUI({
    input$Member
    my_test <- tags$iframe(src="https://wecafc-firms.d4science.org/data-viewer", height="800px", width="100%")
    print(my_test)
    my_test
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
