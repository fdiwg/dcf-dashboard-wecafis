ui <- fluidPage(
  useShinyjs(),
  useShinydashboard(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny-wecafis.css"),
    tags$script('var dimension = [0, 0];
                 $(document).on("shiny:connected", function(e) {
                      dimension[0] = window.innerWidth;
                      dimension[1] = window.innerHeight;
                      Shiny.onInputChange("dimension", dimension);
                  });
                  $(window).resize(function(e) {
                      dimension[0] = window.innerWidth;
                      dimension[1] = window.innerHeight;
                      Shiny.onInputChange("dimension", dimension);
                  });')
    ),
  uiOutput("menu"),
)