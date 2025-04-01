#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#packages
#---------------------------------------------------------------------------------------
library(dcf.dashboard)
library(vrule) #explicit call to vrule (not on CRAN) to reference it to renv.lock

# Run the application 
#---------------------------------------------------------------------------------------
shinyApp(
  ui = ui, 
  server = server,
  onStop(function(){
   if(!is.null(pool)) DBI::dbDisconnect(pool)
  })
)
