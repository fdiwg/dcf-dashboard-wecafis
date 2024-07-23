#options
#---------------------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

#packages
#---------------------------------------------------------------------------------------
library(dcf.dashboard)

# Run the application 
#---------------------------------------------------------------------------------------
shinyApp(
  ui = ui, 
  server = server,
  onStop(function(){
   if(!is.null(pool)) DBI::dbDisconnect(pool)
  })
)
