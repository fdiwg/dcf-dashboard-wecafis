# Run the application 
shinyApp(ui = ui, 
         server = server,
         onStop(function(){
           if(!is.null(pool)) DBI::dbDisconnect(pool)
         })
         )
