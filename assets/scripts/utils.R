withBusyIndicatorCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
"

withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  print("HERE BUSY INDICATOR")
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

#getDataTaskTablename
getDataTaskTablename <- function(task_id){
  table_id <- tolower(gsub("-", "_", gsub("\\.", "_", task_id)))
  return(table_id)
}

#getDataTaskDBData
getDataTaskDBData <- function(pool, task_id){
  out <- NULL
  table_id <- getDataTaskTablename(task_id)
  hasData <- DBI::dbExistsTable(pool, table_id, schema = "public")
  if(hasData){
    out <- DBI::dbReadTable(pool, table_id, schema = "public")
  }
  return(out)
}

pretty_seq<-function(x){
  
  break_list<-which(c(1,diff(x)) != 1)
  done<-c()
  new_vec<-c()
  
  if(length(break_list>0)){
    for(i in break_list){
      
      target<-x[1:i-1]
      
      target<-setdiff(target,done)
      
      done<-c(done,target)
      
      min_v<-min(target)
      
      max_v<-max(target)
      
      seq_v<-if(min_v!=max_v){paste0(min_v,"-",max_v)}else{as.character(min_v)}
      
      new_vec<-c(new_vec,seq_v)
    }
    
    remaining<-setdiff(x,done)
    new_vec<-c(new_vec,remaining)
  }else{
    min_v<-min(x)
    max_v<-max(x)
    new_vec<-if(min_v!=max_v){paste0(min_v,"-",max_v)}else{as.character(min_v)}
  }
  return(new_vec)
}

getUniqueValues<-function(data,tasks,target){
  summary<-do.call("c",lapply(tasks,function(x){
    task<-data[[x]]
    if(target%in%names(task)){
      unique(task[[target]])
    }else{
      c()
    }}))
  summary<-unname(summary)
  summary<-unique(summary)
  summary[!is.na(summary)]
}

