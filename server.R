# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data_tasks<-lapply(setNames(tasks,tasks),function(x){
    file<-getDataTaskDBData(pool, x)
  })
  
  reporting_entities<-dt_reporting_entities$code
  
  output$indicators<-renderUI({
    div(
      column(12,
             infoBox("Flagstates",length(getUniqueValues(data_tasks,tasks,"flagstate")), icon = icon("flag"), fill = TRUE,color="blue",width = 3),
             infoBox("Tasks",length(data_tasks), icon = icon("list-check"), fill = TRUE,color="yellow",width = 3),
             infoBox("Period",sprintf("%s-%s",min(year(getUniqueValues(data_tasks,tasks,"time_start"))),max(year(getUniqueValues(data_tasks,tasks,"time_end")))), icon = icon("clock"), fill = TRUE,color="green",width = 3),
             infoBox("Species",length(getUniqueValues(data_tasks,tasks,"species")), icon = icon("fish"), fill = TRUE,color="aqua",width = 3)
      )
    )
  })
  
  
  data<-reactiveVal(NULL)
  data_s<-reactiveVal(NULL)
  dataAvailable<-reactiveVal(ifelse(length(data_tasks)==0,FALSE,TRUE))
  
  output$summary_content<-renderUI({
    tagList(
      fluidRow(
        div(
          class = "col-md-2",
          uiOutput("entities_selector_s")
        ),
        div(
          class = "col-md-2",
          uiOutput("stat_selector_s")
        ),
        div(
          class = "col-md-2",
          uiOutput("download_wrapper")
        )
      ),
      uiOutput("heatmap_s_wrapper")
      
    )
  })
  
  output$by_task_content<-renderUI({
    if(dataAvailable()){
      tagList(
        fluidRow(
          div(
            class = "col-md-2",
            uiOutput("task_selector")
          ),
          div(
            class = "col-md-2",
            uiOutput("entities_selector")
          )
        ),
        fluidRow(
          withSpinner(plotlyOutput("heatmap"),type=4)
        )
      )}else{
        p("(no data available)")
      }
  })
  
  output$menu<-renderUI({
    tagList(
    uiOutput("indicators"),
    tabBox(id = "tabbox",title=NULL,height="600px",width = "100%",
           tabPanel(title=tagList(icon("clipboard")," Summary"),
                    value="tab_summary",
                    uiOutput("summary_content")
           ),
           tabPanel(title=tagList(icon("list")," By Task"),
                    value="tab_by_task",
                    uiOutput("by_task_content")
           ),
           tabPanel(title=tagList(icon("map")," Map"),
                    value="tab_map",
                    shiny::htmlOutput("frame")
           )
    )
    )
  })
  
  
  
  #status selector
  output$entities_selector <- renderUI({
    checkboxInput("limit_entities", "Limit to entities with data", TRUE)
  })
  
  output$entities_selector_s <- renderUI({
    checkboxInput("limit_entities_s", "Limit to entities with data", dataAvailable())
  })
  
  output$stat_selector_s<-renderUI({
    selectizeInput("stat_s",
                   label="Statistic",
                   multiple = F,
                   choices = c("Oldest available year"="min_year",
                               "Most recent available year"="max_year",
                               "Covered period"="period",
                               "Available years"="available_years",
                               "Number of years"="nb_year",
                               "Number of records"="nb_record"),
                   selected="period"
    )
  })
  
  #Task selector
  output$task_selector<-renderUI({
    selectizeInput("task",
                   label="Task",
                   multiple = F,
                   choices = tasks,
                   selected=NULL,
                   options = list(
                     placeholder = "Please select a task",
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })
  
  observeEvent(data_tasks,{
      if(dataAvailable()){
        
        summary<-do.call("rbind",lapply(tasks,function(x){
          data_task<-data_tasks[[x]]
          
          data_task<-data_task%>%
            group_by(flagstate)%>%
            summarise(period=paste0("first:",year(min(time_end,na.rm=T)),"- last:",year(max(time_end,na.rm=T))),
                      min_year=as.character(year(min(time_end,na.rm=T))),
                      max_year=as.character(year(max(time_end,na.rm=T))),
                      nb_year=as.character(length(unique(year(time_end)))),
                      nb_record=as.character(length(flagstate)),
                      available_years=paste0(pretty_seq(sort(unique(year(time_end)))),collapse=";"))%>%
            arrange(desc(flagstate))%>%
            ungroup()%>%
            mutate(task=x)
          
        })
        )
        
      }else{
        summary<-do.call("rbind",lapply(tasks,function(x){
          
          data_task<-data.frame(flagstate="",
                           period="(no data)",
                           min_year="(no data)",
                           max_year="(no data)",
                           nb_year="0",
                           nb_record="0",
                           available_years="(no data)",
                           task=x)%>%
            arrange(desc(flagstate))%>%
            ungroup()
        })
        )
        
      }
      data_s<-data_s(summary)
  })
  
  output$download_wrapper<-renderUI({
    req(data_s())
    if(nrow(data_s())>0){
      downloadButton("download",label="Download summary",icon=shiny::icon("file-excel"),style = "padding: 5px 20px; margin: 2px 8px;")
    }
  })
  
  output$download <- downloadHandler(
    filename = function() { 
      sprintf("summary.xlsx")
    },
    content = function(filename) {
      req(nrow(data_s())>0)
      wb = createWorkbook()
      
      for(i in c("period","available_years","min_year","max_year","nb_year","nb_record")){
        df<-data_s()
        df<-df[,c("flagstate","task",i)]
        
        names(df)[names(df) == i] <- "stat"
        df<-unique(df)
        
        entity_list <- NULL
        if(isTRUE(input$limit_entities_s)){
          entity_list<-unique(df$flagstate)
        }else{
          entity_list<-reporting_entities
        }
        
        df<-df%>%
          rowwise()%>%
          mutate(value=ifelse(input$stat_s%in%c("nb_year","nb_record"),as.numeric(stat),1))%>%
          ungroup()%>%
          complete(nesting(task),flagstate=entity_list,fill = list(value=0,stat="(no data)"))%>%
          arrange(desc(flagstate))
        
        df_values<-df$value
        
        df<-df%>%
          select(-value)%>%
          pivot_wider(names_from = task,values_from = stat,names_sort=T)%>%
          filter(flagstate %in% entity_list)%>%
          rename(` `=flagstate)
        
        addWorksheet(wb, i)
        setColWidths(wb, i, cols = 1:ncol(df), widths = "auto")
        nodataStyle <- createStyle(fontColour = "black", bgFill = "gray")
        dataStyle <- createStyle(fontColour = "black", bgFill = "#45AD15")
        conditionalFormatting(wb, i, cols = 2:ncol(df), rows = 1:nrow(df)+1, type = "contains", rule = "(no data)", style = nodataStyle)
        conditionalFormatting(wb, i, cols = 2:ncol(df), rows = 1:nrow(df)+1, type = "notcontains", rule = "(no data)", style = dataStyle)
        writeData(wb, sheet = i, x = df, startCol = 1)
        
      }
      saveWorkbook(wb, filename, overwrite = TRUE)
      
    })
  
  observeEvent(input$task,{
    req(input$task)
    if(!is.null(input$task)|input$task!=""){
      file<-data_tasks[[input$task]]
      data<-data(file)
    }
  })
  
  output$heatmap<-renderPlotly({
    req(!is.null(data()))
    req(!is.null(input$limit_entities))
    print("RUN HEATMAP")
    df<-subset(data(),select=c(flagstate,time_end))
    df$time_end<-year(df$time_end)
    df<-unique(df)
    df$value<-1
    
    entity_list <- NULL
    if(isTRUE(input$limit_entities)){
      entity_list<-unique(df$flagstate)
    }else{
      entity_list<-reporting_entities
    }
    
    df<-df%>%
      complete(nesting(time_end=full_seq(time_end, 1)),flagstate=entity_list,fill = list(value=0))%>%
      rename(year=time_end)%>%
      arrange(desc(flagstate),year)%>%
      filter(flagstate %in% entity_list)%>%
      pivot_wider(names_from = year,values_from = value,names_sort=T)
    
    print(head(as.data.frame(df)))
    
    y_lab<-df$flagstate
    x_lab<-colnames(df)[-1]
    
    df_matrix<-as.matrix(df[,-1])
    
    colorScale <- data.frame(
      z = c(0,1),
      col=c("grey","green")
    ) 
    
    color_s <- setNames(data.frame(c(0,1),c("grey","green") ), NULL)
    
    fig<-plot_ly(
      height = 150+40*length(y_lab),
      x=x_lab,
      y=y_lab,
      z = df_matrix,
      zmin=0,
      zmax=1,
      xgap=10,
      ygap=10,
      colorscale = color_s,
      colorbar=list(tickmode='array',tickvals=c(0,1),ticktext=c("missing","validate"),len=0.2), type = "heatmap"
    )
    
    fig<-layout(fig,
                showlegend = FALSE,
                xaxis = list(tickvals=seq(min(x_lab),max(x_lab),1),ticktext=as.character(seq(min(x_lab),max(x_lab),1)),showgrid = F)
    )
    return(fig)
  })
  
  output$heatmap_s<-renderPlotly({
    req(!is.null(data_s()))
    req(!is.null(input$limit_entities_s))
    req(dataAvailable()|(!dataAvailable()&!input$limit_entities_s))
    print("RUN HEATMAP")
    req(!is.null(input$stat_s))
    print(input$stat_s)
    df<-data_s()
    df<-df[,c("flagstate","task",input$stat_s)]
    
    names(df)[names(df) == input$stat_s] <- "stat"
    df<-unique(df)
    
    df<-df%>%
      rowwise()%>%
      mutate(value=ifelse(input$stat_s%in%c("nb_year","nb_record"),as.numeric(stat),1))%>%
      ungroup()
    
    max_value<-max(df$value,na.rm=T)
    
    print(max_value)
    entity_list <- NULL
    if(isTRUE(input$limit_entities_s)){
      entity_list<-unique(df$flagstate)
    }else{
      entity_list<-reporting_entities
    }
    
    df<-df%>%
      complete(nesting(task),flagstate=entity_list,fill = list(value=0,stat="(no data)"))%>%
      arrange(desc(flagstate))%>%
      filter(flagstate %in% entity_list)
    
    text<-df%>%
      select(-value)
    
    dfm<-df%>%
      select(-stat)%>%
      pivot_wider(names_from = task,values_from = value,names_sort=T)
    
    print(head(as.data.frame(dfm)))
    print(head(as.data.frame(text)))
    
    y_lab<-dfm$flagstate
    x_lab<-colnames(dfm)[-1]
    
    df_matrix<-as.matrix(dfm[,-1])
    
    # colorScale <- data.frame(
    #   z = c(0,1),
    #   col=c("grey","green")
    # ) 
    
    if(input$stat_s%in%c("nb_year","nb_record")){
      print("HERE")
      fig<-plot_ly(
        height = 150+40*length(y_lab),
        x=x_lab,
        y=y_lab,
        z = df_matrix,
        zmin=0,
        zmax=max(as.numeric(text$stat)),
        xgap=10,
        ygap=10,
        color = df$value,
        colors = c("grey", "#45AD15"),
        type = "heatmap"
      )
      
    }else{
      
      fig<-plot_ly(
        height = 150+40*length(y_lab),
        x=x_lab,
        y=y_lab,
        z = df_matrix,
        zmin=0,
        zmax=1,
        xgap=10,
        ygap=10,
        colorscale = setNames(data.frame(c(0,1),c("grey","#45AD15") ), NULL),
        colorbar=list(tickmode='array',tickvals=c(0,1),ticktext=c("no data","data"),len=0.2), 
        type = "heatmap"
      )
      
    }
    
    fig<-layout(fig,
                showlegend = FALSE,
                xaxis = list(side ="top",showgrid = F)
    )%>% add_annotations(x = text$task, y = text$flagstate, text = text$stat, xref = 'x', yref = 'y', showarrow = FALSE, font=list(color='black'))
    return(fig)
  })
  output$nodata_s<-renderUI({
    req(!is.null(data_s()))
    req(!is.null(input$limit_entities_s))
    req(!dataAvailable()&input$limit_entities_s)
    p("(no data available)")
  })
  
  output$heatmap_s_wrapper<-renderUI({
    if(!dataAvailable()&input$limit_entities_s){
      uiOutput("nodata_s")
    }else{
      fluidRow(
        withSpinner(plotlyOutput("heatmap_s"),type=4)
      )
    }
  })
  
  
  output$frame <- renderUI({
    my_iframe <- tags$iframe(src="https://wecafc-firms.d4science.org/data-viewer", height="700px", width="100%")
    my_iframe
  })
}