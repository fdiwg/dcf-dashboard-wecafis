# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  waiting_screen<-tagList(
    h3("Initialisation of Application"),
    spin_flower()
  )
  
  waiter_show(html = waiting_screen)
  
  #tasks
  #---------------------------------------------------------------------------------------
  tasks<-c(
    "Nominal catches"="task-I.2",
    "Catch"="task-II.1",
    "Effort"="task-II.2",
    "Fleet engagement"="task-III.1"
  )
  
  #datasets
  #---------------------------------------------------------------------------------------
  dt_reporting_entities<-readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/regional/wecafc/cl_flagstate.csv")
  
  #db
  #---------------------------------------------------------------------------------------
  cat("DB connection parameters\n")
  conn_args <- config::get("dataconnection")
  cat(paste0("DB_DRV: ", conn_args$drv, "\n"))
  cat(paste0("DB_HOST: ", conn_args$host, "\n"))
  cat(paste0("DB_PORT: ", conn_args$port, "\n"))
  cat(paste0("DB_DBNAME: ", conn_args$dbname, "\n"))
  cat(paste0("DB_USER: ", conn_args$user, "\n"))
  if(conn_args$password!="") cat(paste0("DB_PASSWORD: **********\n"))
  conn_start = Sys.time()
  cat(paste0("Before connection: ",format(conn_start,"%y%m%d %H:%M:%S"),"\n"))
  pool <- pool::dbPool(
    drv = DBI::dbDriver(conn_args$drv),
    dbname = conn_args$dbname,
    host = conn_args$host,
    port = conn_args$port,
    user = conn_args$user,
    password = conn_args$password
  )
  end_start = Sys.time()
  cat(paste0("After connection:", format(end_start,"%y%m%d %H:%M:%S"),"\n"))
  cat(paste0("Difference: ", as.character(end_start-conn_start), " s\n"))
  
  
  
  data_tasks<-lapply(setNames(tasks,tasks),function(x){
    file<-getDataTaskDBData(pool, x)
  })
  
  reporting_entities<-dt_reporting_entities$code
  
  data<-reactiveVal(NULL)
  data_s<-reactiveVal(NULL)
  gap<-reactiveVal(5)
  heatmap_size<-reactiveVal(12)
  dataAvailable<-reactiveVal(ifelse(length(data_tasks)==0,FALSE,TRUE))
  
  waiter_hide()
  
  output$indicators<-renderUI({
    div( class="row",
         div(class = "col-12 col-sm-6 col-md-6 col-lg-3 col-xl-3",infoBox("Flagstates",length(getUniqueValues(data_tasks,tasks,"flagstate")), icon = icon("flag"), fill = TRUE,color="blue",width = NULL)),
         div(class = "col-12 col-sm-6 col-md-6 col-lg-3 col-xl-3",infoBox("Tasks",length(data_tasks), icon = icon("list-check"), fill = TRUE,color="yellow",width = NULL)),
         div(class = "col-12 col-sm-6 col-md-6 col-lg-3 col-xl-3",infoBox("Period",sprintf("%s-%s",min(year(getUniqueValues(data_tasks,tasks,"time_start"))),max(year(getUniqueValues(data_tasks,tasks,"time_end")))), icon = icon("clock"), fill = TRUE,color="green",width = NULL)),
         div(class = "col-12 col-sm-6 col-md-6 col-lg-3 col-xl-3",infoBox("Species",length(getUniqueValues(data_tasks,tasks,"species")), icon = icon("fish"), fill = TRUE,color="aqua",width = NULL))
    )
  })
  
  observeEvent(input$dimension,{
    
    print(sprintf("WINDOW WIDTH : %s ; WINDOW HEIGHT : %s",input$dimension[1],input$dimension[2]))
    
    newgap<-ifelse(input$dimension[1]>2000,10,
                   ifelse(input$dimension[1]>1000,5,
                          ifelse(input$dimension[1]>600,3,
                                        1)))
    gap<-gap(newgap)
    
    newsize<-ifelse(input$dimension[1]>2000,14,
                    ifelse(input$dimension[1]>1000,12,
                           ifelse(input$dimension[1]>600,8,
                                  6)))
      
    heatmap_size<-heatmap_size(newsize)
  })
  
  output$summary_content<-renderUI({
    tagList(
      fluidRow(
        div(class = "row",
          class = "col-12 col-sm-6 col-md-4 col-lg-2 col-xl-2",
          uiOutput("entities_selector_s")
        ),
        div(
          class = "col-12 col-sm-6 col-md-4 col-lg-2 col-xl-2",
          uiOutput("stat_selector_s")
        ),
        div(
          class = "col-12 col-sm-6 col-md-4 col-lg-2 col-xl-2",
          uiOutput("download_wrapper")
        )
      ),
      uiOutput("heatmap_s_legend"),
      uiOutput("heatmap_s_wrapper")
      
    )
  })
  
  output$by_task_content<-renderUI({
    if(dataAvailable()){
      tagList(
        fluidRow(
          div(class="row",
            class = "col-12 col-sm-6 col-md-4 col-lg-2 col-xl-2",
            uiOutput("task_selector")
          ),
          div(
            class = "col-12 col-sm-6 col-md-4 col-lg-2 col-xl-2",
            uiOutput("entities_selector")
          )
        ),
        fluidRow(
          uiOutput("heatmap_legend"),
          withSpinner(plotlyOutput("heatmap"),type=4)
        )
      )}else{
        p("(no data available)")
      }
  })
  
  output$menu<-renderUI({
    tagList(
    uiOutput("indicators"),
    div(class = "col-md-12",
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
      downloadButton("download",label="Download summary",icon=shiny::icon("file-excel"),style = "background: #0d6cac !important;  padding: 5px 20px!important; margin: 2px 8px; color: #fff !important; border-radius: 0.25rem; border: 0;")
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
      xgap=gap(),
      ygap=gap(),
      colorscale = color_s,
      colorbar=list(tickmode='array',tickvals=c(0,1),ticktext=c("missing","available"),len=0.2), 
      type = "heatmap",
      showscale = FALSE 
      
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
        xgap=gap(),
        ygap=gap(),
        color = df$value,
        colors = c("grey", "green"),
        type = "heatmap",
        showscale = FALSE,
        textposition = 'inside'
      )
      
    }else{
      
      fig<-plot_ly(
        height = 150+40*length(y_lab),
        x=x_lab,
        y=y_lab,
        z = df_matrix,
        zmin=0,
        zmax=1,
        xgap=gap(),
        ygap=gap(),
        colorscale = setNames(data.frame(c(0,1),c("grey","green") ), NULL),
        colorbar=list(tickmode='array',tickvals=c(0,1),ticktext=c("no data","data"),len=0.2), 
        type = "heatmap",
        showscale = FALSE,
        textposition='inside'
      )
      
    }
    
    fig<-layout(fig,
                showlegend = FALSE,
                uniformtext=list(minsize=0, mode='show'),
                xaxis = list(side ="top",showgrid = F)
    )%>%add_annotations(x = text$task, y = text$flagstate, text = text$stat, xref = 'x', yref = 'y', showarrow = FALSE, font=list(size=heatmap_size(),color='black'))
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
  
  output$heatmap_s_legend<-renderUI({
    fluidRow(HTML("<i class='fa-solid fa-circle' style='color: green;'></i>&nbsp;data&emsp;&emsp;<i class='fa-solid fa-circle' style='color: grey;'></i>&nbsp;no data"),align="center")
  })
  
  output$heatmap_legend<-renderUI({
    req(!is.null(data()))
    req(!is.null(input$limit_entities))
    fluidRow(HTML("<i class='fa-solid fa-circle' style='color: green;'></i>&nbsp;available&emsp;&emsp;<i class='fa-solid fa-circle' style='color: grey;'></i>&nbsp;missing"),align="center")
  })
  
  output$frame <- renderUI({
    gisviewer_link = "https://wecafc-firms.d4science.org/data-viewer/?about=false&find=false&mode=2D&baseview=World%20Imagery&views=%5B%22pid%3Drdb_firms_resource_points%2Clyr%3Drdb_firms_resource_points%2Cstrategy%3Dogc_filters%2Cpar%3D(AGENCY%20IN('WECAFC'))%2Cq%3Dfalse%22%2C%22pid%3Drdb_firms_fishery_points%2Clyr%3Drdb_firms_fishery_points%2Cstrategy%3Dogc_filters%2Cpar%3D(AGENCY%20IN('WECAFC'))%2Cq%3Dfalse%22%2C%22pid%3Drdb_wecafc_task_i_2%2Clyr%3Drdb_wecafc_task_i_2%2Cstrategy%3Dogc_viewparams%2Cpar%3Dyear%3A1981%2B1982%2B1983%2B1984%2B1985%2B1986%2B1987%2B1988%2B1989%2B1990%2B1991%2B1992%2B1993%2B1994%2B1995%2B1996%2B1997%2B1998%2B1999%2B2000%2B2001%2B2002%2B2003%2B2004%2B2005%2B2006%2B2007%2B2008%2B2009%2B2010%2B2011%2B2012%2B2013%2B2014%2B2015%2B2016%2B2017%2B2018%2B2019%2B2020%2B2021%3Baggregation_method%3Aavg_by_year%2Cvar%3Dmeasurement_value%2Cfun%3Dckmeans%2Cmaptype%3Dchoropleth%2Cenv%3Dgeom%3Aundefined%3Bvar%3Ameasurement_value%3Bv1%3A2141.9900000000157%3Bv2%3A6201.652860070296%3Bv3%3A9491.593350267623%3Bv4%3A12145.910000000169%3Bv5%3A19467.330000000173%3Bv6%3A19514.360000000495%3Bc1%3A%23fee5d9%3Bc2%3A%23fcae91%3Bc3%3A%23fb6a4a%3Bc4%3A%23de2d26%3Bc5%3A%23a50f15%3B%2Ccs%3DReds%2Ccount%3D8%2Cstyle%3Ddyn_geom-polygon_map-choropleth_class-5%2Cq%3Dfalse%22%5D&extent=-96.69803700673097,-15.229528784752688,-22.86991200673097,45.94234621524731&center=-59.78397450673097,15.356408715247312&zoom=4"
    my_iframe <- div(
      tags$a("Open map viewer in a new tab", href = gisviewer_link, target = "_blank"), tags$br(),
      tags$iframe(src = gisviewer_link, height="700px", width="100%")
    )
    my_iframe
  })
  
  session$allowReconnect(TRUE)
}