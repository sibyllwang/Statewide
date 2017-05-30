library(choroplethr)
library(choroplethrMaps)
library(mapproj)
library(dplyr)
library(ggplot2)
library(shiny)
#library(shinythemes)

load("foods.RDA")
load("pop6015.RDA")
load("df_pop_state.RDA")
source("abb2state.R")


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  #theme = shinytheme("slate"),
  
  # Application title
  titlePanel("Statewide"),
  
  tabsetPanel(              
    tabPanel(title = "Intro", includeMarkdown("StatewideTutorial.Rmd")),
    
    tabPanel(title = "Tutorial",
             tabsetPanel(
               tabPanel(title = "Sample Data", dataTableOutput("foodstb")), ### here we put foods data
               tabPanel(title = "Sample Map and Plot", 
                        fluidRow(
                          sidebarLayout(
                            # Sidebar with a slider input for Year 
                            sidebarPanel(
                              strong("Inputs that affect both the map and plot"),
                              hr(),
                              sliderInput("Year",
                                          min = min(foods$year),
                                          max = max(foods$year),
                                          #### Problem: in server, when using "Year" to subset 
                                          #### foods, is it 2005 and 2008, instead of 2005 to 2008
                                          value = c(min(foods$year),max(foods$year)), 
                                          sep="",
                                          label = "Choose year(s)"),
                              selectInput("Variable",
                                          label = "Response Variable", 
                                          choices = names(foods[,sapply(foods, is.numeric)]),
                                          selected = "illnesses"),
                              width = c(3,9)
                              ),
                            mainPanel(fluidRow(
                                              column(width=3, selectInput("Statistic",
                                                                           label = "Map Statistic", 
                                                                           choices = c("sum", "mean"),
                                                                           selected = "mean")),
                                              column(width=5, selectInput("PopPercap",
                                                                          label = "Crude, Per Capita or Per 10,000", 
                                                                          choices = c("crude", "per capita",
                                                                                      "per ten thousand"),
                                                                          selected = "crude"))),
                                      fluidRow(plotOutput("map")),
                                      #button to save map and plot
                                      fluidRow(column(width = 5, downloadButton("downloadMap", "Download Map"))),
                                      hr()))),
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              strong("Inputs that only affect the plot"),
                              hr(),
                              selectInput("PlotX",
                                          label = "Plot X Variable",
                                          choices = names(foods),
                                          selected = "month"),
                              selectInput("TypeofPlot",
                                          label = "Type of Plot",
                                          choices = c("boxplot", "dotplot"),
                                          selected = "dotplot"),
                              width = c(3,9)),
                            mainPanel(fluidRow(plotOutput("plot")),
                                      #button to save map and plot
                                      fluidRow(column(width = 5, downloadButton("downloadPlot", "Download Plot"))),
                                      hr())))),
               tabPanel(title = "Sample Table", 
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("TableYear",
                                        min = min(foods$year),
                                        max = max(foods$year),
                                        #### Problem: in server, when using "Year" to subset 
                                        #### foods, is it 2005 and 2008, instead of 2005 to 2008
                                        value = c(min(foods$year),max(foods$year)), 
                                        sep="",
                                        label = "Choose year(s)"),
                            selectInput("Table_Var",
                                        label = "Numeric Table Variable",
                                        choices = names(foods[,sapply(foods, is.numeric)]),
                                        selected = "hospitalizations"),
                            selectInput("Table_Var2",
                                        label = "Categorical Table Variable",
                                        choices = names(foods[,sapply(foods, is.character)]),
                                        selected = "state"),
                            selectInput("Table_Stat",
                                        label = "Table Statistic",
                                        choices = c("sum", "mean"),
                                        selected = "sum"),
                            width = c(3,9)),
                          mainPanel(dataTableOutput("table")))))),
    
    
    #### http://shiny.rstudio.com/gallery/file-upload.html
    tabPanel(title = "Upload and View Data",
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 tags$hr(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
               ),
               mainPanel(dataTableOutput("Data_Table")
               ))),
    
    tabPanel(title = "Analysis",
             uiOutput("ui"))
  )))




# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  ################### label user data so it can be referred in analysis ###################
  #### http://stackoverflow.com/questions/24599141/dealing-with-an-input-dataset-in-r-shiny
  userData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    names(data) <- gsub("states|States|State", "state", names(data))
    data$state <- abb2state(data$state)
    names(data) <- gsub("Year", "year", names(data))
    names(data) <- tolower(names(data))
    data<-data[,-1]
    return(data)
  })
  
  
  ######################################## Tutorial #######################################
  
  ################### Sample Data (foodstb) ##################
  output$foodstb = renderDataTable({
    foods
  },options = list(pageLength = 10))
  
  
  ##################### Sample Map (map) #######################
  mapInput <- reactive({
    
    # generate map based on input$year from ui.R
    # http://stackoverflow.com/questions/38181744/r-shiny-input-slider-range-values
    food_year <- subset(foods, year >= input$Year[1] & year <= input$Year[2])
    food_year<-merge(food_year, pop6015,by=c("state", "year"))
    ## change all state names to lowercase
    food_year$state <- tolower(food_year$state)
    food_year$state <- gsub("dc|DC|District of Columbia|Washington DC|washington dc",
                          "district of columbia", food_year$state)
    
    food_year$percap<-food_year[,input$Variable]/food_year$population
    food_year$pertenthou<-food_year[,input$Variable]/food_year$population*10000
    
    ## create titles for maps
    titlePop<-paste("US map of", input$Statistic, input$Variable, "across all observations")
    titlePercap<-paste("US map of", input$Statistic, input$Variable, "across all observations (Per Capita)")
    titlePertenthou<-paste("US map of", input$Statistic, input$Variable, "across all observations (Per 10,000)")
    
  ##choroplethr package: https://cran.r-project.org/web/packages/choroplethr/choroplethr.pdf
  if(input$PopPercap=="crude"){ 
    ## aggregate data based on input$Variable and input$Statistic
    formula <- paste(input$Variable, "~", "state")
    byState <- aggregate(as.formula(formula), data = food_year, input$Statistic)
    ## subsetting states that is coded in the choropleth function
    sameStates<-byState$state %in% df_pop_state$region
    byStatewk<-byState[sameStates,]
    ## change col name from "state" to "region" and create "value" column
    ## per requirement of choropleth function
    names(byStatewk)<-c("region",input$Variable)
    byStatewk$value = byStatewk[,input$Variable]
    mapp<-state_choropleth(byStatewk, title = titlePop, num_colors = 8)
    mapp
  }else if(input$PopPercap=="per capita"){
    byState <- aggregate(percap~state, data = food_year, input$Statistic)
    sameStates<-byState$state %in% df_pop_state$region
    byStatewk<-byState[sameStates,]
    names(byStatewk)<-c("region","percap")
      byStatewk$value = byStatewk$percap
      mapp<-state_choropleth(byStatewk, title = titlePercap, num_colors = 8)
      mapp
  }else if(input$PopPercap=="per ten thousand"){
    byState <- aggregate(pertenthou~state, data = food_year, input$Statistic)
    sameStates<-byState$state %in% df_pop_state$region
    byStatewk<-byState[sameStates,]
    names(byStatewk)<-c("region","pertenthou")
    byStatewk$value = byStatewk$pertenthou
    mapp<-state_choropleth(byStatewk, title = titlePertenthou, num_colors = 8)
    mapp
  }
  })
  
  output$map <- renderPlot({
    print(mapInput())
  })
  
  # how to downlod images
  #http://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
  output$downloadMap <- downloadHandler(
    filename = function() { paste('MapOf',input$Variable, input$Statistic,input$Year[1],'to',
                                  input$Year[2], '.png', sep='') },
    content = function(file) {
      ggsave(file,mapInput())
    }
  )
  
  
  ##################### Sample Plot (plot) ######################
  #create reactive plot 
  plotInput <- reactive({
    food_year <- subset(foods, year >= input$Year[1] & year <= input$Year[2])
    
    ###### this worked but is boring
    ## http://stackoverflow.com/questions/3480388/how-to-fit-a-smooth-curve-to-my-data-in-r
    if(input$TypeofPlot=="dotplot")  {
      if(typeof(food_year[input$PlotX][[1]][1])=="character"){
        return()
      }else{
      formula<-paste(input$Variable, "~", input$PlotX)
      plott<-plot(as.formula(formula), data=food_year)
      plott
      }
    } else if(input$TypeofPlot=="boxplot") {
      formula<-paste(input$Variable, "~", input$PlotX)
      plott<-boxplot(as.formula(formula), data=food_year)
      plott
    }
  })
  
  #render plot
  output$plot <- renderPlot({
    print(plotInput())
  })
  # code to download plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste('PlotOf ',input$Variable, 
                            input$Statistic,input$Year[1],'to',input$Year[2], '.png', sep='') },
    content = function(file) {
      ggsave(file,plotInput())
    }
  )
  
  
  #################### Sample Table (table) ######################
  
  output$table <- renderDataTable({
    food_year <- subset(foods, year >= input$TableYear[1] & year <= input$TableYear[2])
    formula<-paste(input$Table_Var, "~", input$Table_Var2)
    tab<-as.data.frame(aggregate(as.formula(formula), data = food_year, input$Table_Stat))
    tab
  }, options = list(pageLength = 10))
  
  
  #################################### Upload and View Data ###################################
  
  #### http://shiny.rstudio.com/gallery/file-upload.html
  output$Data_Table <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  }, options = list(pageLength = 10))
  #### http://shiny.rstudio.com/articles/datatables.html
  
  
  
  
  ######################################## Analysis #######################################
  
  ######################## Analysis ui ##########################
  
  ###http://stackoverflow.com/questions/22423363/r-shiny-access-input-fields-in-ui
  output$ui<-renderUI({
    tabPanel(title = "Analysis",
             tabsetPanel(
               tabPanel(title = "Map and Plot", 
                        fluidRow(
                          sidebarLayout(
                            # Sidebar with a slider input for Year 
                            sidebarPanel(
                              strong("Inputs that affect both map and plot"),
                              hr(),
                              sliderInput("RealYear",
                                          min = min(userData()$year),
                                          max = max(userData()$year),
                                          value = c(min(userData()$year),max(userData()$year)), 
                                          sep="",
                                          step = 1,
                                          label = "Choose year(s)"),
                              selectInput("RealVariable",
                                          label = "Response Variable", 
                                          choices = names(userData()[,sapply(userData(), is.numeric)]),
                                          selected = names(userData()[,sapply(userData(), is.numeric)])[1]),
                              width = c(3,9)),
                            mainPanel(fluidRow(
                              column(width=5, selectInput("RealStatistic",
                                                          label = "Map Statistic", 
                                                          choices = c("sum", "mean"),
                                                          selected = "mean")),
                              column(width=6, selectInput("RealPopPercap",
                                                          label = "Crude, Per Capita or Per 10,000", 
                                                          choices = c("crude", "per capita",
                                                                      "per ten thousand"),
                                                          selected = "crude"))),
                                      fluidRow(plotOutput("Realmap")),
                                      #button to save map and plot
                                      fluidRow(column(width = 5, downloadButton("RealdownloadMap", "Download Map"))),
                                      hr()))),
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              strong("Inputs that only affect plot"),
                              hr(),
                              selectInput("RealPlotX",
                                          label = "Plot X Variable",
                                          choices = names(userData()),
                                          selected = names(userData())[1]),
                              selectInput("RealTypeOfPlot",
                                          label = "Choose Type of Plot",
                                          choices = c("DotPlot", "BoxPlot"),
                                          selected = "DotPlot"),
                              width = c(3,9)),
                            mainPanel(fluidRow(plotOutput("RealPlot")),
                                      #button to save map and plot
                                      fluidRow(column(width = 5, downloadButton("RealDownloadPlot", "Download Plot"))),
                                      hr())))),
               tabPanel(title = "Table", 
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("RealYearTable",
                                        min = min(userData()$year),
                                        max = max(userData()$year),
                                        value = c(min(userData()$year),max(userData()$year)), 
                                        sep="",
                                        label = "Choose year(s)"),
                            selectInput("RealTable_Var",
                                        label = "Numeric Table Variable",
                                        choices = names(userData()[,sapply(userData(), is.numeric)]),
                                        selected = names(userData()[,sapply(userData(), is.numeric)])[1]),
                            selectInput("RealTable_Var2",
                                        label = "Categorical Table Variable",
                                        choices = names(userData()),
                                        selected = names(userData())[2]),
                            selectInput("RealTable_Stat",
                                        label = "Table Statistic",
                                        choices = c("sum", "mean"),
                                        selected = "sum"),
                            width=c(3,9)),
                          mainPanel(dataTableOutput("Realtable"))))))
  })
  
  
  ###################### Map (user data map, ui:Realmap) ######################
  RealmapInput <- reactive({
    if (is.null(userData()))
      return(NULL)
    file1_year <- subset(userData(), year >= input$RealYear[1] & year <= input$RealYear[2])
    file1_year<-merge(file1_year, pop6015,by=c("state", "year"))
    
    file1_year$state <- gsub("dc|DC|District of Columbia|Washington DC|washington dc", 
                             "district of columbia", file1_year$state)
    file1_year$state <- tolower(file1_year$state)
    
    file1_year$percap<-file1_year[,input$RealVariable]/file1_year$population
    file1_year$pertenthou<-file1_year[,input$RealVariable]/file1_year$population*10000
    
    ## create titles
    titlePop<-paste("US map of", input$RealStatistic, input$RealVariable, "across all observations")
    titlePercap<-paste("US map of", input$RealStatistic, input$RealVariable, "across all observations (per capita)")
    titlePertenthou<-paste("US map of", input$RealStatistic, input$RealVariable, "across all observations (per 10,000 people)")
    
    if(input$RealPopPercap=="crude"){ 
      formula <- paste(input$RealVariable, "~", "state")
      byState <- aggregate(as.formula(formula), data = file1_year, input$RealStatistic)
      sameStates<-byState$state %in% df_pop_state$region
      byStatewk<-byState[sameStates,]
      names(byStatewk)<-c("region",input$RealVariable)
      byStatewk$value = byStatewk[,input$RealVariable]
      mapp<-state_choropleth(byStatewk, title = titlePop, num_colors = 8)
      mapp
    }else if(input$RealPopPercap=="per capita"){
      byState <- aggregate(percap~state, data = file1_year, input$RealStatistic)
      sameStates<-byState$state %in% df_pop_state$region
      byStatewk<-byState[sameStates,]
      names(byStatewk)<-c("region","percap")
      byStatewk$value = byStatewk$percap
      mapp<-state_choropleth(byStatewk, title = titlePercap, num_colors = 8)
      mapp
    }else if(input$RealPopPercap=="per ten thousand"){
      byState <- aggregate(pertenthou~state, data = file1_year, input$RealStatistic)
      sameStates<-byState$state %in% df_pop_state$region
      byStatewk<-byState[sameStates,]
      names(byStatewk)<-c("region","percap")
      byStatewk$value = byStatewk$percap
      mapp<-state_choropleth(byStatewk, title = titlePercap, num_colors = 8)
      mapp
    }
  })
  
  output$Realmap <- renderPlot({
    if (is.null(RealmapInput()))
      return(NULL)
    print(RealmapInput())
  })
  
  # how to downlod images
  #http://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
  output$RealdownloadMap <- downloadHandler(
    filename = function() { paste('MapOf',input$RealVariable, input$RealStatistic,
                                  input$RealYear[1],'to',input$RealYear[2], '.png', sep='') },
    content = function(file) {
      ggsave(file,RealmapInput())
    }
  )
  
  ################ Plot (user data plot, ui: Realplot) #########
  
  output$RealPlot <- renderPlot({
    # data <- userData()
    if(is.null(userData()))
      return(NULL)
    
    data_year <- subset(userData(), year >= input$RealYear[1] & year <= input$RealYear[2])
    
    if(input$RealTypeOfPlot == "DotPlot")  {
      if(typeof(data_year[input$RealPlotX][[1]][1])=="character"){
        return()
      }else{
      formula<-paste(input$RealVariable, "~", input$RealPlotX)
      plot(as.formula(formula), data=data_year)
      }
    } 
    else if(input$RealTypeOfPlot == "BoxPlot") {
      formula<-paste(input$RealVariable, "~", input$RealPlotX)
      boxplot(as.formula(formula), data=data_year)
    }
  })
  
  # code to download plot
  output$RealDownloadPlot <- downloadHandler(
    filename = function() {paste('PlotOf ', input$RealVariable, 
                                 input$RealStatistic, input$Year[1],'to', input$Year[2], '.png', sep='') },
    content = function(file) {
      ggsave(file,plotInput())
    }
  )
  
  ###################### Table (user data table, ui: Realtable) ######################
  output$Realtable <- renderDataTable({
    if (is.null(userData()))
      return(NULL)
    file1_year <- subset(userData(), year >= input$RealYearTable[1] & year <= input$RealYearTable[2])
    formula<-paste(input$RealTable_Var, "~", input$RealTable_Var2)
    tab<-as.data.frame(aggregate(as.formula(formula), data = file1_year, input$RealTable_Stat))
    tab
  }, options = list(pageLength = 10))
  
})

# Run the application 
shinyApp(ui = ui, server = server)
