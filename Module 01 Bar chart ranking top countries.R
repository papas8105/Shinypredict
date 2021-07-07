# R Script: 02 Leaflet 30 JUN Info box WIP.R 
# Updated 29 JUN 2020 (All features included)
# Last update: 14 SEP 2020 

# Script: 50 MY APP wip barplots.R

# C:\Pablo UK\43 R projects 2020\04 My Shiny app\10 Shiny TEMPLATES\LEAFLET MAP\Interactive maps
getwd()

# LOAD DATA SETS
# - POP POPULATED
# - PLOT_LEAFLET_MAPS

# View(POP_POPULATED)
load("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/PLOT LEAFLET CDR NUM.RData")


# 00. Run script to get data from GITHUB 
#source("00 Maps data prep github.R")

# 01. Run script to load previous updated R image (PLOT_LEAFLET.Rdata)
## PLOT_LEAFLET.Rdata
# source("01 Load geodata.R")

# 02. Build Shiny dashboard

# Load required libraries to run the Shiny app
# library(gapminder)
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(leaflet)
library(plotly)

## source 


# [1-2]  User interface
ui <- dashboardPage(
  
                      dashboardHeader(title = "COVID-19"),
                      # THis Sidebar menu allows us to include new items on the sidebar
                      dashboardSidebar(
                        sidebarMenu(
                          # Setting id makes input$tabs give the tabName of currently-selected tab
                          id = "tabs",
                          menuItem("Map", tabName = "map", icon = icon("map")),
                          menuItem("Plots", tabName = "plot", icon = icon("wifi"))))
                      ,
                      dashboardBody(
                                         # Include several infoboxes at the top
                            fluidRow(
                                       infoBoxOutput("Totalrecovered", width = 3),
                                       infoBoxOutput("Totalcases", width = 3),
                                       infoBoxOutput("Totaldeaths", width = 3),
                                       infoBoxOutput("Date", width = 3)
                                      ),
                    
                     # We include the two new items on the sidebar
                        tabItems(
                          
                           tabItem(
                             tabName ="map",
                                     
                              h2("World map COVID19 deaths by contry"),
                    
                                     
                                     fluidRow(       
                                            box(
                                                sliderInput(inputId = "Time_Slider",
                                                            label = "Select Date",
                                                            min = min(PLOT_LEAFLET_MAPS$date),
                                                            max = max(PLOT_LEAFLET_MAPS$date),
                                                            value = max(PLOT_LEAFLET_MAPS$date),
                                                            width = "100%",
                                                            timeFormat = "%d%m%Y",
                                                            animate = animationOptions(interval=3000,loop = TRUE)
                                                            ),
                                                              class = "slider",
                                                              width = 15
                    
                                                )
                                            ),
                                     fluidRow(
                                            box(
                                                  dataTableOutput("mytable"), width = 15)
                                     )
                                     
                                     ),
                          tabItem(
                          
                                                      tabName="plot",  h2("Timeline indicators"), 
                            
                                                      fluidRow( h4("  Select country from dropdown menu")),
                            
                                                      fluidRow(column(4,
                                                              selectInput("country",
                                                                              "Country:",
                                                                                        c("All",
                                                                                            unique(as.character(POP_POPULATED$Country))))
                                                                                                                                          )
                                                                                            ),
                            
                                                      fluidRow(
                                                              box(  
                                                                  column(4, plotlyOutput("trend1")),
                                                                  column(4, plotlyOutput("trend2")),
                                                                  column(4, plotlyOutput("trend3")), width =12)
                    
                                                              ),
                                                      
                                                      fluidRow(
                                                        box(  
                                                            column(4, plotlyOutput("trend4")),
                                                            column(4, plotlyOutput("trend5")),
                                                            column(4, plotlyOutput("trend6")),
                                                            
                                                            
                                                            width =12)
                                                        
                                                      )
                                                      
                                            # Now in the same BODY we want to include a data table 
                                          
                              )
                          )
                )
)

# [2-2] Server  
server <- function(input,output) {
  
  # We include new dataset to subset daily data
  
  # dailydata     (this DATAFRAME comes from PLOT_LEAFLET_MAPS)
  # dailyDatatbl  (this DATAFRAME comes from POP_POPULATED )
  # prevdailyData (this DATAFRAME comes from PLOT_LEAFLET_MAPS but previous day)
  
  
  dailyData <- reactive(PLOT_LEAFLET_MAPS[PLOT_LEAFLET_MAPS$date == format(input$Time_Slider,"%Y/%m/%d"),])
  
  RATESTable <- reactive(POP_POPULATED[POP_POPULATED$date == format(input$Time_Slider,"%Y/%m/%d"),])
  
  prevdailyData <-reactive(PLOT_LEAFLET_MAPS[PLOT_LEAFLET_MAPS$date == format(input$Time_Slider-1,"%Y/%m/%d"),])

#
# TAB01 
#  
# PLACE INFO BOXES HERE ( line 113 onwards ) 
  
  # OUTPUT 01-03 INFOBOXES
  
  # Filter previous table to retain just wanted fields (Country)
  output$Totalrecovered <- renderValueBox({
    
    dataframeConf <- dailyData()
    
    dataframeConf2 <- dailyData() %>% 
      select(Country,Province,date,Recovered) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    
    dataframeConfprev <- prevdailyData() 
    
    dataframeConfprev2 <- prevdailyData() %>% 
      select(Country,Province,date,Recovered) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    
    valueBox(
      paste0(
        
        format(
        dataframeConf2$Recovered   
        , big.mark = ','),
        
             #prettyNum(dataframeConf$Recovered,big.mark = ","),
             
             paste0("[",round(((dataframeConf2$Recovered - dataframeConfprev2$Recovered)/dataframeConfprev2$Recovered)*100,2),"%","]")
      ), "Recovered | % change prev day | UK", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$Totalcases <- renderValueBox({
    
    dataframeConf <- dailyData()
    
    dataframeConf2 <- dailyData() %>% 
      select(Country,Province,date,Confirmed) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    
    dataframeConfprev <- prevdailyData() 
    
    dataframeConfprev2 <- prevdailyData() %>% 
      select(Country,Province,date,Confirmed) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    
    valueBox(
      paste0(
        
        format(
          dataframeConf2$Confirmed
          , big.mark = ','),

             paste0("[",round(((dataframeConf2$Confirmed - dataframeConfprev2$Confirmed)/dataframeConfprev2$Confirmed)*100,2),"%","]")
             
      ), "Confirmed | % change prev day | UK", icon = icon("bar-chart-o"),
      color = "green" 
    ) 
  })
  
  output$Totaldeaths <- renderValueBox({
    
    dataframeD<- dailyData()
    dataframeDP <- prevdailyData() 
    
    dataframeD2 <- dailyData() %>% 
      select(Country,Province,date,Deaths) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    
    dataframeDP2 <- prevdailyData() %>% 
      select(Country,Province,date,Deaths) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    
    valueBox(paste0(format(dataframeD2$Deaths, big.mark = ','),
        paste0("[",round(((dataframeD2$Deaths - dataframeDP2$Deaths)/dataframeDP2$Deaths)*100,2),"%","]")
             
      ), "Deaths | % change prev day | UK", icon = icon("list-alt"),
      color = "purple"
    )
  })
  
  output$Date   <- renderValueBox({
    
    dataframeDeaths <- dailyData()
    dataframeDeaths <- dailyData() %>% 
      select(Country,Province,date,Deaths ) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    valueBox(
      dataframeDeaths$date
      , "Date | Daily figures", icon = icon("calendar"),
      color = "yellow")
    
  })
  
   # OUTPUT 03-03 "DATA TABLE"
  
    output$mytable <- renderDataTable( {
    
                                        Tabledesc <- RATESTable()
                                        Tabledesc  %>%
                                        arrange(desc(Confirmed)) 
    
                                      }
  )
  
# TAB 02 - PLOTS
   #   "Conf_7D_10000" 
   #    "Rec_7D_10000"   
   #   "Death_7D_10000"
   
  
  # Output line plot  1-3 Confirmed cases  
  #   X  = date
  #   Y = Conf_7D_10000
  
  output$trend1 = renderPlotly(
    {
      data <- POP_POPULATED
      if (input$country != "All") {
        data <- data[POP_POPULATED$Country == input$country,] 
      }
     plot_ly(data, x = ~date, y = ~Conf_7D_10000, type = 'scatter', mode = 'lines')%>%
       layout(title=paste0("Covid19 confirmed cases  ", input$country))
       
       
    }
  )
  
  
  
  # THIS IS A LINEPLOT
  # X  = date
  # Y = Death_7D_10000
  
  output$trend2 = renderPlotly(
    {
      data <- POP_POPULATED
      if (input$country != "All") {
        data <- data[POP_POPULATED$Country == input$country,] 
      }
      
      plot_ly(data, x = ~date, y = ~Death_7D_10000, type = 'scatter', mode = 'lines')%>%
        layout(title=paste0("Covid19 deaths", input$country))
      
    }
  )
  
  # THIS IS A LINEPLOT 
  #   X  = date
  #   Y = Rec_7D_10000
  
  output$trend3 = renderPlotly(
    {
      
      data <- POP_POPULATED
      if (input$country != "All") {
        data <- data[POP_POPULATED$Country == input$country,] 
      }
      
      plot_ly(data, x = ~date, y = ~Rec_7D_10000, type = 'scatter', mode = 'lines')%>%
        layout(title=paste0("Covid19 deaths", input$country))
      
      
    }
  )
  
  # THIS IS A BARPLOT INPLOTLY 
  
  # Metrics for bottom plots [ Confirmed,Recovered,Death ]
  
  # PLot bottom row [ 1-3 ]
  #  Confirmed     
  # Top 10 countries by covid19 cases  
    output$trend4 = renderPlotly(
    {
      Tabledesc1 <- POP_POPULATED  %>%
                        select(Confirmed,Country,date) %>% 
                        mutate(Max_date = max(POP_POPULATED$date)) %>% 
                        mutate(Flag_max_date = ifelse(Max_date == date,1,0)) %>% 
                        filter(Flag_max_date==1) %>% 
                        arrange(desc(Confirmed)) %>% 
                        group_by(date) %>% 
                        slice(1:10) %>% 
                        ungroup()
      
      TabledesC <- Tabledesc1 %>% 
        arrange(Confirmed)
      Top10_D <- data.frame(TabledesC,stringsAsFactors = FALSE)
      Top10_D$Country <- factor(Top10_D$Country, 
                                levels = unique(Top10_D$Country)[order(Top10_D$Confirmed, decreasing = FALSE)])
      
      # Barplot top 10 countries sorted by CONFIRMED cases
      
      plot_ly(Top10_D, x = ~Confirmed, y = ~Country,
              type = 'bar', orientation = 'h')%>%
        layout(title=paste0("Top 10 countries Covid19 Confirmed cases"))
      
      
    }
  )
    
    
    # PLot bottom row [ 2-3 ]
    # Recovered     
    # output$trend5
    # Top 10 countries by covid19 cases  
    output$trend5 = renderPlotly(
      {
        Tabledesc1 <- POP_POPULATED  %>%
          select(Recovered,Country,date) %>% 
          mutate(Max_date = max(POP_POPULATED$date)) %>% 
          mutate(Flag_max_date = ifelse(Max_date == date,1,0)) %>% 
          filter(Flag_max_date==1) %>% 
          arrange(desc(Recovered)) %>% 
          group_by(date) %>% 
          slice(1:10) %>% 
          ungroup()
        
        TabledesC <- Tabledesc1 %>% 
          arrange(Recovered)
        Top10_D <- data.frame(TabledesC,stringsAsFactors = FALSE)
        Top10_D$Country <- factor(Top10_D$Country, 
                                  levels = unique(Top10_D$Country)[order(Top10_D$Recovered, decreasing = FALSE)])
        
        # Barplot top 10 countries sorted by CONFIRMED cases
        
        plot_ly(Top10_D, x = ~Recovered, y = ~Country,
                type = 'bar', orientation = 'h')%>%
          layout(title=paste0("Top 10 countries Covid19 Recovered cases"))
        
        
      }
    )
    
    # PLot bottom row [ 3-3 ]
    # Death
    # output$trend6
    # Top 10 countries by covid19 cases  
    output$trend6 = renderPlotly(
      {
        Tabledesc1 <- POP_POPULATED  %>%
          select(Death,Country,date) %>% 
          mutate(Max_date = max(POP_POPULATED$date)) %>% 
          mutate(Flag_max_date = ifelse(Max_date == date,1,0)) %>% 
          filter(Flag_max_date==1) %>% 
          arrange(desc(Death)) %>% 
          group_by(date) %>% 
          slice(1:10) %>% 
          ungroup()
        
        TabledesC <- Tabledesc1 %>% 
          arrange(Death)
        Top10_D <- data.frame(TabledesC,stringsAsFactors = FALSE)
        Top10_D$Country <- factor(Top10_D$Country, 
                                  levels = unique(Top10_D$Country)[order(Top10_D$Death, decreasing = FALSE)])
        
        # Barplot top 10 countries sorted by CONFIRMED cases
        
        plot_ly(Top10_D, x = ~Death, y = ~Country,
                type = 'bar', orientation = 'h')%>%
          layout(title=paste0("Top 10 countries Covid19 Deaths"))
        
        
      }
    )
    
    
    
  
  
}



# Launch it
shinyApp(ui = ui,server = server)
