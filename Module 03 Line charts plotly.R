# R Script: Module 03 Line dharts plotly.R 
# AIM
# Interactive line charts to display in Shiny dashboard using plotly interactive graphs. 

# Location in the whole APP
# This script is for section 1 on PLOT tab in the shiny dashboard


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
      menuItem("About", tabName = "about", icon = icon("desktop")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Plots", tabName = "plot", icon = icon("wifi")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line"))
      
     )
  )
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
        
        tabName="about",
        
        
        h1("About the COVID-19 app"),
        
        fluidRow(box(source("UI/ui_about_1.R",local =TRUE),width=11)),
        fluidRow(box(source("UI/ui_about_2.R",local =TRUE),width=11)),
        fluidRow(box(source("UI/ui_about_3.R",local =TRUE),width=11))
        
        ),
      
       tabItem(
         tabName ="map",
                 
          h2("World map COVID19 deaths by contry"),
                 
               fluidRow(
                 
                        box(
                            leafletOutput("map"),
                                          p("Map Title"),
                                          width = 15 )
                        )
                        ,
                 
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
                                          width = 15,

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
                column(4, plotlyOutput("trend")),
                column(4, plotlyOutput("trend2")),
                column(4, plotlyOutput("trend3")), width =12)
       # ,
      #  box(  
      #      column(4, plotlyOutput("trend")),
      #      column(4, plotlyOutput("trend2")),
      #      column(4, plotlyOutput("trend3")), width =12)
        )
        
              )
      
                        # Now in the same BODY we want to include a data table 
                      
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
  output$Totalrecovered <- renderValueBox({  dataframeConf <- dailyData()
    
                                                  dataframeConf2 <- dailyData() %>% 
                                                  select(Country,Province,date,Recovered) %>% 
                                                  filter(Country=="United Kingdom" &
                                                  is.na(Province))
  dataframeConfprev <- prevdailyData() 
  
  dataframeConfprev2 <- prevdailyData() %>% 
      select(Country,Province,date,Recovered) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    
    valueBox(paste0(format(dataframeConf2$Recovered, big.mark = ','),
         paste0("[",round(((dataframeConf2$Recovered - dataframeConfprev2$Recovered)/dataframeConfprev2$Recovered)*100,2),"%","]")
      ), "Recovered | % change prev day | UK", icon = icon("list"),
      color = "teal"
    )
  })
  
  output$Totalcases <- renderValueBox({ dataframeConf <- dailyData()
    
                                                dataframeConf2 <- dailyData() %>% 
                                                  select(Country,Province,date,Confirmed) %>%  
                                                  filter(Country=="United Kingdom" &
                                                           is.na(Province))
    dataframeConfprev <- prevdailyData() 
  
    dataframeConfprev2 <- prevdailyData() %>% 
      select(Country,Province,date,Confirmed) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    
    valueBox(paste0(format(dataframeConf2$Confirmed, big.mark = ','),
             paste0("[",round(((dataframeConf2$Confirmed - dataframeConfprev2$Confirmed)/dataframeConfprev2$Confirmed)*100,2),"%","]")
             
      ), "Confirmed | % change prev day | UK", icon = icon("bar-chart-o"),
      color = "green" 
    ) 
  })
  
  output$Totaldeaths <- renderValueBox({  dataframeD<- dailyData()
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
    ), "Deaths | % change prev day | UK", icon = icon("list-alt"), color = "purple")
  })
  
  output$Date   <- renderValueBox({
    
    dataframeDeaths <- dailyData()
    dataframeDeaths <- dailyData() %>% 
      select(Country,Province,date,Deaths ) %>% 
      filter(Country=="United Kingdom" &
               is.na(Province))
    valueBox(dataframeDeaths$date, "Date | Daily figures", icon = icon("calendar"),color = "yellow")
  })
  
# TAB 02 - PLOTS
# Output line plot  1-3 Confirmed cases  
  #   X  = date
  #   Y = CONF_ma07
  output$trend = renderPlotly( { data <- POP_POPULATED
                                  if (input$country != "All") {
                                  data <- data[POP_POPULATED$Country == input$country,]}
     
      plot_ly(data, x = ~date, y = ~Conf_7D_10000, type = 'scatter', mode = 'lines')%>%
      layout(title="Covid19 confirmed cases")})
  
  # Output line plot 2-3 Death cases    
  # X  = date
  # Y = DEATH_ma07
  output$trend2 = renderPlotly(  { data <- POP_POPULATED
                                    if (input$country != "All") {
                                   data <- data[POP_POPULATED$Country == input$country,]}
      plot_ly(data, x = ~date, y = ~Death_7D_10000, type = 'scatter', mode = 'lines')%>%
      layout(title="Covid19 deaths")})
  
  # Output line plot 3-3 Recovered cases
  # X  = date
  # Y = REC_ma07
  
  output$trend3 = renderPlotly( {data <- POP_POPULATED
                                  if (input$country != "All") {
                                  data <- data[POP_POPULATED$Country == input$country,]}
      plot_ly(data, x = ~date, y = ~Rec_7D_10000, type = 'scatter', mode = 'lines') %>%
      layout(title="Recovered Covid19 cases")})
}
# Launch it
shinyApp(ui = ui,server = server)
