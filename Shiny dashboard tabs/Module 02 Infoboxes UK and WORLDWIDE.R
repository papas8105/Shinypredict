# KPI Template SHINY DASHBOARD
# 11/07/2021

#  KPI Infoboxes
#   Wireframing
#     - Two rows of three info boxes: one for UK metrics and another row for World metrics
#     - They display daily metrics AND % change previous day for each metric

# ROW 1: WORLDWIDE: Total cases, recoveredd, deaths
# ROW 2: UK: Total cases, recoveredd, deaths


library(shiny)
library(shinydashboard)
library(tidyverse)


# FIRST FOUR SET OF COLOURS

# [1-2]  User interface
ui <- dashboardPage(
  
  dashboardHeader(title = "COVID-19"),
  # THis Sidebar menu allows us to include new items on the sidebar
  dashboardSidebar(
                    sidebarMenu(menuItem("Map", tabName = "map", icon = icon("map")))),
  dashboardBody(
    
    # Include several infoboxes at the top
    
# Labels applied
# Info Box label: , "Total UK cases | % change prev day | -aqua colour- ", icon = icon("list"),
# Info Box label:  " Total UK recovered | % change prev day | -light-blue colour- ", icon = icon("list"),    
# Info Box label:    "Total UK deaths | % change prev day | -teal colour-", icon = icon("list"),
# Note: Remove box() container to expand KPIs to fill whole row , max 6, we need to keep 3 for date infobox        

# Total COVID cases | WORLD
# Total COVID recovered cases | WORLD
# Total COVID deaths  | WORLD
    
    fluidRow( infoBoxOutput("Total_cases_WORLD",width=3),
              infoBoxOutput("Total_recovered_WORLD",width=3),
              infoBoxOutput("Total_deaths_WORLD",width=3)
              
    ),
    
# Total COVID cases | UK
# Total COVID recovered cases | UK
# Total COVID deaths  | UK
    fluidRow( infoBoxOutput("Total_cases_UK", width = 3),
              infoBoxOutput("Total_recovered_UK", width = 3),
              infoBoxOutput("Total_deaths_UK", width = 3)),
    
    # We include the two new items on the sidebar
    tabItems(  tabItem( tabName ="map", h2("World map COVID19 deaths by contry"),
        
        fluidRow(   box(
            sliderInput(inputId = "Time_Slider",
                        label = "Select Date",
                        min = min(PLOT_LEAFLET_MAPS$date),
                        max = max(PLOT_LEAFLET_MAPS$date),
                        value = max(PLOT_LEAFLET_MAPS$date),
                        width = "100%",
                        timeFormat = "%d%m%Y",
                        animate = animationOptions(interval=3000,loop = TRUE)
            ),class = "slider", width = 15,
        )))
      # Now in the same BODY we want to include a data table 
      ))) 
    
    


# Generate random numbers
# Filter previous table to retain just wanted fields (Country)
server <- function(input,output) {
  
dailyData <- reactive(PLOT_LEAFLET_MAPS[PLOT_LEAFLET_MAPS$date == format(input$Time_Slider,"%Y/%m/%d"),])
prevdailyData <-reactive(PLOT_LEAFLET_MAPS[PLOT_LEAFLET_MAPS$date == format(input$Time_Slider-1,"%Y/%m/%d"),])

# [1-6] Total COVID cases | WORLD
# [2-6] Total COVID recovered cases | WORLD
# [3-6] Total COVID deaths  | WORLD

output$Total_cases_WORLD <- renderValueBox({
  
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
    ), "Total cases worldwide | % change prev day |  -orange colour-", icon = icon("list"),
    color = "orange"
  )
})
output$Total_recovered_WORLD <- renderValueBox({
  
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
    ), "Total recovered worldwide | % change prev day |  -purple colour-", icon = icon("list"),
    color = "purple"
  )
})
output$Total_deaths_WORLD <- renderValueBox({
  
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
    ), "Total deaths worldwide | % change prev day |  -maroon colour-", icon = icon("list"),
    color = "maroon"
  )
})


# [4-6] Total COVID cases | UK
# [5-6] Total COVID recovered cases | UK
# [6-6] Total COVID deaths  | UK


# [4-6] Total COVID cases | UK

output$Total_cases_UK <- renderValueBox({
  
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
                                              ), "Total UK cases | % change prev day | -aqua colour- ", icon = icon("list"),
                                              color = "aqua"
                                            )
})

# [5-6] Total COVID recovered cases | UK


output$Total_recovered_UK <- renderValueBox({
  
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
    ), " Total UK recovered | % change prev day | -light-blue colour- ", icon = icon("list"),
    color = "light-blue"
  )
})

# [6-6] Total COVID deaths  | UK

output$Total_deaths_UK <- renderValueBox({
  
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
    ), "Total UK deaths | % change prev day | -teal colour-", icon = icon("list"),
    color = "teal"
  )
})




}

shinyApp(ui, server) 
  
