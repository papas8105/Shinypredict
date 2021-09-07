
# R Script: ui_get_population_figures.R

# 1 Load required libraries
library(tidyverse)
library(WDI)

## ui_get_population figures.R
# Get Total population by country indicator "SP.POP.TOTL"
getwd()

# LOAD population figures in the right way
WDIinputdata<-WDI(indicator=c("SP.POP.TOTL"),extra=TRUE)

file_pathCHK <-('C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/data')
File_name <-'/WDIinputdata.csv' 
write.csv(WDIinputdata,paste0(file_pathCHK,File_name),row.names = T)

# Important load .csv file replacing empty variable names by NA 
# Include na.strings = c("","NA") to replace empty values by NA
WDIinputdata_raw <- read.csv("C:\\Pablo UK\\43 R projects 2020\\04 My Shiny app\\04 Mycovid19 app\\data\\WDIinputdata.csv", header=T, na.strings=c("","NA"))

WDIinputdata <-WDIinputdata_raw %>% select(country,year,population=SP.POP.TOTL,capital)
WDIinputdata2 <-WDIinputdata %>% select(country,year,population,capital) 

table(WDIinputdata2$capital)

# Filter to keep 2019 data and capital is NOT null
POP_DATA_WDI <- WDIinputdata2  %>% 
                select(country,year,population,capital) %>% 
                filter(year=="2019" & !is.na(capital))

POP_DATA_2019 <- POP_DATA_WDI


## sAVE POPULATION DATA
# Keep just plot_leaflet_rates KEEP JUST 
rm(list=ls()[!(ls()%in%c('POP_DATA_WDI','POP_DATA_2019','PLOT_LEAFLET_CDR_NUM','PLOT_LEAFLET_RATES'))])

# sAVE RAW Confirmed Death Recovered figures
save.image("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/UI/POP_FIGURES_RAW.RData")


file_pathCHK <-('C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/UI')
File_name <-'/POP_DATA_2019.csv' 
write.csv(POP_DATA_WDI,paste0(file_pathCHK,File_name),row.names = T)

## tRY TO DO THE SAME WITH MY DATASETS
head(POP_DATA_2019)
str(POP_DATA_2019)


# Input missing values
POP_POPULATED <- POP_DATA_2019

CNpop<-c("Bahamas, The","Brunei Darussalam","Congo, Dem. Rep.", "Congo, Rep." , "Egypt, Arab Rep.", "Gambia, The","Iran, Islamic Rep.","Korea, Rep.",
         "Kyrgyz Republic", "Micronesia, Fed. Sts." ,"Russian Federation"  , "St. Kitts and Nevis",  "St. Lucia","St. Vincent and the Grenadines" ,
         "Slovak Republic","Syrian Arab Republic","United States", "Venezuela, RB", "Yemen, Rep.")

length(CNpop)

CNindic<-c("Bahamas","Brunei","Congo (Brazzaville)","Congo (Kinshasa)","Egypt", "Gambia" , "Iran"   , "Korea, South", 
           "Kyrgyzstan"  , "Micronesia", "Russia", "Saint Kitts and Nevis"  ,"Saint Lucia",  "Saint Vincent and the Grenadines",
           "Slovakia","Syria","US", "Venezuela" ,"Yemen" )
length(CNindic)

# Then we replace those values 
POP_POPULATED[which(POP_POPULATED$country %in% CNpop ), "country"] <- CNindic

# cHECK CLEAN DATASET
POP_POPULATED

# Produce nice output table using gt package
getwd()  


  # sAVE RAW Confirmed Death Recovered figures
  rm(list=ls()[!(ls()%in%c('POP_POPULATED','PLOT_LEAFLET_CDR_NUM'))])

  # Save two main datasets
  file_pathCHK <-('C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/CHECKS')
  File_name <-'/POP_POPULATED_2019.csv' 
  write.csv(POP_POPULATED,paste0(file_pathCHK,File_name),row.names = T)
  File_name <-'/PLOT_LEAFLET_CDR_NUM.csv' 
  write.csv(PLOT_LEAFLET_CDR_NUM,paste0(file_pathCHK,File_name),row.names = T)
 
  save.image("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/CHECKS/MAPS AND POPULATION FILES TO MERGE.RData")
  
  
  save.image("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/POP_FIGURES_FINAL.RData")
  
  load("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/PLOT LEAFLET CDR NUM.RData")
  
  
  