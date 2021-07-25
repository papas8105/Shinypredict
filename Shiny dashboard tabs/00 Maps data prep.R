# Date 06/07/2021
# 01 Maps data prep

# C:\Pablo UK\43 R projects 2020\04 My Shiny app\10 Shiny TEMPLATES\LEAFLET MAP\Interactive maps
# 00 Maps data prep github.R
# C:\Pablo UK\43 R projects 2020\04 My Shiny app\04 Mycovid19 app
setwd("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app")
getwd()
# Install required packages 
# [shiny,shinydashboard,DT,fs,webstats,leaflet,plotly,tidyverse]
#install.packages("shinydashboard",dependencies = TRUE) 
#install.packages("DT",dependencies = TRUE)
#install.packages("webstats",dependencies = TRUE)
#install.packages("leaflet",dependencies = TRUE)
#install.packages("plotly",dependencies = TRUE)

# Load required packages library(tidyverse) library(readr)
library("shiny")
library("shinydashboard")
library("DT")
library("fs")
# library("webstats")
library("leaflet")
library("plotly")
library("tidyverse")

# Check installed packages 
Mypath <-.libPaths() 
(.packages())

# 1 Read some data
#  https://github.com/CSSEGISandData/COVID-19/archive/master.zip
# This function to download the data works fine into a folder called \data.
# FUNCTION 01-02
# Download ZIP file from GITHUB repository
DownloadTheCOVIDData <- function() {
  
  # Create data directory if doesn't exist
    if(!dir.exists("data")){
    dir.create("data")
  }
  
  # Download master.zip file as data/covid19JH.zip
  download.file(
    url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",
    destfile = "data/covid19JH.zip"
  )
  
  # THen we need a path to throw the unzipped data
  # The last bit "time_series_covid19_" will be part of the file name
  data_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
  
  # We need to unzip them and get the three .csv files
  unzip(
    zipfile = "data/covid19JH.zip",
    files = paste0(data_path, c("confirmed_global.csv",
                                "deaths_global.csv",
                                "recovered_global.csv")),
    exdir = "data",
    junkpaths = T
  ) 
}

DownloadTheCOVIDData()

# FUNCTION 02-02
# UPDATE AND DOWNLOAD (UNZIP my data):
# Code explained

UpdateMyData <- function(){
                              T_refresh = 0.5  # hours
                              if(!dir_exists("data")){
                              dir.create("data")
                              DownloadTheCOVIDData()
  }
  else if((!file.exists("data/covid19JH.zip"))||as.double( Sys.time() - file_info("data/covid19JH.zip")$change_time, units = "hours")>T_refresh ){
    # If the latest refresh exceeds 30 minutes, then you download it again
    DownloadTheCOVIDData()
  }
}

# Call this function for testing
UpdateMyData()

# Read in the downloaded .csv files
# C:\Pablo UK\43 R projects 2020\04 My Shiny app\10 Shiny TEMPLATES\LEAFLET MAP\Interactive maps\data

#input_covid <- list.files("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/data",pattern = "_global*.csv")
#file_Name <-c("data_confirmed","data_deceased","data_recovered")
  input_covid <- list.files("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/data",pattern = "_global*.csv")
  file_Name <-c("data_confirmed","data_deceased","data_recovered")

  
  for(i in 1:length(input_covid)) {     
    assign(paste0(file_Name[i]),                                   # Read and store data frames
           read_csv(paste0("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/data/",
                           input_covid[i])))
  }



# Now we need to carry out some data manipulation

# Tidy up original datasets
library(tidyr)

# CONFIRMED TIDY
names(data_confirmed)
# First rename the two first columns using rename() function 
confirmed_tidy <- data_confirmed %>% 
                  rename(Province = 'Province/State',
                         Country = 'Country/Region') %>% 
                  pivot_longer(names_to = "date", 
                  cols = 5:ncol(data_confirmed)) %>% 
                  group_by(Province,Country,Lat,Long,date) %>% 
                  summarise("Confirmed"= sum(value,na.rm = T)) %>% 
                  mutate(date =as.Date(date,"%m/%d/%y"))


# DECEASED TIDY
deceased_tidy <- data_deceased %>% 
                  rename(Province = 'Province/State',
                  Country = 'Country/Region') %>% 
                    pivot_longer(names_to = "date",
                    cols = 5:ncol(data_deceased)) %>% 
                    group_by(Province,Country,Lat,Long,date) %>% 
                    summarise("Deaths" = sum(value,na.rm = T)) %>% 
                    mutate(date = as.Date(date,"%m/%d/%y"))

# RECOVERED TIDY
recovered_tidy <- data_recovered %>% 
                        rename(Province = 'Province/State',
                        Country = 'Country/Region') %>% 
                        pivot_longer(names_to = "date",
                        cols = 5:ncol(data_recovered)) %>% 
                        group_by(Province,Country,Lat,Long,date) %>% 
                        summarise("Recovered" = sum(value,na.rm = T)) %>% 
                        mutate(date = as.Date(date,"%m/%d/%y"))
head(recovered_tidy)

file_pathCHK <-('C://Pablo UK//43 R projects 2020//04 My Shiny app//04 Mycovid19 app//CHECKS')

# Now we merge them together
# 01-02 Merge DECEASED and CONFIRMED files
MAPDATA <- confirmed_tidy %>% 
              full_join(deceased_tidy)

# 01-02 Merge with RECOVERED data
MAPDATAF <- MAPDATA %>% 
              full_join(recovered_tidy) %>% 
              arrange(Province,Country,date)


# 6 Recode NA values into 0 
# Prior to produce final map, recode any NA values into 0. To avoid blanks on the map. 
# Confirmed = ifelse(Confirmed > 0, Confirmed,0)
# Deaths = ifelse(Deaths > 0, Deaths,0)
# Recovered = ifelse(Recovered > 0, Recovered,0)

MAPDATAG <- MAPDATAF %>% 
  mutate(
              Confirmed = ifelse(is.na(Confirmed),0,Confirmed),
              Deaths = ifelse(is.na(Deaths),0,Deaths),
              Recovered = ifelse(is.na(Recovered),0,Recovered)
  )

head(MAPDATAG)

# 7 PIVOT_LONGER
# We need to change the data structure to group Confirmed, Deaths, Recovered 
# into a single variable on its own column in the dataset
MAPDATAH <- MAPDATAG %>% 
            pivot_longer(names_to = "Metric",
                         cols = c("Confirmed","Deaths","Recovered")) %>% 
            ungroup()


# 8 PRODUCE THE PLOT
# we only need the data_evolution dataset
rm(list=ls()[! ls() %in% c("MAPDATAH")])

DATAMAP <- MAPDATAH 


# I NEED TO USE FUNCTION pivot_wide, because for the map a need a different metric on each column
PLOT_LEAFLET <- DATAMAP %>%
                pivot_wider(names_from = Metric, values_from = c(value))

PLOT_LEAFLET_MAPS <- PLOT_LEAFLET


# Remove  (removing coordinates variables)
PLOT_LEAFLET2_conf <- PLOT_LEAFLET_MAPS %>% 
                select(Country,date,Confirmed) %>% 
                group_by(Country,date) %>% 
                summarise("Confirmed" = sum(Confirmed,na.rm = T))

PLOT_LEAFLET2_death <- PLOT_LEAFLET_MAPS %>% 
                       select(Country,date,Deaths) %>% 
                       group_by(Country,date) %>% 
                        summarise("Death" = sum(Deaths,na.rm = T))
            
PLOT_LEAFLET2_Recov <- PLOT_LEAFLET_MAPS %>% 
                            select(Country,date,Recovered) %>% 
                            group_by(Country,date) %>% 
                            summarise("Recovered" = sum(Recovered,na.rm = T))

# Join together
PLOT_LEAFLET_RATES <- PLOT_LEAFLET2_conf %>% 
                       full_join(PLOT_LEAFLET2_death) %>% 
                       arrange(Country,date)

PLOT_LEAFLET_RATES <- PLOT_LEAFLET_RATES %>% 
                      full_join(PLOT_LEAFLET2_Recov) %>% 
                      arrange(Country,date)

PLOT_LEAFLET_CDR_NUM <-PLOT_LEAFLET_RATES

# Keep just plot_leaflet_rates
rm(list=ls()[!(ls()%in%c('PLOT_LEAFLET_CDR_NUM','PLOT_LEAFLET_MAPS'))])

#### We only keep these two files: 

# PLOT_LEAFLET_CDR_NUM
# PLOT_LEAFLET_MAPS

MAPcountrieslist <-unique(PLOT_LEAFLET_RATES$Country)

# wE HAVE PRODUCED THIS DATASET
# PLOT_LEAFLET_CDR_NUM

PLOT_LEAFLET_CDR_NUM

# Then we run "01 MERGE LEAFLET AND POP FIGURES.R"

# sAVE RAW Confirmed Death Recovered figures
save.image("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/CHECKS/PLOT LEAFLET CDR NUM.RData")
save.image("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/PLOT LEAFLET CDR NUM.RData")



