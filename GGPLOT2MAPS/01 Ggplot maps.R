# PRODUCE MAPS USING GGPLOT2
# Date 30 OCT 2021

install.packages("ggplot2", dependencies=TRUE)
install.packages("tidyverse", dependencies=TRUE)


#these libraries need to be loaded
library(tidyverse)

# Download data from ECDC website
# We use UTILS package to download required data
library(utils)

# 1 READ IN COVID DATA AND GEOSPATIAL DATA
# 1-1 Read in COVID19 data sheet into “R”. The dataset will be called "data".
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_ei/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

# EXPLORE DATASET
head(data)
names(data)
str(data)
names(data)

# 1-1.2 Rename variables to merge with geographical data
# countriesAndTerritories -> country
# select( new_name = prev_name )
datac <- data %>% 
         select(
           dateRep,day,                     
           month, year,                    
           cases,  deaths,                  
           country = countriesAndTerritories,   
           geoId,                   
           countryterritoryCode,      
           popData2020,             
           continentExp      
         )

head(datac)

#long      lat group order country subregion
#1 -69.89912 12.45200     1     1   Aruba      <NA>
#  2 -69.89571 12.42300     1     2   Aruba      <NA>
#  3 -69.94219 12.43853     1     3   Aruba      <NA>
  
# 1-2 Load geographical data from ggplot2 package

worldmap <- map_data("world")
view(worldmap)

worldmapprep <- worldmap
head(worldmapprep)

#long      lat group order region subregion
#1 -69.89912 12.45200     1     1  Aruba      <NA>
#  2 -69.89571 12.42300     1     2  Aruba      <NA>
#  3 -69.94219 12.43853     1     3  Aruba      <NA>
#  4 -70.00415 12.50049     1     4  Aruba      <NA>
#  5 -70.06612 12.54697     1     5  Aruba      <NA>
#  6 -70.05088 12.59707     1     6  Aruba      <NA>
  
# 2-2.1 Rename variables to merge with geographical data
# region -> country
# select( new_name = prev_name )
  
worldmapd <- worldmapprep %>% select(
                                      long,lat,group, order,  
                                      country = region,subregion)

rm(data,worldmap,worldmapprep)
rm(worldmapc,worlmapl)

head(worldmapd)
  
#  long      lat group order country subregion
#  1 -69.89912 12.45200     1     1   Aruba      <NA>
#  2 -69.89571 12.42300     1     2   Aruba      <NA>
  

# 3 JOIN MAP AND COVID19 DATA  

MAPDATA <- left_join(worldmapd,datac,
                     by = "country")

# 4 CLEAN UP MERGED DATA
# From this data set we are going to plot TWO metrics (cases,deaths)
# We need to ensure that we don't have any missing data

MAPDATAC <- MAPDATA %>% 
             filter(!is.na(MAPDATA$cases))

View(MAPDATAC)
head(MAPDATAC)

# PRODCE MAP

# we need to include group=group to ensure all the countries fit together

map1 <-ggplot(MAPDATAC, aes( x = long, y = lat, group=group )) 

# polygon, fill my aesthetic fill to cases
# color = "black" there will be a black line aroung each country
map1 <- map1 + geom_polygon(aes(fill = cases),color = "black")
map1

# Add title and subtitle
map2 <- map1
map2 <- map2 + ggtitle("Total COVID19 cases in Europe on 2021") 
map2


# FILTER DATA FOR A SINGLE MONTH
names(MAPDATAC)
table(MAPDATAC$year)
table(MAPDATAC$month)
table(MAPDATAC$day)


# Produce map for 1st March 2021
MARMAP <- MAPDATAC %>% 
          filter(month == 3 &
                 day == 1)

MARMAP
View(MARMAP)



# Now we can plot our map

head(MARMAPUNIC)

map3 <-ggplot(MARMAP, aes( x = long, y = lat, group=group )) 
map3 <- map3 + geom_polygon(aes(fill = cases),color = "black")
map3 <- map3 + labs(title ="COVID19 cases in Europe. 1st March 2021")
map3

ggsave(paste0("COVID19 cases in Europe. 1st March 2021","_",
              format(Sys.time(),"%Y-%m-%d_%H-%M"),".jpeg"),
               width = 30, height = 20, dpi = 150, units = "cm") 

