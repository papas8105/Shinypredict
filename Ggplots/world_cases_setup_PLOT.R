# 01 create world number of cases from MAP AND POPULATION DATA data source
# 31/07/2021
# Load required libraries  
library(tidyverse)


# 1 Load workspace
load("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/18 TABS CONTENT TEMPLATE/03 Forecast/06 WORLD CASES FCAST/WORLD CASES ALL/MAP AND POPULATION DATA.RData")

str(PLOT_LEAFLET_MAPS)

# 2 Retain just "PLOT_LEAFLET_MAPS" data set
rm(POP_POPULATED)

# 2.1 Remove local and country variables
str(PLOT_LEAFLET_MAPS)

> str(PLOT_LEAFLET_MAPS)
Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	152475 obs. of  8 variables:
  $ Province : chr  "Alberta" "Alberta" "Alberta" "Alberta" ...
$ Country  : chr  "Canada" "Canada" "Canada" "Canada" ...
$ Lat      : num  53.9 53.9 53.9 53.9 53.9 ...
$ Long     : num  -117 -117 -117 -117 -117 ...
$ date     : Date, format: "2020-01-22" "2020-01-23" "2020-01-24" "2020-01-25" ...
$ Confirmed: num  0 0 0 0 0 0 0 0 0 0 ...
$ Deaths   : num  0 0 0 0 0 0 0 0 0 0 ...
$ Recovered: num  0 0 0 0 0 0 0 0 0 0 ...

# 2.2 Remove lat and long variables

WORLDCLOACD <- PLOT_LEAFLET_MAPS %>% 
              select(date,Country,Confirmed,Deaths,Recovered)
WORLMETRICS <- WORLDCLOACD
head(WORLMETRICS)

# 2.3 We are only interested in these FIVE variables
# date
# Country
# Confirmed
# Deaths
# Recovered

UNIC_C <-unique(WORLMETRICS$Country)
UNIC_C


# Test unique names selecting Confirmed and ranking them by cases
WORLDCLOACD <- WORLDCLOACD %>% 
               select(date,Country,Confirmed)
WORLDCLOACD

# 2.4  Plot it

# 1 Standard barplot
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + 
       geom_bar( ) +
       scale_fill_hue(c = 40) +
       theme(legend.position="none") +
        ggtitle("Starndar bar chart template")

mtcars_sorted <- mtcars %>% 
  arrange(desc(count))

#  group_by(date) %>% 
#  slice(1:10) %>% 
#  ungroup()

# 2.4.1 Choose latest date
max_date <- max(WORLDCLOAC$date)
max_date

[1] "2021-07-09"
# Filter dataset by that date
# arrange(desc(Confirmed)) %>% 


Latest_conf <- WORLDCLOACD %>% 
               select(date,Country,Confirmed) %>% 
               filter(date=='2021-07-09') %>% 
               arrange(desc(Confirmed)) %>% 
               group_by(date) %>% 
               slice(1:20) %>% 
               ungroup()
Latest_conf

# Now we plot it

TOP20 <- ggplot(Latest_conf,aes(y=Confirmed,
                                fill=as.factor(Country) )) +
               geom_bar( ) 
TOP20


Latest_conf_plot <-Latest_conf %>% 
                   select(Country,Confirmed)

Latest_conf_plot


# 01 standard grey bar chart
# fill is for the inside
BARPLOT_GREY_01 <- ggplot(Latest_conf_plot, aes(x=Country, y=Confirmed)) +
                geom_bar(stat = "identity")+
                ggtitle("Standard bar plot") 
                
BARPLOT_GREY_01

# Save plot
ggsave(paste0("01 BARPLOT_GREY","_",
              format(Sys.time(),"%Y-%m-%d_%H-%M"),".jpeg"),
              width = 30, height = 20, dpi = 150, units = "cm") 


# 02 bar chart with colour
# By using factor(country) we will get a different colour for each country
BARPLOT_COLOUR_02 <- ggplot(Latest_conf_plot, aes(x=Country, y=Confirmed,fill = factor(Country))) +
                     geom_bar(stat = "identity") +
                     ggtitle("Colour barplot") 
BARPLOT_COLOUR_02

ggsave(paste0("02 BARPLOT_COLOUR","_",
              format(Sys.time(),"%Y-%m-%d_%H-%M"),".jpeg"),
              width = 30, height = 20, dpi = 150, units = "cm") 



# 03 NOW WE CAN FLIP IT AROUND
# coord_flip()

BARPLOT_FLIP_03 <- ggplot(Latest_conf_plot, aes(x=Country, y=Confirmed,fill = factor(Country))) +
                            geom_bar(stat = "identity") +
                            coord_flip() +
                            ggtitle("Colour barplot") 
BARPLOT_FLIP_03

ggsave(paste0("03 BARPLOT_FLIP","_",
              format(Sys.time(),"%Y-%m-%d_%H-%M"),".jpeg"),
       width = 30, height = 20, dpi = 150, units = "cm") 

# 04 FINALLY WE CAN SORT BY CONFIRMED CASES
# Arrange in descending order by number of confirmed cases
# IMPORTNAT WE NEED TO RE-ORDER THE FACTOR VARIABLE (COUNTRY), BASED ON
# The fact_reorder() function allows to reorder the factor (data$name for example) following the value of another column (data$val here).

# load the library
library(forcats)

# Reorder following the value of another column:
Flip_plot_final <- Latest_conf_plot %>% 
                   mutate(Country = fct_reorder(Country, Confirmed)) 


BARPLOT_FLIP_04 <- ggplot(Flip_plot_final, aes(x=Country, y=Confirmed,fill = factor(Country))) +
                          geom_bar(stat = "identity") +
                          coord_flip() +
                          ggtitle("Colour barplot SORTED BY confirmed cases") 
BARPLOT_FLIP_04


# Now we plot it
ggsave(paste0("04 BARPLOT_FLIP_SORT","_",
              format(Sys.time(),"%Y-%m-%d_%H-%M"),".jpeg"),
       width = 30, height = 20, dpi = 150, units = "cm") 

# Save workspace with all plots

