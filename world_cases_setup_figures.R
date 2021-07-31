# 01 create world number of cases from MAP AND POPULATION DATA data source
# 31/07/2021
# Load required libraries  
library(tidyverse)

# 0 Avoid scientific notation
options(scipen=999)

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


# 3. Test unique names selecting Confirmed and ranking them by cases
WORLDCLOACD <- WORLDCLOACD %>% 
               select(date,Country,Confirmed)
WORLDCLOACD


# 4.  Group by DATE to have number of cases by DAY
WORLD_CASES <- WORLDCLOACD %>% 
               arrange(date)  %>% 
               group_by(date) %>% 
               mutate(TOTAL = sum(Confirmed)) %>% 
               ungroup() 

WORLD_CASES


# 5. Then we keep only one record per day
# THIS SCRIPT IS IMPORANT MAKE A MARKDOWN DOCUMENT ABOUT IT
DAILY_W_CASES <- WORLDCLOACD %>% 
                  arrange(date)  %>% 
                  group_by(date) %>% 
                  mutate(TOTAL = sum(Confirmed)) %>% 
                  ungroup()  %>% 
                  group_by(date) %>% 
                  slice(1) %>% 
                  ungroup()  


DAILY_W_CASES

# DROP COUNTRY
# 6. FINAL NUMBER OF CASES WORLDWIDE BY DATE
DAILY_W_CASES_FINAL <- DAILY_W_CASES %>% 
                       select(-Country)
DAILY_W_CASES_FINAL

# PLOT IT AS A LINE CHART
DAILY_CASES_W <-  DAILY_W_CASES_FINAL %>% 
                  select(-Confirmed)
DAILY_CASES_W

library(hrbrthemes)

MAX_V <- max(DAILY_CASES_W$TOTAL)
[1] 186,060,062

# Importnat: Max  10000000000
           # BY      10000000 (two orders of magnitude below)

ggplot(DAILY_CASES_W, aes(x=date, y=TOTAL)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=1) +
  scale_y_continuous(breaks = seq(0,10000000000,by = 10000000))+
  ggtitle("Evolution of COVID19 cases")

# Save global number of covid19 cases
ggsave(paste0("05 Total number COVID19 cases worldwide","_",
              format(Sys.time(),"%Y-%m-%d_%H-%M"),".jpeg"),
       width = 30, height = 20, dpi = 150, units = "cm")



ggplot(DAILY_CASES_W, aes(x=date, y=TOTAL)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=1) +
  scale_y_continuous(breaks = seq(0,10000000000,by = 10000000))+
  ggtitle("Evolution of COVID19 cases")

ggsave(paste0("05 01 Total number COVID19 cases worldwide LINETYPE01","_",
              format(Sys.time(),"%Y-%m-%d_%H-%M"),".jpeg"),
       width = 30, height = 20, dpi = 150, units = "cm")


ggplot(DAILY_CASES_W, aes(x=date, y=TOTAL)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  scale_y_continuous(breaks = seq(0,10000000000,by = 10000000))+
  ggtitle("Evolution of COVID19 cases")

ggsave(paste0("05 02 Total number COVID19 cases worldwide LINETYPE02","_",
              format(Sys.time(),"%Y-%m-%d_%H-%M"),".jpeg"),
       width = 30, height = 20, dpi = 150, units = "cm")


ggplot(DAILY_CASES_W, aes(x=date, y=TOTAL)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=3) +
  scale_y_continuous(breaks = seq(0,10000000000,by = 10000000))+
  ggtitle("Evolution of COVID19 cases")

ggsave(paste0("05 03 Total number COVID19 cases worldwide LINETYPE03","_",
              format(Sys.time(),"%Y-%m-%d_%H-%M"),".jpeg"),
       width = 30, height = 20, dpi = 150, units = "cm")


# SAVE WORKSPACE
save.image("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/18 TABS CONTENT TEMPLATE/03 Forecast/06 WORLD CASES FCAST/WORLD CASES ALL/01 WORLD CASES COVID19 FINAL.RData")

