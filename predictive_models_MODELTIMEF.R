# 01 Load required data sets

# After having executed the following scripts
#  00 Maps data prep FINAL
#  01 Leaf and pop figures FINAL
#

# 1 we load pop_populated dataset
load("C:/Pablo UK/43 R projects 2020/04 My Shiny app/04 Mycovid19 app/RATES_FOR_SHINY_APP_FINAL.Rdata")


# 2 Load required libraries for time series univariate modelling
library(tidymodels)
library(tidyverse)
library(modeltime)
library(timetk)
library(lubridate)
library(prophet)

# 2 Then we clean up the data 
names(POP_POPULATED)

table(POP_POPULATED$Country)

FCASTD <- POP_POPULATED %>% 
  select(date,Country,Population,Confirmed) %>% 
  filter(Country=="United Kingdom")

head(FCASTD)
tail(FCASTD)

plot01 <- ggplot(FCASTD,aes(x=date, y=Confirmed)) +
  geom_line(size=1.0)+
  ggtitle("COVID19 new cases in the UK")
plot01

# Create data
FCASTE <- FCASTD %>% 
  drop_na() %>% 
  arrange(date)
FCASTE


# We are using data just for UK, ungroup data set to keep just date and Confirmed cases
FCASTF <- FCASTE %>% 
  ungroup() %>% 
  select(date,Confirmed)
FCASTF


# USE MODELTIME USING MODELTIME WORKFLOW

# 1. CREATE TRAIN TEST SPLIT

train_data <-training(initial_time_split(FCASTF, prop= .8))
test_data <-testing(initial_time_split(FCASTF, prop= .8))

# We train our data using the train set and evaluate our model using our test set

# Append both datasets together

train_test <-train_data

train_test <- train_data %>% 
  mutate(type="train") %>% 
  bind_rows(test_data %>% mutate(type="test"))

train_test

head(train_test)
tail(train_test)

plot02 <-ggplot(train_test,aes(x=date, y=Confirmed, color = type)) +
  geom_line(size=1.0)+
  ggtitle("Train test split COVID19 new cases UK")

plot02

# 2 Define our list of models
# UNIVARIATE MODELS 

# AUTOARIMA
# PROPHET
# TSLM
# ARIMA BOOST (Tree based model)


# AUTOARIMA model 
# Auto-Arima model successfully setup: Auto-Regressive Integrated Moving Average
arima_model <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(Confirmed~date, data = train_data)
# PROPHET model
prophet_model <-prophet_reg() %>% 
  set_engine("prophet") %>% 
  fit(Confirmed~date, data = train_data)

# TSLM model: Linear regression model with time series components
tslm_model <-linear_reg() %>% 
  set_engine("lm") %>% 
  fit(Confirmed~as.numeric(date)+ factor(month(date, label=TRUE)), data = train_data)

# ARIMA Boost model (tree based model)
arima_boosted_model <-arima_boos(learn_rate = .015, min_n = 2) %>% 
  set_engine("auto_arima_xgboost") %>% 
  fit(Confirmed~date + as.numeric(date)+ factor(month(date, label=TRUE)), data = train_data)

# Used modeltime function to combine all models together to evaluate their performance

forecast_table <- modeltime_table(
  arima_model,
  prophet_model,
  tslm_model,
  arima_boosted_model
)
forecast_table

# Calibrate our models using TEST set
# We use modeltime_calibrate to fit our models to TEST sets and obtain different accuracy measures

Models_desc<- forecast_table %>% 
  modeltime_calibrate(test_data)
Models_desc

Models_accuracy <- forecast_table %>% 
  modeltime_accuracy()
Models_accuracy
