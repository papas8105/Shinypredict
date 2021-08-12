# Shinypredict
A predictive model running inside a shiny app from open data sources.

Data is refreshed daily from JHU Github repository and daily 10,000 population rates are calculated in two different R scripts that feed into the Shiny app displayed on the Shiny server. 

The Shiny app is structured into three sections: Map, Plots and Forecast tabs. First two tabs display grographical information in a leaflet map alllowing user to hover over countries to display tooltips on specific metircs for each country that change daily as the animation updates. 

It  also includes a tab with several predictive models, as an example of further modelling capacibilities that can be implemented in the Shiny app, using several univariate time series models: this tab R scripts start by splitting original data set into Train and Test sets, to allow futher down to calculate RMSE accuracy measure  to assess each model. 

Also the predictive models will include SARIMA, PROPHET, ETS, ENSAMBLE and TBATS models for univariate Time Series forecasting using tsensembler R package and also Tidymodels as the R model framework to select the best performing model for the final output prdiction. 
