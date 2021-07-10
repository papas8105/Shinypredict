# Shinypredict
A predictive model running inside a shiny app from open data sources.

Data is refreshed daily from JHU Github repository and daily 10,000 population rates are calculated in two different R scripts that feed into the Shiny app displayed on the Shiny server. 

The Shiny app is structured into three sections: Map, Plots and Forecast tabs. The frst two tabs displays grographical information in a amp alllowing usert to hover over country circles shapes to display tooltips on specific metircs for each country that change daily as the animation updates daily figures every few seconds. 

It  also includes a tab with two predictive models, as an example of further modelling capacibilities that can be implemented in the Shiny app, using two supervised learning models: XGBoost and  Seasonal Arima SARIMA model.  All the  modeling R scripts start by splitting original data set into Train and Test sets, to allow futher down to calculate RMSE accuracy measure  to assess each mode. 
