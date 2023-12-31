# GDP Growth Forecast with Keras (Tensorflow)

<img src="Screenshots/gdp.jpg" width="650" height="375" />

A one-step GDP Growth Forecast using all available quarterly GDP (GDPC1) data and user-selected predictors from the [Federal Reserve Economic Data](https://fred.stlouisfed.org/). 
Used R's built-in REST API to prepare a machine-learning function (keras) that returns the single growth forecast value for the variable in the first column 
for the specified time period, using the remaining columns as predictors.

GDP Predictors Used:
- UNRATE: Unemployment Rate
- CPIAUCSL: Consumer Price Index for All Urban Consumers (All Items in U.S. City Average)
- GFDEGDQ188S: Federal Debt (Total Public Debt as Percent of Gross Domestic Product)

## Packages Used 
R 4.3.0
- keras
- fredr
- xts
- PerformanceAnalytics

## Backtest and Modeling
<img src="Screenshots/Screenshot 2023-06-25 181554.png" width="550" height="635" />

To ensure safe and accurate testing, I generated a backtest loop for all quarters 2007 Q1 up until the latest 2023 data. First
backtest forecast in 2007 is based on 104 observations starting from the earliest 1979 data. 

The neural network uses an expanding time window for a specified duration that trains predictors and creates a forecast one
quarter into the future. Each loop repeats the training process and adds additional information by binding the trained forecast. 
The goal is to encapsulate the process that happens every quarter. 
- Ex. Unemployment, Consumer Price Index, and Federal Debt are lagged by one quarter each to predict the GDP for the following quarter. 

## Evaluation 
<img src="Screenshots/Forecast.png" width="500" height="300" />
<img src="Screenshots/mase.png" width="175" height="60" />

At the end of the one-step forecast, actual GDP is binded to the generated .xts. This allows for calculations and charts.
Forecast model evaluation reported using mean absolute scaled error (MASE). Actual vs. forecast quarterly 
GDP growth values since 2007 Q1 visualized using PerformanceAnalytics time-series charts.

In this example, user-selected predictors showcased a .7076 MASE (better than the naive) and identical growth 
patterns during the duration. 

## Notes
Personal API key omitted in code. 

Please retrieve a personal key from https://fred.stlouisfed.org/ to perform GDP forecasts with this model.
  
