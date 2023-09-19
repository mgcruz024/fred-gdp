# GDP Growth Forecast with Keras (Tensorflow)

<img src="Screenshots/gdp.jpg" width="650" height="375" />

A one-step GDP Growth Forecast using all available quarterly GDP (GDPC1) data and user-selected predictors from the Federal Reserve Economic Data website. Used R's built-in 
REST API 

GDP Predictors:



## Packages Used 
R 4.3.0
- keras
- fredr
- xts
- PerformanceAnalytics


## Backtest
<img src="Screenshots/Screenshot 2023-06-25 182005.png" width="550" height="635" />

## Evaluation 
<img src="Screenshots/Forecast.png" width="500" height="300" />
<img src="Screenshots/mase.png" width="175" height="60" />

## Notes
Personal API key voided in code. 

Retrieve a personal key from https://fred.stlouisfed.org/ to perform GDP forecasts with this model.
  
