GDP Growth Forecast with Keras 
rm(list=ls(all=TRUE))

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage('keras')
usePackage('fredr')
usePackage('xts')

fredr::fredr_set_key(
  # replace with fred key
  'fred_key')

getMASE<-function(y.xts, yhat.xts){
  #Mean Absolute Scaled Error
  comp<-na.omit(cbind(y.xts,yhat.xts))
  comp$AE<-abs(comp[,1]-comp[,2])
  sum.abs.diff.y<-sum(abs(diff(y.xts)),na.rm=T)
  TT<-nrow(comp)
  MASE=sum(comp$AE)/( (TT/(TT-1)) * sum.abs.diff.y)
  return(MASE)
}

fredr.xts <-function(series_id, start_date){
  fred<-fredr( 
    series_id = series_id,
    observation_start = as.Date(start_date),
    frequency = "q"
  )
  fred.df<-as.data.frame(fred[,c(1,3)])
  row.names(fred.df)<-fred.df$date
  fred.df$date<-NULL
  names(fred.df)<-series_id
  fred.xts<-as.xts(fred.df)
  return(fred.xts)
} # of fredr.xts()


#gdp.fcst<-oneStepGDPGrowthForecast(time.series.xts=getRawFred(symbols=c('GDPC1','UNRATE','CPIAUCSL', 'GFDEGDQ188S'),start_date='2006-09-01',collapse='quarterly',type='xts'))
oneStepGDPGrowthForecast<-function(time.series.xts,seed=1){
  differenced.xts<-diff(log(time.series.xts))
  
  #Do not lag y
  y<-differenced.xts[,1]
  
  x<-lag(differenced.xts[,2:ncol(differenced.xts)])
  names(x)<-paste(names(x),'.lag1',sep='')
  
  #combine
  transformed.xts<-merge(y,x)
  #extract x.new
  x.new<-tail(transformed.xts,1); x.new<-x.new[,2:ncol(x.new)]
  #remove na's
  transformed.xts<-na.omit(transformed.xts)
  
  x.matrix<-as.matrix(transformed.xts[,2:ncol(transformed.xts)])
  y.vector<-as.vector(transformed.xts[,1])
  
  x.train<-x.matrix
  y.train<-y.vector
  
  # scale (normalize) data
  
  #plot(density(y.train))
  #shapiro.test(y.train) #small p-values indicate extreme non-normality
  #qqnorm(y.train);qqline(y.train)
  
  x.train.scaled<-scale(x.train)
  y.train.scaled<-scale(y.train)
  
  # scale (normalize) new x
  x.new.scaled <- scale(x.new, center = attr(x.train.scaled, "scaled:center") , scale = attr(x.train.scaled, "scaled:scale"))  
  

  tensorflow::set_random_seed(seed)
  #if the above causes problems, comment it and uncomment the line below
  #use_session_with_seed(seed,disable_gpu=T,disable_parallel_cpu = T,quiet=T)
  #keras:: use_session_with_seed(1,disable_gpu=T,disable_parallel_cpu = T,quiet=T)
  
  
  # Model 
  hidden.units.1=5
  hidden.units.2=3
  
  model = keras_model_sequential()
  model %>%
    layer_dense(input_shape = ncol(x), units = hidden.units.1, activation = "relu") %>%
    layer_dropout(0.2) %>%
    layer_dense(units = hidden.units.2, activation = "relu") %>%
    layer_dropout(0.2) %>%
    layer_dense(units = 1)
  
  #regression-specific compilation
  model %>%
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error","MSE")
    )
  
  #summary(model)
  
  #fitting parameters
  verbose=1
  validation.split=0.2
  epochs=100
  batch.size=5
  patience=10
  
  callbacks=NULL
  if(!is.na(patience)) callbacks = list(keras::callback_early_stopping(patience=patience,mode="auto"))
  
  
  fit = model %>%
    keras::fit(
      x = x.train.scaled,
      y = y.train.scaled,
      shuffle = T,
      verbose=verbose,
      validation_split = validation.split,
      epochs = epochs,
      callbacks=callbacks,
      batch_size = batch.size
    )
  #plot(fit)
  
  predictions <- model %>% predict(x.new.scaled)
  #unscale (denormalize)
  predictions <- predictions * attr(y.train.scaled, "scaled:scale") + attr(y.train.scaled, "scaled:center")
  
  #recycle x.new 
  fcst.xts<-x.new
  fcst.xts$fcst<-predictions
  fcst.xts<-fcst.xts[,'fcst']
  
  return(fcst.xts)
} # of oneStepGDPGrowthForecast()


# Run ---------------------------------------------------------------------
#Run forecast in a loop, plot and report MASE

#parameters
backtest.start='2007-01-01'
gdp.forecast.backtest<-NULL

#load data for series (forecast series first)
series_ids=c('GDPC1','UNRATE','CPIAUCSL', 'GFDEGDQ188S')
start.date='1979-10-01'

time.series.xts=NULL
for(series in series_ids){
  #series<-series_ids[1]
  fred.xts<-fredr.xts(series,start.date)
  time.series.xts<-merge.xts(time.series.xts,fred.xts)
}

# ---------------------------- Q3 ------------------------------
#enumerator
backtest.quarters<-index(time.series.xts)
backtest.quarters<-backtest.quarters[which(backtest.quarters>=backtest.start)]

#backtest loop: subset and run 

for(q in backtest.quarters){
  #q<-backtest.quarters[1]
  cat('Backtesting',format(q),'...\n')
  #subset
  time.series.xts.subset<-time.series.xts[which(index(time.series.xts)<=q),]
  #forecast
  one.step.fcst<-oneStepGDPGrowthForecast(time.series.xts=time.series.xts.subset)
  #append
  gdp.forecast.backtest<-rbind(gdp.forecast.backtest,one.step.fcst)
} # of for each quarter

#attach actuals
gdp.forecast.backtest<-cbind(diff(log(time.series.xts[,1]))[which(index(time.series.xts)>=backtest.start),],gdp.forecast.backtest)
#col names
names(gdp.forecast.backtest)<-c('GDP.growth','GDP.growth.fcst')

usePackage('PerformanceAnalytics')
chart.TimeSeries(gdp.forecast.backtest,legend.loc='topleft')
tail(gdp.forecast.backtest)

#MASE
MASE=getMASE(gdp.forecast.backtest[,1],gdp.forecast.backtest[,2])
cat('MASE:',MASE,'\n')
