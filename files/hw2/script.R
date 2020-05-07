time_index <- seq(from = as.POSIXct("2016-01-01 00:00"),to = as.POSIXct("2020-04-26 00:00"), by = "hour")
electric=xts(electric2[,3],order.by = time_index)
names(electric)[1]="consumption"
electricdaily=apply.daily(electric,FUN=sum)
electricmonthly=apply.monthly(electric,FUN=sum)
electricweekly=apply.weekly(electric,FUN=sum)
conweekly=as.vector(electricweekly)
conhourly=as.vector(electric)

acf(conhourly,lag.max = 180,plot = T)

tselectricmonth=ts(conmonthly,frequency = 12,start = c(2016,1))
plot(decompose(tselectricmonth))
dc_elecmonth=decompose(tselectricmonth,type = "additive")
ds_elecmonth=tselectricmonth-dc_elecmonth$seasonal
dt_elecmonth=ds_elecmonth-dc_elecmonth$trend
acf(dt_elecmonth,na.action = na.pass)
plot(dt_elecmonth)

tselectricweek=ts(conweekly,frequency = 52,start = c(2016,1))
plot(decompose(tselectricweek))
dc_elecweek=decompose(tselectricweek,type = "additive")
ds_elecweek=tselectricweek-dc_elecweek$seasonal
dt_elecweek=ds_elecweek-dc_elecweek$trend
acf(dt_elecweek,na.action = na.pass,lag.max = 120)
plot(dt_elecweek)

tselectricday=ts(condaily,frequency = 7,start = c(2016,1))
plot(decompose(tselectricday))
dc_elecday=decompose(tselectricday,type = "additive")
ds_elecday=tselectricday-dc_elecday$seasonal
dt_elecday=ds_elecday-dc_elecday$trend
acf(dt_elecday,na.action = na.pass)
plot(dt_elecday)


model1=arima(dt_elecweek,order = c(1,0,0))
print(model1)
AIC(model1)
BIC(model1)
    
model2=arima(tselectricday,order = c(0,0,1))
print(model2)
AIC(model2)
BIC(model2)

model_fit_electric=tselectricday-residuals(model2)
ts.plot(tselectricday)
points(model_fit_electric,type = "l",col=2,lty=2)

dtplot

tselectric=ts(conhourly,frequency = 24)
plot(decompose(tselectric))
dc_elec=decompose(tselectric,type = "additive")
ds_elec=tselectric-dc_elec$seasonal
dt_elec=ds_elec-dc_elec$trend
acf(dt_elec,na.action = na.pass,lag.max = 20000)

tselectric=ts(conhourly,frequency = 168)
plot(decompose(tselectric,type = "additive"))
dc_elec=decompose(tselectric,type = "additive")
ds_elec=tselectric-dc_elec$seasonal
dt_elec=ds_elec-dc_elec$trend
acf(dt_elec,na.action = na.pass,lag.max = 350)

seasonalityt=dc_elec$seasonal[0:168]
ts.plot(seasonalityt)

#loghourly=log(conhourly)

#tselectrict=ts(loghourly,frequency = 168)
#plot(decompose(tselectrict,type = "additive"))
#dc_elect=decompose(tselectrict,type = "additive")
#ds_elect=tselectrict-dc_elect$seasonal
#dt_elect=ds_elect-dc_elect$trend
#acf(dt_elect,na.action = na.pass,lag.max = 350)

tselectric=ts(conhourly,frequency = 8760)
plot(decompose(tselectric,type = "additive"))
dc_elec=decompose(tselectric,type = "additive")
ds_elec=tselectric-dc_elec$seasonal
dt_elec=ds_elec-dc_elec$trend
testacf=acf(dc_elec$random,na.action = na.pass,lag.max = 20000,plot = F)

modelhour1=arima(dt_elec,order=c(10,0,0))
print(modelhour1)
AIC(modelhour1)
BIC(modelhour1)
model_fit_electrica=dt_elec-residuals(modelhour1)
ts.plot(dt_elec)
points(model_fit_electrica,type = "l",col=2,lty=2)


modelhour2=arima(dt_elec,order=c(0,0,1))
print(modelhour2)
AIC(modelhour2)
BIC(modelhour2)
model_fit_electricb=dt_elec-residuals(modelhour2)
ts.plot(dt_elec)
points(model_fit_electricb,type = "l",col=3,lty=2)

model_forecast <- predict(modelhour1, n.ahead = 24)$pred
model_forecast_se <- predict(modelhour1, n.ahead = 24)$se
points(model_forecast, type = "l", col = 2)
points(model_forecast - 1.96*model_forecast_se, type = "l", col = 2, lty = 2)
points(model_forecast + 1.96*model_forecast_se, type = "l", col = 2, lty = 2)


dt_elec_plot=dt_elec[37000:37848]
ts.plot(dt_elec_plot)


model_forecast <- predict(modelhour2, n.ahead = 24)$pred
model_forecast_se <- predict(modelhour2, n.ahead = 24)$se
points(model_forecast, type = "l", col = 2)
points(model_forecast - 1.96*model_forecast_se, type = "l", col = 2, lty = 2)
points(model_forecast + 1.96*model_forecast_se, type = "l", col = 2, lty = 2)

model_forecast - 1.96*model_forecast_se
