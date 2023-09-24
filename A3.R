library(fpp3)
data<- read.csv("10Minuten_Daten.csv")
names(data)[c(2,3,5)] <- tolower(names(data)[c(2,3,5)])
data<- data|> mutate(time_start=as_datetime(time_start)) |> as_tsibble()
mean_pm25<-mean(data$pm2.5)
mean_pm25
mean_pm10<-mean(data$pm10)
mean_pm10
mean_relf<-mean(data$rel_LF)
mean_relf
mean_temp<-mean(data$temperatur)
mean_temp

data<-data|> fill_gaps()

data$pm2.5[is.na(data$pm2.5)] <- mean_pm25
data$pm10[is.na(data$pm10)] <- mean_pm10
data$rel_LF[is.na(data$rel_LF)] <- mean_relf
data$temperatur[is.na(data$temperatur)] <- mean_temp

print(sum(is.na(data$temperatur)))


# .step was set to 120 hours, to get a prediction of year+5 days
data_stretch<- data|>
  stretch_tsibble(.init = 8760, .step =120, .id="window")|> 
  filter(window!=max(window))

mae_display<- function(fit_cv){
  forecast_CV<- fit_CV|> forecast(h=1)  
  acc<-forecast_CV |> accuracy(data)
  print(acc$MAE)

  forecast_CV<- fit_CV|> forecast(h=24)  
  acc<-forecast_CV |> accuracy(data)
  print(acc$MAE)
  forecast_CV<- fit_CV|> forecast(h=120)  
  acc<-forecast_CV |> accuracy(data)
  print(acc$MAE)
}
fit_CV<- data_stretch |> model(MEAN(pm2.5))
mae_display(fit_cv)

fit_CV<- data_stretch |> model(MEAN(pm10))
mae_display(fit_cv)

fit_CV<- data_stretch |> model(MEAN(rel_LF))
mae_display(fit_cv)

fit_CV<- data_stretch |> model(MEAN(temperatur))
mae_display(fit_cv)



# Arithmetic mean for 
data|>model(MEAN(pm2.5)) |> forecast(h = "1 hours")|>
  autoplot(filter(data, time_start > '2023-05-25 00:00:00')) + 
  labs(title= "PM2.5 Forecasting (Mean) Stutensee 1 hour", y = "PM2.5 in µg/m³")

data|>model(MEAN(pm2.5)) |> forecast(h = "24 hours")|>
  autoplot(filter(data, time_start > '2023-05-18 00:00:00')) + 
  labs(title= "PM2.5 Forecasting (Mean) Stutensee 24 hours", y = "PM2.5 in µg/m³")

data|>model(MEAN(pm2.5)) |> forecast(h = "120 hours")|>
  autoplot(filter(data, time_start > '2023-05-01 00:00:00')) + 
  labs(title= "PM2.5 Forecasting (Mean) Stutensee 120 hours", y = "PM2.5 in µg/m³")


# Arithmetic mean for PM10
data|>model(MEAN(pm10)) |> forecast(h = "1 hours")|>
  autoplot(filter(data, time_start > '2023-05-25 00:00:00')) + 
  labs(title= "PM10 Forecasting (Mean) Stutensee 1 hour", y = "PM10 in µg/m³")

data|>model(MEAN(pm10)) |> forecast(h = "24 hours")|>
  autoplot(filter(data, time_start > '2023-05-18 00:00:00')) + 
  labs(title= "PM10 Forecasting (Mean) Stutensee 24 hours", y = "PM10 in µg/m³")

data|>model(MEAN(pm10)) |> forecast(h = "120 hours")|>
  autoplot(filter(data, time_start > '2023-05-01 00:00:00')) + 
  labs(title= "PM10 Forecasting (Mean) Stutensee 120 hours", y = "PM10 in µg/m³")


# Arithmetic mean for rel_LF
data|>model(MEAN(rel_LF)) |> forecast(h = "1 hours")|>
  autoplot(filter(data, time_start > '2023-05-25 00:00:00')) + 
  labs(title= "Relative Humidity Forecasting (Mean) Stutensee 1 hour", y = "Relative Humidity in %")

data|>model(MEAN(rel_LF)) |> forecast(h = "24 hours")|>
  autoplot(filter(data, time_start > '2023-05-18 00:00:00')) + 
  labs(title= "Relative Humidity Forecasting (Mean) Stutensee 24 hours", y = "Relative Humidity in %")

data|>model(MEAN(rel_LF)) |> forecast(h = "120 hours")|>
  autoplot(filter(data, time_start > '2023-05-01 00:00:00')) + 
  labs(title= "Relative Humidity Forecasting (Mean) Stutensee 120 hours", y = "Relative Humidity in %")


# Arithmetic mean for temperature
data|>model(MEAN(temperatur)) |> forecast(h = "1 hours")|>
  autoplot(filter(data, time_start > '2023-05-25 00:00:00')) + 
  labs(title= "Temperature Forecasting (Mean) Stutensee 1 hour", y = "Temperature in °C")

data|>model(MEAN(temperatur)) |> forecast(h = "24 hours")|>
  autoplot(filter(data, time_start > '2023-05-18 00:00:00')) + 
  labs(title= "Temperature Forecasting (Mean) Stutensee 24 hours", y = "Temperature in °C")

data|>model(MEAN(temperatur)) |> forecast(h = "120 hours")|>
  autoplot(filter(data, time_start > '2023-05-01 00:00:00')) + 
  labs(title= "Temperature Forecasting (Mean) Stutensee 120 hours", y = "Temperature in °C")




#Seasonal Naive Model

data|>model(SNAIVE(pm2.5)) |> forecast(h = "1 hours")|>
  autoplot(filter(data, time_start > '2023-05-25 00:00:00'))
data|>model(SNAIVE(temperatur)) |> forecast(h = "1 hours")|>
  autoplot()
data|>model(SNAIVE(pm2.5)) |> forecast(h = "24 hours")|>
  autoplot(filter(data, time_start > '2023-05-18 00:00:00'))
data|>model(SNAIVE(temperatur)) |> forecast(h = "24 hours")|>
  autoplot()
data|>model(SNAIVE(pm2.5)) |> forecast(h = "120 hours")|>
  autoplot(filter(data, time_start > '2023-05-01 00:00:00'))
data|>model(SNAIVE(temperatur)) |> forecast(h = "120 hours")|>
  autoplot()


data|>model(SNAIVE(pm10)) |> forecast(h = "1 hours")|>
  autoplot(filter(data, time_start > '2023-05-25 00:00:00'))
data|>model(SNAIVE(temperatur)) |> forecast(h = "1 hours")|>
  autoplot()
data|>model(SNAIVE(pm10)) |> forecast(h = "24 hours")|>
  autoplot(filter(data, time_start > '2023-05-18 00:00:00'))
data|>model(SNAIVE(temperatur)) |> forecast(h = "24 hours")|>
  autoplot()
data|>model(SNAIVE(pm10)) |> forecast(h = "120 hours")|>
  autoplot(filter(data, time_start > '2023-05-01 00:00:00'))
data|>model(SNAIVE(temperatur)) |> forecast(h = "120 hours")|>
  autoplot()

data|>model(SNAIVE(rel_LF)) |> forecast(h = "1 hours")|>
  autoplot(filter(data, time_start > '2023-05-25 00:00:00'))
data|>model(SNAIVE(temperatur)) |> forecast(h = "1 hours")|>
  autoplot()
data|>model(SNAIVE(rel_LF)) |> forecast(h = "24 hours")|>
  autoplot(filter(data, time_start > '2023-05-18 00:00:00'))
data|>model(SNAIVE(temperatur)) |> forecast(h = "24 hours")|>
  autoplot()
data|>model(SNAIVE(rel_LF)) |> forecast(h = "120 hours")|>
  autoplot(filter(data, time_start > '2023-05-01 00:00:00'))
data|>model(SNAIVE(temperatur)) |> forecast(h = "120 hours")|>
  autoplot()


