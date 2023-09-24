data<- read.csv("10Minuten_Daten.csv")
names(data)[c(2,3,5)] <- tolower(names(data)[c(2,3,5)])
data<- data|> mutate(time_start=as_datetime(time_start)) |> as_tsibble()
data<-data |> fill_gaps(pm2.5 = mean(pm2.5, na.rm = TRUE))
autoplot (data,pm2.5)
dcmp <- data |>
  model(stl= STL(pm2.5))
components(dcmp) |> autoplot()

components(dcmp) |>
  as_tsibble() |>
  autoplot(pm2.5, colour = "pink")+
  geom_line(aes (y = trend),colour = "darkred")

data |>
  ACF(pm2.5, lag_max = 10) |> autoplot()
acf(data$pm2.5, pl=TRUE)
acf(data$pm2.5, lag.max=10, plot=FALSE)

data<- read.csv("10Minuten_Daten.csv")
names(data)[c(2,3,5)] <- tolower(names(data)[c(2,3,5)])
data<- data|> mutate(time_start=as_datetime(time_start)) |> as_tsibble()
data<-data |> fill_gaps(pm10 = mean(pm10, na.rm = TRUE))
autoplot (data,pm10)
dcmp <- data |>
  model(stl= STL(pm10))
components(dcmp) |> autoplot()

components(dcmp) |>
  as_tsibble() |>
  autoplot(pm10, colour = "pink")+
  geom_line(aes (y = trend),colour = "darkred")

data |>
  ACF(pm10, lag_max = 10) |> autoplot()
acf(data$pm10, pl=TRUE)
acf(data$pm10, lag.max=10, plot=FALSE)


data<- read.csv("10Minuten_Daten.csv")
names(data)[c(2,3,5)] <- tolower(names(data)[c(2,3,5)])
data<- data|> mutate(time_start=as_datetime(time_start)) |> as_tsibble()
data<-data |> fill_gaps(temperatur = mean(temperatur, na.rm = TRUE))
autoplot (data,temperatur)
dcmp <- data |>
  model(stl= STL(temperatur))
components(dcmp) |> autoplot()
components(dcmp) |>
  as_tsibble() |>
  autoplot(temperatur, colour = "pink")+
  geom_line(aes (y = trend),colour = "darkred")
data |>
  ACF(temperatur, lag_max = 10) |> autoplot()
acf(data$temperatur, pl=TRUE)
acf(data$temperatur, lag.max=10, plot=FALSE)



data<- read.csv("10Minuten_Daten.csv")
names(data)[c(2,3,5)] <- tolower(names(data)[c(2,3,5)])
data<- data|> mutate(time_start=as_datetime(time_start)) |> as_tsibble()
data<-data |> fill_gaps(rel_LF = mean(rel_LF, na.rm = TRUE))
autoplot (data,rel_LF)
dcmp <- data |>
  model(stl= STL(rel_LF))
components(dcmp) |> autoplot()
components(dcmp) |>
  as_tsibble() |>
  autoplot(rel_LF, colour = "pink")+
  geom_line(aes (y = trend),colour = "darkred")
data |>
  ACF(rel_LF, lag_max = 10) |> autoplot()
acf(data$rel_LF, pl=TRUE)
acf(data$rel_LF, lag.max=10, plot=FALSE)
