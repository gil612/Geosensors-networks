data<- read.csv("10Minuten_Daten.csv")
names(data)[c(2,3,5)] <- tolower(names(data)[c(2,3,5)])

summary(data)

data<- data|> mutate(time_start=as_datetime(time_start)) |> as_tsibble()



for (i in 2:5) {
  print(sum(is.na(data[i])))
}

# data<-data |> fill_gaps(pm2.5 = mean(pm2.5, na.rm = TRUE))
# data<-data |> fill_gaps(pm10 = mean(pm10, na.rm = TRUE))
data<-data |> fill_gaps(rel_LF = mean(rel_LF, na.rm = TRUE))
data<-data |> fill_gaps(temperatur = mean(temperatur, na.rm = TRUE))


# ## plot all columns
# plot.ts(data[2:5])
# 
# ## plot pm2.5 and pm10
# ggplot() + 
#   geom_line(data = data, aes(x = time_start, y = pm2.5), color = "red")+
#   geom_line(data = data, aes(x = time_start, y = pm10), color = "black")
  
## plot temperature and humidity
ggplot() + 
  geom_line(data = data, aes(x = time_start, y = temperatur), color = "blue")+
  geom_line(data = data, aes(x = time_start, y = rel_LF), color = "green") 

autoplot (data,temperatur)

dcmp <- data |>
  model(stl= STL(temperatur))

components(dcmp) |> autoplot()


autoplot (data,rel_LF)

dcmp <- data |>
  model(stl= STL(rel_LF))

components(dcmp) |> autoplot()