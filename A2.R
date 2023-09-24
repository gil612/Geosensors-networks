data<-read.csv("1h_df.csv")
data<-data|> mutate(time_start = as_datetime(time_start)) |> as_tsibble()
data22 <- filter(data, year(data$time_start) == 2022)
data22<-data22|> fill_gaps(time_start)
data22$time_start<- floor_date(data22$time_start, "hour")
data22<-rename(data22, hour = time_start)



data22$day <- rep(1:365, each = 24)
data22$hour1 <- hour(data22$hour) 

df<-data22[,c(1:7)]


mean_pm25 <- mean(df$pm2.5, na.rm = TRUE)
df$pm2.5[is.na(df$pm2.5)] <- mean_pm25
mean_pm10 <- mean(df$pm10, na.rm = TRUE)
df$pm10[is.na(df$pm10)] <- mean_pm10

mean_relf <- mean(df$rel_LF, na.rm = TRUE)
df$rel_LF[is.na(df$rel_LF)] <- mean_relf

mean_temp <- mean(df$temperatur, na.rm = TRUE)
df$temperatur[is.na(df$temperatur)] <- mean_temp


mat <- matrix(df$pm2.5, nrow = 24, byrow= TRUE)
colnames(mat) <- paste("day.", c(1:365))
rownames(mat) <- paste("hour.", c(0:23))
mat
m<-t(mat)
DF<-as.data.frame(m)
dist_ts<-diss (SERIES = DF, METHOD = "EUCL")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "PM2.5 EUCL Clust")
dist_ts <- diss(SERIES = DF, METHOD = "DTW")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "PM2.5 DTW Clust")

mat <- matrix(df$pm10, nrow = 24, byrow= TRUE)
colnames(mat) <- paste("day.", c(1:365))
rownames(mat) <- paste("hour.", c(0:23))
mat
m<-t(mat)
DF<-as.data.frame(m)
dist_ts<-diss (SERIES = DF, METHOD = "EUCL")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "PM10 EUCL Clust")
dist_ts <- diss(SERIES = DF, METHOD = "DTW")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "PM10 DTW Clust")




mat <- matrix(df$rel_LF, nrow = 24, byrow= TRUE)
colnames(mat) <- paste("day.", c(1:365))
rownames(mat) <- paste("hour.", c(0:23))
mat
m<-t(mat)
DF<-as.data.frame(m)
dist_ts<-diss (SERIES = DF, METHOD = "EUCL")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "Relative Humidity EUCL Clust")
dist_ts <- diss(SERIES = DF, METHOD = "DTW")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "Releative Humidity DTW Clust")


mat <- matrix(df$temperatur, nrow = 24, byrow= TRUE)
colnames(mat) <- paste("day.", c(1:365))
rownames(mat) <- paste("hour.", c(0:23))
mat
m<-t(mat)
DF<-as.data.frame(m)
dist_ts<-diss (SERIES = DF, METHOD = "EUCL")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "Temperature EUCL Clust")
dist_ts <- diss(SERIES = DF, METHOD = "DTW")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "Temperature DTW Clust")


mat <- matrix(df$pm2.5, nrow = 24, byrow= TRUE)
colnames(mat) <- paste("day.", c(1:365))
rownames(mat) <- paste("hour.", c(0:23))
mat
m<-t(mat)
DF<-as.data.frame(m)
dist_ts<-diss (SERIES = DF, METHOD = "EUCL")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "PM10 Clust")



mat <- matrix(df$temperatur, nrow = 24, byrow= TRUE)
colnames(mat) <- paste("day.", c(1:365))
rownames(mat) <- paste("hour.", c(0:23))
mat
m<-t(mat)
DF<-as.data.frame(m)
dist_ts<-diss (SERIES = DF, METHOD = "EUCL")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "RELF Clust")


dist_ts <- diss(SERIES = DF, METHOD = "DTW")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "RELF Clust")

dist_ts <- diss(SERIES = DF, METHOD = "t")
hc<-hclust(dist_ts, method = "complete")
plot(hc,main = "RELF Clust")