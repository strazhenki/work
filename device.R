install.packages("highcharter")
install.packages("forecast")

ga_auth()
device <- google_analytics(viewId = 166777564,
                           date_range  = c(start.date = "2018-06-01", end.date = "2018-10-11"),
                           metrics = c("sessions","goalcompletionsAll"),
                           dimensions = c("ga:deviceCategory","date"),
                           anti_sample = TRUE)

gg <- ggplot(device, mapping = aes(x = sessions, y = goalcompletionsAll,date, colour = deviceCategory)) +
  geom_point() +
  theme_light()

device_n <- google_analytics(viewId = 166777564,
                           date_range  = c(start.date = "2018-06-01", end.date = "2018-10-11"),
                           metrics = c("sessions","goalcompletionsAll"),
                           dimensions = c("ga:deviceCategory","date"),
                           filter = "ga:goalcompletionsAll<=17;ga:goalcompletionsAll>=1;ga:sessions<=600",
                           anti_sample = TRUE)

cor.test(device$sessions, device$goalcompletionsAll)

shapiro.test(device_n$goalcompletionsAll)
shapiro.test(device$sessions)
barplot(device$goalcompletionsAll)
barplot(device_n$goalcompletionsAll)
boxplot(device$goalcompletionsAll)
boxplot(device_n$goalcompletionsAll)
boxplot(device_n$sessions)
mean(device$goalcompletionsAll)
mean(device_n$goalcompletionsAll)


gg <- ggplot(device_n, mapping = aes(x = sessions, y = goalcompletionsAll,date, colour = deviceCategory)) +
  geom_point() +
  theme_light()

datanew <- google_analytics(viewId = 146866395,
                           date_range  = c(start.date = "2018-03-01", end.date = "2018-08-31"),
                           metrics = "goalcompletionsAll",
                           dimensions = c("week"),
                           anti_sample = TRUE)

ga_ts2 <- ts(datanew$goalcompletionsAll, start = c(2018,03), end = c(2018,08), frequency = 3)
forecast1 <- HoltWinters(ga_ts)
hchart(forecast(forecast1, h = 3))

?ts
?HoltWinters
?forecast
