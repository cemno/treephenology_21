library(chillR)
## Porto: 41.149178,-8.612112; crs=wgs84
station_list <- handle_gsod(action = "list_stations",
                            location = c(y = 41.149178, x = -8.612112),
                            time_interval = c(1973, 2019))
weather <- handle_gsod(action = "download_weather",
                       location = "085450_99999",
                       time_interval = c(1973,2019))
weather_cleaned <- handle_gsod(weather)
weather_cleaned <- weather_cleaned$PORTO$weather
write.csv(weather_cleaned, file = "data/gsod_weather_porto.csv", row.names = FALSE)
