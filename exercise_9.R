library(chillR)

station_list <- handle_gsod(action = "list_stations",
                            location = c(y = 61.623929, x = 12.677148),
                            time_interval = c(1980, 2020))
weather <- handle_gsod(action = "download_weather",
                       location = "023070_99999",
                       time_interval = c(1980,2020))
weather_cleaned <- handle_gsod(weather)
weather_cleaned <- weather_cleaned$weather

write.csv(weather_cleaned, file = "data/gsod_weather_fjaell.csv", row.names = FALSE)
