library(chillR)
library(kableExtra)

## Porto: 41.149178,-8.612112; crs=wgs84
weather <- read.csv("data/gsod_weather_porto.csv")
fixed_weather <- fix_weather(weather)

kable(fixed_weather$QC, caption="Quality control summary produced by *fix_weather()*, with only winter days interpolated") %>%
  kable_styling("striped", position = "left", font_size = 10)

# Download data for nearby weather stations to substitute from on large gaps
station_list <- handle_gsod(action = "list_stations",
                            location = c(y = 41.149178, x = -8.612112),
                            time_interval = c(1973, 2019))

# Select all Stations closer than 50km 
selected_stations_chillR_code <- station_list[station_list$distance <= 50 & station_list$Overlap_years > 0, "chillR_code"]

patch_weather <- list()
for(i in 1:length(selected_stations_chillR_code)){
  station_name <- station_list[selected_stations_chillR_code[i]==station_list$chillR_code, "STATION.NAME"]
  print(station_name)
  patch_weather <- append(patch_weather, list(handle_gsod(handle_gsod(action="download_weather",
                                                                      location=selected_stations_chillR_code[i],
                                                                      time_interval=c(1973,2019))[[1]]$weather)))
  if(length(patch_weather) < i){
    patch_weather[[i]] <- "No Data"
  }
  names(patch_weather)[i] <- station_name
}

# Remove No Data values from list
patch_weather <-patch_weather[unlist(lapply(patch_weather, is.data.frame), use.names = FALSE)]

patched <- patch_daily_temperatures(weather = weather,
                                    patch_weather = patch_weather)
porto_weather <- fix_weather(patched)
write.csv(porto_weather$weather,file = "data/porto_weather.csv", row.names = FALSE)
