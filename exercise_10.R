# Fullufjället Nationalpark Rösjöstugorna:61.623929, 12.677148; crs=wgs84

library(chillR)
library(kableExtra)

# Load weather data and check for integrity
weather <- read.csv("data/gsod_weather_fjaell.csv")
fixed_weather <- fix_weather(weather)

kable(fixed_weather$QC, caption="Quality control summary produced by *fix_weather()*, with only winter days interpolated") %>%
  kable_styling("striped", position = "left", font_size = 10)

# Download data for nearby weather stations to substitute from on large gaps
station_list <- handle_gsod(action = "list_stations",
                            location = c(y = 61.623929, x = 12.677148),
                            time_interval = c(1999, 2020))

# Select all Stations closer than 50km 
selected_stations_chillR_code <- station_list[station_list$distance <= 50 & station_list$Overlap_years > 0, "chillR_code"]
patch_weather <- list()

for(i in 1:length(selected_stations_chillR_code)){
  patch_weather[[i]] <- handle_gsod(handle_gsod(action="download_weather",
                                               location=selected_stations_chillR_code[i],
                                               time_interval=c(1999,2020)))$weather
  if(length(patch_weather) < i){
    patch_weather[[i]] <- "No Data"
    }
  names(patch_weather)[i] <- station_list[selected_stations_chillR_code[i]==station_list$chillR_code, 
                                          "STATION.NAME"]
}

# Remove No Data values from list
patch_weather <-patch_weather[unlist(lapply(patch_weather, is.data.frame), use.names = FALSE)]

patched <- patch_daily_temperatures(weather = weather,
                                    patch_weather = patch_weather)
patched$statistics

fulufjaellet_weather <- fix_weather(patched)
fulufjaellet_weather$QC


write.csv(fulufjaellet_weather$weather,"data/fulufjaellet_weather.csv", row.names = FALSE)

