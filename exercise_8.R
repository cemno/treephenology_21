require(chillR)
require(reshape2)
require(ggplot2)

sun_path <- data.frame(JDay = 1:365,daylength(61.8, 1:365))
sun_path_long <- melt(sun_path, id = c("JDay"))

sun_path_plot <- ggplot(sun_path_long, aes(x = JDay, y = value))+
  geom_line(aes(colour = variable))+
  xlab("Julian day")+
  ylab("Hours")+
  theme_bw(base_size = 15)
sun_path_plot


str(KA_weather)
hour_temps_CKA_ideal <- stack_hourly_temps(KA_weather, 51)


str(Winters_hours_gaps)
temp_coeffs <- Empirical_daily_temperature_curve(Winters_hours_gaps)
Winter_daily <- make_all_day_table(Winters_hours_gaps, input_timestep="hour")
hour_temps_winter_emp <- Empirical_hourly_temperatures(Winter_daily, temp_coeffs)


# Self excerise: Interpolating data for a Fjäll in sweden (historical hourly data available)
fjaell_a_hourly <- read.csv2(file = "data/smhi-opendata_1_112540_20211031_164311.csv",header = TRUE, dec = ".", sep = ";", quote = "", skip = 10)
fjaell_a_hourly$date_time <- as.POSIXct(paste(fjaell_a_hourly$Datum, fjaell_a_hourly$"Tid..UTC."), tz = "UTM")
names(fjaell_a_hourly)[names(fjaell_a_hourly)=="Lufttemperatur"] <- "Temp"

ggplot(fjaell_a_hourly, aes(x = date_time, y = Temp))+
  geom_line()+
  xlab("timeline")+
  ylab("air temperate (°C)")+
  theme_bw(base_size = 14)


require(data.table)
library(data.table)
test <- c(as.POSIXct("1985-05-01 06:00:00", tz = "UTC"), as.POSIXct("1985-05-01 12:00:00", tz = "UTC"))
year(test)

fjaell_a_hourly <- data.frame(fjaell_a_hourly, 
                              Year = year(fjaell_a_hourly$date_time), 
                              Month = month(fjaell_a_hourly$date_time), 
                              Day = mday(fjaell_a_hourly$date_time), 
                              Hour = hour(fjaell_a_hourly$date_time))


fjaell_a_hourly_to_day <- make_all_day_table(fjaell_a_hourly, timestep = "hour")

#fjaell_a_hourly_inter <- interpolate_gaps_hourly(fjaell_a_hourly_to_day, latitude = 61.8)
fjaell_a_hourly_inter <- read.csv("data/fjaell_a_hourly_data_interpolated.csv")
fjaell_a_hourly_inter$date_time <- as.POSIXct(fjaell_a_hourly_inter$date_time)
write.csv(fjaell_a_hourly_inter$weather, file = "data/fjaell_a_hourly_data_interpolated.csv", row.names = FALSE)
#write.csv(fjaell_a_hourly_inter$daily_patch_report, file = "data/fjaell_a_hourly_data_interpolated_quality_check.csv", row.names = FALSE)

nrow(fjaell_a_hourly_inter[fjaell_a_hourly_inter$Tmin_source == "interpolated",]) + nrow(fjaell_a_hourly_inter[fjaell_a_hourly_inter$Tmax_source == "interpolated",]) 
nrow(fjaell_a_hourly_inter[fjaell_a_hourly_inter$Tmin_source == "solved",]) + nrow(fjaell_a_hourly_inter[fjaell_a_hourly_inter$Tmax_source == "solved",])
nrow(fjaell_a_hourly_inter[is.na(fjaell_a_hourly_inter$Tmin_source),]) + nrow(fjaell_a_hourly_inter[is.na(fjaell_a_hourly_inter$Tmax_source),])

ggplot(fjaell_a_hourly_inter, aes(x = date_time, y = Temp))+
  geom_line()+
  ggtitle("Weatherstation \"Fjäll A\" from Schweden (Latitude = 61.8)")+
  xlab("timeline")+
  ylab("air temperate (°C)")+
  theme_bw(base_size = 14)

