<<<<<<< HEAD
require(chillR)
require(reshape2)
require(ggplot2)
=======
library(chillR)
library(reshape2)
library(ggplot2)
>>>>>>> db7272278e37753f0a17dd9f61b5869db4530899

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
<<<<<<< HEAD
fjaell_a_hourly <- read.csv2(file = "data/smhi-opendata_1_112540_20211031_164311.csv",header = TRUE, dec = ".", sep = ";", quote = "", skip = 10)
=======
fjaell_a_hourly <- read.csv2(file = "smhi-opendata_1_112540_20211031_164311.csv",header = TRUE, dec = ".", sep = ";", quote = "", skip = 10)
>>>>>>> db7272278e37753f0a17dd9f61b5869db4530899
fjaell_a_hourly$date_time <- as.POSIXct(paste(fjaell_a_hourly$Datum, fjaell_a_hourly$"Tid..UTC."), tz = "UTM")
names(fjaell_a_hourly)[names(fjaell_a_hourly)=="Lufttemperatur"] <- "Temp"

ggplot(fjaell_a_hourly, aes(x = date_time, y = Temp))+
  geom_line()+
  xlab("timeline")+
  ylab("air temperate (°C)")+
  theme_bw(base_size = 14)


<<<<<<< HEAD
require(data.table)
=======
library(data.table)
>>>>>>> db7272278e37753f0a17dd9f61b5869db4530899
test <- c(as.POSIXct("1985-05-01 06:00:00", tz = "UTC"), as.POSIXct("1985-05-01 12:00:00", tz = "UTC"))
year(test)

fjaell_a_hourly <- data.frame(fjaell_a_hourly, 
                              Year = year(fjaell_a_hourly$date_time), 
                              Month = month(fjaell_a_hourly$date_time), 
                              Day = mday(fjaell_a_hourly$date_time), 
                              Hour = hour(fjaell_a_hourly$date_time))

fjaell_a_hourly_to_day <- make_all_day_table(fjaell_a_hourly, timestep = "hour")
<<<<<<< HEAD
#stack_hourly_temps(fjaell_a_hourly_to_day, 61.8)
=======
stack_hourly_temps(fjaell_a_hourly_to_day, 61.8)
>>>>>>> db7272278e37753f0a17dd9f61b5869db4530899
