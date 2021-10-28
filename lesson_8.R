library(chillR)

### calculate daylength etc.

daylength

Days <- daylength(latitude = 50.4, JDay = 1:365)
Days_df <- data.frame(JDay = 1:365,
                      Sunrise = Days$Sunrise, 
                      Sunset = Days$Sunset, 
                      Daylength = Days$Daylength)

plot(x = Days_df$JDay, y = Days_df$Daylength)

## using ggplot
library(ggplot2)
?ggplot2
## find the cheatsheet

## reorganizeing the data.frame
library(reshape2)
Days_df <- melt(Days_df, id = c("JDay"))
Days_df

g <- ggplot(Days_df, aes(JDay, value))+
  geom_line(lwd = 1.5)+
  facet_grid(cols = vars(variable))+
  ylab("Time of Day; Daylength (Hours)")+
  theme_bw(base_size = 15)
g <- g + xlab("Date (Julian Day)")
g

### KA_weather data
KA_weather

### making hourly temperatures
temps <- stack_hourly_temps(KA_weather, latitude = 50.4)
str(temps)
hourtemps <- temps$hourtemps
hourtemps[,"DateTime"] <- ISOdate(year = hourtemps$Year,
                                  month = hourtemps$Month,
                                  day = hourtemps$Day,
                                  hour = hourtemps$Hour)
ggplot(hourtemps, aes(DateTime, Temp))+
  geom_line()

ggplot(hourtemps[1000:1300,], aes(DateTime, Temp))+
  geom_line()

### no let's see how we can derive empirical temperature curves from hourly data
empi_curve <- Empirical_daily_temperature_curve((Winters_hours_gaps))
empi_curve[1:48,]

g <- ggplot(data = empi_curve[1:96,], aes(Hour, Prediction_coefficient)) +
  geom_line(lwd=1.3, col = "red")

g <- g + facet_grid(rows = vars(Month))
g + theme_bw(base_size = 12)

### Now let's generate temperatures form this information

?Empirical_hourly_temperatures

coeffs <- Empirical_daily_temperature_curve(Winters_hours_gaps)

Winters_daily <- make_all_day_table(Winters_hours_gaps, input_timestep = "hour")

Winters_hours <- Empirical_hourly_temperatures(Winters_daily, coeffs)
