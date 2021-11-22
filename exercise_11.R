# Weather Generator
library(chillR)
library(ggplot2)

fulufjaellet_weather <- read.csv("data/fulufjaellet_weather.csv")
fulufjaellet_weather <- fulufjaellet_weather[,c("Year","Month","Day","Tmin","Tmax")]

temperature <- temperature_generation(fulufjaellet_weather,
                                      years = c(1999, 2020),
                                      sim_years = c(2000,2099))

temperature_compare <- cbind(fulufjaellet_weather[
  which(fulufjaellet_weather$Year %in% 1999:2020),],
  data_source="observed")

temperature_compare <- rbind(temperature_compare, 
                           cbind(temperature[[1]][,c("Year","Month","Day","Tmin","Tmax")],
                                 data_source="simulated"))

temperature_compare$date <- as.Date(ISOdate(2000, 
                                      temperature_compare$Month,
                                      temperature_compare$Day))

ggplot(data=temperature_compare, aes(date,Tmin)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")

