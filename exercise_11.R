# Weather Generator
library(chillR)
library(tidyverse)
library(ggplot2)

porto_weather <- read.csv("data/porto_weather.csv")
porto_weather <- porto_weather[,c("Year","Month","Day","Tmin","Tmax")]

temperature <- temperature_generation(porto_weather,
                                      years = c(1973, 2019),
                                      sim_years = c(2000, 2099))

temperature_compare <- cbind(porto_weather[
  which(porto_weather$Year %in% 1973:2019),],
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

ggplot(data=temperature_compare, aes(date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")

# Chill distribution
chill_observed <- chilling(stack_hourly_temps(weather = temperature_compare[which(temperature_compare$data_source=="observed"),],
                                              latitude = 41.1),
                           Start_JDay = 305,
                           End_JDay = 59)
chill_simulated <- chilling(stack_hourly_temps(weather = temperature_compare[which(temperature_compare$data_source=="simulated"),],
                                              latitude = 41.1),
                            Start_JDay = 305,
                            End_JDay = 59)

chill_comparison <- cbind(chill_observed, data_source = "observed")
chill_comparison <- rbind(chill_comparison, cbind(chill_simulated, data_source = "simulated"))

chill_comparison_full_season <- chill_comparison[which(chill_comparison$Perc_complete == 100),]

# The accumulated chill portions for the full season 
ggplot(chill_comparison_full_season, aes(x=Chill_portions)) + 
  geom_histogram(binwidth=1,aes(fill = factor(data_source))) +
  theme_bw(base_size = 20) +
  labs(fill = "Data source") +
  xlab("Chill accumulation (Chill Portions)") +
  ylab("Frequency")

# Probability of reaching a specific number of chill portion
chill_simulations<-chill_comparison_full_season[which(chill_comparison_full_season$data_source=="simulated"),]
ggplot(chill_simulations, aes(x=Chill_portions)) +
  stat_ecdf(geom = "step",lwd=1.5,col="blue") +
  ylab("Cumulative probability") +
  xlab("Chill accumulation (in Chill Portions)") +
  theme_bw(base_size = 20)

