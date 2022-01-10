# Weather Generator
library(chillR)
library(tidyverse)
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

ggplot(data=temperature_compare, aes(date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(data_source)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")

# Chill distribution

# As the location of interest is rather close to the polar circle, a usual growing season (or dormancy period) might not be applicable.
# Thus the time frame relevant for chill portions accumulation should be further investigated.
# Different season lengths were tested, centering around January 13, as the hottest mean temperature was around July 13.

chill_observed <- chilling(stack_hourly_temps(weather = temperature_compare[which(temperature_compare$data_source=="observed"),],
                                              latitude = 61.6),
                           Start_JDay = 196,
                           End_JDay = 195)
chill_simulated <- chilling(stack_hourly_temps(weather = temperature_compare[which(temperature_compare$data_source=="simulated"),],
                                              latitude = 61.6),
                            Start_JDay = 196,
                            End_JDay = 195)

chill_comparison <- cbind(chill_observed, data_source = "observed")
chill_comparison <- rbind(chill_comparison, cbind(chill_simulated, data_source = "simulated"))

chill_comparison_max_season <- chill_comparison[which(chill_comparison$Perc_complete == 100),]

for (i in c(182:50)){
  chill_observed <- chilling(stack_hourly_temps(weather = temperature_compare[which(temperature_compare$data_source=="observed"),],
                                                latitude = 61.6),
                             Start_JDay = 365-i+13,
                             End_JDay = i-1)
  chill_simulated <- chilling(stack_hourly_temps(weather = temperature_compare[which(temperature_compare$data_source=="simulated"),],
                                                 latitude = 61.6),
                              Start_JDay = 365-i+13,
                              End_JDay = i-1)
  chill_comparison <- rbind(chill_comparison, cbind(chill_observed, data_source = "observed"))
  chill_comparison <- rbind(chill_comparison, cbind(chill_simulated, data_source = "simulated"))
}

chill_comparison_full_seasons <- chill_comparison[which(chill_comparison$Perc_complete == 100),]

# As to be seen, the Chill portions are highly dependent on the considered days to make up one season. This suggests even in summer its cold enough to accumulate chill portions.
# The rate for Chill portion accumulation seem to decrease only slightly in summer. Thus the whole season is considered relevant int he following study of this specific location.
ggplot(chill_comparison_full_seasons, aes(x = as.factor(Season_days)))+
  geom_boxplot(aes(y = Chill_portions, colour = factor(data_source)))+
  scale_x_discrete(breaks = seq(80,370,20))+
  labs(colour = "Data_source") +
  xlab("Season days (centering around Jan 13)")+
  ylab("Chill portion")+
  ggtitle("Chill portions dependend on season days")+
  theme_bw(base_size = 20)

# The accumulated chill portions for the full season 
ggplot(chill_comparison_max_season, aes(x=Chill_portions)) + 
  geom_histogram(binwidth=1,aes(fill = factor(data_source))) +
  theme_bw(base_size = 20) +
  labs(fill = "Data source") +
  xlab("Chill accumulation (Chill Portions)") +
  ylab("Frequency")

# Probability of reaching a specific number of chill portion
chill_simulations<-chill_comparison_max_season[which(chill_comparison_max_season$data_source=="simulated"),]

ggplot(chill_simulations, aes(x=Chill_portions)) +
  stat_ecdf(geom = "step",lwd=1.5,col="blue") +
  ylab("Cumulative probability") +
  xlab("Chill accumulation (in Chill Portions)") +
  theme_bw(base_size = 20)

# Nice, seems like there are a lot of chilly days up there! :D