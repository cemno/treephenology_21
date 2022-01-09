library(chillR)
library(tidyverse)
library(ggplot2)

fulufjaellet_weather <- read.csv("data/fulufjaellet_weather.csv")
fulufjaellet_weather <- fulufjaellet_weather[,c("Year","Month","Day","Tmin","Tmax")]

baseline_scenario <- temperature_scenario_from_records(weather = fulufjaellet_weather,
                                                       year = 2009.5)
all_past_scenarios <- temperature_scenario_from_records(weather = fulufjaellet_weather,
                                                        year = c(2000, 2005, 2010, 2015, 2020))
adjusted_scenarios <- temperature_scenario_baseline_adjustment(baseline_temperature_scenario = baseline_scenario,
                                                               temperature_scenario = all_past_scenarios)

all_scenario_temps <- temperature_generation(fulufjaellet_weather,
                                      years = c(1999, 2020),
                                      sim_years = c(2000,2099),
                                      temperature_scenario = adjusted_scenarios)

chill_hist_scenario_list <- tempResponse_daily_list(all_scenario_temps,
                                                  latitude=61.6,
                                                  Start_JDay = 196,
                                                  End_JDay = 195)
chill_hist_scenario_list

scenarios <- names(chill_hist_scenario_list)[1:5]

all_scenarios<-chill_hist_scenario_list[[scenarios[1]]]
all_scenarios[,"scenario"]<-as.numeric(scenarios[1])


for (sc in scenarios[2:5]){
  all_scenarios<-rbind(all_scenarios,
                       cbind(chill_hist_scenario_list[[sc]],scenario=as.numeric(sc)))
}

all_scenarios<-all_scenarios[which(all_scenarios$Perc_complete==100),]

# Actual chill metrics in this period for comparison
actual_chill<-tempResponse_daily_list(fulufjaellet_weather, latitude=61.6,
                                      Start_JDay = 196,
                                      End_JDay = 195)[[1]]

actual_chill<-actual_chill[which(actual_chill$Perc_complete==100),]

# There doesn't seem to be a clear trend in chill portion changes over the years
ggplot(data = all_scenarios, aes(scenario, Chill_Portions, fill = factor(scenario)))+
  geom_violin()+ 
  ylab("Chill accumulation (Chill Portions)") +
  xlab("Scenario year") +
  theme_bw(base_size=15)+ 
  geom_point(data=actual_chill,
             aes(End_year,Chill_Portions,fill="blue"),
             col="blue",show.legend = FALSE)+
  scale_fill_discrete(name="Scenario", breaks = unique(all_scenarios$scenario)) 
  

