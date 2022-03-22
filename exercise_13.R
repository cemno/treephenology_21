library(chillR)
library(ggplot2)

# Load data
porto_weather <- read.csv("data/porto_weather.csv")
porto_weather <- porto_weather[,c("Year","Month","Day","Tmin","Tmax")]

baseline_scenario <- temperature_scenario_from_records(weather = porto_weather,
                                                       year = 1996)
all_past_scenarios <- temperature_scenario_from_records(weather = porto_weather,
                                                        year = c(1975, 1985, 1995, 2005, 2015))
adjusted_scenarios <- temperature_scenario_baseline_adjustment(baseline_temperature_scenario = baseline_scenario,
                                                               temperature_scenario = all_past_scenarios)

all_scenario_temps <- temperature_generation(porto_weather,
                                      years = c(1973, 2019),
                                      sim_years = c(2000,2099),
                                      temperature_scenario = adjusted_scenarios)

chill_hist_scenario_list <- tempResponse_daily_list(all_scenario_temps,
                                                  latitude=41.1,
                                                  Start_JDay = 305,
                                                  End_JDay = 59)
scenarios <- names(chill_hist_scenario_list)[1:6]
all_scenarios<-chill_hist_scenario_list[[scenarios[1]]]
all_scenarios[,"scenario"]<-as.numeric(scenarios[1])

# Loop through the other scenarios
for (sc in scenarios[2:5]){
  all_scenarios<-rbind(all_scenarios,
                       cbind(chill_hist_scenario_list[[sc]],scenario=as.numeric(sc)))
}

all_scenarios<-all_scenarios[which(all_scenarios$Perc_complete==100),]

# Actual chill metrics in this period for comparison
actual_chill<-tempResponse_daily_list(porto_weather, latitude=41.1,
                                      Start_JDay = 305,
                                      End_JDay = 59)[[1]]

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
