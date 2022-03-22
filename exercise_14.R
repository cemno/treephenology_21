library(chillR)

# Loading downloaded data set
porto_weather <- read.csv("data/porto_weather.csv")

# Get Climate secenario data
RCPs <- c("rcp45", "rcp85")
Times <- c(2050, 2085)


for (RCP in RCPs)
  for (Time in Times)
  {
    start_year <- Time - 15
    end_year <- Time + 15
    clim_scen <- getClimateWizardData(
      c(longitude = -8.612112, latitude = 41.149178),
      RCP,
      start_year,
      end_year,
      temperature_generation_scenarios = TRUE,
      baseline = c(1975, 2005),
      metric = "monthly_min_max_temps",
      GCMs = "all"
    )
    save_temperature_scenarios(clim_scen,
                               "data/ClimateWizard",
                               paste0("porto_futures_", Time, "_", RCP))
  }

# Baseline adjustment
scenario_1990 <- temperature_scenario_from_records(porto_weather, 1990)
scenario_1996 <- temperature_scenario_from_records(porto_weather, 1996)
adjustment_scenario <- temperature_scenario_baseline_adjustment(scenario_1996,
                                           scenario_1990)
#adjustment_scenario

RCPs<-c("rcp45","rcp85")
Times<-c(2050,2085)
for(RCP in RCPs)
  for(Time in Times)
  {
    clim_scen<-load_ClimateWizard_scenarios(
      "data/ClimateWizard",
      paste0("porto_futures_",Time,"_",RCP))
    clim_scen_adjusted<-
      temperature_scenario_baseline_adjustment(
        baseline_temperature_scenario=adjustment_scenario,
        temperature_scenario=clim_scen)
    Temps<-temperature_generation(
      weather=porto_weather, 
      years = c(1973, 2019),
      sim_years=c(2001, 2101),
      temperature_scenario = clim_scen_adjusted)
    
    save_temperature_scenarios(
      Temps,
      "data/Weather",
      paste0("porto_",Time,"_",RCP))
  }

# Adding historic scenarios
all_past_scenarios<-temperature_scenario_from_records(
  weather=porto_weather,
  year=c(1980,1990,2000,2010))

adjusted_scenarios<-temperature_scenario_baseline_adjustment(
  baseline=scenario_1996,
  temperature_scenario = all_past_scenarios)

all_past_scenario_temps <- temperature_generation(
  weather = porto_weather,
  years = c(1973, 2019),
  sim_years = c(2001, 2101),
  temperature_scenario = adjusted_scenarios
)

save_temperature_scenarios(all_past_scenario_temps,
                           "data/Weather",
                           "porto_historic")

# Add our own and some existing models
frost_model <- function(x)
  step_model(x,
             data.frame(
               lower = c(-1000, 0),
               upper = c(0, 1000),
               weight = c(1, 0)
             ))

models <- list(Chill_CP = Dynamic_Model,
               Heat_GDH = GDH,
               Frost_H = frost_model)

# Calculate chill for observed and historic scenarios
Temps <- load_temperature_scenarios("data/Weather", "porto_historic")
chill_past_scenarios <- tempResponse_daily_list(
  Temps,
  latitude = 41.149178,
  Start_JDay = 305,
  End_JDay = 59,
  models = models,
  misstolerance = 10
)
chill_observed <- tempResponse_daily_list(
  porto_weather,
  latitude = 41.149178,
  Start_JDay = 305,
  End_JDay = 59,
  models = models,
  misstolerance = 10
)

save_temperature_scenarios(chill_past_scenarios,
                           "data/chill",
                           "porto_historic")
save_temperature_scenarios(chill_observed,
                           "data/chill",
                           "porto_observed")

# Plot chill scenarios
chill_past_scenarios<-load_temperature_scenarios(
  "data/chill",
  "porto_historic")
chill_observed<-load_temperature_scenarios(
  "data/chill",
  "porto_observed")

chills <- make_climate_scenario(
  chill_past_scenarios,
  caption = "Historic",
  historic_data = chill_observed,
  time_series = TRUE
)

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)")

# Apply same procedure on future data
for(RCP in RCPs)
  for(Time in Times)
  {
    Temps<-load_temperature_scenarios(
      "data/Weather",
      paste0("porto_",Time,"_",RCP))
    chill<-tempResponse_daily_list(
      Temps,
      latitude=41.149178,
      Start_JDay = 305,
      End_JDay = 59,
      models=models,
      misstolerance = 10)
    save_temperature_scenarios(
      chill,
      "data/chill",
      paste0("porto_",Time,"_",RCP))
  }

# Name each scenario

for(RCP in RCPs)
  for(Time in Times)
  {
    chill<-load_temperature_scenarios(
      "data/chill",
      paste0("porto_",Time,"_",RCP))
    if(RCP=="rcp45") RCPcaption <- "RCP4.5"
    if(RCP=="rcp85") RCPcaption <- "RCP8.5"
    if(Time=="2050") Time_caption <- "2050"
    if(Time=="2085") Time_caption <- "2085"
    chills <-make_climate_scenario(
      chill,
      caption =c(RCPcaption, Time_caption),
      add_to = chills)
  }

# Plotting each chill model
# Chilling Portions

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Chill_CP",
  metric_label="Chill (Chill Portions)",
  texcex=1.5)

# Growing Degree Dates

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Heat_GDH",
  metric_label="Heat (Growing Degree Hours)",
  texcex=1.5)

# Frost hours

plot_climate_scenarios(
  climate_scenario_list=chills,
  metric="Frost_H",
  metric_label="Frost hours",
  texcex=1.5)

