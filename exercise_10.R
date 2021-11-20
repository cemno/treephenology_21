# Fullufjället Nationalpark Rösjöstugorna:61.623929, 12.677148; crs=wgs84

library(chillR)
library(kableExtra)

weather <- read.csv("data/gsod_weather_fjaell.csv")

fixed_weather <- fix_weather(weather)

kable(fixed_weather$QC, caption="Quality control summary produced by *fix_weather()*, with only winter days interpolated") %>%
  kable_styling("striped", position = "left", font_size = 10)
