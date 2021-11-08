require(chillR)
?Chilling_Hours
Chilling_Hours
#Chilling_Hours(Winters_hours_gaps$Temp, summ = FALSE)
plot(Utah_Model(Winters_hours_gaps$Temp, summ = TRUE))
own_step_model <- function (HourTemp, summ = TRUE){
  return(step_model(HourTemp, 
                    df = data.frame(lower = c(-1000, 1.5, 2.5, 9, 12.5, 16, 18), 
                                    upper = c(1.5, 2.5, 9, 12.5, 16, 18, 1000), 
                                    weight = c(0, 0.5, 1, 0.5, 0, -0.5, -1)),
                    summ = summ))
}
own_step_model(Winters_hours_gaps$Temp, summ = TRUE)[1:100]
#Dynamic_Model(Winters_hours_gaps$Temp, summ = TRUE)
#chilling(make_JDay(Winters_hours_gaps))
#chilling(make_JDay(Winters_hours_gaps), Start_JDay = 100, End_JDay = 200)
tempResponse(make_JDay(Winters_hours_gaps),
             Start_JDay = 100,
             End_JDay = 200,
             models = list(Chilling_Hours = Chilling_Hours,
                           Utah_Chill_Units = Utah_Model,
                           Chill_Portions = Dynamic_Model,
                           GDH = GDH,
                           own_step_model = own_step_model))
