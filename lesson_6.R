library(chillR)
library(tidyverse)

hourtemps<-Winters_hours_gaps[,c("Year","Month","Day","Hour","Temp")]
hourtemps[,"test"] <- NA
hourtemps <- hourtemps[,c("Year","Month","Day","Hour","Temp")]
hourtemps

hourtemps[hourtemps$Temp <= 7.2 & hourtemps$Temp >= 0,]

hourtemps[,"chilling_hour"] <- hourtemps$Temp <= 7.2 & hourtemps$Temp >= 0
sum(hourtemps$chilling_hour) #its an hourly dataset

start_date <- which(hourtemps$Year==2008 & hourtemps$Month==10 & hourtemps$Day==1 & hourtemps$Hour==12)
end_date <- which(hourtemps$Year==2008 & hourtemps$Month==10 & hourtemps$Day==31 & hourtemps$Hour==12)
# Sum up chilling hours
sum(hourtemps$chilling_hour[start_date:end_date])

fun_test <- function(x){
  x+1
}
fun_test(10)

fun_test_2 <- function(x,y){
  z <- x * y
  a <- z ^ 4
  d <- z + a
  return(d)
}
fun_test_2(3,4)

CH <- function(Temp, start_date, end_date, lower = 0, upper = 7.2){
  return (Temp >= lower & Temp <= upper)
}
CH(5)
hourtemps[,"CH"] <-CH(hourtemps$Temp, lower = 0) 
date_num_to_data_vec <-function(date){
  year <- trunc(date/10000)
  month <- trunc((date-year*10000)/100)
  day <- date - (month*100) - year * 10000
  return (list(year = year, month = month, day = day)) #no vector because you can add different data types
}
CH_flex <- function(hourly, start_date = NA, end_date = NA, lower = 0, upper = 7.2){
  start <- date_num_to_data_vec(start_date)
  end <- date_num_to_data_vec(end_date)
  start_row <- which(hourly$Year == start$year&
                       hourly$Month == start$year&
                       hourly$Day == start$year)
  end_row <- which(hourly$Year == end$year&
                       hourly$Month == end$year&
                       hourly$Day == end$year)
  hourly[,"CH"] <- (hourly$Temp >= lower) & (hourly$Temp <= upper)
  CH <- sum(hourly$CH[start_row:end_row])
  somethingmissing
  return(hourly)
}


