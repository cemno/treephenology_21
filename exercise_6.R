library(chillR)
library(microbenchmark)

Winters_hours_gaps <- Winters_hours_gaps

# it seems like ifelse() is actually quite slow. The dplyr version if_else() is faster, but not as versatile
warm_hours_ifelse <- function(num_vec){
  warm_hours_vec <- ifelse(num_vec > 25, TRUE, FALSE)
  return(warm_hours_vec)
  }

# Efficient way to calculate a boolean vector (neglects NAs, but still more efficient with NA test.)
warm_hours_rep <- function(num_vec){
  warm_hours_vec <- rep(FALSE, length(num_vec))
  warm_hours_vec[num_vec > 25] <- TRUE
  #warm_hours_vec[is.na(num_vec)] <- NA
  return(warm_hours_vec)
}

microbenchmark(
  warm_hours_rep = {warm_hours_rep(Winters_hours_gaps$Temp)},
  warm_hours_ifelse = {warm_hours_ifelse(Winters_hours_gaps$Temp)},
  times = 100000
  )

# Input single number which then is transformed to fit current table schema
# Advantages: no hassle with POSIXct
filter_temp_on_num_dates <- function(df, start_date, end_date){
  start_year<-trunc(start_date/1000000)
  start_month<-trunc((start_date-start_year*1000000)/10000)
  start_day<-trunc((start_date-start_year*1000000-start_month*10000) / 100)
  start_hour<-start_date-start_year*1000000-start_month*10000-start_day*100
  end_year<-trunc(end_date/1000000)
  end_month<-trunc((end_date-end_year*1000000)/10000)
  end_day<-trunc((end_date-end_year*1000000-end_month*10000) / 100)
  end_hour<-end_date-end_year*1000000-end_month*10000-end_day*100
  start_row <- which((df$Year == start_year) & 
                       (df$Month == start_month) & 
                       (df$Day == start_day) & 
                       (df$Hour == start_hour))
  end_row <- which((df$Year == end_year) & 
                     (df$Month == end_month) & 
                     (df$Day == end_day) & 
                     (df$Hour == end_hour))
  return(sum(warm_hours_rep(df[start_row:end_row,]$Temp)))
}

# Input POSIXct dates, which are then transform to fit current table schema
# Comment: No real performance advantage but enables standardized POSIXct input
# which may help for the transition
filter_temp_on_dates <- function(df, start_date, end_date){
  start_date <- as.POSIXlt(start_date)
  end_date <- as.POSIXlt(end_date)
  start_year = 1900 + start_date$year
  start_month = start_date$mon + 1 # Valuerange is 0-12
  start_day = start_date$mday
  start_hour = start_date$hour
  end_year = 1900 + end_date$year
  end_month = end_date$mon +1
  end_day = end_date$mday
  end_hour = end_date$hour
  start_row <- which((df$Year == start_year) & 
                       (df$Month == start_month) & 
                       (df$Day == start_day) & 
                       (df$Hour == start_hour))
  end_row <- which((df$Year == end_year) & 
                     (df$Month == end_month) & 
                     (df$Day == end_day) & 
                     (df$Hour == end_hour))
  return(sum(warm_hours_rep(df[start_row:end_row,]$Temp)))
}

# Filter with POSIXct dates on a POSIXct date column
# Comment: Easy to understand, fast to program and 
# if exact start or end date/hour isn't available it still works
filter_temp_on_dates_with_dates <- function(df, start_date, end_date){
  return(sum(warm_hours_rep(df[df$date >= start_date & df$date <= end_date])))
}

# Preparation
start_date = as.POSIXct("2008030400", tz = "UTC", tryFormats = c("%Y%m%d", "%Y%m%d%H"))
end_date = as.POSIXct("2008063023", tz = "UTC", tryFormats = c("%Y%m%d", "%Y%m%d%H"))
start_date_num = 2008030400
end_date_num = 2008053023
Winters_hours_gaps_real_dates <- within(Winters_hours_gaps, 
                                        Date <- as.POSIXct(
                                          sprintf("%d-%02d-%02d %02d:00", Year, Month, Day, Hour))
                                        )[,c("Date","Temp_gaps", "Temp")]

microbenchmark(
  using_numbers = {
    filter_temp_on_num_dates(Winters_hours_gaps, start_date_num, end_date_num)
  },
  using_dates = {
    filter_temp_on_dates(Winters_hours_gaps, start_date, end_date)
  },
  using_dates_on_dates = {
    filter_temp_on_dates_with_dates(Winters_hours_gaps_real_dates, start_date, end_date)
  },
  times = 100000
)
