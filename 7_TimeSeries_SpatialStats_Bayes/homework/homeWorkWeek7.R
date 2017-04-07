# Homework 7
# Hint

setwd("/Users/figaro/Desktop/personal_git/schoolwork/dataSci350/7_TimeSeries_SpatialStats_Bayes/homework")

##-----Load Libraries-----
library(dplyr)
library(data.table)
library(reshape)
library(timeDate)
library(ggplot2)

usHolidayList = c("USNewYearsDay", "USInaugurationDay", "USMLKingsBirthday", "USLincolnsBirthday", "USWashingtonsBirthday", 
                  "USMemorialDay", "USIndependenceDay", "USLaborDay", "USColumbusDay", 
                  "USElectionDay", "USVeteransDay", "USThanksgivingDay", "USChristmasDay", 
                  "USCPulaskisBirthday", "USGoodFriday")

##-----Load Data-----
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
weather = read.csv('las_vegas_hourly_weather.csv', stringsAsFactors = FALSE)


##-----Format Data----
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")
names(weather) = c('time','temp','dew_pt','humidity','pressure',
                   'visibility','wind_dir','wind_speed','gust_speed',
                   'precipitation','events','conditions',
                   'wind_dir_deg','date')

weather$datetime = paste(weather$date,weather$time)
weather$datetime = strptime(weather$datetime, format="%Y-%m-%d %I:%M %p")
weather$Hour = as.numeric(format(round(weather$datetime, units="hours"), format="%H"))

##----Drop Duplicates----
weather = weather[!duplicated(weather[c("date", 'Hour')]),]


##----Merge Data-----
weather$DateFormat = weather$date
weather$date = NULL
weather$DateFormat = as.Date(weather$DateFormat, format="%Y-%m-%d")

headcount = merge(headcount, weather, all.x=TRUE, by=c("DateFormat","Hour"))

##----Imputation for NAs in weather-----
numeric_cols = c(11:15, 17:19, 22)
# Linear Interpolation:
headcount[,numeric_cols] = apply(headcount[,numeric_cols], 2, function(x) approx(x, xout=1:length(x), rule=2)$y)

##---Drop character columns----
headcount$wind_dir = NULL
headcount$time = NULL
headcount$datetime = NULL

##-----Deal with events/conditions----
headcount$events[headcount$events == ""] = "None"
headcount$events[is.na(headcount$events)] = "None"
headcount$conditions[is.na(headcount$conditions)] = "None"

##----Format Data for Time Series Exploration-----
##Only interested in top 8 game codes
headcount <- as.data.table(headcount)

gameCodesByHeadCount <- headcount[, list(totalHeadCount = sum(HeadCount)),
                                  by=list(GameCode)]

top8gameHeadCount <- gameCodesByHeadCount[order(-totalHeadCount),.SD[1:8]]
top8gameHeadCount <- top8gameHeadCount[,.SD[1:8]]
top8Codes <- c(top8gameHeadCount[,1])
headcount <- subset(headcount, GameCode %in% top8Codes$GameCode)

isPresent <- function(x){
  if(x>0){
    return(1)
  }
}

dailyData <- headcount[, list(
  heads_mean=mean(HeadCount),
  tables_mean=mean(TablesOcc),
  tablesOpen_mean=mean(TablesOpen),
  tablesClosed_mean=mean(TablesClosed),
  date_format=max(DateFormat),
  mean_temp = mean(temp),
  max_temp = max(temp),
  daily_precipitation = sum(precipitation),
  mean_press = mean(pressure),
  max_gust = max(gust_speed),
  mean_vis = mean(visibility),
  max_humidity = max(humidity),
  mean_humidity = mean(humidity)
  ),
  by=list(DayNumber)]


#wide_condition <- as.data.table(cast(headcount, DayNumber ~ conditions, 
#                      function(x) isPresent(x), fill=0))
#dailyData <- merge(dailyData, wide_condition, all.x = TRUE, by = "DayNumber")

headcount$DayOfWeek <- factor(headcount$DayOfWeek)
days_wide <- cast(headcount, DayNumber ~ DayOfWeek, function(x) isPresent(x),fill=0)
dailyData <- merge(dailyData, days_wide, all.x = TRUE, by = "DayNumber")

wide_event <- cast(headcount, DayNumber ~ events, function(x) isPresent(x), fill=0)
wide_event <- as.data.table(wide_event[!names(wide_event) %in% c("None")])
dailyData <- merge(dailyData, wide_event, all.x = TRUE, by = "DayNumber")

prev_n <- function(atomic_table, day_lag){
  regress_data <- sapply(1:length(atomic_table), function(x){
    if(x <= day_lag){
      return(atomic_table[x])
    }
    else {
      return(atomic_table[x-day_lag])
    }
  })
  return(regress_data)
}

prev_mean_n <- function(atomic_table, day_lag){
  regress_data <- sapply(1:length(atomic_table), function(x){
    if(x <= day_lag){
      return(atomic_table[x])
    }
    else {
      return(mean(atomic_table[x-day_lag:x]))
    }
  })
  return(regress_data)
}

hnyse <-c(holidayNYSE(year=c(2011,2012)))
char_holidays <- sapply(1:length(hnyse), function(x){as.character(hnyse[x])})
dailyData$nyseHoliday <- sapply(1:nrow(dailyData), function(x){
  if(as.character(dailyData[x, date_format]) %in% char_holidays){
    return(1)
  }else{
    return(0)
  }
})

usHolidays <- as.data.frame(sapply(usHolidayList, function(x){
  cur_holiday <- holiday(year=c(2011,2012), Holiday=x)
  return(sapply(1:length(cur_holiday), function(y){as.character(cur_holiday[y])}))
}))
holidayList <- c(t(usHolidays))
holidayList <- sapply(1:length(holidayList), function(x){as.character(holidayList[x])})
definedHolidays <- c("2012-02-14", "2011-12-31", "2012-02-05", "2011-10-31", "2011-03-17")
append(holidayList, definedHolidays)
dailyData$usHolidays <- sapply(1:nrow(dailyData), function(x){
  if(as.character(dailyData[x, date_format]) %in% holidayList){
    return(1)
  }else{
    return(0)
  }
})

#dailyData <- subset(dailyData, select = !(names(dailyData) %in% "date_format"))
dailyData$yest_meanTemp <- prev_n_day_data(dailyData$mean_temp, 1)
dailyData$yest_maxTemp <- prev_n_day_data(dailyData$max_temp, 1)
#dailyData$day2_maxTemp <- prev_n_day_data(dailyData$max_temp, 2)
#dailyData$day2_meanTemp <- prev_n_day_data(dailyData$mean_temp, 2)
#dailyData$day3_maxTemp <- prev_n_day_data(dailyData$max_temp, 3)
#dailyData$day3_meanTemp <- prev_n_day_data(dailyData$mean_temp, 3)
#dailyData$day7_maxTemp <- prev_n_day_data(dailyData$max_temp, 7)
#dailyData$day7_meanTemp <- prev_n_day_data(dailyData$mean_temp, 7)
dailyData$yest_precip <- prev_n_day_data(dailyData$daily_precipitation, 1)
dailyData$yest_Humidity <- prev_n_day_data(dailyData$mean_humidity, 1)
#dailyData$day2_Humidity <- prev_n_day_data(dailyData$mean_humidity, 2)
#dailyData$day3_Humidity <- prev_n_day_data(dailyData$mean_humidity, 3)
dailyData$day7_Humidity <- prev_n_day_data(dailyData$mean_humidity, 7)
dailyData$yest_fog <- prev_n_day_data(dailyData$Fog, 1)
dailyData$yest_vis <- prev_n_day_data(dailyData$mean_vis, 1)
dailyData$day2_vis <- prev_n_day_data(dailyData$mean_vis, 2)
#dailyData$day3_vis <- prev_n_day_data(dailyData$mean_vis, 3)
#dailyData$day7_vis <- prev_n_day_data(dailyData$mean_vis, 7)
dailyData$rain_3day_mean <- prev_mean_n_days(dailyData$daily_precipitation, 3)
dailyData$average_week_heat <- prev_mean_n_days(dailyData$max_temp, 7)
#dailyData$day1_nyse <- prev_mean_n_days(dailyData$nyseHoliday, 1)
#dailyData$day1_usHoliday <- prev_mean_n_days(dailyData$usHolidays, 1)
#dailyData$day1_maxHumidity <- prev_n_day_data(dailyData$max_humidity, 1)
#dailyData$day2_maxHumidity <- prev_n_day_data(dailyData$max_humidity, 2)
#dailyData$day3_maxHumidity <- prev_n_day_data(dailyData$max_humidity, 3)
#dailyData$day7_maxHumidity <- prev_n_day_data(dailyData$max_humidity, 7)
dailyData$day1_headCount <- prev_n_day_data(dailyData$heads_mean,1)
#dailyData$day2_headCount <- prev_n_day_data(dailyData$heads_mean,2)
#dailyData$day3_headCount <- prev_n_day_data(dailyData$heads_mean,3)
dailyData$day7_headCount <- prev_n_day_data(dailyData$heads_mean,7)
dailyData$day1_tablesOcc <- prev_n_day_data(dailyData$tables_mean,1)
#dailyData$day2_tablesOcc <- prev_n_day_data(dailyData$tables_mean,2)
#dailyData$day3_tablesOcc <- prev_n_day_data(dailyData$tables_mean,3)
dailyData$day7_tablesOcc <- prev_n_day_data(dailyData$tables_mean,7)
dailyData$day1_tablesOpen <- prev_n_day_data(dailyData$tablesOpen_mean,1)
#dailyData$day2_tablesOpen <- prev_n_day_data(dailyData$tablesOpen_mean,2)
#dailyData$day3_tablesOpen <- prev_n_day_data(dailyData$tablesOpen_mean,3)
dailyData$day7_tablesOpen <- prev_n_day_data(dailyData$tablesOpen_mean,7)
#dailyData$day2_headCount <- prev_n_day_data(dailyData$heads_mean,2)
#dailyData$day3_headCount <- prev_n_day_data(dailyData$heads_mean,3)

count_model <- lm(heads_mean ~ ., data = dailyData)
sumData <- summary(count_model)
summary(count_model)