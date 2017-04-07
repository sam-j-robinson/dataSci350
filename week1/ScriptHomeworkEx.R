##--------------------------------------------
##
## R Review Homework Headstart
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------

##-----Set working directory-----
setwd('E:/Work/Teaching/PCE_Data_Science/1_Intro_Lecture/')

##-----Load Libraries-----
library(readr)
library(dplyr)
library(data.table)

# Load jittered Data
headcount <- read_csv("~/Desktop/schoolwork/dataSci350/week1/JitteredHeadCount.csv")
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")

# Check for duplicates!
anyDuplicated(headcount[c("DateFormat", "Hour","GameCode")])

# Use data.table
headcount = data.table(headcount)

# Create an aggregate by day
daily_headcount = headcount[, list(TablesOcc_avg=mean(TablesOcc),
                                   TablesOcc_max=max(TablesOcc),
                                   HeadCount_avg=mean(HeadCount),
                                   HeadCount_max=max(HeadCount),
                                   HeadCount_total=sum(HeadCount)),
                            by=list(DateFormat, GameCode)]
daily_headcount$DayOfWeek = as.numeric(format(daily_headcount$DateFormat, format='%w'))+1

# Look at game code 'S6' by day-average
plot(daily_headcount$HeadCount_total[daily_headcount$GameCode=='S6'])

# Not that enlightening.  Look at S6 total headcount by weekday type
# I.e., we look at the average total daily headcount for each day-type
day_average = daily_headcount[,list(HeadCount_avg=mean(HeadCount_total)),
                              by=list(DayOfWeek, GameCode)]
# Not necessarily in the right order (days 1 -> 7)
s6_days = day_average$DayOfWeek[day_average$GameCode=="S6"]
s6_hc_avg = day_average$HeadCount_avg[day_average$GameCode=="S6"]
day_ordering = sort(s6_days, index.return=TRUE)

# Plot
plot(s6_days[day_ordering$ix],
     s6_hc_avg[day_ordering$ix],
     main="Day-of-Week Headcount Average",
     xlab="Day-of-Week", ylab="Total Headcount Average",
     type='l')

# Other ideas:
# Which Month/week/season is the lowest/highest for all games?
# Which game is the least in demand (controlling for when it was open)?
#    e.g., consider the average headcount/open table.
# Which games have the biggest differences between high and lows for a day/week/month/...?
# Which game appears to be the most stable?
# Which game has the highest headcount/table?
#    Is this constant over hours/days/weeks/months?
# Is there a way to see how fast or slow moving a game is?
#    e.g. Consider change in headcount by hour to hour.