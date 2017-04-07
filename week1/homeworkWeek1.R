library(ggplot2)

library(readr)
library(data.table)
#Questions to answer
#1. If you don't want to play a game alone what game should you play, what time, what day of the week?
#2. If you want to play a game alone what game should you play, what time, what day of the week?
#3. What game produces the most work hours for table runners at empty tables?

jitteredHeadCounts <- read_csv("~/Desktop/schoolwork/dataSci350/week1/JitteredHeadCount.csv")
jitteredHeadCounts <- as.data.table(jitteredHeadCounts)
jitteredHeadCounts$DateFormat = as.Date(jitteredHeadCounts$DateFormat, format="%m/%d/%Y")

gameCodes <- unique(jitteredHeadCounts$GameCode)
days <- unique(jitteredHeadCounts$DateFormat)
hours <- unique(jitteredHeadCounts$Hour)

#dateValues <- expand.grid(days, hours)
dateHourHeadCount <- data.table(jitteredHeadCounts, key = c("DateFormat", "Hour"))

dayHourHeadCount = jitteredHeadCounts[, list(tablesOcc_average=mean(TablesOcc),
                                     tablesOpen_avg=mean(TablesOpen),
                                     tablesOpen_sum=sum(TablesOpen),
                                     headCount_sum=sum(HeadCount),
                                     headCount_avg=mean(HeadCount),
                                     headCount_perTable=(sum(HeadCount)/sum(TablesOcc))),
                              by=list(DayOfWeek, Hour, GameCode)]
dayHourHeadCount$dayHour <- (dayHourHeadCount$DayOfWeek-1)*24 + dayHourHeadCount$Hour

gameCodesByHeadCount <- jitteredHeadCounts[, list(totalHeadCount = sum(TablesOcc)),
                                           by=list(GameCode)]



top5GameHeadCount <- gameCodesByHeadCount[order(-totalHeadCount),.SD[1:5]]
top5GameHeadCount <- top5GameHeadCount[,.SD[1:5]]
top5GameCodes <- c(top5GameHeadCount[,1])
top5Data <- subset(dayHourHeadCount, GameCode %in% top5GameCodes$GameCode)
#Gives me a clear visualization of how the top 5 games stand against each other
#I liked creating both visualizations but really the second one is the most useful.
#ggplot(top5Data, aes(x=Hour, y=GameCode)) + geom_tile(aes(fill=tablesOpen_avg))
ggplot(top5Data, aes(x=dayHour, y=headCount_avg, colour = GameCode)) + geom_line()

top5Data[, lapply(.SD, mean, na.rm=TRUE), by=GameCode]

bottomHeadCount <- gameCodesByHeadCount[order(totalHeadCount),.SD[1:10]]
bottomHeadCount <- bottomHeadCount[,.SD[1:10]]
bottomGameCodes <- c(bottomHeadCount[,1])
bottomData <- subset(dayHourHeadCount, GameCode %in% bottomGameCodes$GameCode & tablesOpen_avg > 1)

bottomData[order(-tablesOpen_avg),]

#This visualization made it very easy to see when there were no tables open or not even a single table available.
ggplot(bottomData, aes(x=dayHour, y=GameCode)) + geom_tile(aes(fill=tablesOpen_avg))

# We would want to select a time where there was at least 1 open table, having no open tables means our players wouldn't get to play their game of choice.
mostOpenTableTime <- jitteredHeadCounts[, list(openTables=sum(TablesOpen), 
                                               occupiedTables=sum(TablesOcc)),
                                               by=list(GameCode)]

#putting the data together so that I can see what games have the most open and unoccupied tables compared to their up time
mostOpenTableTime$openTableTime <- mostOpenTableTime$openTables - mostOpenTableTime$occupiedTables
openData <- mostOpenTableTime[order(-openTableTime)]
openData$ratio <- as.double(openData$openTableTime) / as.double(openData$openTables)
openData <- openData[order(-ratio)]
View(openData)