##--------------------------------------------
##
## Test Farm-Subsidies Data set
##
## Class: PCE Data Science Methods Class
##
## Purpose: Homework Week 3
##
## Name: Sam Robinson
##
## Datasets located:
##
##  http://www.fsa.usda.gov/FSA/webapp?area=newsroom&subject=landing&topic=foi-er-fri-pfi
##
##   Need:
##
##    -2011 Farm Payment File (27MB) txt file
##    -State and County Code List (374KB) xls file (probably convert this to csv)
##
##--------------------------------------------

##----Import Libraries-----
require(RSQLite)
require(logging)
require(plyr)
require(datasets)
require(data.table)
require(ggplot2)
##----Hypotheses to test-----
#
#  Test these two things:
#
#    1.  Does our sample equally represent all 50 states?
#
#    2.  Does our sample equally represent all 50 states, weighted by number of farms/state?
#
#     Note- you can find the farms per state in census data.
#

trim = function (x) gsub("^\\s+|\\s+$", "", x)

##-----Declare Functions Here-----

##----Run Main Function Here----
if(interactive()){
  setwd("/Users/figaro/Desktop/schoolwork/dataSci350/week3")
  
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')
  
  ##-----Read in the data-----
  data = read.csv("CAS.WDC11019.PMT11.FINAL.DT11186.TXT", sep=";",
                  header=FALSE, stringsAsFactors=FALSE)

    ##----Trim Whitespaces-----
  data = as.data.frame(apply(data,2,trim), stringsAsFactors=FALSE)
  
  names(data) = c('state_code', 'county_code', 'cust_num', 'program_code', 'program_year',
                  'commodity_code', 'amount', 'date', 'cat_code', 'farm_num',
                  'calendar_year', 'fiscal_year', 'seq_num')
  
  ##------Read State/County File-----
  county_state_codes = read.csv("foia_state_county_codes-1.csv", stringsAsFactors=FALSE)
  county_state_codes$state_code = county_state_codes$Stcd
  county_state_codes$Stcd = NULL
  county_state_codes$county_code = county_state_codes$Cntycd
  county_state_codes$Cntycd = NULL
  
  ##----Merge files together----
  data = as.data.table(merge(data, county_state_codes, by=c("state_code", "county_code"), all.x=TRUE))
  data$amount <- as.numeric(data$amount)
  
  ##-----Probably do some data exploration----
  stateFactor <- as.data.frame(factor(data$state_code))
  stateFactor <- count(stateFactor[1])
  colnames(stateFactor) <- c("state_code", "frequency")

  stateData <- as.data.frame(state.abb)
  stateData$area <- state.area
  stateData$state_code <- as.numeric(row.names(stateData)) #Documentation tells us that this is alphebatical 1-50
  colnames(stateData) <- c("ST", "area", "state_code")
  
  
  ##----Perform a test for equal repreentation by farms/state-----
  #I decided to use value per farm as the normalized data for subsidies.
  #Without knowing the total number of farms in the country I can only go with what I know here.
  #Going to test for an even distribution of the normalized data.
  
  stateCodes <- unique(county_state_codes$state_code)
  
  data <- na.omit(data)
  by_state_data <- data[, list(amount_sum = sum(amount),
                                 mean_farms = mean(amount),
                                 total_count = .N),
    by=list(ST)]
  
  #This lets us know if the sum of amount of subsidies in a state is dependent on a count.
  count_chisq <- chisq.test(by_state_data$total_count, p=rep(1/50, 50))
  #Looking at the results it's clear that there are is not an even distribution
  
  by_state_data$amount_per_farm <- by_state_data$amount_sum / by_state_data$total_count
  by_state_data$farm_weights <- by_state_data$amount_sum / sum(by_state_data$amount_sum)
  
  weighted_chisq <- chisq.test(by_state_data$amount_sum, by_state_data$farm_weights)

    #Handy visualizations
  ggplot(by_state_data, aes(x=total_count, y=farm_weights)) + geom_point() + geom_smooth(method="lm", se=FALSE)
  #ggplot(by_state_data, aes(x=total_count, y=amount_sum, colour=ST)) + geom_point()
  
  ##----Output Results----
  # Acceptable to print output, log output, save in DB, or write to file. Your choice.
  
}




