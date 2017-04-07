##--------------------------------------------
##
## Chicago Diabetes Homework (Lecture 4)
##
## Class: PCE Data Science Methods Class
##
##--------------------------------------------


setwd('/Users/figaro/Desktop/schoolwork/dataSci350/week4')

data = read.csv('ChicagoDiabetesData.csv', stringsAsFactors = FALSE)

data_means = apply(data[-1],2,mean)

hospitalizations = data_means[grepl('Hospitalizations', names(data))]
admit_rate = data_means[grepl('Crude.Rate.[0-9]+$', names(data), perl = TRUE)]

hospitalizations_diff = diff(hospitalizations)
admit_rate_diff = diff(admit_rate)

plot(hospitalizations_diff, admit_rate_diff)
