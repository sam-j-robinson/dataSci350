##--------------------------------------------
##
## Facebook Edges Homework (Lecture 5)
##
## Class: PCE Data Science Methods Class
##
##  Name: Sam Robinson
##
##--------------------------------------------

## 1: Write Script level R-code that computes the mean degree of the 
##      graph and shows a plot of the histogram of the degrees. 
## 2: Write R code that performs a K-S test for the two hypotheses:
##      Test if the distribution of degrees is Poisson. (reuse K-S code) Just report the k-s distance from our code.
##      Test if the distribution of degrees is a Power Law. (Use igraph library)

require(ggplot2)
require(igraph)
require(data.table)

setwd("/Users/figaro/Desktop/schoolwork/dataSci350/5_HypothesisTesting_ConfidenceIntervals")
facebook_edge <- read.csv('facebook_edge_list.csv', stringsAsFactors = FALSE)
facebook_edge <- as.data.table(facebook_edge)
facebook_degrees <- degree(facebook_network)
hist <- hist(facebook_degrees, breaks=10) 
degree_mean <- mean(facebook_degrees)

# Create k-s statistic function
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}

rpoiss <- rpois(1000, degree_mean)

samples <- 1000
sample_null_ks = sapply(1:samples, function(x) {
  r_poiss1 <- rpois(1000, degree_mean)
  r_poiss2 <- rpois(1000, degree_mean)
  ks_stat(0, max(facebook_degrees), r_poiss1, r_poiss2)
})

test_ks_sample <- hist(sample_null_ks)
View(test_ks_sample)

power_fit <- power.law.fit(degree(facebook_network))
View(power_fit)