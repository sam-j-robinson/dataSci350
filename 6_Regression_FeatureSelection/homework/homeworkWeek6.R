#Get the log file name
get_log_filename = function(){
  #Prefer to use epoch time.
  epoch_time = toString(as.integer(Sys.time()))
  log_file_name = paste("HW6_", epoch_time, ".log", sep="")
  return(log_file_name)
}

clean_data <- function(cur_data, unused_headers){
  #Remove data that is missing from our data
  cur_data = cur_data[colSums(is.na(cur_data)) < 100]
  
  #Removed headers that aren't related to our data
  cur_data <- cur_data[, !names(cur_data) %in% unused_headers]
  cur_data <- na.omit(cur_data) 
  
  #Returned cleaned data
  return(cur_data)
}

test_data_for_model <- function(cur_data, compare_value){
  #Check that we have a data table and our comparison value in the data table
  stopifnot(length(cur_data) > 0)
  stopifnot(length(names(cur_data)) > 0)
  stopifnot(length(cur_data[,compare_value]) > 0)
}

#Functionized getting the pc model for cleaner main thread code
get_pc_values <- function(cur_data, compare_value){
  cur_data_matrix <- model.matrix(cur_data[,compare_value] ~., data = cur_data, drop=FALSE)
  model_pc <- prcomp(cur_data_matrix)
  return(model_pc)
}

#Let the user select what they consider important for our model.
select_pc_x_values <- function(cur_pc, check_field, check_value){
  #Ensure a valid value to check is passed into our function
  stopifnot(check_field %in% c('Standard deviation', 'Proportion of Variance', 'Cumulative Proportion'))
  stopifnot(check_value < 1)
  
  #Loop through the importance values until we hit our target
  importance_values <- summary(cur_pc)$importance
  n_importance_rows <- 1
  while(importance_values[check_field,n_importance_rows] < check_value){
    n_importance_rows <- n_importance_rows +1
  }
  
  #Return the values from the pc model
  return(cur_pc$x[,0:n_importance_rows])
}

if(interactive()){
  require(logging)
  require(ggplot2)
  #Generate log file
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')

  loginfo("Title: Homework Week 6, Crime Data SVD analysis")
  loginfo("By: Sam Robinson")
  
  compare_value <- 'ViolentCrimesPerPop'
  
  loginfo("loading data")
  setwd('/Users/figaro/Desktop/schoolwork/dataSci350/6_Regression_FeatureSelection/homework')
  crime_data = read.table('communities.data', sep=",", header=FALSE, na.strings = c("NA","?"))
  crime_headers = read.table('crime_headers.txt')
  headers <- crime_headers$V1
  names(crime_data) = crime_headers$V1
  
  loginfo("Cleaning data and performing unit test on final data")
  crime_data <- clean_data(crime_data, c('state', 'communityname'))
  test_data_for_model(crime_data, compare_value)
  loginfo("Unit tests complete")
  
  loginfo("Getting initial AIC Value")
  total_crime_lm <- lm(crime_data[,compare_value] ~ ., data = crime_data)
  all_crime_aic <- AIC(total_crime_lm)
  summary_total_crime_lm <- summary(total_crime_lm)
  loginfo(paste("Initial AIC Value:", all_crime_aic))

  loginfo("Getting PC values and making model from PC values")
  crime_pc <- get_pc_values(crime_data, compare_value)
  crime_x_values <- select_pc_x_values(crime_pc, 'Cumulative Proportion', 0.9)
  crime_svd_lm_model <- lm(crime_data[,compare_value] ~ crime_x_values)
  summary_crime_svd_lm <- summary(crime_svd_lm_model)
  svd_aic <- AIC(crime_svd_lm_model)
  loginfo(paste("SVD AIC Values:", svd_aic))
  diff_aic <- svd_aic - all_crime_aic
  loginfo(paste("Our SVD AIC value was", diff_aic, "points better in our AIC test. This lets us know that when you account for efficiency our SVD model performs better on this test."))
  loginfo("")
  loginfo(paste("All Column R-Squared:", summary_total_crime_lm$r.squared))
  loginfo(paste("PAC R-Squared Value:", summary_crime_svd_lm$r.squared))
  loginfo(paste("The R-Squared value of our LM that uses all columns has more predictive qualities than our SVD LM but it is much less efficient to run based on the value of our AIC output."))
}