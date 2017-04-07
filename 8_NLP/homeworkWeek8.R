#Get the log file name
get_log_filename = function(){
  #Prefer to use epoch time.
  epoch_time = toString(as.integer(Sys.time()))
  log_file_name = paste("HW6_", epoch_time, ".log", sep="")
  return(log_file_name)
}

#Create regressive feature from N units ago. Finding specific value
prev_value_n <- function(atomic_table, lag_value){
  regress_data <- sapply(1:length(atomic_table), function(x){
    if(x <= lag_value){
      return(atomic_table[x])
    }
    else {
      return(atomic_table[x-lag_value])
    }
  })
  return(regress_data)
}

prev_mean_n <- function(atomic_table, lag_value){
  regress_data <- sapply(1:length(atomic_table), function(x){
    if(x <= lag_value+1){
      return(atomic_table[x])
    }
    else {
      return(mean(atomic_table[x-1-lag_value:x-1]))
    }
  })
  return(regress_data)
}

prev_delta_n <- function(atomic_table, lag_value){
  regress_data <- sapply(1:length(atomic_table), function(x){
    if(x <= lag_value+1){
      return(atomic_table[x])
    }
    else {
      return(atomic_table[x-lag_value-1] - atomic_table[x-1])
    }
  })
  return(regress_data)
}
#Create a table of regressive features for our predictive model
create_arma_table <- function(start_table, n_back_values, compare_value){
  print("Creating ARMA table")
  nameList = names(start_table)
  for(name in nameList){
    for(i in hours_vector){
      print(paste0(name,i))
      start_table[,paste0(name,"_value_",as.character(i))] <- prev_value_n(start_table[,name], i)
      start_table[,paste0(name,"_mean_",as.character(i))] <- prev_mean_n(start_table[,name], i)
      start_table[,paste0(name,"_delta_",as.character(i))] <- prev_delta_n(start_table[,name], i)
    }
  }
  
  non_regressive_columns <- sapply(names(start_table), function(x){
    if(startsWith(x, "buy_")){
      return(x)
    }
  })
  return(start_table)
}

getModelFromPopulation = function(cur_dataFrame, chromosomes, compare_value){
  print("Getting linear model from known best Genetic Algorithm Chromosomes.")
  cur_headers = names(cur_dataFrame)
  stopifnot(length(cur_headers)==length(chromosomes))
  stopifnot(sum(chromosomes)>1)
  cur_names <- sapply(seq(1:length(chromosomes)), function(i){ 
    if(chromosomes[i]==1){
      print(cur_headers[i])
      return(cur_headers[i])
    }else{
      return("UNUSED") #Custom null value fro the sake of this function
    }})
  temp_df <- as.data.frame(cur_dataFrame[,!cur_names %in% c(compare_value, "UNUSED")])
  return(lm(cur_dataFrame[,compare_value]~., data=temp_df))
}

hour_minute_output <- function(seconds){
  cur_seconds = seconds
  hour = 3600
  minute = 60
  duration_hours = 0
  duration_minutes = 0
  
  while(cur_seconds > 60){
    if(cur_seconds >= 3600){
      duration_hours = duration_hours + 1
      cur_seconds = cur_seconds - hour
    }else if(cur_seconds >= 60){
      duration_minutes = duration_minutes + 1
      cur_seconds = cur_seconds - minute
    } else {
      break
    }
  }
  print(paste0("Run Time - ", duration_hours, ":", duration_minutes, ":", cur_seconds))
}

getModelFromPopulation = function(cur_dataFrame, chromosomes, compare_value){
  cur_headers = names(cur_dataFrame)
  stopifnot(length(cur_headers)==length(chromosomes))
  stopifnot(sum(chromosomes)>1)
  cur_names <- sapply(seq(1:length(chromosomes)), function(i){ 
    if(chromosomes[i]==1){
      print(cur_headers[i])
      return(cur_headers[i])
    }else{
      return("UNUSED") #Custom null value fro the sake of this function
    }})
  temp_df <- as.data.frame(cur_dataFrame[,!cur_names %in% c("UNUSED")])
  return(lm(cur_dataFrame$spent ~., data=temp_df))
}

clean_data_table <- function(cur_table){
  print("Cleaning data table")
  cur_table <- cur_table[,sapply(cur_table, function(col) length(unique(col)) > 1)]  
  names(cur_table) <- sapply(names(cur_table), function(name){gsub('-','_', name)})
  #names(cur_table) <- sapply(names(cur_table), function(name){gsub('_','', name)})
  cur_table$saleHour <- factor(cur_table$saleHour)
  cur_table$eventHour <- factor(cur_table$eventHour)  
  #cur_table <- cur_table[, -which(names(cur_table) %in% c("spenders", "saleHour", "eventHour"))]
  cur_table <- cur_table[, -which(names(cur_table) %in% c("spenders", "rpp", "rps", "conversion"))]
  return(cur_table)
}

if(interactive()){
  require(readr)
  require(ggplot2)
  require(stringr)
  require(genalg)
  require(reshape)
  require(GA)
  require(logging)
  
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')
  
  start_time <- proc.time() #Track how long the script takes to run
  print(start_time)
  #all_items_ez <- read_csv("~/ember-app-logs/stainz/cleanedData/sales_empirez_2015-12-1-16_2017-2-25-16.csv")
  all_items_ez <- read_csv("~/ember-app-logs/stainz/cleanedData/ez_items_purchased_2015-12-1-16_2017-2-25-16.csv")
  all_items_ez <- clean_data_table(all_items_ez)
  all_items_new_values <- read_csv("~/ember-app-logs/stainz/cleanedData/ez_items_purchased_2017-2-25-14_2017-3-7-16.csv")
  all_items_new_values <- clean_data_table(all_items_new_values)
  compare_value <- "spent"
  #hours_vector <- c(24,48,72,168)
  hours_vector <- c(24,168)
  
  #hours_vector <- c(1,24,168)
  new_values <- create_arma_table(all_items_new_values, hours_vector, compare_value)
  predictor_table <- create_arma_table(all_items_ez, hours_vector, compare_value)
  predictor_table <-  predictor_table[, sapply(predictor_table, function(col) length(unique(col))) > 1] 
  header_names <- names(predictor_table)
  #stopifnot(iter > 1000)
  print("Time Before starting Genetic Algorithm")
  print(proc.time()-start_time)
  
  predictor_table <- as.data.frame(predictor_table[, sapply(predictor_table, function(col) length(unique(col))) > 1])
  print("")
  print("finished running GA model")
  finish_ga_time <- proc.time()-start_time
  hour_minute_output(finish_ga_time[['elapsed']])
  #stopifnot(1==2) 
  print("my lm")
  #my_lm <- getModelFromPopulation(predictor_table, GAmodel$population[1,], compare_value)
  #plot(GAmodel)
  #print("model lm")
  #View(predictor_table)
  
  #model <- lm(spent ~ . , data = predictor_table)
  model_matrix <- model.matrix(spent~., data = predictor_table)
  
  model_pc <- prcomp(model_matrix)
  plot(model_pc$sdev[0:157])
  #print("pc model")
  #spent_model_pc157_72hour_min <- lm(predictor_table$spent ~ model_pc$x[,0:157])
  
  best_pcValues <- 0
  adjustedRSquaredValues <- c(0)
  best_model <- lm(predictor_table$spent ~ model_pc$x[,0:1])
  for(i in seq(1:length(model_pc$sdev))){
    cur_model <- lm(spent ~ model_pc$x[,0:i])
    cur_summary <- summary(cur_model)
    print(cur_summary$adj.r.squared)
    adjustedRSquaredValues <- c(adjustedRSquaredValues, cur_summary$adj.r.squared)
    if(cur_summary$adj.r.squared >= .999999){
      bes_pcValues = i-1
      best_model <- cur_model
      break
    }
  }
  
  rows <- nrow(new_values)
  new_names <- names(new_values)
  for(name in names(predictor_table)){
    if(!(name %in% new_names)){
      new_values[,name] <- 0
    }
  }
  
  #new_test_fitted_values <- predict.lm(best_model, new_values)
  plot(adjustedRSquaredValues)
  print("finished running script")
  endTime <- proc.time()-pt
  print(endTime)
}
