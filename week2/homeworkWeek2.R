##--------------------------------------------
##
## Monty Hall R Solution Homework Week 2
##
## Class: PCE Data Science Methods Class
##
## Name: Sam Robinson
##
##--------------------------------------------

library(logging)
library(ggplot2)
library(data.table)
# Get the log file name that has a date-time in the name
get_log_filename = function(){
  #Prefer to use epoch time.
  epoch_time = toString(as.integer(Sys.time()))
  log_file_name = paste("HW2_", epoch_time, ".log", sep="")
  return(log_file_name)
}

# Redefine the sample() function properly (Straight from the help file)
resample <- function(x, ...) x[sample.int(length(x), ...)]

#Function to account for vector list of one
return_from_vector = function(vector, return_values){
  if(length(vector) < 2){
    return(vector)
  }
  else {
    return(sample(vector, return_values))
  }
}

play_game = function(switch_logic, number_of_doors, remaining_doors){
  #Game is not valid with fewer than 2 doors at the end
  stopifnot(remaining_doors >= 2)
  
  #Set winning and selected doors
  all_doors <- c(1:number_of_doors)
  winning_door <- sample(all_doors, 1)
  selected_door <- sample(all_doors, 1)
  loginfo(paste('Winning Door:', winning_door))
  loginfo(paste('Selected Door:', selected_door))
  
  #Have a list of doors that are available to open
  unsealed_doors = all_doors[-c(winning_door, selected_door)]

  #Select doors to open
  opened_doors <- return_from_vector(unsealed_doors, (number_of_doors-remaining_doors))
  loginfo(paste('Opened doors:', opened_doors))
  
  #Select randomly from one of the remaining doors if switching
  if(switch_logic == TRUE) {
    switchable_doors <- c(all_doors[-c(opened_doors, selected_door)])
    selected_door <- return_from_vector(switchable_doors, 1)
    loginfo(paste("Player switched to door:", selected_door))
  }
  
  if(selected_door == winning_door){
    loginfo("The player won!")
    return(TRUE)
  }
  else{
    loginfo("The player lost.")
    return(FALSE)
  }
}

# Unit test
# Test if a simulation returns TRUE or FALSE
test_simulation_return_val = function(){
  #Test for true logic
  loginfo("Begin Testing Return Val")
  one_game_outcome = play_game(switch_logic=TRUE, 3, 2)
  stopifnot(one_game_outcome %in% c(TRUE, FALSE))
  #Test for false logic
  one_game_outcome = play_game(switch_logic=TRUE, 3, 2)
  stopifnot(one_game_outcome %in% c(TRUE, FALSE))
  loginfo("End Testing Return Val")
  loginfo("--------------------")
}

test_valid_sim_parameters = function(number_of_doors, doors_remaining){
  loginfo("Begin Testing Door numbers")
  #Ensure that the player has a choice at the end of the game
  stopifnot(doors_remaining >= 2)
  #Can't have more doors remaining than doors
  stopifnot(doors_remaining < number_of_doors)
  loginfo("End Testing Door Numbers")
  loginfo("--------------------")
}

if(interactive()){
  
  #Generate log file
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')

  # Setup working directory
  setwd("/Users/figaro/Desktop/schoolwork/dataSci350/week2")
  
  # Perform unit test
  test_simulation_return_val()
  
  # Set simulation parameters
  num_sims = 10000 # Number of games to simulate
  number_of_doors = 3
  doors_remaining = 2
  
  loginfo(paste("Number of doors:", number_of_doors))
  loginfo(paste("Doors left after opening:", doors_remaining))
  
  test_valid_sim_parameters(number_of_doors, doors_remaining)
  
  # No switch probability
  stay_results = sapply(1:num_sims, function(x) play_game(FALSE, number_of_doors, doors_remaining))
  # Switch results
  switch_results = sapply(1:num_sims, function(x) play_game(TRUE, number_of_doors, doors_remaining))
  
  # Compute Results
  # First we compute the average
  prob_win_switch = mean(switch_results)
  prob_win_stay = mean(stay_results)

  # Then we compute the variance of the results
  var_switch = var(switch_results)
  var_stay = var(stay_results)
  
  loginfo(paste(""))
  loginfo(paste("Getting Results"))
  loginfo(paste("Number of doors:", number_of_doors))
  loginfo(paste("Doors left after opening:",doors_remaining))
  loginfo(paste(""))
  loginfo(paste("Switching:"))
  loginfo(paste("Probability:", (prob_win_switch)))
  loginfo(paste("Variance:", (var_switch)))
  loginfo(paste(""))
  loginfo(paste("Staying:"))
  loginfo(paste("Probability When staying:", (prob_win_stay)))
  loginfo(paste("Variance:", (var_stay)))
  loginfo(paste(""))
  
  switch_outcomes = switch_results*1
  stay_outcomes = stay_results*1
  switch_running = sapply(1:num_sims, function(x) mean(switch_outcomes[1:x]))
  stay_running = sapply(1:num_sims, function(x) mean(stay_outcomes[1:x]))
  plotDf <- as.data.table(switch_running)
  plotDf$stay_running <- as.data.table(stay_running)
  ggplot(plotDf) + 
    geom_line(aes(y=plotDf$switch_running, x=as.numeric(row.names(plotDf))), color="red") +
    geom_line(aes(y=plotDf$stay_running, x=as.numeric(row.names(plotDf))), color="blue")
}