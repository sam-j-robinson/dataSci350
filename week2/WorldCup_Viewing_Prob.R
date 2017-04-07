##  Women's World Cup Probability of Viewing Goal Simulator

usa_scores = c(3,5,14,16,54)
japan_scores = c(27,52)
score_vec = c(usa_scores, japan_scores)
match_length = 90
watch_window = 10

avg_min_between = match_length/(length(usa_scores) + length(japan_scores))

n_repeats = 1000
start_view_times = sample(1:(match_length-watch_window), n_repeats, replace=TRUE)

count_goals_in_window = function(start_time, watch_window){
  sum(score_vec %in% (start_time:(start_time+watch_window)))
}

num_goals_seen = sapply(start_view_times, function(x){
  count_goals_in_window(x, watch_window)
})

table(num_goals_seen)/n_repeats

length(score_vec)/9

mean(num_goals_seen)

sd(num_goals_seen)

