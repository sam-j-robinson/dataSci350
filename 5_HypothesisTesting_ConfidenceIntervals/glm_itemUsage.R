library(readr)
library(ggplot2)
library(MASS)
library(stringr)

all_items_stainz <- read_csv("~/Desktop/personal_git/schoolwork/finalProject/share_all_items_empireZ_2016-7-1-0_2017-2-1-0.csv")
n <- names(all_items_stainz)
all_items_stainz <- all_items_stainz[!grepl("buy.value.vipTime_1",n) ] #not sure why this value is so bad
n <- names(all_items_stainz)

all_items_empireZ <- all_items_stainz[4:length(all_items_stainz)]
all_items_empireZ <- na.omit(all_items_empireZ)

model <- lm(all_items_empireZ$revenue ~ . , data = all_items_empireZ)

model_matrix <- model.matrix(log(revenue)~., data = all_items_empireZ)

model_pc <- prcomp(model_matrix)

plot(model_pc$sdev[0:7])

pc_top7 <- lm(all_items_empireZ$revenue ~ model_pc$x[,0:7])
AIC(pc_top7)

use_count <- all_items_stainz[grepl("usecount", n)]
use_count <- use_count[!grepl("goodiebag", names(use_count))]
use_count <- use_count[!grepl("leaders", names(use_count))]
use_count$revenue <- all_items_stainz$revenue
cur_names <- names(use_count)
f <- as.formula(paste("revenue ~", paste(cur_names[!cur_names %in% c("revenue")], collapse = " + ")))
use_count_lm <- lm(f, data=use_count)
step_use_count <- step(use_count_lm)

use_value <- all_items_stainz[grepl("usevalue", n)]
use_value <- use_value[!grepl("goodiebag", names(use_value))]
#use_value <- use_value[!grepl("leaders", names(use_value))]
use_value$revenue <- all_items_stainz$revenue
cur_names <- names(use_value)
f <- as.formula(paste("revenue ~", paste(cur_names[!cur_names %in% c("revenue")], collapse = " + ")))
use_value_lm <- lm(f, data=use_value)
step_use_value <- step(use_value_lm)

buy_count <- all_items_stainz[grepl("buycount", n)]
#buy_count <- buy_count[!grepl("leaders", names(buy_count))]
buy_count$revenue <- all_items_stainz$revenue
cur_names <- names(buy_count)
f <- as.formula(paste("revenue ~", paste(cur_names[!cur_names %in% c("revenue")], collapse = " + ")))
but_count_lm <- lm(f, data=buy_count)
step_buy_count <- step(but_count_lm)

buy_value <- all_items_stainz[grepl("buyvalue", n)]
#buy_value <- buy_value[!grepl("leaders", names(buy_value))]
buy_value$revenue <- all_items_stainz$revenue
cur_names <- names(buy_value)
f <- as.formula(paste("revenue ~", paste(cur_names[!cur_names %in% c("revenue")], collapse = " + ")))
buy_value_lm <- lm(f, data=buy_value)
step_buy_value <- step(buy_value_lm)

sum_buy_count <- summary(step_buy_count)$coefficients
#sum_buy_count[, rowid := rownames(sum_buy_count)]
sum_buy_value <- summary(step_buy_value)$coefficients
#sum_buy_value[, rowid := rownames(sum_buy_value)]
sum_use_count <- summary(step_use_count)$coefficients
#sum_use_count[, rowid := rownames(sum_use_count)]
sum_use_value <-summary(step_use_value)$coefficients
#sum_use_value[, rowid := rownames(sum_use_value)]

create_coefficient_table <- function(x){
  coef <- as.data.table(x)
  coef[,rowid := rownames(x)]
  final_col_names <- c('Estimate', 'error', 'tValue', 'pValue', 'item_name')
  colnames(coef) <- final_col_names
  coef <- subset(coef, pValue < 0.001)
  return(coef)
}

sbc_coefficients <- create_coefficient_table(sum_buy_count)
sbv_coefficients <- create_coefficient_table(sum_buy_value)
suc_coefficients <- create_coefficient_table(sum_use_count)
suv_coefficients <- create_coefficient_table(sum_use_value)

best_columns <- rbind(sbc_coefficients, sbv_coefficients, suv_coefficients, suc_coefficients)
best_col_names <- best_columns[,item_name]
column_idx <- match(best_col_names, n)
column_idx <- sort(c(column_idx-1, column_idx))
best_df <- as.data.tabel(all_items_stainz[,column_idx,with = FALSE])
best_df$revenue <- all_items_stainz$revenue
f <- as.formula(paste("revenue ~", paste(cur_names[!cur_names %in% c("revenue")], collapse = " + ")))
best_lm <- lm(f, data=best_df)
summary(best_lm)
step_best <- step(best_lm)