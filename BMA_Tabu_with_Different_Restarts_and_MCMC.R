
# Remove existing bnlearn package and install the updated version
#remove.packages("bnlearn")
#library(devtools)
#devtools::install_github("azar2020/bnlearn")

# Load required libraries
library(bnlearn)
library(Rgraphviz)

# Set working directory
setwd("C:/Azar_Drive/relationships-between-variables1/01_preprocessing/results")

# Define number of rounding digits
options(digits = 3)

# Read data
#Creel_app_data_daily_agg = read.csv("Discretized_Bow_Creel_app_data_daily_agg.csv")
Creel_app_data_daily_agg = read.csv("Discretized_Bow_Oldman_Creel_app_data_daily_agg.csv")

# Subset data
Creel_app_data_daily_agg = Creel_app_data_daily_agg[c("app_catch_rate","app_fishing_duration", 
                                                      "creel_catch_rate", "creel_fishing_duration",
                                                      "webpage_views", "air_temperature",
                                                      "total_precipitation","wind_speed",
                                                      "relative_humidity","solar_radiation",
                                                      "degree_days", "is_weekend")]

# Convert variables to factors
character_vars <- c("app_catch_rate", "app_fishing_duration", "creel_catch_rate", 
                    "creel_fishing_duration", "webpage_views", "air_temperature",
                    "total_precipitation", "wind_speed", "relative_humidity",
                    "solar_radiation", "degree_days", "is_weekend")
Creel_app_data_daily_agg[character_vars] <- lapply(Creel_app_data_daily_agg[character_vars], as.factor)
Creel_app_data_daily_agg = as.data.frame(Creel_app_data_daily_agg)

# Redirect output to a text file
sink("BMA_Combined_TABU_different_resrarts.txt")

# Tabu search 1 time, start point = empty graph
tabu_object = tabu(Creel_app_data_daily_agg, tabu = 30000, score = "bic", max.iter = 1000000)
plot(tabu_object)

# Save results
best_scores_df = tabu_object$best_scores_list
best_params_df = tabu_object$best_params_list
print(best_params_df)

# BMA using Tabu, with different random starting points
num = 5 # number of random starting points
nodes = names(Creel_app_data_daily_agg)
start = random.graph(nodes = nodes, method = "melancon", num = num, every = 1)

tabu_object_random_start <- list()
final_symmetric_matrix <- list()
sum_matrix <- matrix(0, nrow = 12, ncol = 12) 

# Loop over different starting points
for (i in 1:num) {
  tabu_object_random_start[[i]] <- tabu(Creel_app_data_daily_agg, score = "bic", start = start[[i]], tabu = 3000, max.iter = 10000)
  final_symmetric_matrix[[i]] <- tabu_object_random_start[[i]]$final_symmetric_matrix
  sum_matrix <- sum_matrix + final_symmetric_matrix[[i]]
}

# Print mean matrix containing the probability of the links (BMA)
print("mean_matrix") 
print(sum_matrix/num)

# Print tabu_object_random_start
print("tabu_object_random_start")
print(tabu_object_random_start)

# Loop through results and save them
for (i in 1:num) {
  print(best_params_df)
  write.csv(best_scores_df, file = paste("BIC_iteration_Bow_starting_point", i, ".csv", sep = ""), row.names = FALSE)
  write.csv(best_params_df, file = paste("best_params_Bow_starting_point", i, ".csv", sep = ""), row.names = FALSE)
  write.csv(final_symmetric_matrix, file = paste("best_final_symmetric_matrix", ".csv", sep = ""), row.names = FALSE)
}

# BMA using BDgraph package
library(BDgraph)
bdgraph.obj <- bdgraph(data = Creel_app_data_daily_agg, method = "gcgm", iter = 100000, save = TRUE)
plinks(bdgraph.obj, round = 20, burnin = NULL)

# Close the text output file
sink()
