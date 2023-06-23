library(readxl)
library(dplyr)
library(ggplot2)
library(readxl)
set.seed(2023)
return <- read_excel("~/Desktop/ETC3430/Assignment/data.xlsx", 
                     sheet = "Return data")
#1. TRANSITION MATRIX 
#Function to calculate the transition matrix for two investment option
transition_matrix_investment <- function(option) {
  return_vector <- unname(unlist(return[, option]))
  states <- sort(unique(return_vector))
  n <- length(states)
  P <- matrix(NA, nrow = n, ncol = n)
  for (j in 1:n) {
    for (k in 1:n) {
      num <- 0
      for (i in 2:length(return_vector)) {
        if (return_vector[i - 1] == states[j] &
            return_vector[i] == states[k])
          num <- num + 1
      }
      P[j, k] <- num
    }
    P[j, ] <- P[j, ] / sum(P[j, ])
  }
  return(P)
}
transition_matrix_investment("Growth")
transition_matrix_investment("Defensive") 

#2.GENERATE SAMPLE PATH
#Generate 100000 sample paths for GROWTH
return_vector_growth <- unname(unlist(return[, "Growth"]))
#Count value for calculating the transition matrix
states_growth <- c("-0.05", "0.2")
length <- 40 # time periods
N <- 1000 # number of paths
path_growth <- matrix(NA, nrow = N, ncol = length)
path_growth[, 1] <-
  sample(x = states_growth, N, replace = TRUE) #uniform distribution
P_Growth <- transition_matrix_investment("Growth")
row.names(P_Growth) <- c("-0.05", "0.2")
colnames(P_Growth) <- c("-0.05", "0.2")
for (j in 1:N) {
  for (i in 2:length) {
    path_growth[j, i] <-
      sample(x = states_growth,
             size = 1,
             prob = P_Growth[path_growth[j, i - 1], ])
  }
}
path_growth <- apply(path_growth, c(1, 2), as.numeric)


#Generate 10000 sample paths for DEFENSIVE
return_vector_defensive <- unname(unlist(return[, "Defensive"]))
#Count value for calculating the transition matrix
states_defensive <- c("-0.01", "0.05")
length <- 40 # time periods
N <- 1000 # number of paths
path_defensive <- matrix(NA, nrow = N, ncol = length)
path_defensive[, 1] <-
  sample(x = states_defensive, N, replace = TRUE) #uniform distribution
P_defensive <- transition_matrix_investment("Defensive")
rownames(P_defensive) <- c("-0.01", "0.05")
colnames(P_defensive) <- c("-0.01", "0.05")
for (j in 1:N) {
  for (i in 2:length) {
    path_defensive[j, i] <-
      sample(x = states_defensive,
             size = 1,
             prob = P_defensive[path_defensive[j, i - 1], ])
  }
}
path_defensive <- apply(path_defensive, c(1, 2), as.numeric)
#3.DISTRIBUTION
#GROWTH
# long term distribution for the last 10 periods
# N1_growth <- matrix(NA, nrow=2, ncol=length)
# for (i in 1:2){
#   for (j in 1:length){
#     N1_growth[i,j] <- sum(path_growth[,j]==states_growth[i])
#   }
# }
# Distribution_Growth <- N1_growth/N
# 
# # DEFENSIVE
# N1_defensive <- matrix(NA, nrow=2, ncol=length)
# for (i in 1:2){
#   for (j in 1:length){
#     N1_defensive[i,j] <- sum(path_defensive[,j]==states_defensive[i])
#   }
# }
# Distribution_Defensive <- N1_defensive/N
# 
# #4.PLOT
# #GROWTH
# matplot(t(Distribution_Growth), type = "l", lty = 1:2, col = c("#2C7BB6", "#D7191C")) 
# legend("center", legend = c("-0.05", "0.2"), lty = 1:2, col = c("#2C7BB6", "#D7191C"))
# 
# #DEFENSIVE
# matplot(t(Distribution_Defensive), type = "l", lty = 1:2, col = c("#2C7BB6", "#D7191C")) 
# legend("center", legend = c("-0.01", "0.05"), lty = 1:2, col = c("#2C7BB6", "#D7191C"))
# 



