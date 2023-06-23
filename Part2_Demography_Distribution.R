library(readxl)
library(dplyr)
library(ggplot2)
library(readxl)
set.seed(2023)
demography <- read_excel("~/Desktop/ETC3430/Assignment/data.xlsx", 
                         sheet = "Demography data")
#1. TRANSITION PROBABILITY MATRIX
#Function to calculate the transition matrix at each age
transition_matrix_demography <- function(age) {
  #Extract the data from the data frame
  age_jan80 <- demography$`Age at Jan 1980`
  paths <- demography %>% filter(age_jan80 == age) %>% select(-1) %>% slice(-1) 
  list_sample_paths <- list()
  
  for (i in 1:nrow(paths)) {
    list_sample_paths[i] <- list(as.vector(unname(unlist(paths[i,]))))
  }
  
  #Count values for calculating the transition matrix 
  states <- sort(unique(unlist(list_sample_paths)))
  a <- length(list_sample_paths)
  n <- length(states)
  P <- matrix(NA, nrow = n, ncol = n)
  
  #Calculate the transition matrix
  for (j in 1:n) {
    for (k in 1:n) {
      num <- 0
      for (i in seq_along(list_sample_paths)) {
        for (l in 2:length(list_sample_paths[[i]])) {
          if (list_sample_paths[[i]][l - 1] == states[j] &
              list_sample_paths[[i]][l] == states[k])
            num <- num + 1
        }
        P[j, k] <- num
      }
    }
    P[j, ] <- P[j, ] / sum(P[j, ])
  }
  return(P)
}

transition_matrix_demography(84)
#2.GENERATE SAMPLE PATH + CALCULATE DISTRIBUTION
#Generate the path from the transition matrix
path_age <- function (age) {
  states_age <- 1:3
  set.seed(2023)
  length <- 40 # time periods
  N_age <- 1000 # number of paths
  path_age <- matrix(NA, nrow = N_age, ncol = length)
  path_age[, 1] <- 1 #state at state health
  P_age <- transition_matrix_demography(age)
  for (j in 1:N_age) {
    for (i in 2:length) {
      path_age[j, i] <-
        sample(x = states_age,
             size = 1,
             prob = P_age[path_age[j, i - 1], ])
    }
  }
  return(path_age)
}
  # long term distribution for the last 10 periods
  # N1_age <- matrix(NA, nrow = 3, ncol = length)
  # for (i in 1:3) {
  #   for (j in 1:length) {
  #     N1_age[i, j] <- sum(path_age[, j] == i)
  #   }
  #   P_age <- N1_age / N_age
  # }
age65 <- path_age(65)


# matrix_list <- list()
# for (j in 65:84) {
#   matrix_list <- c(matrix_list, list(age_dist(j)))
# }
# 
# #2.1. PLOT HEALTHY DISTRIBUTION
# first_rows <- matrix(NA, nrow = 20, ncol = 40)
# for (i in 1:20) {
#   first_rows[i,] <- matrix_list[[i]][1,]
# }
# age_colors <- colorRampPalette(c("#D7191C","#66c2a5", "#2C7BB6"))
# age_levels <- seq(65, 84)
# color_palette <- age_colors(length(age_levels))
# matplot(t(first_rows), type = "l", lty = 1, col = color_palette, xlab = "Time", ylab = "Value")
# legend("topright", legend = age_levels, col = color_palette, lty = 1, cex = 0.8)
# 
# #2.2. PLOT ILLNESS DISTRIBUTION
# second_rows <- matrix(NA, nrow = 20, ncol = 40)
# for (i in 1:20) {
#   second_rows[i,] <- matrix_list[[i]][2,]
# }
# age_colors <- colorRampPalette(c("#D7191C","#66c2a5", "#2C7BB6"))
# age_levels <- seq(65, 84)
# color_palette <- age_colors(length(age_levels))
# matplot(t(second_rows), type = "l", lty = 1, col = color_palette, xlab = "Time", ylab = "Value")
# legend("topright", legend = age_levels, col = color_palette, lty = 1, cex = 0.8)
# 
# #2.3. PLOT DEATH DISTRIBUTION
# third_rows <- matrix(NA, nrow = 20, ncol = 40)
# for (i in 1:20) {
#   third_rows[i,] <- matrix_list[[i]][3,]
# }
# age_colors <- colorRampPalette(c("#D7191C","#66c2a5", "#2C7BB6"))
# age_levels <- seq(65, 84)
# color_palette <- age_colors(length(age_levels))
# matplot(t(third_rows), type = "l", lty = 1, col = color_palette, xlab = "Time", ylab = "Value")
# legend("bottomright", legend = age_levels, col = color_palette, lty = 1, cex = 0.8)