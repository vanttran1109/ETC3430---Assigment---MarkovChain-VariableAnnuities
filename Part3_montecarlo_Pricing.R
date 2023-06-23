#FUND VALUE
#path_defensive
#path_growth
#path_age

#AGE 
age <- path_age(65)

defensive_fund_value <- matrix(0, nrow = 1000, ncol = 40)
for (i in 1:1000) {
  defensive_fund_value[i, 1] <- 0.95 * 1 * (1 + path_defensive[i, 1])
  for (j in 2:40) {
    if (age[ i, j] == 1)  {
      defensive_fund_value[i, j] <- 0.95 * defensive_fund_value[i, j-1] * (1 + path_defensive[i, j]) 
                                     - 0.03 * defensive_fund_value[i, j-1]
    }
    if (age[i, j] == 2 && all(age[i, 1:(j - 1)] != 2)) {
       defensive_fund_value[i, j] <- 0.95 * defensive_fund_value[i, j-1] * (1 + path_defensive[i, j]) 
                                     - max(0.5 * defensive_fund_value[i, j-1], 0.5 * 1)
    }
    if (age[i, j] == 2 && any(age[i, 1:(j - 1)] == 2)) {
      defensive_fund_value[i, j] <- 0.95 * defensive_fund_value[i, j-1] * (1 + path_defensive[i, j]) 
                                      - 0.03 * defensive_fund_value[i, j-1]
    }
    if (age[i, j] == 3) {
      break
    }
  }
}
defensive_fund_value

growth_fund_value <- matrix(0, nrow = 1000, ncol = 40)
for (i in 1:1000) {
  growth_fund_value[i, 1] <- 0.95 * 1 * (1 + path_growth[i, 1])
  for (j in 2:40) {
    if (age[ i, j] == 1)  {
      growth_fund_value[i, j] <- 0.95 * growth_fund_value[i, j-1] * (1 + path_growth[i, j]) 
      - 0.03 * growth_fund_value[i, j-1]
    }
    if (age[i, j] == 2 && all(age[i, 1:(j - 1)] != 2)) {
      growth_fund_value[i, j] <- 0.95 * growth_fund_value[i, j-1] * (1 + path_growth[i, j]) 
      - max(0.5 * growth_fund_value[i, j-1], 0.5 * 1)
    }
    if (age[i, j] == 2 && any(age[i, 1:(j - 1)] == 2)) {
      growth_fund_value[i, j] <- 0.95 * growth_fund_value[i, j-1] * (1 + path_growth[i, j]) 
      - 0.03 * growth_fund_value[i, j-1]
    }
    if (age[i, j] == 3) {
      break
    }
  }
}
growth_fund_value

#BENEFIT
benefit_growth_matrix <- matrix(0, nrow = 1000, ncol = 40)
for (i in 1:1000) {
  for (j in 1:40) {
    if (age[ i, j] == 1)  {
      benefit_growth_matrix[i, j] <- 0.03*growth_fund_value[i, j]
    }
    if (age[i, j] == 2 && all(age[i, 1:(j - 1)] != 2)) {
      benefit_growth_matrix[i, j] <- max(0.5 * growth_fund_value[i, j], 0.5 * 1)
    }
    if (age[i, j] == 2 && any(age[i, 1:(j - 1)] == 2)) {
      benefit_growth_matrix[i, j] <- 0.03*growth_fund_value[i, j]
    }
    if (age[i, j] == 3) {
      benefit_growth_matrix[i, j] <- max(growth_fund_value[i, j-1], 0.3)
      break
    }
  }
}
benefit_growth_matrix

net_present_value <- list()
for (i in 1:1000) {
  for (j in 1:40) {
  net_present_value[i] <- sum((benefit_growth_matrix[i, ])*(1+0.01)^(-j))
  }
}
sum(unlist(net_present_value))/1000

benefit_defensive_matrix <- matrix(0, nrow = 1000, ncol = 40)
for (i in 1:1000) {
  for (j in 1:40) {
    if (age[ i, j] == 1)  {
      benefit_defensive_matrix[i, j] <- 0.03*defensive_fund_value[i, j]
    }
    if (age[i, j] == 2 && all(age[i, 1:(j - 1)] != 2)) {
      benefit_defensive_matrix[i, j] <- max(0.5 * defensive_fund_value[i, j], 0.5 * 1)
    }
    if (age[i, j] == 2 && any(age[i, 1:(j - 1)] == 2)) {
      benefit_defensive_matrix[i, j] <- 0.03*defensive_fund_value[i, j]
    }
    if (age[i, j] == 3) {
      benefit_defensive_matrix[i, j] <- max(defensive_fund_value[i, j-1], 0.3)
      break
    }
  }
}

net_present_value_def <- list()
for (i in 1:1000) {
  for (j in 1:40) {
    net_present_value_def[i] <- sum((benefit_defensive_matrix[i, ])*(1+0.01)^(-j))
  }
}
sum(unlist(net_present_value_def))/1000

