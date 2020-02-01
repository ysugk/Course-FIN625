set.seed(2020)

alpha <- theta <- 0
beta <- 0.210
rho <- 0.972

mu <- c(0,0)

sigma_u2 <- 0.0030050
sigma_e2 <- 0.0000108
sigma_ue <- -0.0001621

Sigma <- matrix(c(sigma_u2, sigma_ue, sigma_ue, sigma_e2), nrow = 2, ncol = 2)

calculate_forwardx <- function(x, e){
  xforward <- theta + rho * x + e
  
  xforward
}

simulate_x <- function(n, x0, e_vec){
  x_vec <- x0
  
  for (i in 1:n) {
    xforward <- calculate_forwardx(x_vec[i], e_vec[i])
    
    x_vec <- append(x_vec, xforward)
  }
  
  x_vec
}

i <- 1

?mvrnorm

simulate <- function(i, n = 840){
  ue_mat <- MASS::mvrnorm(n, mu, Sigma)
  x0 <- 0
  
  u_vec <- ue_mat[ , 1]
  e_vec <- ue_mat[ , 2]
  
  x_vec <- simulate_x(n, x0, e_vec)[1:n + 1]
  lag_x_vec <- append(0, x_vec)[1:n]
  
  r_vec <- alpha + beta * lag_x_vec + u_vec
  
  beta <- sum((lag_x_vec - mean(lag_x_vec)) * (r_vec - mean(r_vec)))/sum((lag_x_vec - mean(lag_x_vec))^2) 
  rho <- sum((lag_x_vec - mean(lag_x_vec)) * (x_vec - mean(x_vec)))/sum((lag_x_vec - mean(lag_x_vec))^2)
  
  tibble(rep = i,
         beta = beta,
         rho = rho)
}

library(tidyverse)
df <- map_dfr(1:10000, simulate)

mean(df$beta)
mean(df$rho)
