#' Set parameters
set.seed(2020)

alpha <- theta <- 0
beta <- 0.210
rho <- 0.972

mu <- c(0,0)

sigma_u2 <- 0.0030050
sigma_e2 <- 0.0000108
sigma_ue <- -0.0001621

Sigma <- matrix(c(sigma_u2, sigma_ue, sigma_ue, sigma_e2), nrow = 2, ncol = 2)

#' Generate x
simulate_x <- function(n, x0, e_vec){
  x_vec <- vector(length = n + 1)
  x_vec[1] <- x0
  
  for (i in 1:n) {
    xforward <- theta + rho * x_vec[i] + e_vec[i]
    
    x_vec[i + 1] <- xforward
  }
  
  x_vec
}

#' simulation
simulate <- function(i, n = 840){
  ue_mat <- MASS::mvrnorm(n, mu, Sigma)
  x0 <- 0
  
  u_vec <- ue_mat[ , 1]
  e_vec <- ue_mat[ , 2]
  
  x_vec <- simulate_x(n, x0, e_vec)[1:n + 1]
  lag_x_vec <- append(x0, x_vec)[1:n]
  
  r_vec <- alpha + beta * lag_x_vec + u_vec
  
  beta <- sum((lag_x_vec - mean(lag_x_vec)) * (r_vec - mean(r_vec)))/sum((lag_x_vec - mean(lag_x_vec))^2) 
  rho <- sum((lag_x_vec - mean(lag_x_vec)) * (x_vec - mean(x_vec)))/sum((lag_x_vec - mean(lag_x_vec))^2)
  
  tibble(rep = i,
         beta = beta,
         rho = rho)
}

library(tidyverse)
library(moments)
library(stargazer)

#' simulate 10000 times with 840 observations
df_840 <- map_dfr(1:10000, simulate)

summary_840 <- df_840 %>%
  select(-rep) %>%
  gather("param", "value") %>%
  group_by(param) %>%
  summarise(n = n(),
            mean = mean(value),
            sd = sd(value),
            skewness = skewness(value),
            kurtosis = kurtosis(value))

summary_840 %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, out = "HW/output/tabs/HW1/Q5.a.tex", float = FALSE)

#' simulate 10000 times with 240 observations
df_240 <- map_dfr(1:10000, simulate, 240)

summary_240 <- df_240 %>%
  select(-rep) %>%
  gather("param", "value") %>%
  group_by(param) %>%
  summarise(n = n(),
            mean = mean(value),
            sd = sd(value),
            skewness = skewness(value),
            kurtosis = kurtosis(value))

summary_240 %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, out = "HW/output/tabs/HW1/Q5.b.tex", float = FALSE)


ggplot(df_840) +
  geom_histogram(aes(x = beta))

ggplot(df_840) +
  geom_histogram(aes(x = rho))

