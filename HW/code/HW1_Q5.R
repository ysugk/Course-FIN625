library(tidyverse)
set.seed(2020)

theta <- 0.1
mu <- 1.5
sigma_2 <- 1.9

n <- 1000

e_vec <- rnorm(n = n, mean = mu, sd = sqrt(sigma_2))
lag_e_vec <- append(0, lag(e_vec, 1)[2:1000])

Y <- mu + e_vec + theta * lag_e_vec

log_likelihood_function <- function(mu, sigma_2, theta, Y) {
  e <- 0
  
  for (i in 1:n) {
    eforward = Y[i] - mu - theta * e[i]
    
    e <- append(e, eforward)
  }
  
  (-n/2) * log(2*pi) + (-n/2) * log(sigma_2) - 1/(2*sigma_2) * sum((e[1:n + 1])^2)
  
}

mu_vec <- runif(10000, min = 0, max = 3)
sigma_2_vec <- runif(10000, min = 1, max = 3)
theta_vec <- runif(10000, min = 0, max = 0.99)

grid <- tibble(mu = mu_vec,
               sigma_2 = sigma_2_vec,
               theta = theta_vec) %>%
  mutate(likelihood = pmap_dbl(list(mu, sigma_2, theta), log_likelihood_function, Y))

#' Maximum mean log likelihood value and maximum likelihood estimates.
MLE <- grid[grid$likelihood == max(grid$likelihood), ]
knitr::kable(MLE)
