library(tidyverse)
set.seed(2020)

theta <- 0.1
mu <- 1.5
sigma_2 <- 1.9

E <- rnorm(n = 1000, mean = mu, sd = sqrt(sigma_2))
lagE <- append(0, lag(E, 1)[2:1000])

y <- mu + E + theta * lagE

