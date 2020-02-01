library(tidyverse)
set.seed(2020)

theta <- 0.1
mu <- 1.5
sigma_2 <- 1.9

n <- 1000

e_vec <- rnorm(n = n, mean = 0, sd = sqrt(sigma_2))
lag_e_vec <- append(0, lag(e_vec, 1)[2:1000])

Y <- mu + e_vec + theta * lag_e_vec

log_likelihood_function <- function(mu_hat, sigma_2_hat, theta_hat) {
  e_hat <- 0
  
  for (i in 1:n) {
    eforward <- Y[i] - mu_hat - theta_hat * e_hat[i]
    
    e_hat <- append(e_hat, eforward)
  }
  
  (-n/2) * log(2*pi) + (-n/2) * log(sigma_2_hat) - 1/(2*sigma_2_hat) * sum((e_hat[1:n + 1])^2)
  
}

optimize <- function(f, start, h) {
  p <- start
  
  df <- tibble(mu = p[1],
               sigma2 = p[2],
               theta = p[3],
               l = log_likelihood_function(p[1], p[2], p[3]))

  g <- c(1, 1, 1)
  i <- 1
  
  while (sum(abs(g) >= 5e-2) != 0) {
    
    print(paste0("Number of interation is ", i))
    
    
    df <- add_row(df, mu = p[1],
                        sigma2 = p[2],
                        theta = p[3],
                        l = log_likelihood_function(p[1], p[2], p[3]))
    
    u1 <- log_likelihood_function(p[1] + h, p[2], p[3])
    d1 <- log_likelihood_function(p[1] - h, p[2], p[3])
    g1 <- (u1 - d1)/(2 * h)
    
    u2 <- log_likelihood_function(p[1], p[2] + h, p[3])
    d2 <- log_likelihood_function(p[1], p[2] - h, p[3])
    g2 <- (u2 - d2)/(2 * h)
    
    u3 <- log_likelihood_function(p[1], p[2], p[3] + h)
    d3 <- log_likelihood_function(p[1], p[2], p[3] - h)
    g3 <- (u3 - d3)/(2 * h)
    
    g <- c(g1, g2, g3)
    print(paste0("Gradient is ", g))
    
    update <- c(if_else(abs(g1) >= 1, 1e-3, 1e-4) * sign(g1), if_else(abs(g2) >= 1, 1e-3, 1e-4) * sign(g2), if_else(abs(g3) >= 1, 1e-3, 1e-4) * sign(g3))
    
    p <- p + update
    print(paste0(" mu: ", p[1], " sigma2:", p[2], " theta: ", p[3]))
    i <- i + 1
  }
  
  df
}

sim <- optimize(log_likelihood_function, start = c(1.9, 1.7, 0.2), h = 1e-5)

sim %>%
  slice(n())
