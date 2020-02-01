library(tidyverse)
library(ggthemes)

# b)
dt <- 1/12
k <- 0.1
xbar <- 0.01
x0 <- 0.1

calculate_xforward <- function(x) {
  dx <- k * (xbar - x) * dt
  xforward <- x + dx
  
  xforward
} 

simulate_xvec <- function(x0, n) {
  x_vec <- x0
  
  for (i in 1:n) {
    xforward <- calculate_xforward(x_vec[i])
    
    x_vec <- append(x_vec, xforward)
  }

  x_vec
}

x_vec <- simulate_xvec(x0, 2500)

tibble(t = dt*(1:length(x_vec) - 1),
       x_vec) %>%
  ggplot(aes(x = t, y = x_vec)) +
  geom_line() +
  geom_hline(yintercept = xbar, color = "blue", linetype = "dashed") +
  
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL, breaks = seq(from = 0.01, to = 0.1, by = 0.01)) +
  theme_few(base_size = 12, base_family = "sans")

ggsave("Q6_b.pdf", path = "HW/output/figure/HW1/")

# d)
set.seed(2020)

dt <- 1/12
k <- 0.1
xbar <- 0.01
x0 <- 0.1
sigma <- 0.05

calculate_xforward <- function(x, e) {
  dx <- k * (xbar - x) * dt + sigma * (dt)^(1/2) * e
  xforward <- x + dx
  
  xforward
} 

simulate_xvec <- function(x0, n) {
  x_vec <- x0
  e_vec <- rnorm(n, mean = 0, sd = 1)
  
  for (i in 1:n) {
    xforward <- calculate_xforward(x_vec[i], e_vec[i])
    
    x_vec <- append(x_vec, xforward)
  }
  
  x_vec
}

# i)
x_vec <- simulate_xvec(x0, 5/dt)

tibble(t = dt*(1:length(x_vec) - 1),
       x_vec) %>%
  ggplot(aes(x = t, y = x_vec)) +
  geom_line() +
  geom_hline(yintercept = xbar, color = "blue", linetype = "dashed") +
  
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL) +
  theme_few(base_size = 12, base_family = "sans")

ggsave("Q6_d_i.pdf", path = "HW/output/figure/HW1/")

# ii)
x_vec <- simulate_xvec(x0, 100/dt)

tibble(t = dt*(1:length(x_vec) - 1),
       x_vec) %>%
  ggplot(aes(x = t, y = x_vec)) +
  geom_line() +
  geom_hline(yintercept = xbar, color = "blue", linetype = "dashed") +
  
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL) +
  theme_few(base_size = 12, base_family = "sans")

ggsave("Q6_d_ii.pdf", path = "HW/output/figure/HW1/")

