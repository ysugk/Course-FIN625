library(tidyverse)

set.seed(2020)

X <- rnorm(n = 100, mean = 1, sd = 2)

log_likelihood_function <- function(mu, sigma, x) {
  -1/2 * log(2 * pi * sigma^2) - 1/2 * ((x - mu)^2)/(sigma^2)
}

mu_ls <- seq(from = 0, to = 4, by = 0.01)
sigma_ls <- seq(from = 1, to = 5, by = 0.01)

df <- crossing(mu_ls, sigma_ls) %>%
  rowwise() %>%
  mutate(mll = log_likelihood_function(mu_ls, sigma_ls, X) %>%
           mean())

ggplot(df, aes(mu_ls, sigma_ls)) +
  geom_contour(aes(z = mll), )