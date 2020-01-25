#' ---
#' title: ""
#' author: ""
#' date: ""
#' output: pdf_document
#' ---
#' 


#' Generate random sample
set.seed(2020)
X <- rnorm(n = 100, mean = 1, sd = 2)

#' Define mean log likelihood function
log_likelihood_function <- function(mu, sigma, X) {
  (-1/2) * log(2*pi) + (-1/2) * log(sigma^2) - 1/(2*sigma^2) * (X - mu)^2
}

#' Generate grid
library(tidyverse)

mu_vec <- seq(from = 0, to = 2, by = 0.01)
sigma_vec <- seq(from = 1, to = 3, by = 0.01)

grid <- crossing(mu = mu_vec, sigma = sigma_vec) %>%
  mutate(mean_likelihood = map2_dbl(mu, sigma, function(mu, sigma, X) mean(log_likelihood_function(mu, sigma, X)), X))

#' Maximum mean log likelihood value and maximum likelihood estimates.
MLE <- grid[grid$mean_likelihood == max(grid$mean_likelihood), ]
knitr::kable(MLE)

#' Draw plot
plot <- ggplot(grid) +
  geom_point(aes(x = MLE$mu, y = MLE$sigma)) +
  annotate(geom = "text", x = MLE$mu + 0.05, y = MLE$sigma + 0.05, label = "MLE") +
  geom_contour(aes(x = mu, y = sigma, z = mean_likelihood), binwidth = 0.025) +
  
  labs(x = "Mean", y = "Standard Deviation") +
  ggthemes::theme_few(base_size = 12, base_family = "sans")

ggsave(filename = "Q2.pdf", plot = plot, device = "pdf", path = "HW/output/figure/HW1/")

