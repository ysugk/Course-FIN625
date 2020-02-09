#' Load required packages
library(tidyverse)
library(stargazer)

#' Load data
ff3 <- readxl::read_excel("HW/input/FF3factorsMonthly.xlsx")

#' a. Summarise data
ff3 %>%
  select(-Date) %>% 
  as.data.frame() %>%
  stargazer(type = "text", out = "HW/output/tabs/HW1/Q1.a.tex", float = FALSE,
            summary = TRUE, summary.stat = c("n", "mean", "sd",  "min", "p25", "median", "p75", "max"), digits = 3)


#' b. Estimate mu and sigma2
n <- nrow(ff3)
mu <- mean(ff3$`Mkt-RF`)
sigma2 <- mean((ff3$`Mkt-RF` - mu)^2)

var_mu <- sigma2/n
se_mu <- var_mu^(1/2)
var_sigma2 <- 2*(sigma2^2)/n
se_sigma2 <- var_sigma2^(1/2)

#' c. SE of sharpe ratio
#' Analytic approximation
se_sharpe_anal <- 1/n

#' Numerical approximation
g <- function(x, sigma2) {
  x/sqrt(sigma2)
}

dg <- function(x, sigma2, h = 1e-4) {
  (g(x + h, sigma2) - g(x - h, sigma2))/(2 * h)
}

se_sharpe_num <- dg(mu, sigma2)^2 * sigma2 / n
