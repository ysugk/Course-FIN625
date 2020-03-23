#' Required libraries
library(tidyverse)
library(lubridate)
library(stargazer)

#' Load data
df <- readxl::read_excel("HW/input/Problem5.2_data.xlsx") %>%
  mutate(Date = ymd(paste0(Date, ".01")),
         lag1_dp = lag(dp, 1),
         lag1_xp = lag(xp, 1))

#' a.
m.a.1 <- lm(Rm - Rf ~ lag1_dp, df)
m.a.2 <- lm(Rm - Rf ~ lag1_xp, df)

y <- (df$Rm - df$Rf)[2:581]
yhat1 <- m.a.1$fitted.values
yhat2 <- m.a.2$fitted.values

rsq.a.1 <- sum(yhat1^2)/sum(y^2)
rsq.a.2 <- sum(yhat2^2)/sum(y^2)

data.frame(predictor = c("dp", "xp"),
           rsq = c(rsq.a.1, rsq.a.2)) %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW2/Q1.a.tex")

#' b.

subsample <- function(x, k = (1926 - 1872 + 1) * 4){
  indexes <- 1:(length(x) - k + 1)
  
  map(indexes, function(i) x[i:(i + k - 1)])
}

rm_ls <- subsample(df$Rm[2:581])
rf_ls <- subsample(df$Rf[2:581])

y_ls <- subsample(y)
dp_ls <- subsample(df$lag1_dp[2:581])
xp_ls <- subsample(df$lag1_xp[2:581])

calculate_ols_coef <- function(y, x){
  X <- matrix(c(rep(1, length(x)), x), nrow = length(x), ncol = 2)
  
  coef <- solve(t(X) %*% X) %*% t(X) %*% y
  
  coef
} 

coef_ls1 <- map2(y_ls, dp_ls, calculate_ols_coef)
coef_ls2 <- map2(y_ls, xp_ls, calculate_ols_coef)

df_os <- filter(df, Date >= ymd(19270101))
rm_os <- df_os$Rm
rf_os <- df_os$Rf
dp_os <- df_os$lag1_dp
xp_os <- df_os$lag1_xp

calculate_predicted_value <- function(x, coef_ls){
  coef <- coef_ls %>%
    bind_cols()

  alpha <- slice(coef, 1) %>%
    as_vector()
  
  beta <- slice(coef, 2) %>%
    as_vector()
  
  n <- length(x)
  alpha[1:n] + beta[1:n] * x
}

rmhat1 <- calculate_predicted_value(dp_os, coef_ls1) + rf_os
rmhat2 <- calculate_predicted_value(xp_os, coef_ls2) + rf_os
rmbar <- map_dbl(rm_ls, mean)[1:360]

rsq_os.b.1 <- 1 - sum((rm_os - rmhat1)^2)/sum((rm_os - rmbar)^2)
rsq_os.b.2 <- 1 - sum((rm_os - rmhat2)^2)/sum((rm_os - rmbar)^2)

m.b.1 <- lm(rm_os - rf_os ~ dp_os)
m.b.2 <- lm(rm_os - rf_os ~ xp_os)

yhat1 <- m.b.1$fitted.values
yhat2 <- m.b.2$fitted.values
y <- rm_os - rf_os

rsq.b.1 <- sum(yhat1^2)/sum(y^2)
rsq.b.2 <- sum(yhat2^2)/sum(y^2)

data.frame(predictor = c("dp", "xp"),
           rsq_os = c(rsq_os.b.1, rsq_os.b.2),
           rsq_is = c(rsq.b.1, rsq.b.2)) %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW2/Q1.b.tex")

#' c.

calculate_modified_predicted_value <- function(x, coef_ls){
  coef <- coef_ls %>%
    bind_cols()
  
  alpha <- slice(coef, 1) %>%
    as_vector()
  
  beta <- slice(coef, 2) %>%
    as_vector()
  
  mbeta <- map_dbl(beta, function(z) max(z, 0))
  
  n <- length(x)
  map_dbl(alpha[1:n] + mbeta[1:n] * x, function(z) max(z, 0))
}

xbar1 <- cummean(df$lag1_dp[2:581])[(580 - 360 + 1):580]
xbar2 <- cummean(df$lag1_xp[2:581])[(580 - 360 + 1):580]

rmhat1 <- calculate_modified_predicted_value(xbar1, coef_ls1) + rf_os
rmhat2 <- calculate_modified_predicted_value(xbar2, coef_ls2) + rf_os

rsq_os.c.1 <- 1 - sum((rm_os - rmhat1)^2)/sum((rm_os - rmbar)^2)
rsq_os.c.2 <- 1 - sum((rm_os - rmhat2)^2)/sum((rm_os - rmbar)^2)

data.frame(predictor = c("dp", "xp"),
           rsq_os = c(rsq_os.c.1, rsq_os.c.2)) %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW2/Q1.c.tex")


#' d.
D <- df$D

G <- ((D - lag(D, 1))/lag(D, 1))[2:581]
d <- log(D)
g <- ((d - lag(d, 1))/lag(d, 1))[2:581]
Eg <- cummean(g)

R <- (df$Rm + 1)[2:581]
r <- log(R)
Er <- cummean(r)
Er2 <- cummean(r^2)
varr <- Er2 - Er

dp <- df$dp[2:581]

x <- dp * (1 + G) + exp(Eg) + 0.5 * varr
summary(x)

#' e.

y <- (df$Rm - df$Rf)[3:581]
y_ls <- subsample(y)
x_ls <- subsample(x[1:579])

coef_ls <- map2(y_ls, x_ls, calculate_ols_coef)

df_os <- filter(df, Date >= ymd(19270101))
rm_os <- df_os$Rm[2:360]
rf_os <- df_os$Rf[2:360]
x_os <- x[(579 - 360 + 2):579]

rmhat <- calculate_predicted_value(x_os, coef_ls) + rf_os
rmbar <- map_dbl(rm_ls, mean)[2:360]

rsq_os.e.1 <- 1 - sum((rm_os - rmhat)^2)/sum((rm_os - rmbar)^2)

m.e.1 <- lm(rm_os - rf_os ~ x_os)

yhat <- m.e.1$fitted.values
y <- rm_os - rf_os

rsq.e.1 <- sum(yhat^2)/sum(y^2)

xbar <- cummean(x)[(579 - 360 + 2):579]
rmhat <- calculate_modified_predicted_value(xbar, coef_ls) + rf_os
rsq_os.e.2 <- 1 - sum((rm_os - rmhat)^2)/sum((rm_os - rmbar)^2)

data.frame(part = c("b)", "c)"),
           rsq_os = c(rsq_os.e.1, rsq_os.e.2),
           rsq_is = c(rsq.e.1, NA)) %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW2/Q1.e.tex")

#' f.
rmhat <- x - 1
rmhat <- rmhat[(579 - 360 + 2):579]

rsq_os.f <- 1 - sum((rm_os - rmhat)^2)/sum((rm_os - rmbar)^2)

data.frame(restriction = "a = -1, b = 1",
           rsq_os = rsq_os.f) %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW2/Q1.f.tex")
  
