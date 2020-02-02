#' ---
#' title: ""
#' author: ""
#' date: ""
#' output: pdf_document
#' ---
#' 

#' Load required packages
library(tidyverse)
library(stargazer)

#' Load data
ff3 <- readxl::read_excel("HW/input/FF3factorsMonthly.xlsx")

#' a. Summarise data
ff3 %>%
  select(-Date, -RF) %>% 
  as.data.frame() %>%
  stargazer(type = "text", out = "HW/output/tabs/HW1/Q3.a.summary.tex", float = FALSE,
            summary = TRUE, summary.stat = c("n", "mean", "sd",  "min", "p25", "median", "p75", "max"), digits = 3)

#' and Calculate OLS estimators
#' For model 1
X <- matrix(data = c(rep(1, nrow(ff3)), ff3$`Mkt-RF`), nrow = nrow(ff3), ncol = 2)
y <- ff3$SMB

ols <- solve(t(X) %*% X) %*% (t(X) %*% y)
e <- y - X %*% ols
s2 <- sum(e^2)/nrow(ff3)
var_ols <- s2 * solve(t(X) %*% X)

t_alpha <- (ols[1] - 0)/sqrt(var_ols[1, 1])
p_alpha <- pt(t_alpha, df = nrow(ff3) - 2, lower.tail = FALSE) * 2

tbl_eq1 <- tibble(param = c("alpha", "beta"),
                  estimate = ols[ ,1],
                  se = c(var_ols[1, 1], var_ols[2, 2])^(1/2),
                  tstat = c(t_alpha, NA),
                  pvalue = c(p_alpha, NA))

tbl_eq1 %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW1/Q3.a.eq1.tex")

#' For model 2
X <- matrix(data = c(rep(1, nrow(ff3)), ff3$`Mkt-RF`), nrow = nrow(ff3), ncol = 2)
y <- ff3$HML

ols <- solve(t(X) %*% X) %*% (t(X) %*% y)
e <- y - X %*% ols
s2 <- sum(e^2)/nrow(ff3)
var_ols <- s2 * solve(t(X) %*% X)

t_alpha <- (ols[1] - 0)/sqrt(var_ols[1, 1])
p_alpha <- pt(t_alpha, df = nrow(ff3) - 2, lower.tail = FALSE) * 2

tbl_eq2 <- tibble(param = c("alpha", "beta"),
                  estimate = ols[ ,1],
                  se = c(var_ols[1, 1], var_ols[2, 2])^(1/2),
                  tstat = c(t_alpha, NA),
                  pvalue = c(p_alpha, NA))

tbl_eq2 %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW1/Q3.a.eq2.tex")


#' b. 
#' For model 1
X <- matrix(data = c(rep(1, nrow(ff3)), ff3$`Mkt-RF`), nrow = nrow(ff3), ncol = 2)
y <- ff3$SMB

ols <- solve(t(X) %*% X) %*% (t(X) %*% y)
e <- y - X %*% ols
omega <- diag(x = e[ ,1]^2, nrow = nrow(ff3), ncol = nrow(ff3))
var_ols <- solve(t(X) %*% X) %*% (t(X) %*% omega %*% X) %*% solve(t(X) %*% X)

t_alpha <- (ols[1] - 0)/sqrt(var_ols[1, 1])
p_alpha <- pt(t_alpha, df = nrow(ff3) - 2, lower.tail = FALSE) * 2

tbl_eq1 <- tibble(param = c("alpha", "beta"),
                  estimate = ols[ ,1],
                  se = c(var_ols[1, 1], var_ols[2, 2])^(1/2),
                  tstat = c(t_alpha, NA),
                  pvalue = c(p_alpha, NA))

tbl_eq1 %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW1/Q3.b.eq1.tex")

#' For model 2
X <- matrix(data = c(rep(1, nrow(ff3)), ff3$`Mkt-RF`), nrow = nrow(ff3), ncol = 2)
y <- ff3$HML

ols <- solve(t(X) %*% X) %*% (t(X) %*% y)
e <- y - X %*% ols
omega <- diag(x = e[ ,1]^2, nrow = nrow(ff3), ncol = nrow(ff3))
var_ols <- solve(t(X) %*% X) %*% (t(X) %*% omega %*% X) %*% solve(t(X) %*% X)

t_alpha <- (ols[1] - 0)/sqrt(var_ols[1, 1])
p_alpha <- pt(t_alpha, df = nrow(ff3) - 2, lower.tail = FALSE) * 2

tbl_eq2 <- tibble(param = c("alpha", "beta"),
                  estimate = ols[ ,1],
                  se = c(var_ols[1, 1], var_ols[2, 2])^(1/2),
                  tstat = c(t_alpha, NA),
                  pvalue = c(p_alpha, NA))

tbl_eq2 %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW1/Q3.b.eq2.tex")
