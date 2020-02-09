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


#' b. t-statistics
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

#' c. Wald statistics
#' For the restriction 3
#' unrestricted

X <- matrix(data = c(rep(1, nrow(ff3)), ff3$`Mkt-RF`), nrow = nrow(ff3), ncol = 2)
y <- ff3$SMB

n <- length(y)
k <- ncol(X)

ols <- solve(t(X) %*% X) %*% (t(X) %*% y)
e <- y - X %*% ols

#' restricted
x <- ff3$`Mkt-RF`
u <- y - x * sum(x*y) / sum(x^2)

wald1 <- (sum(u^2) - sum(e^2))/(sum(e^2)/(n - k))
pvalue1 <- pf(wald1, 1, n - k, lower.tail = FALSE)

#' For the restriction 4
#' unrestricted

X <- matrix(data = c(rep(1, nrow(ff3)), ff3$`Mkt-RF`), nrow = nrow(ff3), ncol = 2)
y <- ff3$HML

n <- length(y)
k <- ncol(X)

ols <- solve(t(X) %*% X) %*% (t(X) %*% y)
e <- y - X %*% ols

#' restricted
x <- ff3$`Mkt-RF`
u <- y - x * sum(x*y) / sum(x^2)

wald2 <- (sum(u^2) - sum(e^2))/(sum(e^2)/(n - k))
pvalue2 <- pf(wald2, 1, n - k, lower.tail = FALSE)

#' For the restriction 5
#' unrestricted

d1 <- c(rep(1, nrow(ff3)), rep(0, nrow(ff3)))
d2 <- c(rep(0, nrow(ff3)), rep(1, nrow(ff3)))

X <- matrix(data = c(d1, d2, d1 * rep(ff3$`Mkt-RF`, 2), d2 * rep(ff3$`Mkt-RF`, 2)), nrow = 2 * nrow(ff3), ncol = 4)
y <- c(ff3$SMB, ff3$HML)

n <- length(y)
k <- ncol(X)

ols <- solve(t(X) %*% X) %*% (t(X) %*% y)
e <- y - X %*% ols

#' restricted
X <- matrix(data = c(d1 * rep(ff3$`Mkt-RF`, 2), d2 * rep(ff3$`Mkt-RF`, 2)), nrow = 2 * nrow(ff3), ncol = 2)
ols <- solve(t(X) %*% X) %*% (t(X) %*% y)
u <- y - X %*% ols

wald3 <- ((sum(u^2) - sum(e^2))/2)/(sum(e^2)/(n - k))
pvalue3 <- pf(wald3, 2, n - k, lower.tail = FALSE)

tibble("eq" = c(3, 4, 5),
       "wald" = c(wald1, wald2, wald3),
       "pvalue" = c(pvalue1, pvalue2, pvalue3)) %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, rownames = FALSE, float = FALSE, out = "HW/output/tabs/HW1/Q3.c.tex")
