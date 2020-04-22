# laod required packages
library(tidyverse)
library(stargazer)

# read data
sheets <- readxl::excel_sheets("HW/input/rates.xls")
annual_rates <- readxl::read_excel("HW/input/rates.xls", sheet = 1)
quarterly_rates <- readxl::read_excel("HW/input/rates.xls", sheet = 2)

# a.
# annual data
## mutate variables
annual_rates_a <- annual_rates %>%
  mutate(gc = 100 * (C/lag(C, 1) - 1),
         gc_real = gc - Inflation,
         rm_real = Stock - Inflation,
         rf_real = `T-Bill` - Inflation,
         erm = rm_real - rf_real) %>%
  select(gc_real, rm_real, rf_real, erm)

## summarise and tabulate stats
annual_rates_a %>%
  ### summarise
  gather("variable", "value") %>%
  drop_na() %>%
  group_by(variable) %>%
  summarise(n = n(),
            mean = mean(value),
            sd = sd(value),
            ar1coef = mean((value - mean(value)) * (lag(value, 1) - mean(value)), na.rm = TRUE)/var(value)) %>%
  ### tabulate
  right_join(tibble(variable = c("gc_real", "rm_real", "rf_real", "erm")), by = "variable") %>%
  column_to_rownames("variable") %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.a.annualsummary.tex")


# quarterly data
## mutate variables
quarterly_rates_a <- quarterly_rates %>%
  mutate(gc = 100 * (C/lag(C, 1) - 1),
         gc_real = gc - Inflation,
         rm_real = Stock - Inflation,
         rf_real = Bill - Inflation,
         erm = rm_real - rf_real) %>%
  select(gc_real, rm_real, rf_real, erm)

## summarise and tabulate stats
quarterly_rates_a %>%
  ### summarise
  gather("variable", "value") %>%
  drop_na() %>%
  group_by(variable) %>%
  summarise(n = n(),
            mean = mean(value) * 4,
            sd = sd(value) * 2,
            ar1coef = mean((value - mean(value)) * (lag(value, 1) - mean(value)), na.rm = TRUE)/var(value)) %>%
  ### tabulate
  right_join(tibble(variable = c("gc_real", "rm_real", "rf_real", "erm")), by = "variable") %>%
  column_to_rownames("variable") %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.a.quarterlysummary.tex")

# b.

# c.
# annual data
## define variables
annual_rates_c <- annual_rates %>%
  mutate(dc = log(C/lag(C, 1)),
         R = (100 + Stock - Inflation)/100,
         r = log(R),
         Rf = (100 + `T-Bill` - Inflation)/100,
         rf = log(Rf)) %>%
  select(dc, r, rf)

## summarise and tabulate stats
annual_rates_c %>%
  ### calculate moments
  gather("variable", "value") %>%
  drop_na() %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%
  ### tabulate
  right_join(tibble(variable = c("dc", "r", "rf")), by = "variable") %>%
  column_to_rownames("variable") %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.c.annualsummary.tex")

### calculate correlations
annual_cor <- cor(annual_rates_c, use = "pairwise.complete.obs")
### tabulate
annual_cor[upper.tri(annual_cor, diag = FALSE)] <- NA
stargazer(annual_cor, type = "text", digits = 2, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.c.annualcor.tex")

# quarterly data
## define variables
quarterly_rates_c <- quarterly_rates %>%
  mutate(dc = log(C/lag(C, 1)),
         R = (100 + Stock - Inflation)/100,
         r = log(R),
         Rf = (100 + Bill - Inflation)/100,
         rf = log(Rf)) %>%
  select(dc, r, rf)

## summarise and tabulate stats
quarterly_rates_c %>%
  ### calculate moments
  gather("variable", "value") %>%
  drop_na() %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%
  ### tabulate
  right_join(tibble(variable = c("dc", "r", "rf")), by = "variable") %>%
  column_to_rownames("variable") %>%
  as.data.frame() %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.c.quarterlysummary.tex")

### calculate correlations
quarterly_cor <- cor(quarterly_rates_c, use = "pairwise.complete.obs")
### tabulate
quarterly_cor[upper.tri(quarterly_cor, diag = FALSE)] <- NA
stargazer(quarterly_cor, type = "text", digits = 2, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.c.quarterlycor.tex")

# d.
## the first measure
### annual
(0.058 - 0.006 + 0.5 * (0.203)^2) / (0.203 * 0.021 * 0.09)
## quarterly
(0.017 - 0.002 + 0.5 * (0.083)^2) / (0.083 * 0.005 * 0.18)

## the second measure
### annual
(0.058 - 0.006 + 0.5 * (0.203)^2) / (0.203 * 0.021)
## quarterly
(0.017 - 0.002 + 0.5 * (0.083)^2) / (0.083 * 0.005)

# e.
## define variables
annual_rates_e <- annual_rates %>%
  mutate(G = C/lag(C, 1),
         lead1G = lead(G, 1),
         R = (100 + Stock - Inflation)/100,
         lead1R = lead(R, 1),
         Rf = (100 + `T-Bill` - Inflation)/100,
         lead1Rf = lead(Rf, 1)) %>%
  select(G:lead1Rf) %>%
  drop_na()

X <- cbind(annual_rates_e$lead1G, annual_rates_e$lead1R, annual_rates_e$lead1Rf)
inst <- cbind(rep(1, nrow(annual_rates_e)), annual_rates_e$G, annual_rates_e$R - annual_rates_e$Rf)

## compute sample moments
calculate_h <- function(delta, gamma, X, inst) {
  
  h1 <- delta * (X[ ,1]^(-gamma)) * X[ ,2] * inst - inst
  h2 <- delta * (X[ ,1]^(-gamma)) * X[ ,3] * inst - inst
  
  cbind(h1, h2)
}

## compute quadratic form
calculate_firststepQ <- function(theta) {
  delta <- theta[1]
  gamma <- theta[2]
  
  h <- calculate_h(delta, gamma, X, inst)
  
  m <- colMeans(h)
  
  t(m) %*% m
}

## first-step GMM
firststepGMM <- optim(c(0.9, 30), calculate_firststepQ, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, Inf))
firststep_par <- firststepGMM$par

## compute newey-west vcov matrix
calculate_covmat <- function(k, theta, X, inst) {
  delta <- theta[1]
  gamma <- theta[2]
  
  n <- nrow(X)
  
  if (k == 0) {
    h <- calculate_h(delta, gamma, X, inst)
    
    V <- (t(h) %*% h)/n
  } else {
    h1 <- calculate_h(delta, gamma, X[-(1:k), ], inst[-(1:k), ])
    h2 <- calculate_h(delta, gamma, X[1:(n - k), ], inst[1:(n - k), ])
    
    V <- (t(h1) %*% h2)/n
  }
  
  V
}

nwcov <- calculate_covmat(0, firststep_par, X, inst) + (1/2) * (calculate_covmat(1, firststep_par, X, inst) + t(calculate_covmat(1, firststep_par, X, inst)))

## second-step GMM
calculate_secondstepQ <- function(theta) {
  delta <- theta[1]
  gamma <- theta[2]
  
  h <- calculate_h(delta, gamma, X, inst)
  
  m <- colMeans(h)
  
  t(m) %*% solve(nwcov) %*% m
}

secondstepGMM <- optim(c(0.9, 30), calculate_secondstepQ, method = "L-BFGS-B", lower = c(0, 0), upper = c(1.000, Inf))
secondstep_par <- secondstepGMM$par

## compute first-order partial derivative of sample moments
calculate_M <- function(theta, X, inst){
  delta <- theta[1]
  gamma <- theta[2]
  
  M11 <- colMeans((X[ ,1]^(-gamma)) * X[ ,2] * inst)
  M21 <- colMeans((X[ ,1]^(-gamma)) * X[ ,3] * inst)
  M1 <- c(M11, M21)
  
  M12 <- colMeans(delta * (X[ ,1]^(-gamma)) * (-log(X[ ,1])) * X[ ,2] * inst)
  M22 <- colMeans(delta * (X[ ,1]^(-gamma)) * (-log(X[ ,1])) * X[ ,3] * inst)
  M2 <- c(M12, M22)
  
  cbind(M1, M2)
}

M <- calculate_M(secondstep_par, X, inst)

## compute standard errors of estimates
par_cov <- solve(t(M) %*% solve(nwcov) %*% M)/nrow(X)

## compute J-statistic
m <- colMeans(calculate_h(secondstep_par[1], secondstep_par[2], X, inst))
J <- nrow(X) * t(m) %*% solve(nwcov) %*% m

## tabulate
tibble(param = c("delta", "gamma"),
       firststage = firststep_par,
       secondstage = secondstep_par,
       se = c(par_cov[1, 1]^(1/2), par_cov[2,2]^(1/2))) %>%
  mutate(teststat = secondstage/se,
         pvalue = pnorm(abs(teststat), mean = 0, sd = 1, lower.tail = FALSE) * 2) %>%
  add_row(param = "J", teststat = J, pvalue = pchisq(J, df = 4, lower.tail = FALSE)) %>%
  column_to_rownames("param") %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.e.tex")

# f.
## define variable
quarterly_rates_f <- quarterly_rates %>%
  mutate(rm = (Stock - Bill)/100) %>%
  mutate_at(.vars = vars(matches("\\d+")), .funs = function(x) (x - quarterly_rates$Bill)/100) %>%
  select(`1`:`10`, rm) %>%
  as.matrix()

Y <- quarterly_rates_f[ ,1:10]
X <- cbind(rep(1, nrow(quarterly_rates_f)), quarterly_rates_f[ ,11])

## run a multivariate regression
B <- solve(t(X) %*% X) %*% t(X) %*% Y
U <- Y - X %*% B
Sigma <- (t(U) %*% U) / nrow(quarterly_rates_f)
coef_cov <- kronecker(solve(t(X) %*% X), Sigma)

## compute statistics
se <- t(matrix(diag(coef_cov)^(1/2), ncol = 2))
t <- B/se
p <- pnorm(abs(t), mean = 0, sd = 1, lower.tail = FALSE) * 2

## tabulate
tab.f <- rbind(B, se, t, p)
row.names(tab.f) <- c("alpha", "beta", "se_alpha", "se_beta", "t_alpha", "t_beta", "p_alpha", "p_beta")
tab.f %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.f.tex")

# g.
# h.

## load required package
library(ggthemes)

## define variables
mu <- colMeans(Y)
beta <- B[2, ]
X <- cbind(rep(1, 10), beta)

## run Fama-Macbeth regression
lambda <- solve(t(X) %*% X) %*% t(X) %*% t(Y)
lambdaFM <- rowMeans(lambda)
var_lambdaFM <- c(sum((lambda[1, ] - lambdaFM[1])^2)/nrow(Y)^2, sum((lambda[2, ] - lambdaFM[2])^2)/nrow(Y)^2)

## compute statistics
predicted_mu <- X %*% lambdaFM
rsq <- sum(predicted_mu^2) / sum(mu^2)

## tabulate
tibble(param = c("lambda0", "lambdaM"),
       estimate = lambdaFM,
       se = var_lambdaFM^(1/2),
       t = estimate/se) %>%
  add_row(param = "rsq", estimate = rsq) %>%
  column_to_rownames("param") %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.h.tex")

## plot
### beta vs mean of excess returns
p1 <- tibble(decile = 1:10,
             mu,
             beta) %>%
  ggplot() +
  geom_abline(slope = lambdaFM[2], intercept = lambdaFM[1], linetype = "dashed", color = "blue") +
  geom_text(aes(x = beta, y = mu, label = decile), fontface = "bold", size = 5) +
  labs(x = expression(beta), y = "mean excess return") +
  coord_cartesian(xlim = c(0.8, 1.4), ylim = c(0, 0.05)) +
  theme_few(base_family = "sans", base_size = 14)

ggsave("Q1.h.1.pdf", path = "HW/output/figure/HW3/", plot =  p1, device = "pdf", width = 8, height = 8)

### predicted vs actual
p2 <- tibble(decile = 1:10,
             actual = mu,
             predicted = predicted_mu) %>%
  ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_text(aes(predicted, actual, label = decile), fontface = "bold", size = 5) +
  coord_cartesian(xlim = c(0, 0.05), ylim = c(0, 0.05)) +
  theme_few(base_family = "sans", base_size = 14)

ggsave("Q1.h.2.pdf", path = "HW/output/figure/HW3/", plot =  p2, device = "pdf", width = 8, height = 8)

# i.
## define variables
quarterly_rates_i <- quarterly_rates %>%
  mutate(G = C/lag(C, 1) - Inflation/100) %>%
  mutate_at(.vars = vars(matches("\\d+")), .funs = function(x) (x - quarterly_rates$Bill)/100) %>%
  select(`1`:`10`, G) %>%
  drop_na() %>%
  as.matrix()

Y <- quarterly_rates_i[ ,1:10]
X <- cbind(rep(1, nrow(quarterly_rates_i)), quarterly_rates_i[ ,11])

## run a multivariate regression
B <- solve(t(X) %*% X) %*% t(X) %*% Y
U <- Y - X %*% B
Sigma <- (t(U) %*% U) / nrow(quarterly_rates_f)
coef_cov <- kronecker(solve(t(X) %*% X), Sigma)

## compute statistics
se <- t(matrix(diag(coef_cov)^(1/2), ncol = 2))
t <- B/se
p <- pnorm(abs(t), mean = 0, sd = 1, lower.tail = FALSE) * 2

## tabulate
tab.i <- rbind(B, se, t, p)
row.names(tab.i) <- c("alpha", "beta", "se_alpha", "se_beta", "t_alpha", "t_beta", "p_alpha", "p_beta")
tab.i %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.i.1.tex")

## define variable
mu <- colMeans(Y)

## run Fama-Macbeth regression
beta <- B[2, ]
X <- cbind(rep(1, 10), beta)

lambda <- solve(t(X) %*% X) %*% t(X) %*% t(Y)
lambdaFM <- rowMeans(lambda)
var_lambdaFM <- c(sum((lambda[1, ] - lambdaFM[1])^2)/nrow(Y)^2, sum((lambda[2, ] - lambdaFM[2])^2)/nrow(Y)^2)

## compute statistics
predicted_mu <- X %*% lambdaFM
rsq <- sum(predicted_mu^2) / sum(mu^2)

## tabulate
tibble(param = c("lambda0", "lambdaM"),
       estimate = lambdaFM,
       se = var_lambdaFM^(1/2),
       t = estimate/se) %>%
  add_row(param = "rsq", estimate = rsq) %>%
  column_to_rownames("param") %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q1.i.2.tex")

## plot
### beta vs mean of excess returns
p1 <- tibble(decile = 1:10,
             mu,
             beta) %>%
  ggplot() +
  geom_abline(slope = lambdaFM[2], intercept = lambdaFM[1], linetype = "dashed", color = "blue") +
  geom_text(aes(x = beta, y = mu, label = decile), fontface = "bold", size = 5) +
  labs(x = expression(beta), y = "mean excess return") +
  coord_cartesian(xlim = c(0.3, 1.4), ylim = c(0, 0.05)) +
  theme_few(base_family = "sans", base_size = 14)

ggsave("Q1.i.1.pdf", path = "HW/output/figure/HW3/", plot =  p1, device = "pdf", width = 8, height = 8)

### predicted vs actual
p2 <- tibble(decile = 1:10,
             actual = mu,
             predicted = predicted_mu) %>%
  ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_text(aes(predicted, actual, label = decile), fontface = "bold", size = 5) +
  coord_cartesian(xlim = c(0, 0.05), ylim = c(0, 0.05)) +
  theme_few(base_family = "sans", base_size = 14)

ggsave("Q1.i.2.pdf", path = "HW/output/figure/HW3/", plot =  p2, device = "pdf", width = 8, height = 8)
