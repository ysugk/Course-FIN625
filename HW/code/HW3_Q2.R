# load required packages
library(tidyverse)
library(stargazer)

# read data: 196307--201902
ff5 <- read_csv("HW/input/FF5factors.CSV", skip = 2) %>%
  slice(1:668) %>%
  mutate_at(vars(2:7), as.numeric)

ff25 <- read_csv("HW/input/FF25.CSV", skip = 15) %>%
  slice(1:1112) %>%
  filter(X1 >= 196307)

ff25 <- apply(ff25[ ,2:26], 2, function(x) (x - ff5$RF)/100)

# a.
## compute mean
ff25_mean <- apply(ff25, 2, mean) %>%
  matrix(nrow = 5)

## tabulate
colnames(ff25_mean) <- c("Small", 2, 3, 4, "Big")
row.names(ff25_mean) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(ff25_mean, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.a.1.tex")

## compute standard deviation
ff25_sd <- apply(ff25, 2, sd) %>%
  matrix(nrow = 5)

## tabulate
colnames(ff25_sd) <- c("Small", 2, 3, 4, "Big")
row.names(ff25_sd) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(ff25_sd, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.a.2.tex")

## compute mean
ff5 <- apply(ff5[ ,2:6], 2, function(x) x/100)
ff5_mean <- apply(ff5, 2, mean)

## compute standard deviation
ff5_sd <- apply(ff5, 2, sd)

## tabulate
ff5_summary <- rbind(ff5_mean, ff5_sd)
row.names(ff5_summary) <- c("mean", "sd")
stargazer(ff5_summary, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.a.3.tex")

# b.
## define variable
X <- cbind(rep(1, nrow(ff5)), ff5[ ,1])

## run a multivariate regression 
B <- solve(t(X) %*% X) %*% t(X) %*% ff25
predicted_market <- ff5[,1] %*% t(B[2,])

## compute statistics
U <- ff25 - X %*% B
Sigma <- (t(U) %*% U) / nrow(ff25)
coef_cov <- kronecker(solve(t(X) %*% X), Sigma)
se <- t(matrix(diag(coef_cov)^(1/2), ncol = 2))
t <- B/se
p <- pnorm(abs(t), mean = 0, sd = 1, lower.tail = FALSE) * 2

## tabualte
tab.b.1 <- matrix(B[1, ], 5)
colnames(tab.b.1) <- c("Small", 2, 3, 4, "Big")
row.names(tab.b.1) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.b.1, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.b.1.tex")

tab.b.2 <- matrix(se[1, ], 5)
colnames(tab.b.2) <- c("Small", 2, 3, 4, "Big")
row.names(tab.b.2) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.b.2, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.b.2.tex")

tab.b.3 <- matrix(t[1, ], 5)
colnames(tab.b.3) <- c("Small", 2, 3, 4, "Big")
row.names(tab.b.3) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.b.3, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.b.3.tex")

tab.b.4 <- matrix(p[1, ], 5)
colnames(tab.b.4) <- c("Small", 2, 3, 4, "Big")
row.names(tab.b.4) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.b.4, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.b.4.tex")

# c.
## run GRS test
vcov <- (t(U) %*% U)/(nrow(U) - ncol(X))
rm_mean <- mean(ff5[ ,1])
rm_sd <- mean((ff5[ ,1] - rm_mean)^2)^(1/2)

df1 <- ncol(ff25)
df2 <- nrow(ff25) - ncol(ff25) - 1

fstat <- (df2/df1) * ((1 + (rm_mean/rm_sd)^2)^(-1)) * (t(B[1, ]) %*% solve(vcov) %*% B[1, ])
pvalue <- pf(fstat, df1, df2, lower.tail = FALSE)

# d.
## ff3
### define variables
X <- cbind(rep(1, nrow(ff5)), ff5[ ,1:3])

### run a multivariate regression
B <- solve(t(X) %*% X) %*% t(X) %*% ff25
predicted_ff3 <- ff5[ ,1:3] %*% B[2:4, ]

### compute statistics
U <- ff25 - X %*% B
Sigma <- (t(U) %*% U) / nrow(ff25)
coef_cov <- kronecker(solve(t(X) %*% X), Sigma)

se <- t(matrix(diag(coef_cov)^(1/2), ncol = 4))
t <- B/se
p <- pnorm(abs(t), mean = 0, sd = 1, lower.tail = FALSE) * 2

### tabulate
tab.d.a1 <- matrix(B[1, ], 5)
colnames(tab.d.a1) <- c("Small", 2, 3, 4, "Big")
row.names(tab.d.a1) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.d.a1, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.d.a1.tex")

tab.d.a2 <- matrix(se[1, ], 5)
colnames(tab.d.a2) <- c("Small", 2, 3, 4, "Big")
row.names(tab.d.a2) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.d.a2, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.d.a2.tex")

tab.d.a3 <- matrix(t[1, ], 5)
colnames(tab.d.a3) <- c("Small", 2, 3, 4, "Big")
row.names(tab.d.a3) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.d.a3, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.d.a3.tex")

tab.d.a4 <- matrix(p[1, ], 5)
colnames(tab.d.a4) <- c("Small", 2, 3, 4, "Big")
row.names(tab.d.a4) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.d.a4, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.d.a4.tex")

### run GRS test
vcov <- (t(U) %*% U)/(nrow(U) - ncol(X))
factor_mean <- colMeans(ff5[ ,1:3])
factor_vcov <- (t(ff5[ ,1:3]) %*% ff5[ ,1:3])/(nrow(ff5) - 1)

df1 <- ncol(ff25)
df2 <- nrow(ff25) - ncol(ff25) - 3

fstat_ff3 <- (df2/df1) * ((1 + (t(factor_mean) %*% solve(factor_vcov) %*% factor_mean))^(-1)) * (t(B[1, ]) %*% solve(vcov) %*% B[1, ])
pvalue_ff3 <- pf(fstat_ff3, df1, df2, lower.tail = FALSE)

## ff5
### define variable
X <- cbind(rep(1, nrow(ff5)), ff5[ ,1:5])

### run a multivariate regression
B <- solve(t(X) %*% X) %*% t(X) %*% ff25
predicted_ff5 <- ff5[ ,1:5] %*% B[2:6, ]

### compute statistics
U <- ff25 - X %*% B
Sigma <- (t(U) %*% U) / nrow(ff25)
coef_cov <- kronecker(solve(t(X) %*% X), Sigma)

se <- t(matrix(diag(coef_cov)^(1/2), ncol = 6))
t <- B/se
p <- pnorm(abs(t), mean = 0, sd = 1, lower.tail = FALSE) * 2

### tabulate
tab.d.b1 <- matrix(B[1, ], 5)
colnames(tab.d.b1) <- c("Small", 2, 3, 4, "Big")
row.names(tab.d.b1) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.d.b1, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.d.b1.tex")

tab.d.b2 <- matrix(se[1, ], 5)
colnames(tab.d.b2) <- c("Small", 2, 3, 4, "Big")
row.names(tab.d.b2) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.d.b2, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.d.b2.tex")

tab.d.b3 <- matrix(t[1, ], 5)
colnames(tab.d.b3) <- c("Small", 2, 3, 4, "Big")
row.names(tab.d.b3) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.d.b3, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.d.b3.tex")

tab.d.b4 <- matrix(p[1, ], 5)
colnames(tab.d.b4) <- c("Small", 2, 3, 4, "Big")
row.names(tab.d.b4) <- c("LoBM", 2, 3, 4, "HiBM")
stargazer(tab.d.b4, type = "text", float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.d.b4.tex")

### run a GRS test
vcov <- (t(U) %*% U)/(nrow(U) - ncol(X))
factor_mean <- colMeans(ff5[ ,1:5])
factor_vcov <- (t(ff5[ ,1:5]) %*% ff5[ ,1:5])/(nrow(ff5) - 1)

df1 <- ncol(ff25)
df2 <- nrow(ff25) - ncol(ff25) - 5

fstat_ff5 <- (df2/df1) * ((1 + (t(factor_mean) %*% solve(factor_vcov) %*% factor_mean))^(-1)) * (t(B[1, ]) %*% solve(vcov) %*% B[1, ])
pvalue_ff5 <- pf(fstat_ff3, df1, df2, lower.tail = FALSE)

### tabulate
tibble("model" = c("market", "FF3", "FF5"),
       "Fstat" = c(fstat, fstat_ff3, fstat_ff5),
       "pvalue" = c(pvalue, pvalue_ff3, pvalue_ff5)) %>%
  column_to_rownames("model") %>%
  stargazer(type = "text", summary = FALSE, float = FALSE, align = TRUE, out = "HW/output/tabs/HW3/Q2.c.tex")

# e.
## load a required package
library(ggthemes)

## plot
### market model
p1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_point(aes(x = colMeans(predicted_market), y = colMeans(ff25))) +
  labs(x = "predicted", y = "actual", title = "Market") +
  coord_cartesian(xlim = c(0, 0.015), ylim = c(0, 0.015)) +
  theme_few(base_family = "sans", base_size = 14)

ggsave("Q2.e.1.pdf", path = "HW/output/figure/HW3/", plot =  p1, device = "pdf", width = 8, height = 8)

### ff3
p2 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_point(aes(x = colMeans(predicted_ff3), y = colMeans(ff25))) +
  labs(x = "predicted", y = "actual", title = "FF3") +
  coord_cartesian(xlim = c(0, 0.015), ylim = c(0, 0.015)) +
  theme_few(base_family = "sans", base_size = 14)

ggsave("Q2.e.2.pdf", path = "HW/output/figure/HW3/", plot =  p2, device = "pdf", width = 8, height = 8)

### ff5
p3 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  geom_point(aes(x = colMeans(predicted_ff5), y = colMeans(ff25))) +
  labs(x = "predicted", y = "actual", title = "FF5") +
  coord_cartesian(xlim = c(0, 0.015), ylim = c(0, 0.015)) +
  theme_few(base_family = "sans", base_size = 14)

ggsave("Q2.e.3.pdf", path = "HW/output/figure/HW3/", plot =  p3, device = "pdf", width = 8, height = 8)
