#' Load required data
ff3 <- readxl::read_excel("HW/input/FF3factorsMonthly.xlsx")
r <- ff3$`Mkt-RF`

n <- length(r)

#. a. GMM estimators
mu <- mean(r)
sigma2 <- mean((r - mu)^2)

#. and Covariance matrix under the no autocorelation assumption.
S <- matrix(rep(0, 4), nrow = 2, ncol = 2)
S[1, 1] <- mean((r - mu)^2)
S[1, 2] <- mean((r - mu) * ((r - mu)^2 - sigma2))
S[2, 1] <- mean((r - mu) * ((r - mu)^2 - sigma2))
S[2, 2] <- mean(((r - mu)^2 - sigma2)^2)

D <- diag(-1, nrow = 2, ncol = 2)

V <- solve(D %*% solve(S) %*% t(D))

se_mu <- (V[1,1]/n)^(1/2)
se_sigma2 <- (V[2,2]/n)^(1/2)

print(c(se_mu, se_sigma2))

#' b. Under the Normality
sigma2 <- mean((r - mu)^2)
se_mu <- (sigma2/n)^(1/2)
se_sigma2 <- ((2 * (n - 1) * sigma2^2)/(n^2))^(1/2)

print(c(se_mu, se_sigma2))

#' c. Newey-West Covariance Matrix
lag_r <- lag(r, 1)
lag_S <- matrix(rep(0, 4), nrow = 2, ncol = 2)
lag_S[1, 1] <- sum((r[2:n] - mu) * (lag_r[2:n] - mu))/n
lag_S[1, 2] <- sum((r[2:n] - mu) * ((lag_r[2:n] - mu)^2 - sigma2))/n
lag_S[2, 1] <- sum((lag_r[2:n] - mu) * ((r[2:n] - mu)^2 - sigma2))/n
lag_S[2, 2] <- sum(((r[2:n] - mu)^2 - sigma2) * ((lag_r[2:n] - mu)^2 - sigma2))/n

S_tilde <- S + (1 - 1/2) * (lag_S + t(lag_S))

V_tilde <- solve(D %*% solve(S_tilde) %*% t(D))

se_mu <- (V_tilde[1,1]/n)^(1/2)
se_sigma2 <- (V_tilde[2,2]/n)^(1/2)

print(c(se_mu, se_sigma2))
