set.seed(2020)

n <- 840
alpha <- theta <- 0
beta <- 0.210
rho <- 0.972

mu <- c(0,0)

sigma_u2 <- 0.0030050
sigma_e2 <- 0.0000108
sigma_ue <- -0.0001621

Sigma <- matrix(c(sigma_u2, sigma_ue, sigma_ue, sigma_e2), nrow = 2, ncol = 2)

ue_mat <- MASS::mvrnorm(n, mu, Sigma)
x0 <- 0

u_vec <- ue[ , 1]
e_vec <- ue[ , 2]

calculate_forwardx <- function(x, e){
  xforward <- theta + rho * x + e
  
  xforward
}

simulate_x <- function(n, x0){
  x_vec <- x0
  
  for (i in 1:n) {
    xforward <- calculate_forwardx(x_vec[i], e_vec[i])
    
    x_vec <- append(x_vec, xforward)
  }
  
  x_vec
}

x_vec <- simulate_x(n, x0)
r_vec <- alpha + beta * x_vec[1:(n - 1)] + u_vec

