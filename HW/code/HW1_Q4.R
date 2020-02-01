ff3 <- readxl::read_excel("HW/input/FF3factorsMonthly.xlsx")
er <- ff3$`Mkt-RF`

mu <- mean(er)
sigma2 <- mean((er - mu)^2)
