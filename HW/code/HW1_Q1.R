library(tidyverse)
library(stargazer)
ff3 <- readxl::read_excel("HW/input/FF3factorsMonthly.xlsx")

ff3 %>%
  select(-Date) %>% 
  as.data.frame() %>%
  stargazer(type = "text", out = "HW/output/tabs/HW1/Q1.a.tex", float = FALSE,
            style = "qje", summary = TRUE, summary.stat = c("n", "mean", "sd",  "min", "p25", "median", "p75", "max"), digits = 3)

