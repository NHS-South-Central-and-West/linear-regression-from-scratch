library(dplyr)
library(ggplot2)

# import data
df <- 
  readr::read_csv(here::here("data", "student-performance.csv")) |> 
  filter(exam_score < 80)

# select predictor
X <- df$hours_studied

# select outcome
y <- df$exam_score

# get summary stats
n <- length(x)
x_mean <- mean(x)
y_mean <- mean(y)

# calculate slope (b1)
beta_1 <- sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean)^2)

# calculate intercept (b0)
beta_0 <- y_mean - beta_1 * x_mean

# predict y
y_pred <- beta_0 + beta_1 * x

# Compare with lm()
model <- lm(exam_score ~ hours_studied, data = df)
model_coef <- coef(model)

# intercept estimate
beta_0

# slope estimate
beta_1

# lm results
print(model_coef)
summary(model)