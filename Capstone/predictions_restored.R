library(dslabs)
library(tidyverse)
library(caret)

# build 100 linear models using the data above and calculate 
# the mean and standard deviation of the combined models.

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100

#======= Why 9*, and why 4 sds? Sigma is how MASS creates correlation between variables ========
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

Sigma
str(dat)

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
rmse_res <- replicate(n, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  y_lm <- train %>% lm(y ~ x, data = .)
  y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
  RMSE(y_hat, test$y) # same as mean((y_hat - test$y)^2)
})
mean(rmse_res)
sd(rmse_res)

# Looking at squared errors across multiple sizes of observations

rmse_fun <- function (obs_size) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = obs_size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse_res <- replicate(100, {
    test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]
    y_lm <- train %>% lm(y ~ x, data = .)
    y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
    RMSE(y_hat, test$y) 
  })
  c(obs_size, mean(rmse_res), sd(rmse_res))
}

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)
rmse_vec <- sapply(n, rmse_fun)

str(rmse_vec)
head(rmse_vec)


# ===== INCREASING CORRELATION between x and y ========
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100

#======= Why 9*, and why 4 sds? Sigma is how MASS creates correlation between variables ========
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)

dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

Sigma
str(dat)

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
rmse_res <- replicate(n, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  y_lm <- train %>% lm(y ~ x, data = .)
  y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
  RMSE(y_hat, test$y) # same as mean((y_hat - test$y)^2)
})

mean(rmse_res)
sd(rmse_res)

# === Two independent predictors ====
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

Sigma
str(dat)
cor(dat)

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]

str(test_index)
str(train)
str(test)

# x_1
y_lm <- train %>% lm(y ~ x_1, data = .)
y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
print(c("x_1", RMSE(y_hat, test$y))) # same as mean((y_hat - test$y)^2)

# x_2
y_lm <- train %>% lm(y ~ x_2, data = .)
y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
print(c("x_2", RMSE(y_hat, test$y))) # same as mean((y_hat - test$y)^2)

# x_1 and x_2
y_lm <- train %>% lm(y ~ x_1 + x_2, data = .)
y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
print(c("x_1 and x_2", RMSE(y_hat, test$y))) # same as mean((y_hat - test$y)^2)

# === Two HIGHLY CORRELATED predictors ====
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
test <- dat[test_index,]
train <- dat[-test_index,]

# x_1
y_lm <- train %>% lm(y ~ x_1, data = .)
y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
print(c("x_1", RMSE(y_hat, test$y))) # same as mean((y_hat - test$y)^2)

# x_2
y_lm <- train %>% lm(y ~ x_2, data = .)
y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
print(c("x_2", RMSE(y_hat, test$y))) # same as mean((y_hat - test$y)^2)

# x_1 and x_2
y_lm <- train %>% lm(y ~ x_1 + x_2, data = .)
y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
print(c("x_1 and x_2", RMSE(y_hat, test$y))) # same as mean((y_hat - test$y)^2)
