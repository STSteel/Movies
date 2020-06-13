library(tidyverse)
library(dplyr)
library(caret)
library(rpart)


# Separate year both training and testing sets
tidy_edx <- edx %>% mutate(year = str_trunc(title, 5, side="left", ellipsis=""))
tidy_edx <- tidy_edx %>% mutate(year = str_trunc(year, 4, side="right", ellipsis=""))
tidy_edx <- tidy_edx %>% mutate(year = as.factor(year))

tidy_validation <- validation %>% mutate(year = str_trunc(title, 5, side="left", ellipsis=""))
tidy_validation <- tidy_validation %>% mutate(year = str_trunc(year, 4, side="right", ellipsis=""))
tidy_validation <- tidy_validation %>% mutate(year = as.factor(year))

# Separate genre combinations 
tidy_edx <- tidy_edx %>% mutate(genre_list = str_split(genres, "\\|"))
tidy_validation <- tidy_validation %>% mutate(genre_list = str_split(genres, "\\|"))

# Change ratings to factors 
tidy_edx <- tidy_edx %>% mutate(rating = as.factor(rating))
tidy_validation <- tidy_validation %>% mutate(rating = as.factor(rating))

# Separate and explore top genres over time and visualise if correlation is apparent
# tidy_edx %>% group_by(genres) %>%
#   summarize(count = n()) %>%
#   arrange(desc(count)) %>%
#   head(., 20)
# 
# drama <- tidy_edx %>% filter(str_detect(genres, "Drama")) %>%
#   mutate(inc_genre = "Drama")
# head(drama)
# 
# comedy <- tidy_edx %>% filter(str_detect(genres, "Comedy")) %>%
#   mutate(inc_genre = "Comedy")
# head(comedy)
# 
# romance <- tidy_edx %>% filter(str_detect(genres, "Romance")) %>%
#   mutate(inc_genre = "Romance")
# head(romance)
# 
# tidy_edx_long <- bind_rows(drama, comedy, romance)
# 
# tidy_edx_long %>% 
#   group_by(year, inc_genre) %>% 
#   summarise(mean = mean(as.numeric(rating)), sd = sd(as.numeric(rating))) %>%
#   ggplot(aes(x=year, y=mean)) +
#   geom_point() +
#   facet_grid(inc_genre ~., scales = "free") 

#NOT USED
#   geom_vline(aes(xintercept=1850)) +
#   ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
#   ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


# action
# 
# thriller
# 
# crime
# 
# scifi
# 
# musical
# 
# adventure
#
# war
#
# horror
#
# children
# 
# animation


# Training 

x <- tidy_edx %>% select (year, genres, movieId, userId) %>% as.matrix()
y <- tidy_edx$rating

fit_tree <- rpart(rating ~ year + genres + movieId + userId, data = tidy_edx) 
rating_hat_tree <- predict(fit_tree, tidy_validation)
rating_hat_tree <- as.factor(rating_hat_tree)
confusionMatrix(data = rating_hat_tree, reference = tidy_validation$rating)$overall[1]
y_hat <- predict(y_lm, tidy_validation) 
RMSE(y_hat, tidy_validation$rating) 

fit_knn <- train(x, y, method = "knn")
fit_knn

library(MASS)
fit_qda <- train(x, y, method='qda')
fit_qda

fit_qda$finalModel$means

# Scaling the columns to better see the parameters that influence the model the most
lda_acc <- train(x, y, method='lda', preProcess="center")
lda_acc

lda_acc$finalModel
lda_acc$finalModel$means


# Testing
y_hat <- predict(fit_rpart, tidy_validation) 
print(c("rpart", RMSE(y_hat, tidy_validation$rating))) 



# Deciding









y_lm <- train %>% lm(y ~ x, data = .)
y_hat <- predict(y_lm, test) # same as y_hat <- y_lm$coef[1] + y_lm$coef[2]*test$x
RMSE(y_hat, test$y) # same as mean((y_hat - test$y)^2)


library(randomForest)
fit_rf <- randomForest(rating ~ year + genres + movieId + userId, 
                       data = tidy_edx, nodesize = 50, maxnodes = 25)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# Trying 
#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# GLM
fit <- train(x_subset, y, method = "glm")
fit$results

tt <- colttests(x, y)
pvals <- tt$p.value
ind <- which(pvals <= 0.01)
length(ind)

# LDA is for single predictor
#fit_lda <- train(x, y, method = "lda", preProcess = c("center"))
#fit_lda$results["Accuracy"]


# 
# titanic %>% filter(!is.na(Age)) %>% group_by(Sex) %>%
#   ggplot(.,aes(x=Age,alpha=0.2, color=Sex)) +
#   geom_histogram(binwidth = 1)
# 
# titanic %>% filter(!is.na(Age)) %>% group_by(Sex) %>%
#   ggplot(.,aes(x=Age,alpha=0.2, color=Sex)) +
#   geom_density()
# 
# thoretical <- seq(0.05, 0.95, 0.05)
# 
# params <- titanic %>%
#   filter(!is.na(Age)) %>%
#   summarize(mean = mean(Age), sd = sd(Age))
# 
# titanic %>% filter(!is.na(Age)) %>%
#   ggplot(.,aes(x=Age,sample(?????))) +
#   geom_qq(dparams = params) + geom_abline(0,1)
# 
# titanic %>% filter(Fare>7 & Fare<9) %>% 
#   group_by(Survived) %>%
#   ggplot(.,aes(x=Survived, y=Fare,color=Survived)) +
#   geom_boxplot() + geom_point() + geom_jitter(position="jitter")
# 
# titanic %>% group_by(Pclass) %>% 
#   ggplot(., aes(x=Age, y=..count.., alpha=0.2, fill=Pclass)) + 
#   geom_density() + facet_grid(Pclass~1)
# 

options(digits = 3)
install.packages("gam")

library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(ggplot2)
data(brca)

str(brca)
mean(brca$y=="M")
str(brca$x)
colnames(brca$x)


# highest mean, lowest sd
which.max(colMeans(brca$x))
which.min(colSds(brca$x))

# Scaling
x_mean_0 <- sweep(brca$x, 2, colMeans(brca$x))
x_mean_0 <- sweep(x_mean_0, 2, colSds(x_mean_0), "/")
sd(x_mean_0[,1])                  
median(x_mean_0[,1])                  

# Distance
d <- dist(x_mean_0)
str(d)

mat_d <- as.matrix(d)[1:569, 1:30] 

mean(mat_d[which(brca$y=="B")])
mean(mat_d[which(brca$y=="M")])


# Heatmap of features
d_features <- dist(t(x_mean_0))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

# Hierarchical Clustering of features (displaying as dendrogram)
h <- hclust(d_features)
plot(h)
groups <- cutree(h, k=5)
str(groups)
names(groups)[groups==3]
plot(groups)

# PCA
pca <- prcomp(x_mean_0)
summary(pca)

str(brca$y) # 1=Benign, 2=Malignant
boxplot(pca$x[,1], brca$y) # PC1
boxplot(pca$x[,2], brca$y) # PC2

temp <- data.frame(pca$x)
temp <- temp %>% mutate(tumour_type = brca$y)
str(temp)
temp %>% ggplot(aes(x=tumour_type, y=PC1)) + geom_boxplot()

# Creating the training and testing sets
#
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_mean_0[test_index,]
test_y <- brca$y[test_index]
train_x <- x_mean_0[-test_index,]
train_y <- brca$y[-test_index]


mean(train_y=="B")
mean(test_y=="B")

# K-means Clustering
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

# k-means clustering with 2 centres
set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later

# Train
k <- kmeans(train_x, centers = 2)
# Predict
y_hat <- predict_kmeans(test_x, k)
y_hat <- as.factor(y_hat)
levels(y_hat) <- levels(test_y)

res <- confusionMatrix(y_hat, test_y)

############ K-Means Clustering results #########
kmeans_y_hat <- y_hat
kmeans_res <- res
#################################################


# Logistic regression
train_set <- as.data.frame(train_x) 
train_set <- train_set %>% mutate(y = as.numeric(train_y=="B"))
glm_fit <- train_set %>% 
  glm(y ~ ., data=., family = "binomial")

# Predict
test_set <- as.data.frame(test_x)
y_hat <- predict(glm_fit, newdata = test_set, type = "response")
y_hat <- ifelse(y_hat>0.5, "B", "M") %>% factor
str(y_hat)

res <- confusionMatrix(y_hat, test_y)
res
############ Logistic regression (GLM) results #########
glm_y_hat <- y_hat
glm_res <- res
########################################################

# LDA model
train_set <- train_set %>% 
  mutate(y = as.factor(ifelse(y==1,"B", "M")))

train_lda <- train(y ~ ., method = "lda", data = train_set)
y_hat <- predict(train_lda, test_x)
res <- confusionMatrix(y_hat, test_y)
res$overall["Accuracy"]
res

############ LDA results #########
lda_y_hat <- y_hat
lda_res <- res
##################################

# QDA model
train_qda <- train(y ~ ., method = "qda", data = train_set)
y_hat <- predict(train_qda, test_x)
res <- confusionMatrix(y_hat, test_y)
res$overall["Accuracy"]
res

############ QDA results #########
qda_y_hat <- y_hat
qda_res <- res
##################################

# Loess model
set.seed(5, sample.kind = "Rounding")    # if using R 3.6 or later
train_loess <- train(y ~ ., method = "gamLoess", data = train_set)
y_hat <- predict(train_loess, test_x)
res <- confusionMatrix(y_hat, test_y)
res$overall["Accuracy"]
res

############ Loess results #########
loess_y_hat <- y_hat
loess_res <- res
##################################

# KNN model
set.seed(7, sample.kind = "Rounding")    # if using R 3.6 or later

# ks <- seq(3, 21, 2)
# F_1 <- sapply(ks, function(k){
#   train_knn <- knn3(y ~ ., data = train_set, k = k)
#   y_hat <- predict(train_knn, as.data.frame(test_x), type = "class") %>% 
#     factor(levels = levels(test_y))
#   F_meas(data = y_hat, reference = test_y)
# })
# plot(ks, F_1)
# max(F_1)
# ks[which.max(F_1)]
# F_meas(data = y_hat, reference = test_y)

train_knn <- train(y ~ ., method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$finalModel

y_hat <- predict(train_knn, as.data.frame(test_x), type = "raw") %>%
  factor(levels = levels(test_y))

res <- confusionMatrix(y_hat, test_y)
res$overall["Accuracy"]
res

############ KNN results #########
knn_y_hat <- y_hat
knn_res <- res
##################################

# Random forest model
library(randomForest)
set.seed(9, sample.kind = "Rounding")    # if using R 3.6 or later

control <- trainControl(method="cv", number = 15)
grid <- data.frame(mtry = c(3, 5, 7, 9))

train_rf <-  train(y ~ ., method = "rf", 
                   data = train_set,
                   trControl = control,
                   tuneGrid = grid,
                   importance = TRUE)

ggplot(train_rf)
train_rf$bestTune
varImp(train_rf)

y_hat <- predict(train_rf, as.data.frame(test_x)) %>%
  factor(levels = levels(test_y))

res <- confusionMatrix(y_hat, test_y)
res$overall["Accuracy"]
res
############ Random Forest results #########
rf_y_hat <- y_hat
rf_res <- res
############################################



# ENSEMBLE #
y_set <- data.frame(kmeans_y_hat = ifelse(kmeans_y_hat=="B", 1, 0))
y_set <- y_set %>% mutate(glm_y_hat = ifelse(glm_y_hat=="B", 1, 0))
y_set <- y_set %>% mutate(lda_y_hat = ifelse(lda_y_hat=="B", 1, 0))
y_set <- y_set %>% mutate(qda_y_hat = ifelse(qda_y_hat=="B", 1, 0))
y_set <- y_set %>% mutate(loess_y_hat = ifelse(loess_y_hat=="B", 1, 0))
y_set <- y_set %>% mutate(knn_y_hat = ifelse(knn_y_hat=="B", 1, 0))
y_set <- y_set %>% mutate(rf_y_hat = ifelse(rf_y_hat=="B", 1, 0))

y_set <- y_set %>% mutate(ens_y_hat = round((kmeans_y_hat+
                                               glm_y_hat+
                                               lda_y_hat+
                                               qda_y_hat+
                                               loess_y_hat+
                                               knn_y_hat+
                                               rf_y_hat)/7))


y_set <- y_set %>% mutate(y_hat = as.factor(ifelse(ens_y_hat==1, "B", "M")))
levels(y_set$y_hat) <- levels(test_y)
head(y_set)
ens_acc <- confusionMatrix(y_set$y_hat, test_y)$overall["Accuracy"]

res <- data.frame(model="Kmeans", acc=as.numeric(kmeans_res$overall["Accuracy"]))
res <- rbind(res, data.frame(model="glm", acc=as.numeric(glm_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="lda", acc=as.numeric(lda_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="qda", acc=as.numeric(qda_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="loess", acc=as.numeric(loess_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="knn", acc=as.numeric(knn_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="rf", acc=as.numeric(rf_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="ensemble", acc=ens_acc))
res

