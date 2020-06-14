library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(matrixStats)
library(ggplot2)

options(digits = 3)
install.packages("gam")



# Separate year both training and testing sets
tidy_edx <- edx %>% mutate(year = str_trunc(title, 5, side="left", ellipsis=""))
tidy_edx <- tidy_edx %>% mutate(year = str_trunc(year, 4, side="right", ellipsis=""))
tidy_edx <- tidy_edx %>% mutate(year = as.numeric(year))

tidy_validation <- validation %>% mutate(year = str_trunc(title, 5, side="left", ellipsis=""))
tidy_validation <- tidy_validation %>% mutate(year = str_trunc(year, 4, side="right", ellipsis=""))
tidy_validation <- tidy_validation %>% mutate(year = as.numeric(year))

tidy_edx %>% mutate(comedy=ifelse(str_detect(genres, "Comedy"), 1, 0),
                    action=ifelse(str_detect(genres, "Action"), 1, 0),
                    romance=ifelse(str_detect(genres, "Romance"), 1, 0),
                    thriller=ifelse(str_detect(genres, "Triller"), 1, 0),
                    adventure=ifelse(str_detect(genres, "Adventure"), 1, 0),
                    scifi=ifelse(str_detect(genres, "Sci-Fi"), 1, 0),
                    drama=ifelse(str_detect(genres, "Drama"), 1, 0),
                    children=ifelse(str_detect(genres, "Children"), 1, 0),
                    fantasy=ifelse(str_detect(genres, "Fantasy"), 1, 0),
                    mystery=ifelse(str_detect(genres, "Mystery"), 1, 0),
                    horror=ifelse(str_detect(genres, "Horror"), 1, 0),
                    western=ifelse(str_detect(genres, "Western"), 1, 0),
                    war=ifelse(str_detect(genres, "War"), 1, 0),
                    musical=ifelse(str_detect(genres, "Musical"), 1, 0),
                    crime=ifelse(str_detect(genres, "Crime"), 1, 0),
                    documentary=ifelse(str_detect(genres, "Documentary"), 1, 0),
                    animation=ifelse(str_detect(genres, "Animation"), 1, 0)) %>%
  gather("genre_name", "genre_value", comedy:animation) %>%
  group_by(genre_name) %>%
  summarise(s=sum(genre_value)) %>%
  ggplot(aes(x=genre_name, y=s)) +
  geom_point() 

# Genres that have over 1million entries:
# Action
# Adventure
# Comedy
# Crime
# Drama
# Romance
# Sci-Fi

# tidy_edx %>% mutate(action=ifelse(str_detect(genres, "Action"), 1, 0),
#                     adventure=ifelse(str_detect(genres, "Adventure"), 1, 0),
#                     comedy=ifelse(str_detect(genres, "Comedy"), 1, 0),
#                     crime=ifelse(str_detect(genres, "Crime"), 1, 0),
#                     drama=ifelse(str_detect(genres, "Drama"), 1, 0),
#                     romance=ifelse(str_detect(genres, "Romance"), 1, 0),
#                     scifi=ifelse(str_detect(genres, "Sci-Fi"), 1, 0)) %>%
#   gather("genre_name", "genre_value", action:scifi) %>%
#   group_by(genre_name) %>%
#   summarise(m=mean(rating)) 
#NOT USED
#   facet_grid(rat ~., scales = "free") 
#   geom_vline(aes(xintercept=1850)) +
#   ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
#   ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


# Separate genre combinations 
tidy_edx <- tidy_edx %>% mutate(action=ifelse(str_detect(genres, "Action"), 1, 0),
                                adventure=ifelse(str_detect(genres, "Adventure"), 1, 0),
                                comedy=ifelse(str_detect(genres, "Comedy"), 1, 0),
                                crime=ifelse(str_detect(genres, "Crime"), 1, 0),
                                drama=ifelse(str_detect(genres, "Drama"), 1, 0),
                                romance=ifelse(str_detect(genres, "Romance"), 1, 0),
                                scifi=ifelse(str_detect(genres, "Sci-Fi"), 1, 0))
tidy_edx <- tidy_edx %>% select(userId, movieId, rating, year, action:scifi)

tidy_validation <- tidy_validation %>% mutate(action=ifelse(str_detect(genres, "Action"), 1, 0),
                                              adventure=ifelse(str_detect(genres, "Adventure"), 1, 0),
                                              comedy=ifelse(str_detect(genres, "Comedy"), 1, 0),
                                              crime=ifelse(str_detect(genres, "Crime"), 1, 0),
                                              drama=ifelse(str_detect(genres, "Drama"), 1, 0),
                                              romance=ifelse(str_detect(genres, "Romance"), 1, 0),
                                              scifi=ifelse(str_detect(genres, "Sci-Fi"), 1, 0))
tidy_validation <- tidy_validation %>% select(userId, movieId, rating, year, action:scifi)

########################
# Improving on Naive Bayes to minimise error: 
# mean + movie bias + user bias 
# (and adopting the best regularisation parameter)

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  m_movie <- mean(edx$rating)
  b_m <- edx %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - m_movie)/(n()+l))
  b_u <- edx %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - m_movie)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = m_movie + b_m + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Calculating average of all movies for all users 
m_movie <- mean(edx_tidy$rating) 

# How much each movie varies from the average, regularising it (lambda <- 3)
tab_movie <- tidy_edx %>% 
  group_by(movieId) %>% 
  summarize(b_m = sum(rating - m_movie)/(n()+3), n_movies = n())

# How much each user varies from the average
tab_user <- tidy_edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - m_movie))

# Applying both deviations from average to make predictions
predicted_ratings <- edx_validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = m_movie + b_m + b_u) %>%
  .$pred

# Checking accuracy and RMSE
bias_rmse <- RMSE(predicted_ratings$pred, edx_validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = bias_rmse ))
rmse_results %>% knitr::kable()
########################

x <- tidy_edx %>% select(userId, movieId, action:scifi) %>% as.matrix()
y <- tidy_edx$rating %>% as.factor()
x_validation <- tidy_validation %>% select(userId, movieId, action:scifi) %>% as.matrix()
y_validation <- tidy_validation$rating %>% as.factor()

# K=5 by default
knn_fit <- knn3(x,y)
y_hat <- predict(knn_fit, x_validation, type = "class")

############ KNN results #########
knn_y_hat <- y_hat
knn_res <- confusionMatrix(y_hat, y_validation)
rmse_res <- RMSE(y_hat, tidy_validation$rating)
#################################################

# 
# library(randomForest)
# fit_rf <- randomForest(rating ~ year + genres + movieId + userId, 
#                        data = tidy_edx, nodesize = 50, maxnodes = 25)

# d_features <- dist(t(x))
# # heatmap(as.matrix(d_features), labRow = NA, labCol = NA)
# 
# # Hierarchical Clustering of features (displaying as dendrogram)
# h <- hclust(d_features)
# plot(h)
# groups <- cutree(h, k=5)
# str(groups)
# names(groups)[groups==3]
# plot(groups)


# x <- tidy_edx %>% select (year, movieId, userId, comedy:animation) %>% as.matrix()
# y <- tidy_edx$rating
# y_num <- as.numeric(as.character(y))
# hist(y_num)
# 
# ### Conclusion: whole ratings far outweight half ratings => round it up)
# y_num <- round(y_num)
# hist(y_num)
# y <- as.factor(y_num)
# levels(y) <- levels(tidy_edx$rating)


# K-means Clustering
# predict_kmeans <- function(x, k) {
#   centers <- k$centers    # extract cluster centers
#   # calculate distance to cluster centers
#   distances <- sapply(1:nrow(x), function(i){
#     apply(centers, 1, function(y) dist(rbind(x[i,], y)))
#   })
#   max.col(-t(distances))  # select cluster with min distance to center
# }
# 
# # k-means clustering with 2 centres
# set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later
# 
# # Train - UserID
# k <- kmeans(as.numeric(x[,3]), centers = 2)
# # Predict
# y_hat <- predict_kmeans(as.matrix(tidy_validation$userId), k)


fit_tree <- rpart(rating ~ ., data = tidy_edx)

y_hat <- predict(fit_tree, tidy_validation)
y_hat <- as.factor(y_hat)
levels(y_hat) <- levels(y)

confusionMatrix(data = y_hat, reference = tidy_validation$rating)$overall[1]
y_hat <- predict(y_lm, tidy_validation)

############ Classification tree (rpart) results #########
rpart_y_hat <- y_hat
rpart_res <- confusionMatrix(y_hat, tidy_validation$rating)
rpart_rmse <- RMSE(y_hat, tidy_validation$rating)
#################################################


# Logistic regression
glm_fit <- tidy_edx %>% 
  glm(rating ~ ., data=., family = "binomial")

# Predict
y_hat <- predict(glm_fit, newdata = tidy_validation, type = "response")
y_hat <- round(y_hat) %>% factor
str(y_hat)

############ Logistic regression (GLM) results #########
glm_y_hat <- y_hat
glm_res <- confusionMatrix(y_hat, tidy_validation$rating)
glm_rmse <- RMSE(y_hat, tidy_validation$rating)
########################################################

# LDA model
train_lda <- train(rating ~ ., method = "lda", data = tidy_edx)
y_hat <- predict(train_lda, tidy_validation)

############ LDA results #########
lda_y_hat <- y_hat
lda_res <- confusionMatrix(y_hat, tidy_validation$rating)
lda_rmse <- RMSE(y_hat, tidy_validation$rating)
##################################

# QDA model
train_qda <- train(rating ~ ., method = "qda", data = tidy_edx)
y_hat <- predict(train_qda, tidy_validation)

############ QDA results #########
qda_y_hat <- y_hat
qda_res <- confusionMatrix(y_hat, tidy_validation$rating)
qda_rmse <- RMSE(y_hat, tidy_validation$rating)
##################################

# Loess model
set.seed(5, sample.kind = "Rounding")    # if using R 3.6 or later
train_loess <- train(rating ~ ., method = "gamLoess", data = tidy_edx)
y_hat <- predict(train_loess, tidy_validation)

############ Loess results #########
loess_y_hat <- y_hat
loess_res <- confusionMatrix(y_hat, tidy_validation$rating)
loess_rmse <- RMSE(y_hat, tidy_validation$rating)
##################################

# KNN model
train_knn <- train(rating ~ ., method = "knn", 
                   data = tidy_edx,
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
# ggplot(train_knn, highlight = TRUE)
# train_knn$finalModel

# KNN model
train_knn <- train(rating ~ ., method = "knn", 
                   data = tidy_edx,
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
# ggplot(train_knn, highlight = TRUE)
# train_knn$finalModel

y_hat <- predict(train_knn, tidy_validation, type = "raw") %>%
  factor(levels = levels(tidy_validation$rating))

############ KNN results #########
knn_y_hat <- y_hat
knn_res <- confusionMatrix(y_hat, tidy_validation$rating)
knn_rmse <- RMSE(y_hat, tidy_validation$rating)
##################################

# Random forest model
library(randomForest)
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later

control <- trainControl(method="cv", number = 15)
grid <- data.frame(mtry = c(3, 5, 7, 9))

train_rf <-  train(rating ~ ., method = "rf", 
                   data = train_set,
                   trControl = control,
                   tuneGrid = grid,
                   importance = TRUE)

y_hat <- predict(train_rf, tidy_edx) %>%
  factor(levels = levels(tidy_validation$rating))

############ Random Forest results #########
rf_y_hat <- y_hat
rf_res <- confusionMatrix(y_hat, tidy_validation$rating)
rf_rmse <- RMSE(y_hat, tidy_validation$rating)
############################################



# ENSEMBLE #
y_set <- data.frame(rpart_y_hat = rpart_y_hat)
y_set <- y_set %>% mutate(glm_y_hat = glm_y_hat)
y_set <- y_set %>% mutate(lda_y_hat = lda_y_hat)
y_set <- y_set %>% mutate(qda_y_hat = qda_y_hat)
y_set <- y_set %>% mutate(loess_y_hat = loess_y_hat)
y_set <- y_set %>% mutate(knn_y_hat = knn_y_hat)
y_set <- y_set %>% mutate(rf_y_hat = rf_y_hat)

y_set <- y_set %>% mutate(ens_y_hat = round((rpart_y_hat+
                                               glm_y_hat+
                                               lda_y_hat+
                                               qda_y_hat+
                                               loess_y_hat+
                                               knn_y_hat+
                                               rf_y_hat)/7))

y_set <- y_set %>% mutate(y_hat = as.factor(ens_y_hat))
levels(y_set$y_hat) <- levels(tidy_validation$rating)
head(y_set)
ens_acc <- confusionMatrix(y_set$y_hat, tidy_validation$rating)$overall["Accuracy"]
ens_rmse <- RMSE(y_set$y_hat, tidy_validation$rating)

res <- data.frame(model="rpart", acc=as.numeric(rpart_res$overall["Accuracy"]))
res <- rbind(res, data.frame(model="glm", acc=as.numeric(glm_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="lda", acc=as.numeric(lda_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="qda", acc=as.numeric(qda_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="loess", acc=as.numeric(loess_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="knn", acc=as.numeric(knn_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="rf", acc=as.numeric(rf_res$overall["Accuracy"])))
res <- rbind(res, data.frame(model="ensemble", acc=ens_acc))
res

rmse <- data.frame(model="rpart", rmse=rpart_rmse)
rmse <- rbind(rmse, data.frame(model="glm", rmse=glm_rmse))

rmse <- rbind(rmse, data.frame(model="lda", rmse=lda_rmse))
rmse <- rbind(rmse, data.frame(model="qda", rmse=qda_rmse))
rmse <- rbind(rmse, data.frame(model="loess", rmse=loess_rmse))
rmse <- rbind(rmse, data.frame(model="knn", rmse=knn_rmse))
rmse <- rbind(rmse, data.frame(model="rf", rmse=rf_rmse))
rmse <- rbind(rmse, data.frame(model="ensemble", rmse=ens_rmse))
rmse
