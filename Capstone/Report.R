##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(stringr)
library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(ggplot2)
library(tinytex)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
# Structure and sample rows
##########################################################
str(edx)
head(edx)

##########################################################
# Analysing rating and time relationship
##########################################################

# Glancing at decades, it appears that average ratings have been declining
tidy_edx <- edx %>% mutate(year = str_trunc(title, 5, side="left", ellipsis=""))
tidy_edx <- tidy_edx %>% mutate(year = str_trunc(year, 4, side="right", ellipsis=""))
tidy_edx <- tidy_edx %>% mutate(year = as.numeric(year))
tidy_edx <- tidy_edx %>% mutate(decade = round(year - 1900)/10)
tidy_edx %>%
  group_by(decade) %>%
  summarise(m=mean(rating)) %>%
  ggplot(aes(x=decade, y=m)) +
  geom_point()

##########################################################
# Analysing rating and genre relationship
##########################################################

# Glancing at genres, it appears that the most common ones don't have any significant relationship to ratings
tidy_edx_genre <- edx %>% mutate(comedy=ifelse(str_detect(genres, "Comedy"), 1, 0),
                                 action=ifelse(str_detect(genres, "Action"), 1, 0),
                                 romance=ifelse(str_detect(genres, "Romance"), 1, 0),
                                 thriller=ifelse(str_detect(genres, "Thriller"), 1, 0),
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
  gather("genre_name", "genre_value", comedy:animation) 

tidy_edx_genre %>%
  group_by(genre_name) %>%
  summarise(s=sum(genre_value)) %>%
  arrange(desc(s))

rm(tidy_edx_genre) # free up memory

##########################################################
# Exploring genre and time together
##########################################################

# Genres that have over 1million entries: Action, Adventure, Comedy, Crime, Drama, Romance, Sci-Fi
# Is there a trend by genre over time?
tidy_edx <- tidy_edx %>% mutate(action=ifelse(str_detect(genres, "Action"), 1, 0),
                                adventure=ifelse(str_detect(genres, "Adventure"), 1, 0),
                                comedy=ifelse(str_detect(genres, "Comedy"), 1, 0),
                                crime=ifelse(str_detect(genres, "Crime"), 1, 0),
                                drama=ifelse(str_detect(genres, "Drama"), 1, 0),
                                romance=ifelse(str_detect(genres, "Romance"), 1, 0),
                                scifi=ifelse(str_detect(genres, "Sci-Fi"), 1, 0),
                                thriller=ifelse(str_detect(genres, "Thriller"), 1, 0)) %>%
  gather("genre_name", "genre_value", action:scifi)

tidy_edx %>%
  group_by(genre_name, decade) %>%
  summarise(m=mean(rating)) %>%
  ungroup() %>%
  ggplot(aes(x=decade, y=m)) +
  geom_point() +
  facet_grid(genre_name ~ ., scales = "free") 

# Conclusion: there is no significant difference by genre, 
# therefore, classification models will be discarded (nearest neighbours, decision trees, clustering)
rm(tidy_edx) # free up memory

# Add decade to training and test data sets

edx <- edx %>% mutate(year = str_trunc(title, 5, side="left", ellipsis="")) 
edx <- edx %>% mutate(year = str_trunc(year, 4, side="right", ellipsis=""))
edx <- edx %>% mutate(year = as.numeric(year))
edx <- edx %>% mutate(decade = round(year - 1900)/10)

validation <- validation %>% mutate(year = str_trunc(title, 5, side="left", ellipsis="")) 
validation <- validation %>% mutate(year = str_trunc(year, 4, side="right", ellipsis=""))
validation <- validation %>% mutate(year = as.numeric(year))
validation <- validation %>% mutate(decade = round(year - 1900)/10)

##########################################################
## Chosing an appropriate predictive model
##########################################################

# Strategy is to improving on Naive Bayes to minimise error: 
# mean + movie bias + user bias + decade bias
# (and adopting the best regularisation parameter: 5)

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
  
  b_t <- edx %>% 
    left_join(b_m, by="movieId") %>%
    group_by(decade) %>%
    summarize(b_t = sum(rating - b_m - m_movie)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "decade") %>%
    mutate(pred = m_movie + b_m + b_u + b_t) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})


##########################################################
## Results
##########################################################

lambda <- lambdas[which.min(rmses)]
print(c("Lamba = ", lambda)) # Best regularisation parameter
qplot(lambdas, rmses)  
rmse_results <- data.frame(method="Regularized Movie, User and Time Effect Model", RMSE = min(rmses))
rmse_results %>% knitr::kable()
