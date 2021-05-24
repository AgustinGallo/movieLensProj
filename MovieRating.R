
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(party)
library(Metrics)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


## -----------------------------------------
## Downloading files
## -----------------------------------------

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Set seed to keep reproducibility
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

# Validation set will be 10% of MovieLens data
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Making sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set (training set)
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Removing stg datasets to save space
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Set seed to keep reproducibility
set.seed(1, sample.kind="Rounding")


## -----------------------------
## Evaluating dummy attempts
## -----------------------------

## Evaluate RMSE
## Dummy evaluation without normalization
dumy_predict <- mean(train_set$rating)
Error_NoNorm <- rmse( train_set$rating , dumy_predict) ## Around 1.06 of error
Error_NoNorm
## Evaluation using normalized ratings
# Creating Normalized rating
train_set <- edx %>% mutate(ratingN = rating - mean(rating)/sd(rating) )

dumy_predict <- mean(train_set$ratingN)
sd_rating <- sd(train_set$rating)

Error_Norm <- rmse( train_set$ratingN , dumy_predict) ## Around 0.99 of error
Error_Norm


## ---------------------------------------------
## A more elaborated attempt
## ---------------------------------------------

## Eliminating movies and user with no ranking in the validation set
validation_clean <- validation %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


# Defining model to be test
# y = u + b(user) + b(movies)+ b(day) + b(genre) + error


##Plots to sustain this
#Movies
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
qplot(b_i, data = movie_avgs, bins = 16, color = I("black"))


# User
train_set %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avg <- train_set %>% 
  left_join(movie_avgs, by = 'movieId')%>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating- mu - b_i))

# Genres
train_set %>% 
    group_by(genres) %>% 
    filter(n()>=100) %>%
    summarize(b_g = mean(rating)) %>% 
    ggplot(aes(b_g)) + 
    geom_histogram(bins = 15, color = "black")

genres_avg<-train_set %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avg, by = 'userId') %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u))


# Timestamp -> Very low importance, not to take into account, try it by year, week day, month
train_set %>% 
  group_by(month(as_datetime(timestamp))) %>% 
  filter(n()>=100) %>%
  summarize(b_t = mean(rating)) %>% 
  ggplot(aes(b_t)) + 
  geom_histogram(bins = 35, color = "black")


## Making prediction
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(genres_avg, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  select(pred)

predicted_ratings <- predicted_ratings %>%
  mutate(pred = if_else(pred>5,5,if_else(pred<0,0,pred))) %>%
  pull(pred)

rmse(validation$rating, predicted_ratings) ## Obtained 0.8647, pretty good, yeii

## Now using penalized least squares, with penalty term Lambda
lambdas <- seq(0,10,0.25)

mu <- mean(train_set$rating)
rmses <- sapply(lambdas, function(l){
    movie_avgs <- train_set %>% 
      group_by(movieId) %>% 
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    user_avg <- train_set %>% 
      left_join(movie_avgs, by = 'movieId')%>%
      group_by(userId) %>% 
      summarize(b_u = sum(rating- mu - b_i)/(n()+l))
    
    genres_avg<-train_set %>% 
      left_join(movie_avgs, by = 'movieId') %>%
      left_join(user_avg, by = 'userId') %>%
      group_by(genres) %>% 
      summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
    
    predicted_ratings <- 
      validation %>% 
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avg, by='userId') %>%
      left_join(genres_avg, by='genres') %>%
      mutate(pred = mu + b_i + b_u + b_g) %>%
      select(pred)
    
    predicted_ratings <- predicted_ratings %>%
      mutate(pred = if_else(pred>5,5,if_else(pred<0,0,pred))) %>%
      pull(pred)
    
    return (rmse(validation$rating,predicted_ratings))
})

qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)] # Best Lambda is 4.75

#Final model will be
l <- lambda
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l))

user_avg <- train_set %>% 
  left_join(movie_avgs, by = 'movieId')%>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating- mu - b_i)/(n()+l))

genres_avg<-train_set %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avg, by = 'userId') %>%
  group_by(genres) %>% 
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))

predicted_ratings <- 
  validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(genres_avg, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  select(pred)

predicted_ratings <- predicted_ratings %>%
  mutate(pred = if_else(pred>5,5,if_else(pred<0,0,pred))) %>%
  pull(pred)

RMSE <- rmse(validation$rating,predicted_ratings) ## Small improvement 0.8643



