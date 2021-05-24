if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")


if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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


# Number of movies with a given rating
nrow(edx%>%filter(rating == 3))

# Number of diferent Movies
length(unique(edx$movieId))

# Number of diferent Users
length(unique(edx$userId))

# Number of movie of a given category
nrow(edx%>%filter(str_detect(genres, 'Drama')))

# Number of ratings of a given movie
edx%>%filter(str_detect(title, 'Forrest Gump') | 
             str_detect(title, 'Jurassic Park') | 
             str_detect(title, 'Pulp Fiction') |
             str_detect(title, 'The Shawshank Redemption') |
             str_detect(title, 'Speed 2: Cruise Control'))%>%
    group_by(title) %>%
    count() %>% arrange(desc(n))

# Order by most given ratings
edx%>% group_by(rating) %>%
  count() %>% arrange(desc(n))

edx%>% group_by(rating - as.integer(rating)) %>%
  count() %>% arrange(desc(n))



