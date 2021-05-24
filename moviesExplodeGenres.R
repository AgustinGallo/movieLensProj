
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.5, list = FALSE)
train_set <- edx[-test_index,]
rm(edx,test_index)

## Expanding genres
a <- unique(train_set$genres)
a <- str_split(a, "[|]")
b <- ''
for (i in a){
  b <- c(b, i)
}

b<- unique(b[2:length(b)])

## Filling training set with columns in each category
train_set <- train_set %>%
  # Add an ID column based on row number
  mutate(numero = 1) %>%
  # Separate multiple categories by semicolon
  separate(col = genres, into = b, sep = "[|]", fill = "right")%>%
  # Gather categories into a single column
  gather_("Column", "Value", b) %>%
  # Drop temporary column
  select(-Column) %>%
  # Filter out NA values
  filter(!is.na(Value)) %>%
  # Spread categories to create a column for each category, with the count for each ID in that category
  spread(Value,numero, fill = 0) %>%
  mutate(timestamp = as_datetime(timestamp))

## Normalizing rating
train_set <- train_set %>% mutate(ratingN = (rating-mean(train_set$rating))/sd(train_set$rating))


## Training first attempt with random forest
train_set_norm <- train_set[names(train_set) %in% c('ratingN','userId','movieId', 'timestamp')]

fit <- train(ratingN ~ ., metric = "RMSE", method="glm", data = train_set_norm)

yt_hat <- predict(fit, train_set_norm)

#Training error
rmse(train_set$ratingN, yt_hat)