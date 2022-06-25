##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

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
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#title = as.character(title),
#genres = as.character(genres))
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


##########################################################
# Start Working Here
##########################################################

#############################
# Generate Train and Test Set
#############################

# generate reproducible test set partition
set.seed(1, sample.kind="Rounding") # Set random seed to produce reproducible outcome `
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE) # Create random index for partitioning (10% and 90%)
train <- edx[-test_index,] # Obtain train set using the index created. 
temp <- edx[test_index,] # Obtain test set using the index created. 

# Make sure userId and movieId in test set are also in train set to preserve necessary columns
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into train set to prevent leakage
removed <- anti_join(temp, test)
train <- rbind(train, removed)

# remove redundant objects to save space
rm(removed, temp, test_index) 


##############################
#Exploratory Data Analysis 
##############################

# Examine the train data to understand characteristics
train %>% as_tibble()


# Obtain unique summary statistics from the train dataset
summary_train <- train %>% summarise(n_users=n_distinct(userId),# unique users from train
                                     n_movies=n_distinct(movieId),# unique movies from train
                                     min_rating=min(rating),  # the lowest rating 
                                     max_rating=max(rating) # the highest rating 
)
summary_train


# Understanding Ratings : Plot the Distribution of Ratings in the Train Set
train %>% 
  ggplot(aes(rating)) +
  geom_bar() +
  theme_light() +
  ggtitle("Figure 1. Distribution of Rating") +
  theme(plot.title = element_text(face = "bold")) +
  xlab("Rating") +
  ylab("Count")


## Ascertain how the top 5 frequent movieIds look like. 
keep <- train %>% 
  count(movieId) %>% 
  top_n(5, n) %>% 
  .$movieId

# Understanding what the top users say about the movies. 
tab <- train %>% 
  filter(movieId%in%keep) %>% 
  filter(userId %in% c(13:20)) %>% 
  select(userId, title, rating) %>% 
  mutate(title = str_remove(title, ", The"),
         title = str_remove(title, ":.*")) %>%
  spread(title, rating)
tab %>% knitr::kable()


#Examining the recommendation challenge: Not every user watched/rated every movie. 
# matrix for a random sample of 100 movies and 100 users with yellow 
# indicating a user/movie combination for which we have a rating.
users <- sample(unique(train$userId), 100)  # Sample 100 users from the dataset
rafalib::mypar()
train %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users", 
        main = 'Figure 2. Plot of Rating by 100 Sample Users') #Plotting the Movie Rating by Users
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")


# Plotting the Distribution of movies: using ggplot (in 50 bins), with log10 scaling on x-axis
train %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, color = "blue") + 
  scale_x_log10() + 
  theme_light() +
  ggtitle("Figure 3. Distribution of Movies") +
  theme(plot.title = element_text(face = "bold"))


# Plotting the Distribution of Users; using ggplot (in 50 bins), with log10 scaling of x-axis
train %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, color = "orange") + 
  scale_x_log10() +
  theme_light() +
  ggtitle("Figure 4. Distribution of Users") +
  theme(plot.title = element_text(face = "bold"))


# remove redundant objects to save space
rm(users, tab, keep)


###Rating of Movies by Genre
#Obtain Summary of Ratings by Genres (Sum and Frequency of Ratings)
ratings_genres <- train %>% group_by(genres) %>% 
  mutate(Sum = sum(rating), Frequency = n()) %>% 
  select("genres", "Frequency", "Sum") %>%
  distinct()

#Obtain Average ratings to 4 decimal places
ratings_genres <- as.data.frame(ratings_genres) #Convert tibble to dataframe to easily obtain more than 2 decimals in manipulation
ratings_genres$Average_rating <- round((ratings_genres$Sum/ratings_genres$Frequency), 4) #Obtain Average ratings to 4 decimal places
ratings_genres$Sum <- NULL  #Remove redundant Sum column
ratings_genres <- arrange(ratings_genres, desc(Average_rating)) # Arrange dataframe in descending order of Average Ratings
head(ratings_genres, 10)   #Showcase first 10 genres by Average rating
tail(ratings_genres, 10)   #Showcase last 10 genres by Average rating



################################################################
# Exploratory Analysis : Understanding Time Effects of Ratings
################################################################
## Install and load necessary library for manipulating time effect
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)

#Time Effects: Year Movie was rated & Year Movie was released
train$year_rated <- year(as_datetime(train$timestamp))   # Create year movie was rated in the train set
train$year_released <- gsub(".*[(]|[)].*", "", train$title)   # Create year movie was released in the train set


## Year of Movie Ratings
#Obtain Summary of Ratings by Year of Movie Ratings (Sum and Frequency of Ratings)
ratings_yrated <- train %>% group_by(year_rated) %>% 
  mutate(Sum = sum(rating), Frequency = n()) %>% 
  select("year_rated", "Frequency", "Sum") %>%
  distinct()

#Obtain Average ratings to 4 decimal places
ratings_yrated <- as.data.frame(ratings_yrated) #Convert tibble to dataframe to easily obtain more than 2 decimals in manipulation
ratings_yrated$Average_rating <- round((ratings_yrated$Sum/ratings_yrated$Frequency), 4) #Obtain Average ratings to 4 decimal places
ratings_yrated$Sum <- NULL  #Remove redundant Sum column
ratings_yrated <- arrange(ratings_yrated, year_rated) # Arrange dataframe year chronologically
head(ratings_yrated, 10)  #Showcase first 10 years



#Plot the Evolution of Average Rating per year Rated (year 1995 is excluded bcos of single observation(not representative))
ratings_yrated[2:15,] %>% ggplot(aes(x = year_rated, y = Average_rating, label = Average_rating)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("Year Rated") +
  ylab("Average Rating") +
  ggtitle("Figure 5. Evolution of Average Ratings by Year Rated") +
  theme(plot.title = element_text(face = "bold")) +
  geom_text(nudge_y = -0.005, color = 'blue') 


##Year of Movie Release
#Obtain Summary of Ratings by Year of Movie Release (Sum and Frequency of Ratings)
ratings_yreleased <- train %>% group_by(year_released) %>% 
  mutate(Sum = sum(rating), Frequency = n()) %>% 
  select("year_released", "Frequency", "Sum") %>%
  distinct()

#Obtain Average ratings to 4 decimal places
ratings_yreleased <- as.data.frame(ratings_yreleased) #Convert tibble to dataframe to easily obtain more than 2 decimals in manipulation
ratings_yreleased$Average_rating <- round((ratings_yreleased$Sum/ratings_yreleased$Frequency), 4) #Obtain Average ratings to 4 decimal places
ratings_yreleased$Sum <- NULL  #Remove redundant Sum column
ratings_yreleased <- arrange(ratings_yreleased, year_released) # Arrange dataframe in descending order of Average Ratings
head(ratings_yreleased, 10)  #Showcase first 10 years



#Plot the Evolution of Average Rating per year Rated using ggplot
ratings_yreleased %>% ggplot(aes(x = year_released, y = Average_rating, group = 1)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("Year Released") +
  ylab("Average Rating") +
  ggtitle("Figure 6. Evolution of Average Rating by Year of Movie Release") +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 90, size = 6, vjust = 0.5, hjust=1)) 


##############################
# Model Building 
##############################


# Generate an Evaluation Metric Function: RMSE (Root Means Square Error) to compare metrics
# the Function takes two inputs true ratings and predicted ratings and output the RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



## Model 1: Overall Mean
mu <- mean(train$rating) # compute average ratings for all observations (irrespective of any characteristics)
mu

rmse_model1 <- RMSE(test$rating, mu) # compute root mean standard error
rmse_model1



## Model 2: Incorporate Movie Effects
# fit <- lm(rating ~ as.factor(movieId), data = train) :- this will crash R: lm estimation will be computationally expensive

###Computing the RMSE After incorporaing movie effect
mu <- mean(train$rating) #estimate the mean of ratings
# obtain below tibble for the movie effect.
movie_avgs <- train %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

## Plot the Movie Effect
movie_avgs %>% 
  ggplot(aes(b_i)) + 
  geom_histogram(bins = 50, color = "black") + 
  theme_light() +
  ggtitle("Figure 7. Movie Effect") +
  theme(plot.title = element_text(face = "bold"))


# Compute predicted ratings by incorporating movie effect
predicted_ratings <- mu + test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

# Obtain RMSE for Model 2: Incorporating Movie Effect
rmse_model2 <- RMSE(test$rating, predicted_ratings)
rmse_model2


# Model 3: Incorporate User Effects
# Plot individual User Effects Incorporated into prior model
train %>% 
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 50, color = "black") + 
  theme_light() +
  ggtitle("Figure 8. Individual User Effects") +
  theme(plot.title = element_text(face = "bold"))

##lm(rating ~ as.factor(movieId) + as.factor(userId)) #lm will crash R

# compute user effect b_u
user_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

# compute predicted values on test set
predicted_ratings <- test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Obtain RMSE for Model 3: Individual User Effects
rmse_model3 <- RMSE(test$rating, predicted_ratings)
rmse_model3




## Model 4: Regularization
# Examining whether the frequency of user ratings affect prediction. 

# Examining relationship btn movies and titles
movie_titles <- train %>% 
  select(movieId, title) %>%
  distinct()

# use cross-validation to pick a lambda:
lambda <- seq(0, 10, 0.25)

rmses <- sapply(lambda, function(l){
  mu <- mean(train$rating)
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    train %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(train$rating, predicted_ratings))
})

#Form dataframe of lambda values and rmses for plotting
lambda_rmse_data <- data.frame(lambda, rmses)
# Plot RMSE against lambda 
lambda_rmse_data %>% ggplot(aes(lambda, rmses)) +
  geom_point() +
  theme_light() +
  ggtitle("Figure 9. Regularization Method: Plot of RMSES and Lambda") +
  theme(plot.title = element_text(face = "bold"))


# pick lambda with minimun rmse
min_lambda <- lambda[which.min(rmses)]
# print optimal lambda for regularization 
min_lambda



# compute movie effect with regularization on train set
b_i <- train %>% 
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n()+min_lambda))

# compute user effect with regularization on train set
b_u <- train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu)/(n()+min_lambda))

# # compute predicted values on test set 
predicted_ratings <- 
  test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Obtain RMSE on Regularization Model
rmse_model4 <- RMSE(test$rating, predicted_ratings)
rmse_model4

#There is not much difference using Regularization. 


##############################################
## Model 5: Modelling Evidence of Time Effects
##############################################

#Time Effects: Year Movie was rated & Year Movie was released
#Create year rated and year released columns in test set (Already created in train set)
test$year_rated <- year(as_datetime(test$timestamp))   # Obtain year movie was rated in the test set
test$year_released <- gsub(".*[(]|[)].*", "", test$title)   # Obtain year movie was released in the test set


#Incorporate year movie was released
yreleased_effect <- train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = "userId") %>% 
  group_by(year_released) %>%
  summarise(b_yreleased = mean(rating - mu - b_i - b_u))

#Plot the year movie was released
train %>% 
  left_join(yreleased_effect, by = 'year_released') %>%
  group_by(year_released) %>% 
  ggplot(aes(b_yreleased)) + 
  geom_histogram(bins = 100, color = "black") + 
  theme_light() +
  ggtitle("Figure 10. Incorporating Year Movie was Released") +
  theme(plot.title = element_text(face = "bold"))


#Incorporate year movie was rated
yrated_effect <- train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(yreleased_effect, by = 'year_released') %>%
  group_by(year_rated) %>%
  summarise(b_yrated = mean(rating - mu - b_i - b_u - b_yreleased))


#Plot the year movie was rated
train %>% 
  left_join(yrated_effect, by = 'year_rated') %>%
  group_by(year_rated) %>% 
  ggplot(aes(b_yrated)) + 
  geom_histogram(bins = 100, color = "black") + 
  theme_light() +
  ggtitle("Figure 11. Incorporating Year Movie was Rated") +
  theme(plot.title = element_text(face = "bold"))


# compute predicted values on test set
predicted_ratings <- test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(yreleased_effect, by='year_released') %>%
  left_join(yrated_effect, by = 'year_rated') %>%
  mutate(pred = mu + b_i + b_u + b_yreleased + b_yrated) %>%
  .$pred


# Obtain RMSE for model 
rmse_model5 <- RMSE(test$rating, predicted_ratings)
rmse_model5



##############################################################
## Model 6: Modelling Evidence of Interaction Time Effects
##############################################################

# Interaction Effect: How movies released in year X was rated in year Y ? 
#Create time interaction effect column in train and test sets
train$cross_time <- paste(train$year_released, train$year_rated, sep = "_") # Create time Interaction effect column in train set
test$cross_time <- paste(test$year_released, test$year_rated, sep = "_") # Create time Interaction effect column in test set


##Interaction Effect: Year Released and Year Rated
#Obtain Summary of Ratings by Year of Movie Release (Sum and Frequency of Ratings)
ratings_inteffect <- train %>% group_by(cross_time) %>% 
  mutate(Sum = sum(rating), Frequency = n()) %>% 
  select("cross_time", "Frequency", "Sum") %>%
  distinct()

#Obtain Average ratings to 4 decimal places
ratings_inteffect <- as.data.frame(ratings_inteffect) #Convert tibble to dataframe to easily obtain more than 2 decimals in manipulation
ratings_inteffect$Average_rating <- round((ratings_inteffect$Sum/ratings_inteffect$Frequency), 4) #Obtain Average ratings to 4 decimal places
ratings_inteffect$Sum <- NULL  #Remove redundant Sum column
ratings_inteffect <- arrange(ratings_inteffect, desc(Average_rating)) # Arrange dataframe in descending order of Average Ratings
ratings_inteffect %>% 
  arrange(desc(Frequency), Average_rating) %>% 
  head(.,10) %>%
  knitr::kable(., col.names = c("Year Released Year Rated", "Frequency", "Average Rating")) #Rating for the top 10 with highest frequency
ratings_inteffect %>% 
  arrange(Frequency, Average_rating) %>% 
  head(.,10) %>% 
  knitr::kable(., col.names = c("Year Released Year Rated", "Frequency", "Average Rating")) #Rating for the top 10 with lowest frequency

#Incorporate interaction effect
inttime_effect <- train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = "userId") %>% 
  left_join(yreleased_effect, by = "year_released") %>%
  left_join(yrated_effect, by = "year_rated") %>%
  group_by(cross_time) %>%
  summarise(b_inttime = mean(rating - mu - b_i - b_u - b_yreleased - b_yrated))

  
# Estimate the predicted ratings on test set
predicted_ratings <- test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(yreleased_effect, by = 'year_released') %>%
  left_join(yrated_effect, by='year_rated') %>%
  left_join(inttime_effect, by = "cross_time") %>%
  mutate(pred = mu + b_i + b_u + b_yreleased + b_yrated + b_inttime) %>%
  .$pred

summary(predicted_ratings) #Examine Summary statistics of predicted ratings

predicted_ratings <- ifelse(is.na(predicted_ratings), mean(predicted_ratings, na.rm = TRUE), predicted_ratings) #replace the NA with mean value
predicted_ratings <- ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings) # Ensure Rating Floor of 0.5
predicted_ratings <- ifelse(predicted_ratings > 5, 5, predicted_ratings) # Ensure Rating Ceiling of 5


# Obtain RMSE for model 
rmse_model6 <- RMSE(test$rating, predicted_ratings)
rmse_model6



##############################################################
## Validation Set : Examining Model 6 on validation set
##############################################################

###Generate required year effect columns
validation$year_rated <- year(as_datetime(validation$timestamp))   # Create year movie was rated column
validation$year_released <- gsub(".*[(]|[)].*", "", validation$title)   # Create year movie was released column
validation$cross_time <- paste(validation$year_released, validation$year_rated, sep = "_") # Create time Interraction effect column

# Estimate the predicted ratings on validation set
predicted_val <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(yreleased_effect, by = 'year_released') %>%
  left_join(yrated_effect, by='year_rated') %>%
  left_join(inttime_effect, by = "cross_time") %>%
  mutate(pred = mu + b_i + b_u + b_yreleased + b_yrated + b_inttime) %>%
  .$pred


summary(predicted_val) #Examine Summary statistics of predicted ratings

predicted_val <- ifelse(is.na(predicted_val), mean(predicted_val, na.rm = TRUE), predicted_val) #replace the NA with mean value
predicted_val <- ifelse(predicted_val < 0.5, 0.5, predicted_val) # Ensure Rating Floor of 0.5
predicted_val <- ifelse(predicted_val > 5, 5, predicted_val) # Ensure Rating Ceiling of 5


# Obtain RMSE for model 
rmse_val <- RMSE(validation$rating, predicted_val)
rmse_val


##Create dataframe of Performance Metrics of the Models
Method <- c('Just the Average', 'Movie Effect Model on Test Set', 'Movie and User Effects Model on Test Set', 
            'Reg. Movie and User Effects on Test Set', 'Movie, User, Year Released and Year Rated Effects on Test Set',
            'Movie, User, Year Released, Year Rated and Year Interaction Effects on Test Set',
            'Movie, User, Year Released, Year Rated and Year Interaction Effects on Validation Set') #Create row names

RMSE <- c(round(rmse_model1, 6), round(rmse_model2, 6), round(rmse_model3, 6), round(rmse_model4, 6), round(rmse_model5, 6), 
          round(rmse_model6, 6), round(rmse_val, 6)) # Place model RMSE results into corresponding vector


model_results <- data.frame(Method, RMSE) # Create dataframe of the model results
#print out the results
model_results %>% knitr::kable()

