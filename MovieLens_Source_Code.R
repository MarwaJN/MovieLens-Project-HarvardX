##########################################################
## MovieLens Project
## HarvardX: PH125.9x - Capstone Project
## R Script File
## Author: Marwa J. Nafakh
##########################################################

##########################################################
## MovieLens R Script File
## Generates models for predicted movie ratings and their associated RMSE values
########################################################## 


#--------------------------------------------------------
## Create edx and validation datasets
#--------------------------------------------------------


# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


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

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Clean memory
rm(dl, ratings, movies, test_index, temp, movielens, removed)
#--------------------------------------------------------

#--------------------------------------------------------
## (1) Exploring the structure of the edx dataset 

str(edx) 
head(edx)
class(edx)
summary(edx)

#Calculating and displaying the number of rows and columns in the validation dataset
paste('There are ',nrow(edx),'rows', 'and ', 
      ncol(edx), 'columns in the edx dataset')

# Check for any missing value\rating in the edx dataset
any(is.na(edx))
#--------------------------------------------------------

#--------------------------------------------------------
## (2) Understanding Time Variable

library(lubridate)
# Convert the timestamp factor into more readable date format by adding a new 
## column with the year of rating
edx <- mutate(edx, year_of_rating = year(as_datetime(timestamp)))
# view edx dataframe after new column addition
head(edx)

# Calculate the duration of collected data

tibble('First Date' = min(edx$year_of_rating),
       'Final Date' = max(edx$year_of_rating))%>% 
        mutate(Duration = (max(edx$year_of_rating) -
                            min(edx$year_of_rating)))
#--------------------------------------------------------

#--------------------------------------------------------
## (3) Understanding Users' Variable

# Counting and displaying the number of unique users
paste('There are ', n_distinct(edx$userId), 'different users')

# Understanding raters statistics
edx_users <- edx %>% group_by(userId) %>% 
                    summarize(num=n()) %>% 
                    arrange(desc(num))

summary(edx_users)

# Maximum and minimum number of ratings for users
paste('The most raters of users rated ', max(edx_users$num), 
      'movies, and the least raters rated only ', min(edx_users$num))

# Average number of rating for users
paste('Users in average rated around ', round(mean(edx_users$num)), ' movies.')

#--------------------------------------------------------

#--------------------------------------------------------
## (4) Understanding Movies' Variable

# Counting and displaying the number of unique movies 
paste('There are ', n_distinct(edx$movieId), 'different movies')

# Calculating and displaying the number of movies with 0 ratings
paste('There are ', edx %>% filter(rating == 0) %>% tally(), 'movies with 0 ratings')

# Calculating and displaying the number of movies with ratings = 3
paste('There are ', edx %>% filter(rating == 3) %>% tally(), 'movies with 3 stars ratings')

# Counting and displaying the different number of movies in each genres 
paste('Drama has ', edx %>% filter(str_detect(genres,"Drama")) %>% nrow(), 'movies')
paste('Comedy has ', edx %>% filter(str_detect(genres,"Comedy")) %>% nrow(), 'movies')
paste('Thriller has ', edx %>% filter(str_detect(genres,"Thriller")) %>% nrow(), 'movies')
paste('Romance has ', edx %>% filter(str_detect(genres,"Romance")) %>% nrow(), 'movies')

# Selecting the movie that has the greatest number of ratings
edx %>% group_by(movieId, title) %>% 
        summarize(number = n()) %>% 
        arrange(desc(number))
       
# Selecting the movie genres that has the greatest number of ratings
edx %>% group_by(movieId, genres) %>% 
        summarize(number = n()) %>% 
        arrange(desc(number))
#--------------------------------------------------------

#--------------------------------------------------------
## (5) Understanding Genres Variable

# Total number of genres, it is noted that some movies are tagged with more than one genres 

edx %>% group_by(genres) %>%
  summarize(number=n()) %>%
  arrange(desc(number))

# to count the sum number of genres per each movie

edx %>% group_by(count=str_count(genres, fixed("|")), genres=genres) %>%
        summarize(number= n()) %>%
        arrange(desc(count)) 


#--------------------------------------------------------

#--------------------------------------------------------
## (6) Understanding Rating Variable

# Sorting the five most given ratings in the edx dataset
head(sort(-table(edx$rating)),5)

# Numerically comparing the half and full star ratings
table(edx$rating)

# Graphically displaying the difference between full star and half star ratings
# Define rating scale categories based on full/half star, then visualize it

rating_category <- ifelse((edx$rating == 1|edx$rating == 2
                           |edx$rating == 3|edx$rating == 4
                           |edx$rating == 5) ,"Full_star","Half_star")

edx %>% ggplot(aes(x= rating, fill=rating_category)) +
        geom_histogram() +
        scale_fill_manual(values=c("Half_star"="grey", "Full_star"="black")) +
        scale_x_continuous(breaks=seq(0,5,by= 0.5)) +
        labs(title="Number of Ratings per Rating Score",
             subtitle = "Half and Full Star Comparison",
             x="Rating Scale",
             y="No. of Ratings") +
        theme_light()


# Visualize No. of Ratings per No. of Movies
edx %>% count(movieId) %>% 
        ggplot(aes(n)) + 
        geom_histogram(color="white") + 
        scale_x_log10()+labs(title="Number of Ratings per No. of Movies", 
                            x="No. of Ratings", 
                            y="No. of Movies") +
        theme_light()

# Visualize No. of Ratings per No. of Users
edx %>% count(userId) %>% 
        ggplot(aes(n)) + 
        geom_histogram(color="white") + 
        scale_x_log10() + 
        labs(title="Number of Ratings per No. of Users", 
             x="No. of Ratings", 
             y="No. of Users") +
        theme_light()

# Visualize No. of Ratings per Year
edx %>% ggplot(aes(x=year_of_rating)) + 
        geom_histogram(color="white") + 
        labs(title="Number of Ratings per Year", 
             x="Year", 
             y="No. of Ratings") + 
        scale_x_continuous(breaks=seq(1995,2009,by= 1)) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Visualize Average Number of Ratings per Users
edx %>% group_by(userId) %>% 
        summarize(avg_rating = mean(rating)) %>% 
        ggplot(aes(avg_rating)) + 
        geom_histogram(color = "white") + 
        labs(title="Average Ratings per Users", 
             x="Average Ratings", 
             y="No. of Users")+ 
        scale_x_continuous(breaks = seq(0.5,5,0.5))+
        theme_light()

# Visualize Average Number of Ratings per Year
edx %>% group_by(year_of_rating) %>% 
        summarise(mean(rating)) %>% 
        plot()


#--------------------------------------------------------
## Modeling Approaches
#--------------------------------------------------------

# First the used factors will be selected from both edx and validation datasets

edx <- edx %>% select(userId, movieId, rating, title, genres)
validation <- validation  %>% select(userId, movieId, rating, title, genres)

## (1) First Approach simple prediction based on calculated average of ratings

avg <- mean(edx$rating)
rmse_simple_model <- RMSE(validation$rating, avg)
rmse_simple_model 

# Check this model results and save it in the data.frame
rmse_results <- data_frame(Model_Method = "Average Prediction Model", 
                           RMSE = rmse_simple_model)

# Generate RMSE Result Table using kable() function
rmse_results %>% knitr::kable()

## (2) Second Approach considered Movie, User, and Genres Effects

## Movie Effect Model

bi <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = mean(rating - avg))

bi %>% ggplot(aes(x = b_i)) + 
       geom_histogram(bins=10, col = I("white")) +
       labs(title="Movie Effect Distribution", 
            x="Movie Effect",
            y="No. of Movies") +
        theme_light()

# Check and store model results

predicted_ratings <- avg +  validation %>%
                    left_join(bi, by='movieId') %>%
                    pull(b_i)

rmse_movie_model <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model_Method="Movie Effect Model",  
                                     RMSE = rmse_movie_model ))

# Update RMSE Result Table
rmse_results %>% knitr::kable()

## Movie and User Effect Model

bu <- edx %>%
      left_join(bi, by='movieId') %>%
      group_by(userId)%>% 
      summarize(b_u = mean(rating - avg - b_i))

bu %>% ggplot(aes(x = b_u)) + 
  geom_histogram(bins=10, col = I("white")) +
  labs(title="User and Movie Effect Distribution", 
       x="Combined User and Movie effects",
       y="Count") +
  theme_light()

# Check and store model results
predicted_ratings <- validation %>%
                     left_join(bi, by='movieId') %>%
                     left_join(bu, by='userId') %>%
                     mutate(pred = avg + b_i + b_u) %>%
                     pull(pred)

rmse_movie_user_model <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model_Method="Movie and User Effect Model",  
                                     RMSE = rmse_movie_user_model))

# Update RMSE Result Table
rmse_results %>% knitr::kable()

## Movie, User, and Genres Effect Model

bg <- edx %>%
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - avg - b_i - b_u))

bg %>% ggplot(aes(x = b_g)) + 
  geom_histogram(bins=10, col = I("white")) +
  labs(title="User, Movie, Genres Effect Distribution", 
       x="User, Movie and Genres effects",
       y="Count") +
  theme_light()

# Check and store model results

predicted_ratings <- validation %>%
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  left_join(bg, by='genres') %>%
  mutate(pred = avg + b_i + b_u + b_g) %>%
  pull(pred)

rmse_movie_user_genre_model <- RMSE(predicted_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model_Method="Movie, User and Genres Effect Model",  
                                     RMSE = rmse_movie_user_genre_model))

# Update RMSE Result Table
rmse_results %>% knitr::kable()

## (3) Regularized Movie, User, and Genres Effect 
## Considering adding a tuning factor for regularization (lambda) to the Movie, User and Genres Model to reduce RMSE value

# create variable Lambda (Tuning Factor)
lambdas <- seq(0, 10, 0.25)

# For each lambda,find b_i, b_u and b_g followed by rating prediction & testing
# Using cross validation method

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, rmses)+
  theme_light()


# Find optimal lambda                                                             
lambda <- lambdas[which.min(rmses)]
lambda

# Check and store results                                                             
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model_Method="Regularized Movie, User and Genres Effect Model",  
                                     RMSE = min(rmses)))

# Update RMSE Result Table

rmse_results %>% knitr::kable()



