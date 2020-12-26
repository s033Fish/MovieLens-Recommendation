#Downloading all of the needed packages that are not downloaded
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(clusterSim)) install.packages("clusterSim", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(mltools)) install.packages("mltools", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

#Downloading the data
td = tempdir()
edx = tempfile(tmpdir=td, fileext=".rds")
validation = tempfile(tmpdir=td, fileext=".rds")
download.file("https://www.dropbox.com/s/nspymeso8rmmak1/edx.rds?dl=1", edx,  mode = "wb")
download.file("https://www.dropbox.com/s/x0s477b0kzxpl6i/validation.rds?dl=1", validation,  mode = "wb")
edx = readRDS(edx)
validation = readRDS(validation)
unlink(td)
dim(edx)

#Creating the RMSE function to check my models
RMSE <- function(actual, pred){
  sqrt(mean((actual-pred)^2))
}

#Creating training and testing sets
set.seed(1)
sample.kind="Rounding"
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

#Checking that both the userId and movieId from the test set are also in train set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

sapply(edx,function(x) sum(is.na(x))) # check for missing data


#Method 1: Guessing the mean

set.seed(1)
sample.kind="Rounding"
#finding the mean of the rating
mu_hat <- mean(train_set$rating)

#Finding RMSE for guessing the mean
mean_rmse <- RMSE(test_set$rating, mu_hat)

#Created RMSE table that stores the RMSE's for each method
rmse_table <- data.frame(method = "Just the average", RMSE = mean_rmse)
rmse_table



#Method 2: Accounting for Movie Bias

set.seed(1)
sample.kind="Rounding"
#Averages of every movie
movie_avgs <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

#Model that accounts for movie bias
predicted_ratings <- mu_hat + test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_table <- bind_rows(rmse_table, data_frame(method="Movie Effect Model", RMSE = model_1_rmse))

#Method 3: Accounting for Movie and User bias

set.seed(1)
sample.kind="Rounding"
#Averages of every user
user_avgs <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

#Model that accounts for user and movie bias
predicted_ratings2 <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred

#Adding the next RMSE to the table
model_2_rmse <- RMSE(predicted_ratings2, test_set$rating)
rmse_table <- bind_rows(rmse_table, data_frame(method="Movie + User Effect Model", RMSE = model_2_rmse))
rmse_table



#Method 4: Regularization

set.seed(1)
sample.kind="Rounding"
#finding the best lambda
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
#setting lambda to the value that minimizes RMSE
lambda <- lambdas[which.min(rmses)]
lambda
rmses
lambdas
plot(x = lambdas, y = rmses)

#Averages for movies with regularization
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda), n_i = n())

#Averages for users with regularization
user_reg_avgs <- test_set %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i)/(n()+lambda), n_u = n())

#Creating model
predicted_ratings3 <- test_set %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred

#Adding RMSE to the table
model_3_rmse <- RMSE(predicted_ratings3, test_set$rating)
rmse_table <- bind_rows(rmse_table, data_frame(method="Regularized Movie EFF", RMSE = model_3_rmse))
rmse_table



#Method 5: Genre Bias

set.seed(1)
sample.kind="Rounding"
#Averages for each genre
genre_avgs <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u))

#Creating a model
predicted_ratings4 <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  .$pred

#Adding RMSE to the table
model_4_rmse <- RMSE(predicted_ratings4, test_set$rating)
rmse_table <- bind_rows(rmse_table, data_frame(method="Movie + User + Genre Effect Model", RMSE = model_4_rmse))
rmse_table



#Method 6: Matrix Factorization

set.seed(1)
sample.kind="Rounding"
#formating the data
train_format_set <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_format_set  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

# Creating the object for the model
r <-  recosystem::Reco()

# Finding the best parameters
par <- r$tune(train_format_set, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))

# Training the algorithm 
r$train(train_format_set, opts = c(par$min, nthread = 4, niter = 20))

#Creating the prediction
predicted_ratings5 <-  r$predict(test_format_set, out_memory())

#Calculating the RMSE
model_5_rmse <- RMSE(predicted_ratings5, test_set$rating)

#Adding it to the table
rmse_table <- bind_rows(rmse_table, data.frame(method = "Matrix Factorization", RMSE = model_5_rmse))
rmse_table



#Method 7: Matrix Factorization on full edx set

set.seed(1)
sample.kind="Rounding"
#Formatting the edx and validation data set
format_edx <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
format_validation <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

# Creating the model
r <-  recosystem::Reco()

#Finding the best parameters
par <-  r$tune(format_edx, opts = list(dim = c(10, 20, 30), 
                                      lrate = c(0.1, 0.2),
                                      costp_l2 = c(0.01, 0.1), 
                                      costq_l2 = c(0.01, 0.1),
                                      nthread  = 4, niter = 10))

# Training the algorithm
r$train(format_edx, opts = c(par$min, nthread = 4, niter = 20))

#Predicting the ratings
predicted_ratings6 <-  r$predict(format_validation, out_memory())

model_6_rmse <- RMSE(predicted_ratings6, validation$rating)
#Adding the RMSE to the table
rmse_table <- bind_rows(rmse_table, data.frame(method = "Matrix Factorization Final", RMSE = model_6_rmse))
rmse_table

