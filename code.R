
##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)





# Data Exploration

library(tibble)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(knitr)
library(kableExtra)


# Setze einen Seed für Reproduzierbarkeit
set.seed(1)

# Überprüfe die Anzahl der Zeilen im reduzierten Datensatz
data.frame(Rows = nrow(edx)) %>%
  my_kable(caption = "Number of rows in edx Dataset")



#Head anzeigen
head(edx, 10) %>%
  my_kable(caption = "First 10 Rows of edx Dataset")


#Distribution of film ratings
ggplot(edx, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(min(edx$rating)-0.5, max(edx$rating), by = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(length(edx$rating)), by = 500000),labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
  labs(x = "Rating", y = "Number of ratings", caption = "Source: edx data")



library(dplyr)

#Top 10 Films with the most ratings
edx %>%
  group_by(movieId, title) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  head(10) %>% my_kable(caption = "Top 10 Films with the most ratings")



#Bewertung über Zeit
edx$date <- as.Date(as.POSIXct(edx$timestamp, origin = "1970-01-01"))

ggplot(edx, aes(x = floor_date(as.Date(as.POSIXct(timestamp, origin = "1970-01-01")), "week"))) +
  geom_line(stat = "count", color = "darkgreen") +
  labs(x = "Year", 
       y = "Number of ratings", 
       caption = "Source: edx data") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(trans = "sqrt", labels = function(x) format(x, scientific = FALSE,big.mark=","))


library(gridExtra)

grid.arrange(
  # Plot 1: Top 10 Film Genres
  ggplot(edx %>%
           separate_rows(genres, sep = "\\|") %>%
           count(genres, sort = TRUE) %>%
           slice_head(n = 10),
         aes(x = reorder(genres, n), y = n)) +
    geom_col(fill = "orange", color = "black") +
    coord_flip() +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ",")) +
    labs(x = "Genre", y = "Number", caption = "Source: edx data"),
  
  # Plot 2: Average Rating per Genre
  ggplot(edx %>%
           separate_rows(genres, sep = "\\|") %>%
           group_by(genres) %>%
           summarise(avg_rating = mean(rating), count = n()) %>%
           filter(count > 1000) %>%
           arrange(desc(avg_rating)),
         aes(x = reorder(genres, avg_rating), y = avg_rating)) +
    geom_point(aes(size = count), color = "black", fill = "purple", shape = 21, stroke = 0.5) +
    coord_flip() +
    labs(x = "Genre", y = "Average Rating", size = "Number of Ratings", caption = "Source: edx data") +
    scale_size_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ",")) +
    theme(
      legend.position = c(0.975, 0.025),
      legend.justification = c("right", "bottom"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7),
      legend.key.size = unit(0.4, "lines"),
      legend.background = element_rect(fill = "white", color = NA)
    ),
  
  ncol = 2, 
  widths = c(1, 1)
)


#Anzahl Bewertung vs. Released year
edx$released <- str_extract(edx$title, "\\((\\d{4})\\)") %>% 
  str_remove_all("[()]") %>% 
  as.numeric()


library(gridExtra)

grid.arrange(
  
  # Gruppieren nach movieId und Zählen der Bewertungen pro Film
  edx %>% group_by(userId) %>%
    summarise(ave_rating = sum(rating)/n()) %>%
    ggplot(aes(ave_rating)) +
    geom_histogram(bins=30, fill = "skyblue", color = "black") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
    labs(x = "Average rating", y = "Number of users", caption = "Source: edx data"),
  
  edx %>% 
    count(userId) %>% 
    ggplot(aes(n)) + 
    geom_histogram(bins=30, fill = "skyblue", color = "black") +
    scale_x_log10(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
    labs(x = "Users", y = "Number of ratings", caption = "Source: edx data"),
  
  
  ncol = 2, 
  widths = c(1, 1)
)





# Results


if(!require(pdflatex)) install.packages("pdflatex")
if(!require(tinytex)) install.packages("tinytex")
if(!require(knitr)) install.packages("knitr")
if(!require(rmarkdown)) install.packages("rmarkdown")

library(dslabs)
library(tidyverse)
library(caret)
library(dplyr)
library(gam)
library(mgcv)
library(tibble)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(tinytex)

set.seed(1)

edx$date <- as.Date(as.POSIXct(edx$timestamp, origin = "1970-01-01"))
edx <- edx %>%
  mutate(released = str_extract(title, "\\((\\d{4})\\)") %>% 
           str_remove_all("[()]") %>% 
           as.numeric())



test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
mu <- mean(train_set$rating)
mu_rmse <- RMSE(test_set$rating, mu)
rmse_results <- data_frame(Method = "Just the average", RMSE = mu_rmse)



rmse_results %>% knitr::kable()


movie_avg <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avg %>% group_by(movieId) %>%
  summarise(ave_deviation = sum(b_i)/n()) %>%
  ggplot(aes(ave_deviation)) +
  geom_histogram(bins=30, fill = "skyblue", color = "black") +
  labs(x = "Average deviation", y = "Number of movies", caption = "Source: train data")


predicted_ratings <- mu + test_set %>%
  left_join(movie_avg, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
suppressWarnings({
  rmse_results <- bind_rows(rmse_results,
                            tibble(Method="Movie effect model",
                                   RMSE = model_1_rmse ))
})

rmse_results %>% knitr::kable()


user_avg <- train_set %>% 
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

user_avg %>% group_by(userId) %>%
  summarise(ave_deviation = sum(b_u)/n()) %>%
  ggplot(aes(ave_deviation)) +
  geom_histogram(bins=30, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = -3:2) +
  labs(x = "Average deviation", y = "Number of users", caption = "Source: train data")


predicted_ratings <- test_set %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

suppressWarnings({
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie + User effect model",  
                                     RMSE = model_2_rmse ))
})

rmse_results %>% knitr::kable()



 released_avg <- train_set %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  group_by(released) %>%
  summarize(b_r = mean(rating - mu - b_i - b_u))


library(gridExtra)

grid.arrange(
  edx %>%
    group_by(released) %>%
    summarize(rating = mean(rating)) %>%
    ggplot(aes(released, rating)) +
    geom_point(size = 0.7) +
    geom_smooth(method="gam", formula = y ~ s(x, bs = "cs"), color = "darkgreen") +
    labs(x = "Released", y = "Rating", caption = "Source: edx data"),

released_avg %>% group_by(released) %>%
  summarise(ave_deviation = sum(b_r)/n()) %>%
  ggplot(aes(ave_deviation)) +
  geom_histogram(bins=30, fill = "skyblue", color = "black") +
  labs(x = "Average deviation", y = "Number of years", caption = "Source: train data"),
  
  ncol = 2, 
  widths = c(1, 1)
)


predicted_ratings <- test_set %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(released_avg, by='released') %>%
  mutate(pred = mu + b_i + b_u + b_r) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie + User effect Model + Release year effect",  
                                     RMSE = model_3_rmse ))

rmse_results %>% knitr::kable()


#genre effect
genre_avg <- train_set %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(released_avg, by='released') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u - b_r)) 


#genre deviation
genre_avg %>% group_by(genres) %>%
  summarise(ave_deviation = sum(b_g)/n()) %>%
  ggplot(aes(ave_deviation)) +
  geom_histogram(bins=30, fill = "skyblue", color = "black") +
  labs(x = "Average deviation", y = "Genre", caption = "Source: train data")


predicted_ratings <- test_set %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(released_avg, by='released') %>%
  left_join(genre_avg, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_r + b_g) %>%
  pull(pred)
model_4_rmse <- RMSE(predicted_ratings, test_set$rating)

suppressWarnings({
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie + User + Release year + Genre effect",  
                                     RMSE = model_4_rmse ))
})

rmse_results %>% knitr::kable()


train_set <- train_set %>% mutate(weekNumber = as.integer(difftime(date, min(date), units = "weeks")) + 1)

test_set <- test_set %>% mutate(weekNumber = as.integer(difftime(date, min(date), units = "weeks")) + 1)

# Fit a smooth curve to the ratings as a function of time
fit <- gam(rating ~ s(weekNumber, bs = "cs"),
           family = gaussian(), data = train_set) # apply smoothing

# Evaluate the fitted curve for each week number
fit_curve <- data.frame(weekNumber = seq(1, max(train_set$weekNumber), length.out = max(train_set$weekNumber))) %>%
  mutate(fit = predict.gam(fit, newdata = .) - mu)

fit_curve %>% 
  left_join(
    train_set %>% 
      group_by(weekNumber) %>% 
      summarize(date = min(date)),
    by = "weekNumber"
  ) %>%
  filter(!is.na(date)) %>%
  ggplot(aes(date, fit)) +
  geom_point(data = train_set %>% 
               mutate(date = round_date(date, unit = "week")) %>%
               group_by(date) %>%
               summarize(rating = mean(rating) - mu),
             aes(x = date, y = rating), 
             alpha = 0.5, color = "black", size = 0.7) +
  geom_line(color = "darkgreen", linewidth = 1, alpha = 1) +
  labs(x = "Date", y = "Rating (centered)", caption = "Source: train data") +
  coord_cartesian(ylim = c(-0.5, 0.5))



#alles time korrigieren
# 1. Zeiteffekt aus den Ratings entfernen
train_set_time_adjusted <- train_set %>%
  mutate(time_effect = predict.gam(fit, newdata = .),
         rating_time_adjusted = rating - time_effect + mu)

# 2. Movie-Effekte neu berechnen
movie_avg_time <- train_set_time_adjusted %>% 
  group_by(movieId) %>% 
  summarize(b_i_time = mean(rating_time_adjusted - mu))

# 3. User-Effekte neu berechnen
user_avg_time <- train_set_time_adjusted %>% 
  left_join(movie_avg_time, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u_time = mean(rating_time_adjusted - mu - b_i_time))

# 4. Release Year-Effekte neu berechnen
released_avg_time <- train_set_time_adjusted %>% 
  left_join(movie_avg_time, by='movieId') %>%
  left_join(user_avg_time, by='userId') %>%
  group_by(released) %>%
  summarize(b_r_time = mean(rating_time_adjusted - mu - b_i_time - b_u_time))

# 5. Genre-Effekte neu berechnen
genre_avg_time <- train_set_time_adjusted %>% 
  left_join(movie_avg_time, by='movieId') %>%
  left_join(user_avg_time, by='userId') %>%
  left_join(released_avg_time, by='released') %>%
  group_by(genres) %>%
  summarize(b_g_time = mean(rating_time_adjusted - mu - b_i_time - b_u_time - - b_r_time))


if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
# Arrange the plots in a 2x2 grid
grid.arrange(
  # Visualisierung der bereinigten Movie-Effekte
movie_avg_time %>% 
  ggplot(aes(b_i_time)) +
  geom_histogram(bins=30, fill = "skyblue", color = "black") +
  labs(x = "Average deviation (time-adjusted)", y = "Number of movies", 
       title = "Movie effect (time-adjusted)",
       caption = "Source: train data"),

# Visualisierung der bereinigten User-Effekte
user_avg_time %>% 
  ggplot(aes(b_u_time)) +
  geom_histogram(bins=30, fill = "skyblue", color = "black") +
  labs(x = "Average deviation (time-adjusted)", y = "Number of users", 
       title = "User effect (time-adjusted)",
       caption = "Source: train data"),

# Visualisierung der bereinigten User-Effekte
released_avg_time %>% 
  ggplot(aes(b_r_time)) +
  geom_histogram(bins=30, fill = "skyblue", color = "black") +
  labs(x = "Average deviation (time-adjusted)", y = "Number of years", 
       title = "Release year effect (time-adjusted)",
       caption = "Source: train data"),

# Visualisierung der bereinigten User-Effekte
genre_avg_time %>% 
  ggplot(aes(b_g_time)) +
  geom_histogram(bins=30, fill = "skyblue", color = "black") +
  labs(x = "Average deviation (time-adjusted)", y = "Genre", 
       title = "Genre effect (time-adjusted)",
       caption = "Source: train data"),
  ncol = 2
)



# 6. Predicted Ratings berechnen
predicted_ratings_time <- test_set %>% 
  mutate(time_effect = predict.gam(fit, newdata = .) - mu) %>%
  left_join(movie_avg_time, by='movieId') %>%
  left_join(user_avg_time, by='userId') %>%
  left_join(released_avg_time, by='released') %>%
  left_join(genre_avg, by='genres') %>%
  mutate(pred = mu + b_i_time + b_u_time + b_r_time + b_g + time_effect) %>%
  pull(pred)



# 7. RMSE evaluieren
model_time_rmse <- RMSE(predicted_ratings_time, test_set$rating)
suppressWarnings({
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Time-adjusted (Movie, User, Release year) + Genre effect",  
                                     RMSE = model_time_rmse ))
})

# Ergebnisse anzeigen
rmse_results %>% knitr::kable()



if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
# Arrange the plots in a 2x2 grid
grid.arrange(
##darstellungen wie die verschiedenen effekte sich über die zeit verhalten
#movie time effect
train_set %>%
  filter(date >= as.Date("1996-01-01")) %>%
  left_join(movie_avg, by = "movieId") %>%
  mutate(month = floor_date(date, "month")) %>%  # Aggregiere nach Monat
  group_by(movieId, month) %>%                  # Gruppiere nach Film und Monat
  summarize(avg_rating = mean(rating - mu), .groups = "drop") %>%  # Berechne Durchschnitt pro Monat
  ggplot(aes(month, avg_rating)) +
  geom_smooth(method = "gam", se = FALSE, color = "darkgreen") +  # Glättungseffekt
  labs(title = "Time vs. Movie effect (monthly averaged)",
       x = "Date", y = "Averaged deviation from mean", caption = "Source: train data") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y"),


#user time effect
train_set %>%
  filter(date >= as.Date("1996-01-01")) %>%
  left_join(user_avg, by = "userId") %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(userId, month) %>%
  summarize(avg_rating = mean(rating - mu), .groups = "drop") %>%
  ggplot(aes(month, avg_rating)) +
  geom_smooth(method = "gam", se = FALSE, color = "#5593ff") +
  labs(title = "Time vs. User effect (monthly averaged)",
       x = "Date", y = "Averaged deviation from mean", caption = "Source: train data") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y"),

#release time effect
train_set %>%
  filter(date >= as.Date("1996-01-01")) %>%
  left_join(released_avg, by = "released") %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(released, month) %>%
  summarize(avg_rating = mean(rating - mu), .groups = "drop") %>%
  ggplot(aes(month, avg_rating)) +
  geom_smooth(method = "gam", se = FALSE, color = "purple") +
  labs(title = "Time vs. Release year effect (monthly averaged)",
       x = "Date", y = "Averaged deviation from mean", caption = "Source: train data") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y"),


#genre time effect
# Genres in Hauptkategorien zusammenfassen
train_set %>%
  filter(date >= as.Date("1996-01-01")) %>%
  mutate(main_genre = str_extract(genres, "^[^|]+")) %>%  # Nimmt nur das erste Genre
  group_by(main_genre) %>%
  mutate(total_ratings = n()) %>%  # Zählt die Anzahl der Ratings pro Genre
  ungroup() %>%
  filter(main_genre %in% (train_set %>%  # Filtert auf die Top-10-Genres
                           mutate(main_genre = str_extract(genres, "^[^|]+")) %>%
                           group_by(main_genre) %>%
                           summarise(n = n(), .groups = "drop") %>%
                           arrange(desc(n)) %>%
                           slice_head(n = 10) %>%
                           pull(main_genre))) %>%
  left_join(genre_avg, by = "genres") %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(main_genre, month) %>%
  summarize(avg_rating = mean(rating - mu), .groups = "drop") %>%
  ggplot(aes(month, avg_rating, color = main_genre)) +
  geom_smooth(method = "gam", se = FALSE) +
  labs(title = "Time vs. Main Genre Effects (Monthly Averaged)",
       x = "Date", y = "Averaged Deviation from Mean", caption = "Source: train data", color = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  ylim(-0.7, 0.7),

  ncol = 2
)





# Count values out of range [0.5, 5.0]
greater_than_5 <- sum(predicted_ratings_time > 5)
less_than_0_5 <- sum(predicted_ratings_time < 0.5)

ggplot(data.frame(rating = predicted_ratings_time), aes(x = rating)) +
  geom_histogram(binwidth = 0.25, boundary = 0, color = "black", fill = "skyblue") +
  geom_vline(xintercept = c(0.5, 5.0), color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 6, by = 0.5)) +
  scale_y_sqrt(labels = label_comma()) +
  labs(x = "Rating",
       y = "Number of ratings", 
       caption = "Source: train data",
       subtitle = sprintf("Values < 0.5: %d, Values > 5: %d", less_than_0_5, greater_than_5)) +
  theme(plot.subtitle = element_text(size = rel(0.8)))



#correct values out of range
predicted_ratings_time <- pmin(pmax(predicted_ratings_time, 0.5), 5.0)
# evaluate RMSE
model_time_rmse_corr <- RMSE(predicted_ratings_time, test_set$rating)
suppressWarnings({
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Time-adjusted (Movie, User, Release year) + Genre effect + corr",  
                                     RMSE = model_time_rmse_corr ))
})

# Ergebnisse anzeigen
rmse_results %>% knitr::kable()


## Regularization

#Regularization auf movie, user, released and genre effect
lambdas <- seq(4.2, 5.0, 0.1)
rmses <- sapply(lambdas, function(l){
  movie_avg_reg <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  user_avg_reg <- train_set %>%
    left_join(movie_avg_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  released_avg_reg <- train_set %>%
    left_join(movie_avg_reg, by='movieId') %>%
    left_join(user_avg_reg, by='userId') %>%
    group_by(released) %>%
    summarize(b_r = sum(rating - mu - b_i - b_u)/(n()+l))
  #nicht time adjusted
  genre_avg_reg <- train_set %>%
    left_join(movie_avg_reg, by='movieId') %>%
    left_join(user_avg_reg, by='userId') %>%
    left_join(released_avg_reg, by='released') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_r)/(n()+l))
  
  predicted_ratings <-
    test_set %>%
    left_join(movie_avg_reg, by = "movieId") %>%
    left_join(user_avg_reg, by = "userId") %>%
    left_join(released_avg_reg, by = "released") %>%
    left_join(genre_avg_reg, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_r + b_g) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

#mit time effect
rmses_time <- sapply(lambdas, function(l){
  movie_avg_time_reg <- train_set_time_adjusted %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating_time_adjusted - mu)/(n()+l))
  user_avg_time_reg <- train_set_time_adjusted%>%
    left_join(movie_avg_time_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating_time_adjusted - b_i - mu)/(n()+l))
  released_avg_time_reg <- train_set_time_adjusted%>%
    left_join(movie_avg_time_reg, by='movieId') %>%
    left_join(user_avg_time_reg, by='userId') %>%
    group_by(released) %>%
    summarize(b_r = sum(rating_time_adjusted - mu - b_i - b_u)/(n()+l))
  #nicht time adjusted
  genre_avg_reg <- train_set%>%
    left_join(movie_avg_time_reg, by='movieId') %>%
    left_join(user_avg_time_reg, by='userId') %>%
    left_join(released_avg_time_reg, by='released') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_r)/(n()+l))
  
  predicted_ratings_time <-
    test_set %>%
    mutate(time_effect = predict.gam(fit, newdata = .) - mu) %>%
    left_join(movie_avg_time_reg, by = "movieId") %>%
    left_join(user_avg_time_reg, by = "userId") %>%
    left_join(released_avg_time_reg, by = "released") %>%
    left_join(genre_avg_reg, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_r + b_g + time_effect) %>%
    pull(pred)
  return(RMSE(predicted_ratings_time, test_set$rating))
})


ggplot(data.frame(lambdas = lambdas, rmse = c(rmses, rmses_time), 
                  group = rep(c("RMSE", "RMSE Time"), each = length(lambdas))),
       aes(x = lambdas, y = rmse, color = group)) +
  geom_point(size = 2) +  # Punkte hinzufügen
  geom_smooth(aes(linetype = group), method = "loess", se = FALSE, size = 0.7) +
  scale_color_manual(values = c("RMSE" = "darkgreen", "RMSE Time" = "#5593ff")) +
  scale_linetype_manual(values = c("RMSE" = "dashed", "RMSE Time" = "dashed")) +
  labs(
    x = "Lambda",
    y = "RMSE Values",
    caption = "Source: train data",
    color = NULL,  # Legendentitel entfernen
    linetype = NULL  # Legendentitel für Linientypen entfernen
  ) 




lambda <- lambdas[which.min(rmses_time)]

#time adjusted
movie_avg_time_reg <- train_set_time_adjusted %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating_time_adjusted - mu)/(n()+lambda))
user_avg_time_reg <- train_set_time_adjusted%>%
  left_join(movie_avg_time_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating_time_adjusted - b_i - mu)/(n()+lambda))
released_avg_time_reg <- train_set_time_adjusted%>%
  left_join(movie_avg_time_reg, by='movieId') %>%
  left_join(user_avg_time_reg, by='userId') %>%
  group_by(released) %>%
  summarize(b_r = sum(rating_time_adjusted - mu - b_i - b_u)/(n()+lambda))
#nicht time adjusted
genre_avg_reg <- train_set%>%
  left_join(movie_avg_time_reg, by='movieId') %>%
  left_join(user_avg_time_reg, by='userId') %>%
  left_join(released_avg_time_reg, by='released') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_r)/(n()+lambda))

predicted_ratings_time <-
  test_set %>%
  mutate(time_effect = predict.gam(fit, newdata = .) - mu) %>%
  left_join(movie_avg_time_reg, by = "movieId") %>%
  left_join(user_avg_time_reg, by = "userId") %>%
  left_join(released_avg_time_reg, by = "released") %>%
  left_join(genre_avg_reg, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_r + b_g + time_effect) %>%
  pull(pred)

#correct values out of range
predicted_ratings_time <- pmin(pmax(predicted_ratings_time, 0.5), 5.0)
# evaluate RMSE

suppressWarnings({
model_time_reg_rmse_corr <- RMSE(predicted_ratings_time, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized, Time-adjusted (Movie, User, Release year) + Genre + corr",  
                                     RMSE = model_time_reg_rmse_corr ))
})

rmse_results %>% knitr::kable()


## Final holdout test

final_holdout_test$date <- as.Date(as.POSIXct(final_holdout_test$timestamp, origin = "1970-01-01"))
final_holdout_test <- final_holdout_test %>%
  mutate(released = str_extract(title, "\\((\\d{4})\\)") %>% 
           str_remove_all("[()]") %>% 
           as.numeric())
final_holdout_test <- final_holdout_test %>% mutate(weekNumber = as.integer(difftime(date, min(date), units = "weeks")) + 1)

predicted_ratings_final <- 
  final_holdout_test %>% 
  mutate(time_effect = predict.gam(fit, newdata = .) - mu) %>%
  left_join(movie_avg_time_reg, by = "movieId") %>%
  left_join(user_avg_time_reg, by = "userId") %>%
  left_join(released_avg_time_reg, by = "released") %>%
  left_join(genre_avg_reg, by = "genres") %>%
  mutate(pred = mu + coalesce(b_i, 0) + b_u + b_r + b_g + time_effect) %>%
  pull(pred)

suppressWarnings({
model_time_final_rmse <- RMSE(predicted_ratings_final, final_holdout_test$rating)
rmse_results_final <- data.frame(Method = character(), RMSE = numeric())
rmse_results_final <- bind_rows(rmse_results_final,
                          data_frame(Method="Regularized, Time-adjusted (Movie, User, Release year) + Genre (final test)",  
                                     RMSE = model_time_final_rmse ))
})

#werte ausser range korrigieren
predicted_ratings_final <- pmin(pmax(predicted_ratings_final, 0.5), 5.0)
# RMSE evaluieren
model_time_final_rmse_corr <- RMSE(predicted_ratings_final, final_holdout_test$rating)

suppressWarnings({
rmse_results_final <- bind_rows(rmse_results_final,
                          data_frame(Method="Regularized, Time-adjusted (Movie, User, Release year) + Genre + corr (final test)",  
                                     RMSE = model_time_final_rmse_corr ))
})

# Ergebnisse anzeigen
rmse_results_final %>% knitr::kable()


#Final holdout test with and without correction to see the difference.


#chart of deviations
final_holdout_test %>%
  mutate(deviation = predicted_ratings_final - rating) %>%
  {  
    deviation_data <- .
    ggplot(deviation_data, aes(deviation)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      geom_vline(xintercept = model_time_final_rmse_corr, linetype = "dashed", color = "red", size = 0.35) + 
      geom_vline(xintercept = -model_time_final_rmse_corr, linetype = "dashed", color = "red", size = 0.35) + 
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ",")) +
      labs(
        x = "Absolute deviation",
        y = "Number of predictions",
        caption = "Source: final_holdout_test data",
        subtitle = paste0("RMSE: ", round(model_time_final_rmse_corr, 7), 
                          " | Percentage within ±1 RMSE: ", 
                          round(mean(abs(deviation_data$deviation) <= model_time_final_rmse_corr) * 100, 1), "%")
      ) +
      theme(plot.subtitle = element_text(size = rel(0.8)))
  }
