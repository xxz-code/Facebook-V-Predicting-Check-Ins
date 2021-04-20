rm(list = ls())


library(data.table) #reading in the data
library(dplyr) #dataframe manipulation
library(ggplot2) #viz
library(ranger) #the random forest implementation
library(plotly) #3D plotting
library(tidyr) #dataframe manipulation
library(FNN) #k nearest neighbors algorithm
library(xgboost)
library(randomForest)
library(gbm)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(MASS)
library(pander)
library(fromattable)
library(viridis)




setwd("~/MSBA Program/Machine Learning 2/Group Final Project")

#Fread is like read.csv but faster. Given the size of the data, it is needed in the implementation
fb <- fread(file = 'train.csv', integer64 = "character", showProgress = TRUE)

fb %>% filter(x >1, x <1.25, y >2.5, y < 2.75) -> fb
head(fb, 3)

fb$hour = (fb$time/60) %% 24
fb$weekday = (fb$time/(60*24)) %% 7
fb$month = (fb$time/(60*24*30)) %% 12 #month-ish
fb$year = fb$time/(60*24*365)
fb$day = fb$time/(60*24) %% 365

small_train = fb[fb$time < 7.3e5,]
small_val = fb[fb$time >= 7.3e5,]

#Viewing the distribution of place_id
#fb %>%
 # sample_frac(0.01) %>%
  #ggplot(aes(x = place_id)) + geom_density()


#view the x and y hot spots for accuracy
#fb %>% sample_frac(0.01) %>% ggplot(aes(x,y, z = accuracy)) +
 # stat_summary_2d(fun = mean, bins = 50) +
  #scale_fill_viridis()

#fb %>% 
 # sample_frac(0.01) %>%
  #ggplot(aes(x = place_id)) + geom_density()
    


ggplot(small_train, aes(x, y )) +
  geom_point(aes(color = place_id)) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Check-ins colored by place_id")

small_train %>% count(place_id) %>% filter(n > 500) -> ids
small_trainz = small_train[small_train$place_id %in% ids$place_id,]

plot_ly(data = small_trainz, x = small_trainz$x , y = small_trainz$y, z = small_trainz$hour, color = small_trainz$place_id,  type = "scatter3d", mode = "markers",
        marker=list(size= 5)) %>% layout(title = "Place_id's by position and Time of Day")

plot_ly(data = small_trainz, x = small_trainz$x , y = small_trainz$y, z = small_trainz$weekday, color = small_trainz$place_id,  type = "scatter3d", mode = "markers",
        marker=list(size= 5)) %>% layout(title = "Place_id's by position and Day of Week")

length(unique(small_train$place_id))
#[1] 770

small_train %>% count(place_id) %>% filter(n > 3) -> ids
small_train = small_train[small_train$place_id %in% ids$place_id,]


#######################
##K Nearest Neighbors##
#######################
s = 2
l = 125
w = 500

create_matrix = function(train) {
  cbind(s*train$y,
        train$x,
        train$hour/l,
        train$weekday/w,
        train$year/w,
        train$month/w,
        train$time/(w*60*24*7))
}

X = create_matrix(small_train)
X_val = create_matrix(small_val)

model_knn = FNN::knn(train = X, test = X_val, cl = small_train$place_id, k = 15)

preds <- as.character(model_knn)
truth <- as.character(small_val$place_id)
mean(truth == preds)
#[1] 0.5151964


######################
####Random Forrest####
######################
set.seed(131L)

#start off by switching place id to a factor

small_train$place_id <- as.factor(small_train$place_id)

str(small_train)

rf.model <- randomForest(place_id~x + y + accuracy + hour + weekday + month + year, data = small_train)
rf.model

pred <- predict(rf.model, small_val)
pred <- pred$predictions

accuracy = mean(pred == small_val$place_id)
accuracy
#[1] 0.5604151

oob.err = double(7)
test.err = double(7)

for(mtry in 1:7){
  fit = randomForest(small_train$place_id~small_train$x + small_train$y + small_train$accuracy + small_train$hour + small_train$weekday + small_train$month + small_train$year, data = small_train, mtry = mtry, ntree = 400)
  oob.err = fit$mse[400]
  pred2 = predict(fit, small_val$place_id)
  test.err[mtry] = with(small_val$place_id, mean((medv-pred)^2))
  cat(mtry, '')
}

#fit = randomForest(place_id~x + y + accuracy + hour + weekday + month + year, data = small_train, mtry = 7, ntree = 400)
#fit$mse
#?randomForest()
#fit

