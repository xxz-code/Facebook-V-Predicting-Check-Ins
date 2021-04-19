
# Facebook V: Predicting Check Ins (Kaggle)

In this team presentaion, 

### Markdown



### Loading libraries 
```
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
```
.
```
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
```
<details>
<summary>Example</summary>
This is a dropdown with text!
</details>

devtools::install_github("rstudio/rmarkdown")
---
title: "Tabset Example"
output: html_document
---

# The Tabset Section {.tabset .tabset-fade}

## First Tab
Here is the first tab's content.

## Second Tab
Here is the second tab's content
```
```
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
```
- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```
---
title: "This is awkward"
author: "Team 13"
output:
  prettydoc::html_pretty:
    theme: cayman
    highlight: github
    keep_md: true
---
`


## Hello Everyone, I do data and I love cats

![Cats are cute](https://pixabay.com/get/g7f5c8fe15f32f9093c3aa060b1857cea2ee68b9c9aec4feef526a3ea2ad2a194ae3a5ee61105c4746d03fde47bba7ebf_640.jpg?attachment=)

## Here's some of my repository .



## Here's some random Plot of Data



```r
plot
```

```
## Warning: `arrange_()` is deprecated as of dplyr 0.7.0.
## Please use `arrange()` instead.
## See vignette('programming') for more help
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```

<!--html_preserve--><div id="htmlwidget-2efffd4b35f29dfde373" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-2efffd4b35f29dfde373">{"x":{"visdat":{"4a34a7e5ec9":["function () ","plotlyVisDat"]},"cur_data":"4a34a7e5ec9","attrs":{"4a34a7e5ec9":{"x":{},"y":{},"z":{},"color":{},"colors":["#BF382A","#0C4B8E"],"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter3d","mode":"markers","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"scene":{"xaxis":{"title":"Weight"},"yaxis":{"title":"Gross horsepower"},"zaxis":{"title":"1/4 mile time"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[3.215,3.44,3.46,3.57,3.19,3.15,3.44,3.44,4.07,3.73,3.78,5.25,5.424,5.345,2.465,3.52,3.435,3.84,3.845],"y":[110,175,105,245,62,95,123,123,180,180,180,205,215,230,97,150,150,245,175],"z":[19.44,17.02,20.22,15.84,20,22.9,18.3,18.9,17.4,17.6,18,17.98,17.82,17.42,20.01,16.87,17.3,15.41,17.05],"type":"scatter3d","mode":"markers","name":"Automatic","marker":{"color":"rgba(191,56,42,1)","line":{"color":"rgba(191,56,42,1)"}},"textfont":{"color":"rgba(191,56,42,1)"},"error_y":{"color":"rgba(191,56,42,1)"},"error_x":{"color":"rgba(191,56,42,1)"},"line":{"color":"rgba(191,56,42,1)"},"frame":null},{"x":[2.62,2.875,2.32,2.2,1.615,1.835,1.935,2.14,1.513,3.17,2.77,3.57,2.78],"y":[110,110,93,66,52,65,66,91,113,264,175,335,109],"z":[16.46,17.02,18.61,19.47,18.52,19.9,18.9,16.7,16.9,14.5,15.5,14.6,18.6],"type":"scatter3d","mode":"markers","name":"Manual","marker":{"color":"rgba(12,75,142,1)","line":{"color":"rgba(12,75,142,1)"}},"textfont":{"color":"rgba(12,75,142,1)"},"error_y":{"color":"rgba(12,75,142,1)"},"error_x":{"color":"rgba(12,75,142,1)"},"line":{"color":"rgba(12,75,142,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

