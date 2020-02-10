packages <-c("e1071","tseries","caret","dplyr","forecast","h2o","lubridate","prophet","tidyverse","randomForest","xgboost")
lapply(packages, library, character.only = TRUE)
h2o.init()
h2o.no_progress()


df_stores <- read.csv(file="./data/stores.csv")
df_features <- read.csv(file="./data/features.csv")
df_train <- read.csv(file="./data/train.csv")
df_test <- read.csv(file="./data/test.csv")

# check missing values
#colSums(is.na(df_train))
#colSums(is.na(df_features))
#colSums(is.na(df_stores))

#fill markdown by 0
df_features$MarkDown1[is.na(df_features$MarkDown1)] <- 0
df_features$MarkDown2[is.na(df_features$MarkDown2)] <- 0
df_features$MarkDown3[is.na(df_features$MarkDown3)] <- 0
df_features$MarkDown4[is.na(df_features$MarkDown4)] <- 0
df_features$MarkDown5[is.na(df_features$MarkDown5)] <- 0

# fill CPI and Unemployement
df_features$CPI[is.na(df_features$CPI)] <- mean(df_features$CPI, na.rm = TRUE)
df_features$Unemployment[is.na(df_features$Unemployment)] <- mean(df_features$Unemployment, na.rm = TRUE)

df_stores$StoreType = 0
for( i in 1:nrow(df_stores))
{
  if( df_stores$Type[i] == "A")
    df_stores$StoreType[i] <- 1
  if( df_stores$Type[i] == "B")
    df_stores$StoreType[i] <- 2
  if( df_stores$Type[i] == "C")
    df_stores$StoreType[i] <- 3
}

# Merge Type and Size
df_TrainTmp <- merge(x=df_train, y=df_stores, all.x=TRUE)
df_TestTmp <- merge(x=df_test, y=df_stores, all.x=TRUE)

# Merge all the features
train <- merge(x=df_TrainTmp, y=df_features, all.x=TRUE)
final_test <- merge(x=df_TestTmp, y=df_features, all.x=TRUE)


train$year <- as.numeric(substr(train$Date,1,4))
train$month <- as.numeric(substr(train$Date,6,7))
train$day <- as.numeric(substr(train$Date,9,10))
train$IsHoliday[train$IsHoliday=="TRUE"]<- 1
train$IsHoliday[train$IsHoliday=="FALSE"] <- 0
train$days_of30 <- (train$month-1)*30 + train$day
train$dayHoliday <- train$IsHoliday*train$days_of30

final_test$year = as.numeric(substr(final_test$Date,1,4))
final_test$month = as.numeric(substr(final_test$Date,6,7))
final_test$day = as.numeric(substr(final_test$Date,9,10))
final_test$IsHoliday[final_test$IsHoliday=="TRUE"]=1
final_test$IsHoliday[final_test$IsHoliday=="FALSE"]=0
final_test$days_of30 = (final_test$month-1)*30 + final_test$day
final_test$dayHoliday <- final_test$IsHoliday*final_test$days_of30

train$Date <- as.Date(train$Date,  format = "%Y-%m-%d")
final_test$Date <- as.Date(final_test$Date, format = "%Y-%m-%d")

#date max 2012-10-26 in train dataset
train[which.max(as.POSIXct(train$Date)), ]
#date min 2010-02-05 in train dataset
train[which.min(as.POSIXct(train$Date)), ]

#Training a model
# Create a list of holidays for use in one of the models
holiday_dates <- train %>% filter((IsHoliday == TRUE) & (year < 2012)) %>% select(Date) %>% unique() %>%c()
#holiday_dates <- train %>% filter((IsHoliday == TRUE)) %>% filter (Date, date("2010-02-05":"2012-01-01")) %>% select(Date) %>% unique() %>%c()
holidays <- tibble(holiday = "holiday", ds = holiday_dates$Date,lower_window = 0,upper_window = 1)

#Choose store 1 and departement 1 Ex: 1-1
dept_1_store_1 <- train %>% filter(Store == 1 & Dept == 1 ) 

#Adapter les données suivi le prototype de algorithm
train_ts<-dept_1_store_1 %>% filter( (dept_1_store_1$year < 2012) | (dept_1_store_1$year >= 2012 & dept_1_store_1$month < 8 )  )  %>% select(Date, Weekly_Sales)
test_ts <-dept_1_store_1 %>% filter( dept_1_store_1$year >= 2012 & dept_1_store_1$month >= 8  )  %>% select(Date, Weekly_Sales)

train_ts_xreg<-dept_1_store_1 %>% filter( (dept_1_store_1$year < 2012) | (dept_1_store_1$year >= 2012 & dept_1_store_1$month < 8 )  )  %>%select(IsHoliday,Temperature, Fuel_Price,MarkDown1,MarkDown2, MarkDown3, MarkDown4,MarkDown5, CPI, Unemployment,days_of30,dayHoliday)
test_ts_xreg <-dept_1_store_1 %>% filter( dept_1_store_1$year >= 2012 & dept_1_store_1$month >= 8  )  %>% select(IsHoliday,Temperature, Fuel_Price,MarkDown1,MarkDown2, MarkDown3, MarkDown4,MarkDown5, CPI, Unemployment,days_of30,dayHoliday)

train_ts_xreg_matrix<-data.matrix(train_ts_xreg, rownames.force = NA)
test_ts_xreg_matrix<-data.matrix(test_ts_xreg, rownames.force = NA)

train_ml<-dept_1_store_1 %>% filter( (dept_1_store_1$year < 2012) | (dept_1_store_1$year >= 2012 & dept_1_store_1$month < 8 )  )  
test_ml <-dept_1_store_1 %>% filter( dept_1_store_1$year >= 2012 & dept_1_store_1$month >= 8  )  

train_ml <- train_ml %>% select(-Date,-Store,-Dept,-Type,-StoreType,-Size)
test_ml <- test_ml %>% select(-Date,-Store,-Dept,-Type,-StoreType,-Size)

#adf test and decomposion
# adf.test(train$Weekly_Sales)
# ts_1_1 <-ts(train_ts$Weekly_Sales,start = c(2010, 5),frequency = 52)
# train_dec <-decompose(ts_1_1 )
# plot(train_dec)

# Arima
fit_arima_xreg <- auto.arima(train_ts$Weekly_Sales, seasonal = TRUE, stationary = TRUE,xreg = train_ts_xreg_matrix)
fit_arima <- auto.arima(train_ts$Weekly_Sales,seasonal = TRUE, stationary = TRUE)
#neural network

nnet_model_xreg <- nnetar(train_ts$Weekly_Sales, size = 8, repeats = 1000,xreg = train_ts_xreg)
nnet_model <- nnetar(train_ts$Weekly_Sales, size = 8, repeats = 1000)

#prophet
prophet_1_1 <- train_ts %>%rename("ds" = Date,"y" = Weekly_Sales)
prophet_model <- prophet(prophet_1_1,holidays = holidays)
future <-make_future_dataframe(prophet_model, periods = 13, freq = "week")
prophet_forecast <- predict(prophet_model, future)

#auto ml
train_h2o <- as.h2o(train_ts)
test_h2o <- as.h2o(test_ts)

y <- "Weekly_Sales"
x <- setdiff(names(train_h2o), y)

h2o_model <- h2o.automl(x = x,y = y,training_frame = train_h2o,leaderboard_frame = test_h2o,max_runtime_secs = 120,stopping_metric = "RMSE")
h2o_leader <- h2o_model@leader
#automlwith data xreg

library(timetk)
# H2O
train_h2o_xreg <- as.h2o(train_ml)
test_h2o_xreg <- as.h2o(test_ml)

y <- "Weekly_Sales"
x <- setdiff(names(train_h2o_xreg), y)

h2o_model_xreg <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o_xreg,
  leaderboard_frame = test_h2o_xreg,
  max_runtime_secs = 120,
  stopping_metric = "RMSE"
)

h2o_leader_xreg <- h2o_model_xreg@leader

#randomforest - data normal 

train.control <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
my_grid <- expand.grid(mtry = c(3, 4, 5))
model_rf_list <- list()
for (i in seq(100, 200, by = 25)) {
  set.seed(i)
  rf <- train( Weekly_Sales ~., data = train_ts, method = "rf", metric = "RMSE", 
               tuneGrid = my_grid, ntree = i, 
               trControl = train.control)
  key <- toString(i)
  model_rf_list[[key]] <- rf
}
result <- resamples(model_rf_list)
summary(result)
model_rf_list$'200'$bestTune
rf_model <-  randomForest( Weekly_Sales ~., data = train_ts, ntree=200, mtry=3, replace=TRUE)
rf_pred <- predict(rf_model, test_ts)

model_rf_list_xreg <- list()
for (i in seq(100, 200, by = 25)) {
  set.seed(i)
  rf_xreg <- train( Weekly_Sales ~., data = train_ml, method = "rf", metric = "RMSE", 
                    tuneGrid = my_grid, ntree = i, 
                    trControl = train.control)
  key <- toString(i)
  model_rf_list_xreg[[key]] <- rf_xreg
}
# View random forest
result_rf_xreg <- resamples(model_rf_list_xreg)
summary(result_rf_xreg)
model_rf_list_xreg$'150'$bestTune
rf_model_xreg <-  randomForest( Weekly_Sales ~., data = train_ml, ntree=150, replace=TRUE, mtry=5)
rf_pred_xreg <- predict(rf_model_xreg, test_ml)


# View all result and plot comparing

pred_data <-  test_ts %>% 
  #add_column(nnet = forecast(nnet_model, h = 13,  xreg =test_ts$IsHoliday) %>% as_tibble() %>% pull(`Point Forecast`)) %>%
  add_column(nnet = forecast(nnet_model, h = 13) %>% as_tibble() %>% pull(`Point Forecast`)) %>%
  add_column(nnet_xreg  = forecast(nnet_model_xreg, h = 13, xreg = test_ts_xreg) %>% as_tibble() %>% pull(`Point Forecast`)) %>%
  add_column(arima_xreg  = forecast(fit_arima_xreg, h = 13, xreg = test_ts_xreg_matrix) %>% as_tibble() %>% pull(`Point Forecast`)) %>%
  add_column(arima = forecast(fit_arima, h = 13) %>% as_tibble() %>% pull(`Point Forecast`)) %>%
  add_column(prophet = predict(prophet_model, future) %>% as_tibble() %>% filter(year(ds) >= 2012 & month(ds) >=8) %>%  pull(yhat)) %>%
  add_column(h2o_automl = h2o.predict(h2o_leader, test_h2o) %>% as_tibble() %>% pull(predict)) %>%
  add_column(h2o_automl_xreg = h2o.predict(h2o_leader_xreg, test_h2o_xreg) %>% as_tibble() %>% pull(predict)) %>%
  add_column(rf_model = rf_pred) %>%
  add_column(rf_model_xreg = rf_pred_xreg) 


pred_data %>% gather("prediction", "value",-Date) %>% ggplot(aes(x = Date, y = value, color = prediction)) +
  geom_line() + 
  scale_x_date(date_breaks = "4 week", date_labels = "%B %d") +
  scale_color_manual(values = c("Weekly_Sales" = "red","rf_model"="gray","rf_model_xreg"="#e8e5ff","arima"= "blue","arima_xreg"= "green","h2o_automl" = "#a5d8f3","h2o_automl_xreg" = "black","nnet" = "#ff9de6"
                                ,"nnet_xreg" = "orange","prophet" = "#e8e500")) +
  theme_classic() +
  labs(title = "comparing model fit",subtitle = "test data: 8- 2012",x = "date",y = "weekly sales") + 
  theme(text = element_text(family = "Futura Medium"),plot.title = element_text(hjust = .5),plot.subtitle = element_text(hjust = .5),axis.text.x = element_text(angle = 45, hjust = 1))
# evaluation

forecast_measures <- function(df) {
  matrix <- matrix(nrow = 5, ncol = (ncol(df) - 2))
  for (i in 3:ncol(df)) {
    print(i)
    error <- df[2] - df[i]
    error_pct <- error / df[2]
    matrix[1, i - 2] <- colMeans(error)
    matrix[2, i - 2] <- sqrt(colMeans(error ^ 2)) #calculate Root Mean Square Error
    matrix[3, i - 2] <- colMeans(abs(error)) #calculate Mean Absolute Error
    matrix[4, i - 2] <- colMeans(abs(error_pct))
    matrix[5, i - 2] <- colMeans(error_pct)
    for (k in 1:ncol(matrix)) {
      data <- matrix[, k]
      data <- round(data, 2)
      matrix[, k] <- data
    }
  }
  colnames(matrix) <- colnames(df[, 3:ncol(df)])
  rownames(matrix) <- c("Mean Error",  "Root Mean Square Error","Mean Absolute Error", "Mean Absolute Percent Error","Mean Percent Error")
  print(matrix)
}

forecast_measures(pred_data)
# 
# #feature important
# varImpPlot(rf_model_xreg)
# plot(varImp(rf_model_xreg))
# 
# h2o.varimp(h2o_model_xreg@leader)
# h2o.varimp_plot(h2o_model_xreg@leader)

