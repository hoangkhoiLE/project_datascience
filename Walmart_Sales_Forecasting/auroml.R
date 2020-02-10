packages <-c(
  "caret",
  "forecast",
  "h2o",
  "janitor",
  "lubridate",
  "prophet",
  "readxl",
  "tidyverse",
  "timetk",
  "vctrs",
  "randomForest",
  "xgboost",
  "e1071"
)
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
View(train)
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

final_test$Dept_Store <- paste(final_test$Store,final_test$Dept)
train$Dept_Store <- paste(train$Store, train$Dept)

list_store_dept <- unique(final_test$Dept_Store)


final_test_tmp <- data.frame(final_test)
csv_file <- final_test_tmp %>% select(Store, Dept, Date, IsHoliday, Dept_Store )
csv_file$Predict <- NA


for (val in list_store_dept)
{
  # s <- strsplit(val, " ", fixed = TRUE)
  # dept_tmp = as.numeric(s[[1]][1] )
  # store_tmp = as.numeric(s[[1]][2] )
  print(val)
  dept_store_train <- train %>% filter(Dept_Store == val) 
  if (nrow(dept_store_train) > 10)
  {
    dept_store_test <- final_test_tmp %>% filter(Dept_Store == val )
    train_ml <- dept_store_train %>%  filter( (dept_store_train$year < 2012) | (dept_store_train$year >= 2012 & dept_store_train$month < 8 )  ) %>% select(-Dept_Store,-Date,-Store,-Dept,-Type,-StoreType,-Size)
    validation_ml <- dept_store_train %>% filter( (dept_store_train$year >= 2012) & (dept_store_train$month >= 8  )) %>% select(-Date,-Store,-Dept,-Type,-StoreType,-Size,-Dept_Store)
    test_ml <- dept_store_test %>% select(-Date,-Store,-Dept,-Type,-StoreType,-Size, -Dept_Store)
    train_h2o_xreg <- as.h2o(train_ml)
    test_h2o_xreg <- as.h2o(test_ml)
    validation_h2o_xreg <- as.h2o(validation_ml)
    y <- "Weekly_Sales"
    x <- setdiff(names(train_h2o_xreg), y)
    h2o_model_xreg <- h2o.automl(
      x = x,
      y = y,
      training_frame = train_h2o_xreg,
      leaderboard_frame = validation_h2o_xreg,
      max_runtime_secs = 120,
      stopping_metric = "RMSE"
    )
    h2o_leader_xreg <- h2o_model_xreg@leader
    h2o_xreg_pred <- h2o.predict(h2o_leader_xreg, test_h2o_xreg)
    dept_store_test$Predict  <- as.list(h2o_xreg_pred)
    dept_store_test <- dept_store_test%>% select(Dept_Store,Date, Predict)
    csv_file$Predict <- replace(csv_file$Predict, (csv_file$Date %in% dept_store_test$Date) & (csv_file$Dept_Store %in% dept_store_test$Dept_Store), dept_store_test$Predict)
    name_file <- c("./model/type2",val,"final_model.rds")
    name_file <- paste(name_file, collapse="_")
    saveRDS(h2o_xreg_pred, name_file)
    # final_test_tmp <- merge(x = final_test_tmp, y = dept_store_test, by.x  = c("Dept_Store","Date"),by.y  = c("Dept_Store","Date"), all.x = TRUE)
  }else{
    s <- strsplit(val, " ", fixed = TRUE)
    dept_tmp = as.numeric(s[[1]][2] )
    #store_tmp = as.numeric(s[[1]][2] )
    dept_store_train <- train[train$Dept == dept_tmp,] 
    dept_store_test <- final_test_tmp %>% filter(Dept_Store == val )
    train_ml <- dept_store_train %>%  filter( (dept_store_train$year < 2012) | (dept_store_train$year >= 2012 & dept_store_train$month < 8 )  )  %>% select(-Date,-Store,-Dept,-Type, -Dept_Store) 
    validation_ml <- dept_store_train %>% filter( dept_store_train$year >= 2012 & dept_store_train$month >= 8  )   %>% select(-Date,-Store,-Dept,-Type, -Dept_Store) 
    test_ml <- dept_store_test  %>% select(-Date,-Store,-Dept,-Type, -Dept_Store) 
    train_h2o_xreg <- as.h2o(train_ml)
    validation_h2o_xreg  <- as.h2o(validation_ml)
    test_h2o_xreg <- as.h2o(test_ml)
    y <- "Weekly_Sales"
    x <- setdiff(names(train_h2o_xreg), y)
    h2o_model_xreg <- h2o.automl(
      x = x,
      y = y,
      training_frame = train_h2o_xreg,
      leaderboard_frame = validation_h2o_xreg,
      max_runtime_secs = 120,
      stopping_metric = "RMSE"
    )
    h2o_leader_xreg <- h2o_model_xreg@leader
    h2o_xreg_pred <- h2o.predict(h2o_leader_xreg, test_h2o_xreg)
    dept_store_test$Predict  <- as.list(h2o_xreg_pred)
    dept_store_test <- dept_store_test%>% select(Dept_Store,Date, Predict)
    # final_test_tmp <- merge(x = final_test_tmp, y = dept_store_test, by.x  = c("Dept_Store","Date"),by.y  = c("Dept_Store","Date"), all.x = TRUE)
    csv_file$Predict <- replace(csv_file$Predict, (csv_file$Date %in% dept_store_test$Date) & (csv_file$Dept_Store %in% dept_store_test$Dept_Store), dept_store_test$Predict)
    name_file <- c("./model/type2",val,"final_model.rds")
    name_file <- paste(name_file, collapse="_")
    saveRDS(h2o_xreg_pred, name_file)
  }

}

csv_file_export <- csv_file %>% select(-Dept_Store)
csv_file_export <- csv_file_export[order(csv_file_export$Store, csv_file_export$Dept),]
csv_file_export <- as.data.frame(lapply(csv_file_export, unlist))

write.csv(file="result.csv", x= csv_file_export,  row.names = FALSE)

