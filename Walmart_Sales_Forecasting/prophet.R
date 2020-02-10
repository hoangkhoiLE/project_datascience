library(dplyr)  
library(prophet)    


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

View(list_store_dept)
for (val in list_store_dept)
{
  # s <- strsplit(val, " ", fixed = TRUE)
  # dept_tmp = as.numeric(s[[1]][2] )
  # store_tmp = as.numeric(s[[1]][1] )
  print(val)
  #create a limit of record in training data
  
  dept_store_train <- train %>% filter(Dept_Store == val) 
  if (nrow(dept_store_train) > 10)
  {  
    dept_store_test <- final_test_tmp %>% filter(Dept_Store == val ) 
    #data preprocessing for prototype algorithm
    
    holiday_dates_train <- dept_store_train %>% filter((IsHoliday == TRUE)) %>% select(Date) %>% unique() %>%c()
    holidays_train <- tibble(holiday = "holiday", ds = holiday_dates_train$Date,lower_window = 0,upper_window = 1)
    
    holiday_dates_test <- dept_store_train %>% filter((IsHoliday == TRUE)) %>% select(Date) %>% unique() %>%c()
    holidays_test <- tibble(holiday = "holiday", ds = holiday_dates_test$Date,lower_window = 0,upper_window = 1)
    train_ts <- dept_store_train %>% select(Date, Weekly_Sales)
    test_ts <- dept_store_test %>% select(Date)
    
    prophet_ts <- train_ts %>%rename("ds" = Date,"y" = Weekly_Sales)
    prophet_model <- prophet(prophet_ts,holidays = rbind( holidays_train,holidays_test))
    # 
    # Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
    # Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.
    
    prophet_ts <- train_ts %>%rename("ds" = Date,"y" = Weekly_Sales)
    #training model
    prophet_model <- prophet(prophet_ts,holidays = rbind( holidays_train,holidays_test))
    #runing prediction
    week <-  as.numeric(difftime(max(test_ts$Date), min(test_ts$Date),units="weeks")) + 1
    future <-make_future_dataframe(prophet_model, periods = week, freq = "week")
    prophet_forecast <- predict(prophet_model, future)
    #save résultat
    prophet_forecast$Date <-as.Date(prophet_forecast$ds)
    pred_tmp <- prophet_forecast%>%select(Date, yhat)
    names(pred_tmp)[2] <- "Predict"
    dept_store_test <- merge(pred_tmp,dept_store_test) 
    dept_store_test <- dept_store_test%>% select(Dept_Store,Date, Predict)
    csv_file$Predict <- replace(csv_file$Predict, (csv_file$Date %in% dept_store_test$Date) & (csv_file$Dept_Store %in% dept_store_test$Dept_Store), dept_store_test$Predict)
    #save model
    name_file <- c("./model/type1",val,"final_model.rds")
    name_file <- paste(name_file, collapse="_")
    saveRDS(prophet_model, name_file)
  }else{
    print(2)
    s <- strsplit(val, " ", fixed = TRUE)
    #get data by departement
    dept_tmp = as.numeric(s[[1]][2] )
    dept_store_train <- train[train$Dept == dept_tmp,] 
    dept_store_test <- final_test_tmp %>% filter(Dept_Store == val ) 
    # not remove size and storetype     data preprocessing for prototype algorithm
    holiday_dates_train <- dept_store_train %>% filter((IsHoliday == TRUE)) %>% select(Date) %>% unique() %>%c()
    holidays_train <- tibble(holiday = "holiday", ds = holiday_dates_train$Date,lower_window = 0,upper_window = 1)
    holiday_dates_test <- dept_store_train %>% filter((IsHoliday == TRUE)) %>% select(Date) %>% unique() %>%c()
    holidays_test <- tibble(holiday = "holiday", ds = holiday_dates_test$Date,lower_window = 0,upper_window = 1)
    train_ts <- dept_store_train %>% select(Date, Weekly_Sales)
    test_ts <- dept_store_test %>% select(Date)
    prophet_ts <- train_ts %>%rename("ds" = Date,"y" = Weekly_Sales)
    #training model
    prophet_model <- prophet(prophet_ts,holidays = rbind( holidays_train,holidays_test))
    week <-  as.numeric(difftime(max(test_ts$Date), min(test_ts$Date),units="weeks")) + 1
    future <-make_future_dataframe(prophet_model, periods = week, freq = "week")
    #runing prediction
    prophet_forecast <- predict(prophet_model, future)
    #save résultat
    prophet_forecast$Date <-as.Date(prophet_forecast$ds)
    pred_tmp <- prophet_forecast%>%select(Date, yhat)
    names(pred_tmp)[2] <- "Predict"
    dept_store_test <- merge(pred_tmp,dept_store_test) 
    dept_store_test <- dept_store_test%>% select(Dept_Store,Date, Predict)
    #save model
    csv_file$Predict <- replace(csv_file$Predict, (csv_file$Date %in% dept_store_test$Date) & (csv_file$Dept_Store %in% dept_store_test$Dept_Store), dept_store_test$Predict)
    name_file <- c("./model/type2",val,"final_model.rds")
    name_file <- paste(name_file, collapse="_")
    saveRDS(prophet_model, name_file)
  }
}

csv_file_export <- csv_file %>% select(-Dept_Store)
csv_file_export <- csv_file_export[order(csv_file_export$Store, csv_file_export$Dept),]
write.csv(file="result.csv", x= csv_file_export,  row.names = FALSE)



