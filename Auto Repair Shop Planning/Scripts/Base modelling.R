#### Load libraries
library(dplyr) # Used for data manipulation
library(knitr) # Used for R-Markdown knitting
library(kableExtra) # Used for Kable Tables
library(plotly) # Used for plotting
library(lubridate) #Used for Date manipulation
library(randomForest) # Used for building Random Forest models
library(rpart) # Used for building CART models
library(rpart.plot) # Used for plotting tree
library(xgboost) # Used for building boosted tree models


#### Load Data
crashes_df <- read.csv('../Data/Crashes.csv', stringsAsFactors = FALSE) %>% 
  mutate(CRASH.DATE = as.Date(CRASH.DATE, "%m/%d/%Y")) #1,896,229 x 29

person_df <- read.csv('../Data/Person.csv', stringsAsFactors = FALSE) %>% 
  mutate(CRASH_DATE = as.Date(CRASH_DATE, "%m/%d/%Y")) #4,692,054 x 21

vehicles_df <- read.csv('../Data/Vehicles.csv', stringsAsFactors = FALSE) %>% 
  mutate(CRASH_DATE = as.Date(CRASH_DATE, "%m/%d/%Y")) #3,704,406 x 25

# crashes_df
# min(crashes_df$CRASH.DATE) #"2012-07-01"
# max(crashes_df$CRASH.DATE) #"2022-07-02"

# person_df
# min(person_df$CRASH_DATE) #"2012-07-01"
# max(person_df$CRASH_DATE) #2022-07-02"

# vehicles_df
# min(vehicles_df$CRASH_DATE) #"2012-07-01"
# max(vehicles_df$CRASH_DATE) #"2021-12-04"


#### Show issue with Person Dataset
monthly_agg1 <- (person_df %>% 
                   mutate(yr_mo = paste0(substr(CRASH_DATE,1,4),'-',substr(CRASH_DATE,6,7))) %>% 
                   group_by(yr_mo) %>% 
                   summarise(n = n()) %>% 
                   arrange(yr_mo)
                 )

plot_ly(x=monthly_agg1$yr_mo, y=monthly_agg1$n, type='bar') %>%  # Need to use data after 2017-01-01 due to data issue with vehicles dataset
  layout(title = "Crash Data by Month from 2012-2022", xaxis = list(title = 'Month'), yaxis = list(title = 'Number of Crashes'))

#### Process data to get to desired structure for Collision Volume Modeling
combined_df <- (crashes_df %>% 
                  select(-c(CRASH.DATE, CRASH.TIME, CONTRIBUTING.FACTOR.VEHICLE.1, CONTRIBUTING.FACTOR.VEHICLE.2,
                            CONTRIBUTING.FACTOR.VEHICLE.3, CONTRIBUTING.FACTOR.VEHICLE.4, CONTRIBUTING.FACTOR.VEHICLE.5,
                            VEHICLE.TYPE.CODE.4, VEHICLE.TYPE.CODE.5,
                            NUMBER.OF.PEDESTRIANS.INJURED, NUMBER.OF.PEDESTRIANS.KILLED,
                            NUMBER.OF.CYCLIST.INJURED, NUMBER.OF.CYCLIST.KILLED,
                            ON.STREET.NAME, CROSS.STREET.NAME, OFF.STREET.NAME)) %>% 
                  inner_join(vehicles_df %>% 
                               filter(!is.na(VEHICLE_ID)) %>% 
                               select(-c(CRASH_DATE, CRASH_TIME, VEHICLE_ID))
                             , by = 'COLLISION_ID') %>% 
                  inner_join(person_df %>% 
                               select(-c(UNIQUE_ID, CONTRIBUTING_FACTOR_1, CONTRIBUTING_FACTOR_2))
                             , by= c('COLLISION_ID'='COLLISION_ID', 'UNIQUE_ID'='VEHICLE_ID')) %>% 
                  filter((PED_ROLE %in% c('Driver'))) %>%  #drivers only
                  filter((CRASH_DATE >= '2017-01-01') & (CRASH_DATE < '2021-12-01')) %>% #only from 2017-01-01 to 2021-11-30
                  filter(PERSON_AGE > 14 & PERSON_AGE< 101) %>% 
                  filter(PERSON_SEX == 'F' | PERSON_SEX=='M') %>% 
                  mutate(MONTH = substr(CRASH_DATE,6,7)) %>% 
                  mutate(TIMESTEP = as.period(interval(as.Date('2017-01-01'), CRASH_DATE)) %/% months(1)) %>% 
                  mutate(yr_mo = paste0(substr(CRASH_DATE,1,4),'-',substr(CRASH_DATE,6,7))) %>%
                  select(COLLISION_ID, CRASH_DATE, PERSON_AGE, PERSON_SEX, MONTH, TIMESTEP, yr_mo) %>% 
                  arrange(CRASH_DATE)
                )

# min(combined_df$CRASH_DATE) #"2017-01-01"
# max(combined_df$CRASH_DATE) #"2021-11-30"

# Aggregate, Plot of Volume Data Available for modeling
monthly_agg2 <- (combined_df %>% 
                   group_by(TIMESTEP, yr_mo, MONTH) %>% 
                   summarise(n = n(), .groups = 'drop') %>% 
                   arrange(TIMESTEP, yr_mo, MONTH)
                 )

plot_ly(x=monthly_agg2$yr_mo, y=monthly_agg2$n, type='bar') %>% # - Notice huge COVID Impact on this dataset!
  layout(title = "Crash Data by Month from 2017-2021", xaxis = list(title = 'Month'), yaxis = list(title = 'Number of Crashes'))

# Create Coarse Aggregate for modeling with feature groups
monthly_agg3 <- (combined_df %>% 
                   group_by(TIMESTEP, yr_mo, MONTH, PERSON_SEX, PERSON_AGE) %>% 
                   summarise(n = n(), .groups = 'drop') %>% 
                   arrange(TIMESTEP, yr_mo, MONTH, PERSON_SEX, PERSON_AGE)
)

        # Delete-later
          # #Create Training and Test data sets
          # train_df <- monthly_agg3 %>% filter(TIMESTEP <= 47) # <= 2020-12
          # test_df <- monthly_agg3 %>% filter(TIMESTEP > 47) # > 2020-12
        # write.csv('./timestep_data.csv',x = (monthly_agg3 %>% group_by(TIMESTEP, yr_mo) %>% summarise(n=n()) %>% arrange(TIMESTEP)))


#Create Training and Test data sets
train_df <- monthly_agg3 %>% filter(TIMESTEP <= 35) # 2017-01 to 2019-12
val_df <- monthly_agg3 %>% filter((TIMESTEP > 35) & (TIMESTEP <= 47)) # 2020-01 to 2020-12
test_df <- monthly_agg3 %>% filter(TIMESTEP > 47) # 2021-01 to 2021-11

#Create Train_Val data sets (final model after Train/Val hyper parameter tuning)
train_val_df <- monthly_agg3 %>% filter(TIMESTEP <= 47) # 2017-01 to 2020-12


#Train Linear Model
lm_model <- lm(data = train_df, formula = n ~ TIMESTEP + MONTH + PERSON_AGE + PERSON_SEX )
summary(lm_model)
SST_lm_train <- sum((train_df$n - mean(train_df$n))^2)
SSE_lm_train <- sum((train_df$n - predict(lm_model, newdata = train_df))^2)
R_sq_lm_train <- 1-(SSE_lm_train/SST_lm_train)
R_sq_lm_train #0.5799

# plot_ly(x=train_df$yr_mo, y=(predict(lm_model, newdata = train_df)-train_df$n), type='scatter', mode='markers') %>% 
#   layout(title = 'Plot of Linear Model Residuals vs yr_mo for Training Data',
#          xaxis = list(title = 'yr_mo'),
#          yaxis = list(title = 'LM Residuals', rangemode = "tozero"))

#Linear Model Validation Metrics
SST_lm_val <- sum((val_df$n - mean(val_df$n))^2)
SSE_lm_val <- sum((val_df$n - predict(lm_model, newdata = val_df))^2)
R_sq_lm_val <- 1-(SSE_lm_val/SST_lm_val)
R_sq_lm_val #-1.154703

# plot_ly(x=val_df$yr_mo, y=(predict(lm_model, newdata = val_df)-val_df$n), type='scatter', mode='markers') %>% 
#   layout(title = 'Plot of Linear Model Residuals vs yr_mo for Validation Data',
#          xaxis = list(title = 'yr_mo'),
#          yaxis = list(title = 'LM Residuals', rangemode = "tozero"))

#Train_Val Linear Model
lm_model_2 <- lm(data = train_val_df, formula = n ~ TIMESTEP + MONTH + PERSON_AGE + PERSON_SEX )
summary(lm_model_2)
SST_lm_train_val <- sum((train_val_df$n - mean(train_val_df$n))^2)
SSE_lm_train_val <- sum((train_val_df$n - predict(lm_model_2, newdata = train_val_df))^2)
R_sq_lm_train_val <- 1-(SSE_lm_train_val/SST_lm_train_val)
R_sq_lm_train_val #0.545718

plot_ly(x=train_val_df$yr_mo, y=(predict(lm_model_2, newdata = train_val_df)-train_val_df$n), type='scatter', mode='markers') %>% 
  layout(title = 'Plot of Linear Model Residuals vs yr_mo for Train_Val Data',
         xaxis = list(title = 'yr_mo'),
         yaxis = list(title = 'LM Residuals', rangemode = "tozero"))



# #Train & Val Random Forest Model - Loop - Run only once to get optimal settings
# rf_results <- data.frame()
# for(i in seq.int(1, 200)){
#   set.seed(12345)
#   rf_model <- randomForest(n ~ TIMESTEP + MONTH + PERSON_AGE + PERSON_SEX, data=train_df, importance=TRUE, ntree=i)  #
#   summary(rf_model)
#   var_importance_df <- data.frame(importance(rf_model)) %>% rename('PCT_IncMSE'='X.IncMSE') %>% arrange(desc(PCT_IncMSE))
#   var_importance_df
#   SST_rf_train <- sum((train_df$n - mean(train_df$n))^2)
#   SSE_rf_train <- sum((train_df$n - predict(rf_model, newdata = train_df))^2)
#   R_sq_rf_train <- 1-(SSE_rf_train/SST_rf_train)
#   R_sq_rf_train #0.8670912
# 
#   #Random Forest Model Validation Metrics
#   SST_rf_val <- sum((val_df$n - mean(val_df$n))^2)
#   SSE_rf_val <- sum((val_df$n - predict(rf_model, newdata = val_df))^2)
#   R_sq_rf_val <- 1-(SSE_rf_val/SST_rf_val)
#   R_sq_rf_val #-0.5731295
# 
#   rf_results <- rf_results %>% bind_rows(data.frame(i=i, R_sq_rf_train=R_sq_rf_train, R_sq_rf_val=R_sq_rf_val))
# }
# 
# plot_ly(x=rf_results$i, y=rf_results$R_sq_rf_train, type='scatter', mode='lines+markers', name='train') %>%
#   add_trace(x=rf_results$i, y=rf_results$R_sq_rf_val, type='scatter', mode='lines+markers', name='val')
# 
# rf_results %>% filter(rf_results$R_sq_rf_val == max(rf_results$R_sq_rf_val))
# # i R_sq_rf_train R_sq_rf_val
# # 1 41     0.8668467    -0.49019

# Best Model from Train & Val
set.seed(12345)
rf_model <- randomForest(n ~ TIMESTEP + MONTH + PERSON_AGE + PERSON_SEX, data=train_df, importance=TRUE, ntree=41)  #41
summary(rf_model)
var_importance_df <- data.frame(importance(rf_model)) %>% rename('PCT_IncMSE'='X.IncMSE') %>% arrange(desc(PCT_IncMSE))
var_importance_df
SST_rf_train <- sum((train_df$n - mean(train_df$n))^2)
SSE_rf_train <- sum((train_df$n - predict(rf_model, newdata = train_df))^2)
R_sq_rf_train <- 1-(SSE_rf_train/SST_rf_train)
R_sq_rf_train #0.8668467

#Random Forest Model Validation Metrics
SST_rf_val <- sum((val_df$n - mean(val_df$n))^2)
SSE_rf_val <- sum((val_df$n - predict(rf_model, newdata = val_df))^2)
R_sq_rf_val <- 1-(SSE_rf_val/SST_rf_val)
R_sq_rf_val #-0.49019

# plot_ly(x=seq(1,length(rf_model$mse),1), y=rf_model$mse, mode='lines+markers', type='scatter') %>% 
#   layout(title = 'Plot of Random Forest Training Error vs Number of Trees',
#          xaxis = list(title = 'Number of Trees'),
#          yaxis = list(title = 'Error', rangemode = "tozero"))

# plot_ly(x=train_df$yr_mo, y=(predict(rf_model, newdata = train_df)-train_df$n), type='scatter', mode='markers') %>% 
#   layout(title = 'Plot of Random Forest Model Residuals vs yr_mo',
#          xaxis = list(title = 'yr_mo'),
#          yaxis = list(title = 'RF Residuals', rangemode = "tozero"))
# 
# plot_ly(x=val_df$yr_mo, y=(predict(rf_model, newdata = val_df)-val_df$n), type='scatter', mode='markers') %>% 
#   layout(title = 'Plot of Random Forest Model Residuals vs yr_mo for Validation Data',
#          xaxis = list(title = 'yr_mo'),
#          yaxis = list(title = 'RF Residuals', rangemode = "tozero"))


# Train_Val RF model
set.seed(12345)
rf_model_2 <- randomForest(n ~ TIMESTEP + MONTH + PERSON_AGE + PERSON_SEX, data=train_val_df, importance=TRUE, ntree=41)  #41
summary(rf_model_2)
var_importance_df <- data.frame(importance(rf_model_2)) %>% rename('PCT_IncMSE'='X.IncMSE') %>% arrange(desc(PCT_IncMSE))
var_importance_df
SST_rf_train_val <- sum((train_val_df$n - mean(train_val_df$n))^2)
SSE_rf_train_val <- sum((train_val_df$n - predict(rf_model_2, newdata = train_val_df))^2)
R_sq_rf_train_val <- 1-(SSE_rf_train_val/SST_rf_train_val)
R_sq_rf_train_val #0.8480507

plot_ly(x=train_val_df$yr_mo, y=(predict(rf_model_2, newdata = train_val_df)-train_val_df$n), type='scatter', mode='markers') %>%
  layout(title = 'Plot of Random Forest Model Residuals vs yr_mo for Train_Val Data',
         xaxis = list(title = 'yr_mo'),
         yaxis = list(title = 'RF Residuals', rangemode = "tozero"))


#Train CART Model
set.seed(12345)
min_leaf <- 3
min_split <- 3*min_leaf
cart_model <- rpart(n ~ TIMESTEP + MONTH + PERSON_AGE + PERSON_SEX, data=train_df, 
                    control = c(minsplit = min_split, minbucket = min_leaf, cp=0.01)) #default = 0.01
SST_CART_train <- sum((train_df$n - mean(train_df$n))^2)
SSE_CART_train <- sum((train_df$n - predict(cart_model, newdata = train_df))^2)
R_sq_cart_train <- 1-(SSE_CART_train/SST_CART_train)
R_sq_cart_train #0.9473921

data.frame(Variable_Importance = cart_model$variable.importance, 
           Variable_Importance_Pct_Tot = round(100*cart_model$variable.importance/sum(cart_model$variable.importance),0))

rpart.plot(cart_model)

plot_ly(x=train_df$yr_mo, y=(predict(cart_model, newdata = train_df)-train_df$n), type='scatter', mode='markers') %>% 
  layout(title = 'Plot of CART Model Residuals vs yr_mo',
         xaxis = list(title = 'yr_mo'),
         yaxis = list(title = 'CART Residuals', rangemode = "tozero"))

#CART Model Validation Metrics
SST_cart_val <- sum((val_df$n - mean(val_df$n))^2)
SSE_cart_val <- sum((val_df$n - predict(cart_model, newdata = val_df))^2)
R_sq_cart_val <- 1-(SSE_cart_val/SST_cart_val)
R_sq_cart_val #-1.753216

plot_ly(x=val_df$yr_mo, y=(predict(cart_model, newdata = val_df)-val_df$n), type='scatter', mode='markers') %>% 
  layout(title = 'Plot of CART Model Residuals vs yr_mo for Validation Data',
         xaxis = list(title = 'yr_mo'),
         yaxis = list(title = 'CART Residuals', rangemode = "tozero"))


# Train_Val CART model
set.seed(12345)
min_leaf <- 3
min_split <- 3*min_leaf
cart_model_2 <- rpart(n ~ TIMESTEP + MONTH + PERSON_AGE + PERSON_SEX, data=train_val_df, 
                    control = c(minsplit = min_split, minbucket = min_leaf, cp=0.01)) #default = 0.01
SST_CART_train_val <- sum((train_val_df$n - mean(train_val_df$n))^2)
SSE_CART_train_val <- sum((train_val_df$n - predict(cart_model_2, newdata = train_val_df))^2)
R_sq_cart_train_val <- 1-(SSE_CART_train_val/SST_CART_train_val)
R_sq_cart_train_val #0.9023342

data.frame(Variable_Importance = cart_model_2$variable.importance, 
           Variable_Importance_Pct_Tot = round(100*cart_model_2$variable.importance/sum(cart_model_2$variable.importance),0))
# Variable_Importance Variable_Importance_Pct_Tot
# PERSON_AGE           104058660                          55
# PERSON_SEX            58374443                          31
# TIMESTEP              25103141                          13

rpart.plot(cart_model_2)

plot_ly(x=train_val_df$yr_mo, y=(predict(cart_model_2, newdata = train_val_df)-train_val_df$n), type='scatter', mode='markers') %>% 
  layout(title = 'Plot of CART Model Residuals vs yr_mo for Train_Val Data',
         xaxis = list(title = 'yr_mo'),
         yaxis = list(title = 'CART Residuals', rangemode = "tozero"))





# XGB dataset feature changes (factor levels not supported, so integer casting)
#Need to modify train and val data frames for numeric only fields (Month and person_sex)
train_xgb_df <- (train_df %>% 
                   select(TIMESTEP,MONTH,PERSON_AGE,PERSON_SEX) %>% 
                   mutate(MONTH=as.integer(as.factor(MONTH)),
                          PERSON_SEX=ifelse(PERSON_SEX=='M',0,1)))

val_xgb_df <- (val_df %>% 
                 select(TIMESTEP,MONTH,PERSON_AGE,PERSON_SEX) %>% 
                 mutate(MONTH=as.integer(as.factor(MONTH)),
                        PERSON_SEX=ifelse(PERSON_SEX=='M',0,1)))

train_val_xgb_df <- (train_val_df %>% 
                       select(TIMESTEP,MONTH,PERSON_AGE,PERSON_SEX) %>% 
                       mutate(MONTH=as.integer(as.factor(MONTH)),
                              PERSON_SEX=ifelse(PERSON_SEX=='M',0,1)))

test_xgb_df <- (test_df %>% 
                  select(TIMESTEP,MONTH,PERSON_AGE,PERSON_SEX) %>% 
                  mutate(MONTH=as.integer(as.factor(MONTH)),
                         PERSON_SEX=ifelse(PERSON_SEX=='M',0,1)))


# #Train & Val Boosted Tree Model - Loop - Run only once to get optimal settings  
#Train Boosted Tree Model
bt_results <- data.frame()
for(i in seq.int(1, 20)){
  for(j in seq.int(3,6)){
    set.seed(12345)
    bt_model <- xgboost(data=as.matrix(train_xgb_df), label = train_df$n, objective='reg:squarederror', nthread=1,  nrounds=i, max.depth=j, eta=0.05)  #
    summary(bt_model)
    
    SST_bt_train <- sum((train_df$n - mean(train_df$n))^2)
    SSE_bt_train <- sum((train_df$n - predict(bt_model, newdata = as.matrix(train_xgb_df)))^2)
    R_sq_bt_train <- 1-(SSE_bt_train/SST_bt_train)
    R_sq_bt_train #0.4761988
    
    #Boosted Tree Model Validation Metrics
    SST_bt_val <- sum((val_df$n - mean(val_df$n))^2)
    SSE_bt_val <- sum((val_df$n - predict(bt_model, newdata = as.matrix(val_xgb_df)))^2)
    R_sq_bt_val <- 1-(SSE_bt_val/SST_bt_val)
    R_sq_bt_val #0.7243043
    
    bt_results <- bt_results %>% bind_rows(data.frame(n_trees=i, max_depth=j, R_sq_bt_train=R_sq_bt_train, R_sq_bt_val=R_sq_bt_val))

  }
}

plot_ly(x=bt_results$n_trees, y=bt_results$R_sq_bt_train, type='scatter', mode='lines+markers', color=as.factor(paste0('Train-',bt_results$max_depth))) %>%
  add_trace(x=bt_results$n_trees, y=bt_results$R_sq_bt_val, type='scatter', mode='lines+markers', color=as.factor(paste0('Val-',bt_results$max_depth))) %>% 
  layout(title = 'Plot of Boosted Tree Train and Val R_Sq vs Number of Trees (depth in name)',
         xaxis = list(title = 'Number of Trees'),
         yaxis = list(title = 'R_Sq'))


bt_results %>% filter(bt_results$R_sq_bt_val == max(bt_results$R_sq_bt_val))
# n_trees max_depth R_sq_bt_train R_sq_bt_val
# 12         5     0.3742168   0.7650132


# Best model from Train/Val tuning
set.seed(12345)
bt_model <- xgboost(data=as.matrix(train_xgb_df), label = train_df$n, objective='reg:squarederror', nthread=1,  nrounds=12, max.depth=5, eta=0.05)  #
xgb.importance(model=bt_model)
xgb.plot.importance(xgb.importance(model=bt_model))
# barplot(t(xgb.importance(model=bt_model)[,2]) %>% as.vector())

SST_bt_train <- sum((train_df$n - mean(train_df$n))^2)
SSE_bt_train <- sum((train_df$n - predict(bt_model, newdata = as.matrix(train_xgb_df)))^2)
R_sq_bt_train <- 1-(SSE_bt_train/SST_bt_train)
R_sq_bt_train #0.3742168

#Boosted Tree Model Validation Metrics
SST_bt_val <- sum((val_df$n - mean(val_df$n))^2)
SSE_bt_val <- sum((val_df$n - predict(bt_model, newdata = as.matrix(val_xgb_df)))^2)
R_sq_bt_val <- 1-(SSE_bt_val/SST_bt_val)
R_sq_bt_val #0.7650132


# plot_ly(x=bt_model$evaluation_log$iter, y=bt_model$evaluation_log$train_rmse, mode='lines+markers', type='scatter') %>%
#   layout(title = 'Plot of Boosted Tree Training Error vs Number of Trees',
#          xaxis = list(title = 'Number of Trees'),
#          yaxis = list(title = 'Error', rangemode = "tozero"))

# plot_ly(x=train_df$yr_mo, y=(predict(bt_model, newdata = as.matrix(train_xgb_df))-train_df$n), type='scatter', mode='markers') %>%
#   layout(title = 'Plot of Boosted Tree Model Residuals vs yr_mo',
#          xaxis = list(title = 'yr_mo'),
#          yaxis = list(title = 'BT Residuals', rangemode = "tozero"))
# 
# 
# 
# plot_ly(x=val_df$yr_mo, y=(predict(bt_model, newdata = as.matrix(val_xgb_df))-val_df$n), type='scatter', mode='markers') %>%
#   layout(title = 'Plot of Boosted Tree Model Residuals vs yr_mo for Validation Data',
#          xaxis = list(title = 'yr_mo'),
#          yaxis = list(title = 'BT Residuals', rangemode = "tozero"))


# Train_Val Boosted Tree model
set.seed(12345)
bt_model_2 <- xgboost(data=as.matrix(train_val_xgb_df), label = train_val_df$n, objective='reg:squarederror', nthread=1,  nrounds=12, max.depth=5, eta=0.05)  #
xgb.importance(model=bt_model_2)
# Feature      Gain     Cover Frequency
# 1: PERSON_AGE 0.5551876 0.6417496 0.5698925
# 2: PERSON_SEX 0.3061301 0.1695900 0.1236559
# 3: TIMESTEP 0.1386823 0.1886604 0.3064516

xgb.plot.importance(xgb.importance(model=bt_model_2))
# barplot(t(xgb.importance(model=bt_model_2)[,2]) %>% as.vector())

SST_bt_train_val <- sum((train_val_df$n - mean(train_val_df$n))^2)
SSE_bt_train_val <- sum((train_val_df$n - predict(bt_model_2, newdata = as.matrix(train_val_xgb_df)))^2)
R_sq_bt_train_val <- 1-(SSE_bt_train_val/SST_bt_train_val)
R_sq_bt_train_val #0.4057451


plot_ly(x=train_val_df$yr_mo, y=(predict(bt_model_2, newdata = as.matrix(train_val_xgb_df))-train_val_df$n), type='scatter', mode='markers') %>%
  layout(title = 'Plot of Boosted Tree Model Residuals vs yr_mo for Train_Val Data',
         xaxis = list(title = 'yr_mo'),
         yaxis = list(title = 'BT Residuals', rangemode = "tozero"))

#### Overall Model Summary Plots
#Training Data - All Models
plot_ly(x=train_df$n, y=(predict(cart_model, newdata = train_df)), type='scatter', mode='markers', name='CART', alpha=0.3) %>% 
  add_trace(x=train_df$n, y=(predict(rf_model, newdata = train_df)), type='scatter', mode='markers', name='RF', alpha=0.3) %>% 
  add_trace(x=train_df$n, y=(predict(lm_model, newdata = train_df)), type='scatter', mode='markers', name='LM', alpha=0.3) %>%
  add_trace(x=c(0,500), y=c(0,500), type='scatter', mode='line', name='Perfect', alpha=1) %>% 
  add_trace(x=train_df$n, y=(predict(bt_model, newdata = as.matrix(train_xgb_df))), type='scatter', mode='markers', name='BT', alpha=0.3) %>%
  layout(title = 'Plot of All Model Predictions vs Actual Values - Training Data',
         xaxis = list(title = 'Actual Value', range=c(0,700)),
         yaxis = list(title = 'Model Prediction', rangemode = "tozero"))


#Validation Data - All Models
plot_ly(x=val_df$n, y=(predict(cart_model, newdata = val_df)), type='scatter', mode='markers', name='CART', alpha=0.5) %>% 
  add_trace(x=val_df$n, y=(predict(rf_model, newdata = val_df)), type='scatter', mode='markers', name='RF', alpha=0.5) %>% 
  add_trace(x=val_df$n, y=(predict(lm_model, newdata = val_df)), type='scatter', mode='markers', name='LM', alpha=0.5) %>% 
  add_trace(x=c(0,300), y=c(0,300), type='scatter', mode='line', name='Perfect', alpha=1) %>% 
  add_trace(x=val_df$n, y=(predict(bt_model, newdata = as.matrix(val_xgb_df))), type='scatter', mode='markers', name='BT', alpha=0.5) %>%
  layout(title = 'Plot of All Model Predictions vs Actual Values - Validation Data',
         xaxis = list(title = 'Actual Value', range=c(0,325)),
         yaxis = list(title = 'Model Prediction', rangemode = "tozero"))


#Train_Val Data - All Models
plot_ly(x=train_val_df$n, y=(predict(cart_model_2, newdata = train_val_df)), type='scatter', mode='markers', name='CART', alpha=0.3) %>% 
  add_trace(x=train_val_df$n, y=(predict(rf_model_2, newdata = train_val_df)), type='scatter', mode='markers', name='RF', alpha=0.3) %>% 
  add_trace(x=train_val_df$n, y=(predict(lm_model_2, newdata = train_val_df)), type='scatter', mode='markers', name='LM', alpha=0.3) %>%
  add_trace(x=c(0,500), y=c(0,500), type='scatter', mode='line', name='Perfect', alpha=1) %>% 
  add_trace(x=train_val_df$n, y=(predict(bt_model_2, newdata = as.matrix(train_val_xgb_df))), type='scatter', mode='markers', name='BT', alpha=0.3) %>%
  layout(title = 'Plot of All Model Predictions vs Actual Values - Train_Val Data',
         xaxis = list(title = 'Actual Value', range=c(0,700)),
         yaxis = list(title = 'Model Prediction', rangemode = "tozero"))



#### Individual Model Performance Summary Table 
kable(data.frame(Model_Type = c('Linear', 'Random Forest', 'CART', 'Boosted Trees'),
           R_sq_train = round(c(R_sq_lm_train, R_sq_rf_train, R_sq_cart_train, R_sq_bt_train), 2),
           R_sq_val = round(c(R_sq_lm_val, R_sq_rf_val, R_sq_cart_val, R_sq_bt_val), 2),
           R_sq_train_val = round(c(R_sq_lm_train_val, R_sq_rf_train_val, R_sq_cart_train_val, R_sq_bt_train_val), 2))) %>% 
  kable_classic(full_width = FALSE, html_font = "Cambria")%>%
  row_spec(c(2,4), background = c("yellow")) # => moving forward with the Random Forest Model



#### Need try to ensemble final models for better performance
val_pred_actual <- val_df$n
val_pred_lm <- predict(lm_model_2, newdata = val_df)
val_pred_rf <- predict(rf_model_2, newdata = val_df)
val_pred_cart <- predict(cart_model_2, newdata = val_df)
val_pred_bt <- predict(bt_model_2, newdata = as.matrix(val_xgb_df))


ensemble_model <- lm(val_pred_actual ~ val_pred_lm + val_pred_rf + val_pred_cart + val_pred_bt)
summary(ensemble_model)
  # Call:
  #   lm(formula = val_pred_actual ~ val_pred_lm + val_pred_rf + val_pred_cart + 
  #        val_pred_bt)
  # 
  # Residuals:
  #   Min      1Q  Median      3Q     Max 
  # -100.06  -12.18    0.76   10.73  107.40 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)   -15.493854   1.392885 -11.124  < 2e-16 ***
  # val_pred_lm    -0.007568   0.009496  -0.797    0.426    
  # val_pred_rf     0.378762   0.037526  10.093  < 2e-16 ***
  # val_pred_cart  -0.088279   0.015137  -5.832 6.47e-09 ***
  # val_pred_bt     1.746018   0.065075  26.831  < 2e-16 ***
  #   ---
  #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 26.44 on 1823 degrees of freedom
  # Multiple R-squared:  0.9141,	Adjusted R-squared:  0.914 
  # F-statistic:  4853 on 4 and 1823 DF,  p-value: < 2.2e-16
  
  #  =>  ensemble model will be -15.493854 + 1.746018*bt_model_2 + 0.378762*rf_model_2 - 0.088279*cart_model_2

# Ensemble model result calculations
val_pred_ensemble <- -15.493854 + 1.746018*val_pred_bt + 0.378762*val_pred_rf - 0.088279*val_pred_cart

SST_ensemble_val <- sum((val_df$n - mean(val_df$n))^2)
SSE_ensemble_val <- sum((val_df$n - val_pred_ensemble)^2)
R_sq_ensemble_val <- 1-(SSE_ensemble_val/SST_ensemble_val)
R_sq_ensemble_val #0.9139694

train_val_pred_ensemble <- -15.493854 + 1.746018*predict(bt_model_2, newdata = as.matrix(train_val_xgb_df)) + 0.378762*predict(rf_model_2, newdata = train_val_df) - 0.088279*predict(cart_model_2, newdata = train_val_df)
SST_ensemble_train_val <- sum((train_val_df$n - mean(train_val_df$n))^2)
SSE_ensemble_train_val <- sum((train_val_df$n - train_val_pred_ensemble)^2)
R_sq_ensemble_train_val <- 1-(SSE_ensemble_train_val/SST_ensemble_train_val)
R_sq_ensemble_train_val #0.9624101


# Verify all performances on Test dataset
SST_lm_test <- sum((test_df$n - mean(test_df$n))^2)
SSE_lm_test <- sum((test_df$n - predict(lm_model_2, newdata = test_df))^2)
R_sq_lm_test <- 1-(SSE_lm_test/SST_lm_test)
R_sq_lm_test #0.08348344

SST_rf_test <- sum((test_df$n - mean(test_df$n))^2)
SSE_rf_test <- sum((test_df$n - predict(rf_model_2, newdata = test_df))^2)
R_sq_rf_test <- 1-(SSE_rf_test/SST_rf_test)
R_sq_rf_test #0.7298947

SST_cart_test <- sum((test_df$n - mean(test_df$n))^2)
SSE_cart_test <- sum((test_df$n - predict(cart_model_2, newdata = test_df))^2)
R_sq_cart_test <- 1-(SSE_cart_test/SST_cart_test)
R_sq_cart_test #0.6526338

SST_bt_test <- sum((test_df$n - mean(test_df$n))^2)
SSE_bt_test <- sum((test_df$n - predict(bt_model_2, newdata = as.matrix(test_xgb_df)))^2)
R_sq_bt_test <- 1-(SSE_bt_test/SST_bt_test)
R_sq_bt_test #0.2349657

# test_pred_ensemble <- 1.43149 + 1.06208*predict(bt_model, newdata = as.matrix(test_xgb_df)) - 0.17798*predict(rf_model, newdata = test_df) + 0.06312*predict(lm_model, newdata = test_df)
test_pred_ensemble <- -15.493854 + 1.746018*predict(bt_model_2, newdata = as.matrix(test_xgb_df))  + 0.378762*predict(rf_model_2, newdata = test_df) - 0.088279*predict(cart_model_2, newdata = test_df)
SST_ensemble_test <- sum((test_df$n - mean(test_df$n))^2)
SSE_ensemble_test <- sum((test_df$n - test_pred_ensemble)^2)
R_sq_ensemble_test <- 1-(SSE_ensemble_test/SST_ensemble_test)
R_sq_ensemble_test #0.8646812


#### Overall Training/Validation/Test Model Performance Summary Table 
kable(data.frame(Model_Type = c('Linear', 'Random Forest', 'CART', 'Boosted Trees', 'Ensemble'),
                 R_sq_train = round(c(R_sq_lm_train, R_sq_rf_train, R_sq_cart_train, R_sq_bt_train, as.integer(NA)), 2),
                 R_sq_val = round(c(R_sq_lm_val, R_sq_rf_val, R_sq_cart_val, R_sq_bt_val, R_sq_ensemble_val), 2),
                 R_sq_train_val = round(c(R_sq_lm_train_val, R_sq_rf_train_val, R_sq_cart_train_val, R_sq_bt_train_val, R_sq_ensemble_train_val), 2),
                 R_sq_test = round(c(R_sq_lm_test, R_sq_rf_test, R_sq_cart_test, R_sq_bt_test, R_sq_ensemble_test), 2))) %>% 
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  row_spec(c(5), background = c("yellow")) # => moving forward with the Ensemble Model




#Test Data - All Models
plot_ly(x=test_df$n, y=(predict(cart_model_2, newdata = test_df)), type='scatter', mode='markers', name='CART', marker=list(color='#1f77b4', opacity=0.5)) %>% 
  add_trace(x=test_df$n, y=(predict(rf_model_2, newdata = test_df)), type='scatter', mode='markers', name='RF', marker=list(color='#ff7f0e', opacity=0.5)) %>% 
  add_trace(x=test_df$n, y=(predict(lm_model_2, newdata = test_df)), type='scatter', mode='markers', name='LM', marker=list(color='#2ca02c', opacity=0.5)) %>%
  add_trace(x=c(0,350), y=c(0,350), type='scatter', mode='line', name='Perfect', alpha=1, line=list(color='#d62728'), marker=list(color='#2ca02c', opacity=0)) %>% 
  add_trace(x=test_df$n, y=(predict(bt_model_2, newdata = as.matrix(test_xgb_df))), type='scatter', mode='markers', name='BT', marker=list(color='#9467bd', opacity=0.5)) %>%
  add_trace(x=test_df$n, y=(test_pred_ensemble), type='scatter', mode='markers', name='Ensemble', marker=list(color='#8c564b', opacity=0.5)) %>%
  layout(title = 'Plot of All Model Predictions vs Actual Values - Test Data',
         xaxis = list(title = 'Actual Value', range=c(0,375)),
         yaxis = list(title = 'Model Prediction', range=c(-200,350)))









#### NYC Collision Shop has 0.14% of Total market share (from references). Estimate future monthly car volume.
Shop_Market_Share <- 0.14/100

#Compute Actual from Test Data
test_agg_df <- test_df %>% group_by(MONTH) %>% summarise(Actual_NYC_Collisions=sum(n), Actual_Shop_Volume=round(Shop_Market_Share*sum(n), 0))

#Create dataset for prediction year (2021)
temp1 <- data.frame(TIMESTEP = c(48:59))
temp2 <- data.frame(MONTH = c('01','02','03','04','05','06','07','08','09','10','11','12'))
temp3 <- data.frame(PERSON_AGE = c(15:100))
temp4 <- data.frame(PERSON_SEX = c('M','F'))
monthly_predictions_df <- temp1 %>% bind_cols(temp2) %>%  full_join(temp3, by=character()) %>% full_join(temp4, by=character())

monthly_predictions_xgb_df <- (monthly_predictions_df %>% 
                                 select(TIMESTEP,MONTH,PERSON_AGE,PERSON_SEX) %>% 
                                 mutate(MONTH=as.integer(as.factor(MONTH)),
                                        PERSON_SEX=ifelse(PERSON_SEX=='M',0,1)))


#Create predictions
monthly_predictions_df$rf <- predict(rf_model_2, newdata = monthly_predictions_df)
monthly_predictions_df$cart <- predict(cart_model_2, newdata = monthly_predictions_df)
monthly_predictions_df$bt <- predict(bt_model_2, newdata = as.matrix(monthly_predictions_xgb_df))
monthly_predictions_df$ensemble <- -15.493854 + 1.746018*monthly_predictions_df$bt  + 0.378762*monthly_predictions_df$rf - 0.088279*monthly_predictions_df$cart

monthly_predictions_agg_df <- (monthly_predictions_df %>% 
                                 group_by(MONTH) %>% 
                                 summarise(Predicted_NYC_Collisions=round(sum(ensemble), 0)) %>%
                                 mutate(Predicted_Shop_Volume = round(Shop_Market_Share*Predicted_NYC_Collisions, 0)) %>% 
                                 left_join(test_agg_df, by='MONTH') %>% 
                                 mutate(YEAR = 2021) %>% 
                                 select(YEAR, MONTH, Actual_NYC_Collisions, Actual_Shop_Volume, Predicted_NYC_Collisions, Predicted_Shop_Volume)
)   

kable(monthly_predictions_agg_df) %>% 
  kable_classic(full_width = FALSE, html_font = "Cambria")


#Plot of Actual_Shop_Volume and Predicted_Shop_Volume
plot_ly(x=monthly_predictions_agg_df$MONTH, y=monthly_predictions_agg_df$Actual_Shop_Volume, type='bar', name='Actual_Shop_Volume') %>% 
  add_trace(x=monthly_predictions_agg_df$MONTH, y=monthly_predictions_agg_df$Predicted_Shop_Volume, type='bar', name='Predicted_Shop_Volume') %>% 
  layout(title = 'Plot of Actual_Shop_Volume and Predicted_Shop_Volume (Test Set)',
         xaxis = list(title = 'Months in 2021'),
         yaxis = list(title = 'Car volume'))



#Save model objects
saveRDS(lm_model_2, './lm_model_2.rds')
saveRDS(rf_model_2, './rf_model_2.rds')
saveRDS(cart_model_2, './cart_model_2.rds')
saveRDS(bt_model_2, './bt_model_2.rds')
saveRDS(monthly_predictions_df, './monthly_predictions_df.rds')
saveRDS(monthly_predictions_agg_df, './monthly_predictions_agg_df.rds')

#Save model_summary_table_df
model_summary_table_df <- (data.frame(Model_Type = c('Linear', 'Random Forest', 'CART', 'Boosted Trees', 'Ensemble'),
                 R_sq_train = round(c(R_sq_lm_train, R_sq_rf_train, R_sq_cart_train, R_sq_bt_train, as.integer(NA)), 2),
                 R_sq_val = round(c(R_sq_lm_val, R_sq_rf_val, R_sq_cart_val, R_sq_bt_val, R_sq_ensemble_val), 2),
                 R_sq_train_val = round(c(R_sq_lm_train_val, R_sq_rf_train_val, R_sq_cart_train_val, R_sq_bt_train_val, R_sq_ensemble_train_val), 2),
                 R_sq_test = round(c(R_sq_lm_test, R_sq_rf_test, R_sq_cart_test, R_sq_bt_test, R_sq_ensemble_test), 2)))

saveRDS(model_summary_table_df, './model_summary_table_df.rds')



#Save all_model_pred_vs_actual_df
all_model_pred_vs_actual_df <- (data.frame(n=test_df$n,
                                           cart_model_2 = predict(cart_model_2, newdata = test_df),
                                           rf_model_2 = predict(rf_model_2, newdata = test_df),
                                           lm_model_2 = predict(lm_model_2, newdata = test_df),
                                           bt_model_2 = predict(bt_model_2, newdata = as.matrix(test_xgb_df)),
                                           ensemble = -15.493854 + 1.746018*predict(bt_model_2, newdata = as.matrix(test_xgb_df)) 
                                                      + 0.378762*predict(rf_model_2, newdata = test_df) 
                                                      - 0.088279*predict(cart_model_2, newdata = test_df)))

saveRDS(all_model_pred_vs_actual_df, './all_model_pred_vs_actual_df.rds')





# Save monthly_agg_data_splits to explain Train/Val/Test data sets
monthly_agg_data_splits_df <- (combined_df %>% 
                              group_by(TIMESTEP, yr_mo, MONTH) %>% 
                              summarise(n = n(), .groups = 'drop') %>% 
                              arrange(TIMESTEP, yr_mo, MONTH) %>% 
                              mutate(Partition = ifelse(TIMESTEP <= 35,'Train',ifelse(TIMESTEP <= 47,'Val', 'Test')))
                            )

saveRDS(monthly_agg_data_splits_df, './monthly_agg_data_splits_df.rds')


#Save XGB Feature Importance
saveRDS(xgb.importance(model = bt_model_2), './XGB_Feature_Importance.rds')

#Save Random Forest Feature Importance
rf_var_importance_df <- data.frame(importance(rf_model_2)) %>% rename('PCT_IncMSE'='X.IncMSE') %>% arrange(desc(PCT_IncMSE)) %>% as.data.frame()
saveRDS(rf_var_importance_df, './rf_var_importance_df.rds')


# # Load model objects
# lm_model_2 <- readRDS('./lm_model_2.rds')
# rf_model_2 <- readRDS('./rf_model_2.rds')
# cart_model_2 <- readRDS('./cart_model_2.rds')
# bt_model_2 <- readRDS('./bt_model_2.rds')

# #Load summary data objects
# monthly_predictions_df <- readRDS('./monthly_predictions_df.rds')
# monthly_predictions_agg_df <- readRDS('./monthly_predictions_agg_df.rds')
# model_summary_table_df <- readRDS('./model_summary_table_df.rds')
# all_model_pred_vs_actual_df <- readRDS('./all_model_pred_vs_actual_df.rds')


# # Make plot from all_model_pred_vs_actual_df
# plot_ly(x=all_model_pred_vs_actual_df$n, y=all_model_pred_vs_actual_df$cart_model_2, type='scatter', mode='markers', name='CART', marker=list(color='#1f77b4', opacity=0.5)) %>%
#   add_trace(x=all_model_pred_vs_actual_df$n, y=all_model_pred_vs_actual_df$rf_model_2, type='scatter', mode='markers', name='RF', marker=list(color='#ff7f0e', opacity=0.5)) %>%
#   add_trace(x=all_model_pred_vs_actual_df$n, y=all_model_pred_vs_actual_df$lm_model_2, type='scatter', mode='markers', name='LM', marker=list(color='#2ca02c', opacity=0.5)) %>%
#   add_trace(x=c(0,350), y=c(0,350), type='scatter', mode='line', name='Perfect', alpha=1, line=list(color='#d62728'), marker=list(color='#2ca02c', opacity=0)) %>%
#   add_trace(x=all_model_pred_vs_actual_df$n, y=all_model_pred_vs_actual_df$bt_model_2, type='scatter', mode='markers', name='BT', marker=list(color='#9467bd', opacity=0.5)) %>%
#   add_trace(x=all_model_pred_vs_actual_df$n, y=all_model_pred_vs_actual_df$ensemble, type='scatter', mode='markers', name='Ensemble', marker=list(color='#8c564b', opacity=0.5)) %>%
#   layout(title = 'Plot of All Model Predictions vs Actual Values - Test Data',
#          xaxis = list(title = 'Actual Value', range=c(0,375)),
#          yaxis = list(title = 'Model Prediction', range=c(-200,350)))


# # Make plot for data partitioning
# plot_ly(x=monthly_agg_data_splits_df$yr_mo, y=monthly_agg_data_splits_df$n, type='bar', color=monthly_agg_data_splits_df$Partition) %>% # - Notice huge COVID Impact on this dataset!
#   layout(title = "Crash Data by Month from 2017-2021", xaxis = list(title = 'Month'), yaxis = list(title = 'Number of Crashes'))


# # Make plot for cart_model_2
# rpart.plot(cart_model_2)


# # Make plot for XGBoost Feature Importance
# plot_ly(x=xgb.importance(model = bt_model_2)$Gain, y=reorder(xgb.importance(model = bt_model_2)$Feature, xgb.importance(model = bt_model_2)$Gain), type='bar', orientation = 'h') %>%
#   layout(title = "XGBoost Feature Importance", xaxis = list(title = 'Feature'), yaxis = list(title = 'Scaled Gain'))


# 
# # Make plot for Random Forest Feature Importance
# plot_ly(x=rf_var_importance_df$PCT_IncMSE , y=reorder(rf_var_importance_df %>% row.names(), rf_var_importance_df$PCT_IncMSE ), type='bar', orientation = 'h') %>%
#   layout(title = "Random Forest Feature Importance", xaxis = list(title = '% Increase MSE'), yaxis = list(title = 'Feature'))



