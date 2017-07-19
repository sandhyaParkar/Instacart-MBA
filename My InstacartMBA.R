###########################################################################################################
#
# Kaggle Instacart competition
#
###########################################################################################################
# cleaning the environment
rm(list=ls())

#We are going to deal with a large data set so let's check memory limit first
#to know current storage capacity
memory.limit() [1] 

#increasing storage capacity
memory.limit(size=56000) [1]

memory.limit()[1]
library("bigmemory")
# loading the required packages
#install.packages("dtplyr", "tidyr)
#data.table + dplyr code now lives in dtplyr.
library(dtplyr)
install.packages("data.table")
library(data.table)
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
library(caret)

#getting and setting working directory
getwd()
setwd("D:/Edwisor Notes/Projects/Instacart MBA")

#We are having 6 tables let's have a look one by one
# reading csv files
aisles <- fread("aisles.csv")
#str(aisles) # it contains 2 variables id, aisle
#head(aisles)

aisles$aisle <- as.factor(aisles$aisle) # reshaping

departments <- fread("departments.csv")
#head(departments) # contains 2 variables id and department
departments$department <- as.factor(departments$department) # reshaping

order_products_prior <- fread("order_products__prior.csv")
#head(order_products_prior)

order_products_train <- fread("order_products__train.csv")

#head(order_products_train)


orders <- fread("orders.csv")
#head(orders)
orders$eval_set <- as.factor(orders$eval_set) # reshaping

products <- fread("products.csv")
#head(products)
products$product_name <- as.factor(products$product_name) # reshaping

# lets's have a look on data in eval_set (prior, train, test) orders table
counts <- table(orders$eval_set)
counts
barplot(counts, main="Row count of each data set",xlab = "eval_set", ylab = "frequency",
        legend.text =  rownames(counts) ,col =c("Blue","yellow", "green") )
rm(counts)

#Using the merge() function in R on big tables can be time consuming,join functions in the new package dplyr are much faster. 
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)
gc()


# binding order_products_prior and order_products_train
order_products_prior <- rbind(order_products_prior, order_products_train[!rownames(order_products_train) %in% rownames(order_products_prior),]) 
rm(order_products_train)
gc()

# For the prior orders get the associated product, aisle, departments, and users
order_products_prior <- order_products_prior %>% inner_join(products) %>%
  group_by(product_id)

rm(products)
gc()

 order_products_prior <- order_products_prior %>% inner_join(orders) %>%
   group_by(order_id,product_id)
#str(order_products_prior)

 rm(orders)
 gc()
# let's find out how many products a user purchased and his/her last order count
users_products_list <- order_products_prior %>% mutate(last_order = max(order_number) , purchase_count = n()) %>%
  group_by(user_id)
rm(order_products_prior)
gc()


users_products_list <- users_products_list %>% mutate(mean_cart_order= mean(add_to_cart_order),product_reordered = sum(reordered == 1)) %>%
  group_by(product_id)


# now do some feature engineering for eval_set = prior in users_product_list

prior_orders <- users_products_list %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_max_orders = max(order_number),
    purchase_duration = sum(days_since_prior_order, na.rm = T),
    mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )


users <- users_products_list %>% 
  inner_join(prior_orders)
rm(users_products_list,prior_orders)
gc()


users$reordered[is.na(users$reordered)] <- 0
# create training frame---------------------------------------------------

trainIndex = users[sample(nrow(users), 1200000, replace = F), ]
train = trainIndex[sample(nrow(trainIndex), 1000000, replace = F), ]
test <- trainIndex[sample(nrow(trainIndex), 200000, replace = F),]
rm(trainIndex, users)
gc()

train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$eval_set <- NULL
test$eval_set <- NULL
train$product_name <- NULL
test$product_name <- NULL
train$aisle <- NULL
test$aisle <- NULL
train$department <- NULL
test$department <- NULL
# Model building -------------------------------------------------------------------
install.packages("xgboost")
library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "auc",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

X <- xgb.DMatrix(as.matrix(train %>% select(-reordered)), label = train$reordered)
model <- xgboost(data = X, params = params, nrounds = 80)

importance <- xgb.importance(colnames(X), model = model)
library( Ckmeans.1d.dp)
xgb.ggplot.importance(importance)

rm(X, importance)
gc()

# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id,- reordered)))
prediction <- predict(model, X)

predicted <- ifelse(prediction > 0.5,1,0)

test$order_id <- NULL
install.packages("e1071")
library(e1071)
confusionMatrix (predicted, test$reordered)

# #Builing Confusion matrix
# tab <- table(actual = test$reordered, Predicted = predict.dl2$predict)
# confusionMatrix(tab)
# 
 roc.curve(test$reordered, prediction, plotit = F)

final_output <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )


