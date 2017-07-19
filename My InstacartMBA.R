###########################################################################################################
#
# Kaggle Instacart competition
#
###########################################################################################################
# cleaning the environment
rm(list=ls())

# #We are going to deal with a large data set so let's check memory limit first
# #to know current storage capacity
# memory.limit() [1] 
# 
# #increasing storage capacity
# memory.limit(size=56000) [1]
# 
# memory.limit()[1]


# loading the required packages
#install.packages("dtplyr", "tidyr","data.table","dplyr", "caret")
#data.table + dplyr code now lives in dtplyr.

library(dtplyr)
library(data.table)
library(dplyr)
library(tidyr)
library(caret)
library(e1071)

#getting and setting working directory
getwd()
setwd("")

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

# checking for NA's and removing NA's from orders
orders$days_since_prior_order <- ifelse(is.na(orders$days_since_prior_order),0,orders$days_since_prior_order)

products <- fread("products.csv")
#head(products)

products$product_name <- as.factor(products$product_name) # reshaping

# lets's have a look on data in eval_set (prior, train, test) orders table
counts <- table(orders$eval_set)
counts
barplot(counts, main="Row count of each data set",xlab = "eval_set", ylab = "frequency",
        legend.text =  rownames(counts) ,col =c("Blue","yellow", "green") )
rm(counts)

# Merging products with aisle and department tables
#Using the merge() function in R on big tables can be time consuming,join functions in the new package dplyr are much faster. 
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments)%>% 
  select(-aisle_id, -department_id)

rm(aisles, departments)
gc()

# combining order_products_prior and order_products_train
 order_products_prior <- rbind(order_products_prior , order_products_train)
 
 rm(order_products_train)
 gc()

# For the prior orders get the associated product, aisle, departments, and users
order_products_prior <- order_products_prior %>% inner_join(products) %>%
  group_by(product_id)

rm(products)
gc()

 order_products_prior <- order_products_prior %>% inner_join(orders) %>%
   group_by(order_id) 
 
rm(orders)
gc()

#################### Feature Engineering #########################################
 
# user based feature engineering grouped by user_id and product_id
 
user_product_details <- order_products_prior[,.(first_order = min(order_number),
                                                  last_order = max(order_number),
                                                  no_of_prod_purchased = .N,
                                                  mean_add_to_cart = mean(add_to_cart_order),
                                                  puchase_time_diff = sum(days_since_prior_order),
                                                  mean_days_since_prior_order = mean(days_since_prior_order)
                                                  ), keyby = .(user_id, product_id) ]
  
# feature engineering grouped by user_id

user_details <- order_products_prior[ ,.(dist_products = uniqueN(product_name),
                                           dist_aisles = uniqueN(aisle),
                                           dist_dept = uniqueN(department),
                                           total_ordered_products = .N,
                                           user_max_order = max(order_number),
                                           reorder_ratio = sum(reordered == 1)/sum(order_number >1),
                                           mean_wk_day = mean(order_dow),
                                           mean_purchase_hour = mean(order_hour_of_day)
                                          
  ),by = user_id]
  
user_details$avg_basket_size <- user_details$total_ordered_products/user_details$user_max_order
  
user_product_details <- user_product_details %>% inner_join(user_details) %>%
  group_by(user_id)

rm(user_details)
gc()

# some more feature engineering grouped by product_id

product_summ <- order_products_prior[ ,.(product_ordered = .N,
                                           sum_reordered = sum(reordered),
                                           first_order = sum(order_number == 1),
                                           next_order = sum(order_number == 2)),
                                        by = product_id]

product_summ$reorder_prob <- product_summ$next_order/product_summ$first_order
product_summ$ratio_reorder_to_order <- product_summ$sum_reordered/product_summ$product_ordered
  
user_product_details <- user_product_details %>% inner_join(product_summ) %>%
    group_by(product_id)
  
rm(product_summ)
gc()
  
  
# joining order_products_prior with user_product_details
order_products_prior <- order_products_prior %>% inner_join(user_product_details) %>%
  group_by(product_id)

rm(user_product_details) 
gc()


# Creating train and test frames 

trainIndex <- createDataPartition(y= order_products_prior$reordered, p=0.8, list= FALSE)
  
train <- order_products_prior[trainIndex,]
test <- order_products_prior[-trainIndex,]

rm(order_products_prior,trainIndex)
gc()
  
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$eval_set <- NULL
train$reordered[is.na(train$reordered)] <- 0

test$eval_set <- NULL
test$user_id <- NULL

train$product_name <- NULL
test$product_name <- NULL
train$aisle <- NULL
test$aisle <- NULL
train$department <- NULL
test$department <- NULL

############################## Model building -------------------------------------------------------------------

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

# Applying model 

X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id,- reordered)))
prediction <- predict(model, X)

predicted <- ifelse(prediction > 0.5,1,0)

# calculating accuracy of model
confusionMatrix (predicted, test$reordered)

# We need to predict which products a consumer will buy again.
final_output <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

# output data in csv
write.csv(final_output, file="my_output.csv",row.names = F)

######################## End ###############################################################