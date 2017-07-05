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

# loading the required packages
#install.packages("data.table","dplyr", "tidyr")
library(data.table)
library(dplyr)
library(tidyr)

#getting and setting working directory
getwd()
setwd("D:/Edwisor Notes/Projects/Instacart MBA")

#We are having 6 tables let's have a look one by one
# reading csv files
aisles <- fread("aisles.csv")
str(aisles) # it contains 2 variables id, aisle
head(aisles)

aisles$aisle <- as.factor(aisles$aisle) # reshaping

departments <- fread("departments.csv")
head(departments) # contains 2 variables id and department
departments$department <- as.factor(departments$department) # reshaping

order_products_prior <- fread("order_products__prior.csv")
head(order_products_prior)

order_products_train <- fread("order_products__train.csv")
head(order_products_train)

orders <- fread("orders.csv")
head(orders)
orders$eval_set <- as.factor(orders$eval_set) # reshaping

products <- fread("products.csv")
head(products)
products$product_name <- as.factor(products$product_name) # reshaping

# plot for count of rows in each data set
counts <- table(orders$eval_set)
counts
barplot(counts, main="Row count of each data set",xlab = "eval_set", ylab = "frequency",
        legend.text =  rownames(counts) )
rm(counts)

#Using the merge() function in R on big tables can be time consuming,join functions in the new package dplyr are much faster. 
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)

order_products_train$user_id <- if(orders$eval_set == "train") orders$user_id 

