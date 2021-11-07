# Project-2-analytics
Supply chain analytics-Harvard-scholarship


library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(lubridate) ## used to deal with timestamp
library(knitr)
library(rmarkdown)
library(lubridate)
library(stringr)
library(recosystem)
library(tinytex)
library(data.table)
library(readr)
library(tibble)
library(ModelMetrics)
library(rpart)
library(rpart.plot)
library(caret)
library(maps)

#Import "Supply chain" dataset 
Historical_Product_Demand <- read_csv("C:/Users/Maqboul Computer/OneDrive/Desktop/Historical Product Demand.csv")

sum(is.na(Historical_Product_Demand))

--------------------------------------
--------------------------------------
## Data Preparartion:
#Remove NA's from datasets
new_historical_data <- na.omit(Historical_Product_Demand)

format_year <- as.Date(new_historical_data$Date, format = "%Y/%m/%d")
new_historical_data <- new_historical_data %>%
  mutate(years = format(format_year, "%Y"))  # extract year from Date 

new_historical_data <- unique(new_historical_data)

colnames(new_historical_data) # names of variables
dim(new_historical_data)  #how many variables and how many subjects?
----------------------------------
----------------------------------
# Tidy dataset :
# Structure of unique variables & its length 
unique_products <- unique(new_historical_data$Product_Category)
length(unique_products)
unique_products_codes <- unique(new_historical_data$Product_Code)
length(unique_products_codes)
unique_warehouses <- unique(new_historical_data$Warehouse)
length(unique_warehouses)
unique_years <- unique(new_historical_data$years)
length(unique_years)
unique_orders <- unique(new_historical_data$Order_Demand)
length(unique_orders)

X <- as_tibble(data.frame(number_products = length(unique_products),
           number_products_codes = length(unique_products_codes),
           number_warehouses = length(unique_warehouses),
           number_years = length(unique_years),
           number_orders = length(unique_orders)))
glimpse(X)
--------------------------------------------------------
# Descriptive Statistics, "Order Demand" variable
data_DB <- new_historical_data %>% 
  filter(years != "2011" & years != "2017")

data_DB$Order_Demand <- as.numeric(data_DB$Order_Demand) 
data_DB$Order_Demand <-replace_na(data_DB$Order_Demand, 0)
data_DB$Order_Demand

  
summary(data_DB$Order_Demand) # min & max Order Demands
----
----
# Distributions of Order Demand by Warehouses through 5yrs 
data_DB %>% 
  ggplot(aes(x= Warehouse, y= Order_Demand,
                                color=Warehouse, fill= Warehouse))+
  stat_boxplot(geom = "errorbar", width = 0.5)+
  geom_boxplot(aes(fill= Warehouse), alpha= 0.5,
               outlier.colour = "red", outlier.shape = 1)+
  facet_wrap( ~ Warehouse )+
  scale_x_discrete(name="Warehouses") +
  scale_y_continuous(name= quote(log(Order_Demand)), trans = "log10")+
  theme_grey() +
  ggtitle("Order Demand vs. Warehouse")
----------------------------------------------------
----------------------------------------------------
# Data Visualization
# HeatMap plan plot gathering Date, Warehouses,Demands through 5yrs.
  
data_DB %>% 
  ggplot(mapping=aes(x= years, y=Warehouse, na.rm= TRUE,
                     fill =log(Order_Demand))) + 
  geom_tile(colour = "white", size= 0.5) +
  coord_flip() +
  ggtitle("Orders.Demand per each warehouse 2012-2016")+
  ylab("Warehouses")+
  xlab("Years")
### Heatmap easily describes the intensity of colors which Gray color
### lead to missing data in specific two years at warehouse_C; otherwise
### in reality the warehouse_C can't use well in ordering demand.
------------------------------------------------
# Bar plot for product categories by order demand
data_DB %>% 
  ggplot() +
  geom_bar(aes(fct_infreq(Product_Category),Order_Demand),
         stat = "identity", fill="#f68060", size= 0.6) +
  coord_flip() +
  xlab("") +
  ggtitle("Order Demand of Product Categories") +
  theme_bw() 
## Bar plot shows which is a product has high/low order quantity 
## from warehouses, so we can take consider the forecasting quantity stock for each product categories separately.
--------------------------------------------------------
# Warehouses are used in distribution process
data_DB %>% 
  count(Warehouse) %>%
  arrange(desc(n)) 

# Amount of orders shipped by each warehouse 2012-2016  
data_DB %>% 
  group_by(Warehouse) %>%
  summarise(orders_per_WH= sum(Order_Demand)) %>%
  arrange(orders_per_WH) # ascending orders per WH's
-----------------------------------------------------
----------------------------------------------------
### Regression Tree Predictive Modeling
set.seed(1984)
index <-  createDataPartition(data_DB$Order_Demand,
                              times=1, p=0.75, list=FALSE)
trainData <- data_DB[index,]
testData <- data_DB[-index,]

# Growing tree, setting parameters manually.
first_model_tree <- rpart(Order_Demand ~ Warehouse +Product_Category,
                    data = trainData, method = "anova",
                    control = rpart.control(cp = 0.001, minsplit =2,
                                            maxdepth= 4))
                    
first_model_tree
rpart.plot(first_model_tree, type= 1, 
           yesno = TRUE, box.palette="RdBu", shadow.col="gray", nn=TRUE)

rtree.fit <- printcp(first_model_tree)  
plotcp(first_model_tree)  
#Note: Choosing CP with corresponding minimum relative error, and that the 
# tree has 6 terminal nodes with 5 internal nodes.The Error at 
# 6 terminal nodes is 0.942.


# plot tree where splitting data happens
plot(first_model_tree, uniform=TRUE, main="Unpruned Regression Tree", margin = 0.1)
text(first_model_tree, use.n=TRUE, all=TRUE, cex=0.75)

first_model_tree$variable.importance #evidence that warehouses play
# relatively more important role on order demand.

# Pruning, using prp() in the rpart.plot package
pruned1.rtree.fit <- prune(first_model_tree, cp = 0.001)
prp(pruned1.rtree.fit,
    faclen=0, #use full names for factor labels
    extra=1,
    main="Pruned Regression Tree")

pl <- predict(first_model_tree, newdata = testData) # predictions first model
RMSE_1 <- RMSE(testData$Order_Demand, pl)
RMSE_1
#average predicted order demand is 29261.03 from first prediction model.

----
--> # Performance of Regression tree Cross validation 10 for First model
  m2 <- rpart(
    formula = Order_Demand ~ Warehouse +Product_Category,
    data    = trainData,
    method  = "anova", 
    control = list(cp = 0, xval = 10)
  )

plotcp(m2)
abline(v = 6, lty = "dashed")
printcp(m2) 
### we see that the error is 0.9370 slightly low after 6 terminal nodes.
--> # Note: use the alternate model to see what would happen?!
  # Automatically setting parameters >>..
---  
  
--> # Automatically setting Second model across range of differently tuned models
# determine parameters automatically rather than first model above.
 hyper_grid <- expand.grid(
    minsplit = seq(5, 20, 1),
    maxdepth = seq(8, 15, 1)
  )

  models <- list()
  for(i in 1:nrow(hyper_grid)){
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  #train a model and store in the list
  models[[i]] <- rpart(Order_Demand ~ Warehouse +Product_Category,
        data = trainData, method = "anova",
        control = rpart.control(minsplit = minsplit ,
                                maxdepth= maxdepth))
   }

  # function to get optimal cp
   get_cp <- function(x){
   min    <- which.min(x$cptable[, "xerror"])
   cp <- x$cptable[min, "CP"] 
   }

  # function to get minimum error
   get_min_error <- function(x){
    min    <- which.min(x$cptable[, "xerror"])
    xerror <- x$cptable[min, "xerror"] 
   }

   hyper_grid %>%
     mutate(
       cp    = purrr::map_dbl(models, get_cp),
       error = purrr::map_dbl(models, get_min_error)
     ) %>%
     arrange(error) %>%
     top_n(-5, wt = error)
### these results explain that this automatic model of parameters is
### not satisfied because of increasing the Error from (0.942) to 0.946.
   
   second_model_tree <- rpart(
     formula = Order_Demand ~ Warehouse +Product_Category,
     data    = trainData,
     method  = "anova", 
     control = list(minsplit = 11, maxdepth = 11, cp = 0.01)
   )

rpart.plot(second_model_tree, type= 1, 
              yesno = TRUE, box.palette="RdBu", shadow.col="gray", nn=TRUE)
   
pruned2.rtree.fit<- prune( second_model_tree , cp=0.01)
prp(pruned2.rtree.fit, main="Pruned Regression Tree")
   
pred <- predict(second_model_tree, 
                newdata = testData) #Predictions second model
RMSE_2 <- RMSE(testData$Order_Demand, pred)
RMSE_2   
# ----->  RMSE is the average predicted order demands for the first model is 
#  29261.03 while the order demands at the second model is 29335.67, this 
# slight differences in demands leading to increase the error
# in the second predictive model;so it is logically rejected. 

as_tibble(data.frame(rmse_1 = RMSE_1
                     ,rmse_2 = RMSE_2))
          

# First model is recommended, Describes that all products are ordering for 
# warehouses C & S by average order 878, while the high orders 
# are going for warehouses A & J by average order 9,790 except 
# product_006 , _025, _019 & _033 are ordered more for warehouses C&S.









