# Project-2-analytics
Supply chain analytics-Harvard-scholarship

```{r setup, include=FALSE}
library(tinytex)
knitr::opts_chunk$set(echo = TRUE)
tinytex::tlmgr_install("pdfcrop")



library(ModelMetrics)
library(rpart)
library(rpart.plot)
library(caret)



#Import "Supply chain" dataset 

Historical_Product_Demand <- read.csv("C:/Users/Maqboul Computer/OneDrive/Desktop/Historical Product Demand.csv")

sum(is.na(Historical_Product_Demand))



## Data Preparartion:
#Remove NA's from datasets
library(lubridate) 
library(tidyverse)
library(dslabs)


new_historical_data <- na.omit(Historical_Product_Demand)

format_year <- as.Date(new_historical_data$Date, format = "%Y/%m/%d")
new_historical_data <- new_historical_data %>%
  mutate(years = format(format_year, "%Y"))  # extract year from Date 

new_historical_data <- unique(new_historical_data)
colnames(new_historical_data) 



library(ggplot2)
data_DB <- new_historical_data %>% 
  filter(years != "2011" & years != "2017")
  

data_DB$Order_Demand <- as.numeric(data_DB$Order_Demand) 
data_DB$Order_Demand <-replace_na(data_DB$Order_Demand, 0)
data_DB$Order_Demand
```

# Distributions of Order Demand by Warehouses through 5yrs.
```{r}
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

```

Boxplot explains analysis of ordering demands for each warehouse 
included the usual ordering demand (maximum & minimum orders values); there are the unusual ordering demand values which overcome the usual limit of warehouse.
That lead us to make a good emergency Demand Plan C & J Warehouses.

# HeatMap plan plot gathering date, warehouses, order demand through 5yrs.

```{r}
library(ggplot2)
data_DB %>% 
  ggplot(mapping=aes(x= years, y=Warehouse, na.rm= TRUE,
                     fill =log(Order_Demand))) + 
  geom_tile(colour = "white", size= 0.5) +
  coord_flip() +
  ggtitle("Orders.Demand per each warehouse 2012-2016")+
  ylab("Warehouses")+
  xlab("Years")
```

From Heatmap above it easily describes the intensity of colors which Grey color lead to missing data in specific two years at warehouse_C; otherwise in reality the warehouse_C can't use very well in ordering demand.

# Bar plot for product categories by Order Demand.
```{r}
data_DB %>% 
  ggplot() +
  geom_bar(aes(fct_infreq(Product_Category),Order_Demand),
         stat = "identity", fill="#f68060", size= 0.6) +
  coord_flip() +
  xlab("") +
  ggtitle("Order Demand of Product Categories") +
  theme_bw() 
```

Bar plot shows which is a product has high/low order quantity from warehouses, so we can take consider the forecasting quantity stock for each product categories separately.

# Warehouses are used in distribution process.
```{r}
data_DB %>% 
  count(Warehouse) %>%
  arrange(desc(n)) 
```

# Amount of orders shipped by each Warehouse 2012-2016. 
```{r}
data_DB %>% 
  group_by(Warehouse) %>%
  summarise(orders_per_WH= sum(Order_Demand)) %>%
  arrange(orders_per_WH) ## ascending orders per WH's
```

# Regression Tree Predictive Modeling.
```{r}
set.seed(1984)
index <-  createDataPartition(data_DB$Order_Demand,
                              times=1, p=0.75, list=FALSE)
trainData <- data_DB[index,]
testData <- data_DB[-index,]
```

# Growing Regression Tree.
```{r}
first_model_tree <- rpart(Order_Demand ~ Warehouse +Product_Category,
                    data = trainData, method = "anova",
                    control = rpart.control(cp = 0.001, minsplit =2,
                                            maxdepth= 4))
```

```{r}
rpart.plot(first_model_tree, type= 1, 
           yesno = TRUE, box.palette="RdBu", shadow.col="gray", nn=TRUE)
```

Note: Choosing CP with corresponding minimum relative error, and that the tree has 6 terminal nodes with 5 internal nodes in rpart.plot.The Error at 6 terminal nodes is around 0.93992.

```{r}
first_model_tree$variable.importance
```

The evidence that warehouses play relatively more important role on order demand than product categories.

# Prediction Order Demand in Regression Tree model
```{r}
pl <- predict(first_model_tree, newdata = testData)

RMSE_1 <- RMSE(testData$Order_Demand, pl) 
RMSE_1
```

RMSE_1 determines the Average Prediction of Order Demand 29261.03 using Regression Tree Predictive Model.

First model is highly recommended prediction model to our Order Demand, which describes that all products are ordering for warehouses C & S by average order 887, while the high orders are going for warehouses A & J by over 9,851 except  product_006 , _025, _019 & _033 are ordered more for warehouses C&S.

