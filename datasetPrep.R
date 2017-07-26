library(RCurl)
library(data.table)

# aisles has 134 records 


aisles <- fread(file.path(getwd(),paste("instacart_2017_05_01/aisles",".csv",sep = ""))
                ,sep = "," 
                , header = TRUE)




#deparments has 
departments <- fread(file.path(getwd(),paste("instacart_2017_05_01/departments",".csv",sep = ""))
                     ,sep = "," 
                     , header = TRUE)



# read.table does not work on products due to many special characters 

# products has 49,688 records
products <- as.data.frame(fread(file.path(getwd(),paste("instacart_2017_05_01/products",".csv",sep = ""))))


# orders has 3,421,083 records #
orders <- fread(file.path(getwd(),paste("instacart_2017_05_01/orders",".csv",sep = ""))
                ,sep = "," 
                ,quote =  ""#NULL
                , header = TRUE)

## for use in ml model  ##
#  records 32,434,489
order_products__prior <- fread(file.path(getwd(),paste("instacart_2017_05_01/order_products__prior"
                                                       ,".csv",sep = ""))
                               ,sep = "," 
                               ,quote =  ""#NULL
                               , header = TRUE)

#order_products__prior <- data.table(order_products__prior)

# records 1,384,617
#order_products__train <- fread(file.path(getwd(),paste("instacart_2017_05_01/order_products__train"
#                                                       ,".csv",sep = ""))
#                               ,sep = "," 
#                               ,quote =  ""#NULL
#                               , header = TRUE)





####### from kaggle code
#orders <- fread('../input/orders.csv')
#products <- fread('../input/products.csv')
#order_products <- order_products__train    # fread('../input/order_products__train.csv')
#order_products_prior <- order_products__prior#fread('../input/order_products__prior.csv')
#aisles <- fread('../input/aisles.csv')
#departments <- fread('../input/departments.csv')

