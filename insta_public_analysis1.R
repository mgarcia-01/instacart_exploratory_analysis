###########################   License Short Notice Start  #########################
#     Copyright(C) Michael A Garcia 2017. All Rights Reserved.                    #
#     Contact Author: mgar_datascience@protonmail.com for Licensing Inquries      #
#                                                                                 #
#     This program is distributed in the hope that it will be useful,             #
#     but WITHOUT ANY WARRANTY; without even the implied warranty of              #
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               #
#     GNU General Public License Version 3 for more details.                      #
#     See http://www.gnu.org/licenses for full license.                           #              
#                                                                                 #
###########################   License Short Notice End   #########################


##run this for list of packages and licenses ##
listPckg <- function(x){
  a <- as.data.frame(installed.packages())
  return(a[c(10,12)])
}

## URL For Public Data https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2  ##

### NON COMMERCIAL TERMS ###

#The dataset is provided as-is for non-commercial use, and can be downloaded from S3
#at: https://www.instacart.com/datasets/grocery-shopping-2017 and is subject to our 
#Terms and Conditions. For information about the contents of the files, see this data dictionary.

## CITATION REQUIREMENT ##
# “The Instacart Online Grocery Shopping Dataset 2017”, Accessed from https://www.instacart.com/datasets/grocery-shopping-2017 on <date>
# set system memory .





destURL <-  file.path(getwd(),paste("grcshp2017",".temp.tar.gz",sep = ""))

# token is not for use on download function
##fileURL <- "https://s3.amazonaws.com/instacart-datasets/instacart_online_grocery_shopping_2017_05_01.tar.gz?X-Amz-Expires=21600&X-Amz-Date=20170628T001346Z&X-Amz-Security-Token=FQoDYXdzEE4aDAOAUvwa7ayOZlaj6iKtAx8KSZ7LlLdAF%2Bh%2B3JqQXRrJPiTDf6STs2rmbGJUSgbEZCbJPFzxFikvzs6ds10cezVmtUV/weACFaEEApoAnEcrpjqXWvb%2B2GQYhJwOf9ufVS%2BLti3HrFEtAn0jJBx7uSbmxv%2Bb5NZ9NbA5kqztSFLEouLq0lD8qLa6KfYiMIdatXcaEn7lWddkh2JDGRUtDt5ymiMq2CUjX1nwY%2BzfWTbxNFHeiSVd8hRuB9HYz7ALFTXUQjJvOyBh97QWF4EeLf1B6QUggvGYdcE1S84wFH/NAwShAj5hY6hRDqd7hfCpKUpAHetO3M6NxQAmPTiK8%2ByXn05tTo4fhtV5OaZ%2BZFWmhIDOU97mPSEo/iawqC5eqLbOKGdSmGWaTma%2BRppmdsV9nQ6JTQqCmaFold9I3pn1jIPq7qjYMH4dFNqLPqDMRGtztprTHBvt/nexSZf4oIdj5bTxI8tudRRMcJ0RJt24/ptvMCZdSN/nr1G2mvZgrwZKa2pXi3n62cIrFWv0rF9LIqrJMDJygCxBoAa/Ikz65fkQyxYrH5MbGoe4ZeQr0nAFz1b3j5vgSDhQpij0lsvKBQ%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=ASIAJMPKVA54NL6BKWRA/20170628/us-east-1/s3/aws4_request&X-Amz-SignedHeaders=host&X-Amz-Signature=ed86cb3c95923b75a74c5e362ef9762840bd3bda971ceb22940af8137d51e0dd"

library(RCurl)
library(data.table)
download.file(url = fileURL, destfile = destURL,method = "wget")



unzipURL <- file.path(getwd(),paste("grcshp2017",".csv",sep = ""))
grcshp <- read.table(untar(tarfile = destURL, files = "grcshpdata"  ))




library(RCurl)
library(data.table)

# aisles has 134 records 


aisles <- fread(file.path(getwd(),paste("instacart_2017_05_01/aisles",".csv",sep = ""))
                      ,sep = "," 
                      , header = TRUE)

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
order_products__train <- fread(file.path(getwd(),paste("instacart_2017_05_01/order_products__train"
                                                            ,".csv",sep = ""))
                                    ,sep = "," 
                                    ,quote =  ""#NULL
                                    , header = TRUE)

#order_products__train <- data.table(order_products__train)




order_products__prior <- head(order_products__prior,50000)
order_products__train <- head(order_products__train,50000)



## orders_nPrior <- merge(x = orders[which(orders$eval_set == 'prior'),], y = order_products__prior, by = 'order_id')
## temporary limited size only to test program ##
orders_nPrior <- merge(x = orders, y = order_products__prior, by = "order_id")
orders_nTrain <- merge(x = orders, y = order_products__train, by = "order_id")






library(stats)
statresult <- stats::lsfit(x = orders_nPrior$days_since_prior_order, y = orders_nPrior$add_to_cart_order)
fita <- stats::lm(orders_nPrior$days_since_prior_order+orders_nPrior, data = orders_nPrior)

write.csv(statresult,file.path(getwd(),paste("instacart_2017_05_01/stats",".csv",sep = "")))


