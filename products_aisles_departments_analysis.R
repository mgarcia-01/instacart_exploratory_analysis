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

############################################ products, aisles, and departments  #############################################





# use ordersPrior as this is the Orders set for only Prior evalsets. 
# order_products__prior has 32,434,489. It has 3,214,874 unique order_id's. It has the products within the basket
# ordersPrior has 3,214,874
#ordersProdPrior <- data.table(merge(x = ordersPrior, y = order_products__prior, by = "order_id"))

## 1 how many products per order

#prodFreq <- function(x){
#  a <- plyr::ddply(order_products__prior,"product_id",summarize
#           ,no_orders=(length(unique(order_id))))
#}

#hist(log10(a$no_orders), freq = TRUE
#            , breaks = 100
#            , col = "dodgerblue"
#            , main = "Unique Products Per Order Histogram "
#            , xlab = "log10 No of Products Per Order" )
#rug(log10(a$no_orders), col = "blue")
#abline(v = mean(log10(a$no_orders)), col = "red", lwd = 3)

######################### 1 how many products per order Complete for Notebook  ######
## quantile(useridfreq$FrqOrders)



#totalTimesProductPurch <- function(x){
#  a <-  as.data.frame(prodFreq())
#  b <- (hist(log10(a$no_orders))
#             #(a$no_orders)
#            ,  freq = TRUE
#             , breaks = 50
#             , col = "dodgerblue"
#             , main = "Total Times Product Purchased "
#             , xlab = "log10 No of Products Per Order" 
#             , ylab = "How often Products are Purchased"
#        )
#  c <- rug(log10(a$no_orders), col = "blue")
#  d <- abline(v = mean(log10(a$no_orders)), col = "red", lwd = 3)
#  return(d)
#}



##### Cluster of Products and Orders 


## counting product_ids by each order takes a looong time . took 10 minutes ##
#uniqueProdFreq <- function(x){
  #a <- data.table(plyr::ddply(order_products__prior,"order_id",summarize
  #                 ,no_products=(length(product_id)))
  #              )
  
  
 # return(a)
#}


a <- aggregate(order_products__prior$product_id, list(order_products__prior$order_id), length)
names(a) <- c("order_id" ,"no_products")

prodcombinations <- file.path(getwd(),paste("prodcombinations",".png",sep = ""))
png(filename = prodcombinations,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)


hist(a$no_products, freq = TRUE
     , breaks = 145
     , col = "dodgerblue"
     , main = "Basket Variety - Product Combinations"
     , xlab = "Count of Product ID"
     , ylab = "No. of Orders")
rug(a$no_products, col = "blue")
abline(v = mean(a$no_products), col = "red", lwd = 3)


dev.off()


############################### prodcombinations with department and aisle parameter


dept_aisle_select <- function(department = NULL , aisle = NULL){
                            aab <- users_order_prod_prior_dept
                            aaa <- aab[which(aab$department == department & aab$aisle == aisle),]
                            return(aaa)
                          }
  
  aab <- dept_aisle_select(department = "frozen", aisle = "ice cream ice")
  aac <- aggregate(aab$product_id, list(aab$order_id), length)
names(aac) <- c("order_id" ,"no_products")

prdcombprob_frzIcecrm <- file.path(getwd(),paste("prdcombprob_frzIcecrm",".png",sep = ""))
png(filename = prdcombprob_frzIcecrm,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)


hist(aac$no_products, freq = TRUE
     , breaks = 145
     , col = "dodgerblue"
     , main = "Ice Cream Basket Variety Probability"
     , xlab = "Count of Distinct Product ID"
     , ylab = "No. of Orders")
rug(aac$no_products, col = "blue")
abline(v = mean(aac$no_products), col = "red", lwd = 3)


dev.off()



################################################## part 2  ######################################

prdDeptAisle <- aggregate(prodAislDept$product_id, list(prodAislDept$department,prodAislDept$aisle), length)
names(prdDeptAisle) <- c("department","aisle","productCount")

#prdDeptAisle[order(prdDeptAisle$productCount),]

# aisle and department most shopped 
##    Most shopped department and aisle 

deptAisle_OrderCt <- aggregate(order_prod_prior_dept$order_id, list(order_prod_prior_dept$department,order_prod_prior_dept$aisle), length)
names(deptAisle_OrderCt) <- c("department","aisle","orderCount")

NoOrder_DeptAisle <- deptAisle_OrderCt[order(deptAisle_OrderCt$orderCount),]

#################
#### Dataset used is D1

order_basketsize_trend <- aggregate(ordersPriorProduct$product_id,list(ordersPriorProduct$user_id,ordersPriorProduct$order_number),length)
names(order_basketsize_trend) <- c("user_id","order_number","productCt")

basketsize_trend <- aggregate(order_basketsize_trend$productCt,list(order_basketsize_trend$order_number),mean)
names(basketsize_trend) <- c("order_number", "avg_basketsize")


basketsize_trend_Plot <- file.path(getwd(),paste("basketsize_trend_Plot",".png",sep = ""))
png(filename = basketsize_trend_Plot,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)

plot( x = basketsize_trend$order_number, y = basketsize_trend$avg_basketsize, type = "p"
      ,col = "dodgerblue", lwd = 1, pch = 19
      ,xlab = "Order Number (Sequence)", ylab = "Avg Basket Size", sub = "")
#abline(h = mean(basketsize_trend$avg_basketsize), col = "blue", lwd = 1)
abline(lsfit(x = basketsize_trend$order_number,y = basketsize_trend$avg_basketsize ),col = "blue",lwd = 1)

dev.off()


#var(sample(basketsize_trend$avg_basketsize))
#cor(basketsize_trend, method = "spearman")
#sd(basketsize_trend$avg_basketsize)






