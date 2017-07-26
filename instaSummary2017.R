######  Basic Summary  ####
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

hist(orders$order_hour_of_day)
hist(orders$user_id)

summary(orders)

#############  how many reorders. when are they reordered. how long until reordered #####


## orders_nPrior contains 32,434,489  (32 million) observations

# merges the orders and order_products_prior by orderId
orders_nPrior <- merge(x = orders, y = order_products__prior, by = "order_id")
orders_prod_nprior <- merge(x = products, y = orders_nPrior, by = "product_id")

#merges the orders and order_products_train by order_id
orders_nTrain <- merge(x = orders, y = order_products__train, by = "order_id")

hist(order_products__prior$product_id)
library(plyr)
library(Hmisc)
library(ggplot2)



prod_aggregate <- plyr::count(orders,"order_hour_of_day")

########################################### Add to Cart Data  ###############################################
###                                                                                                       ###


# aggregates add to cart by hour, day and product
prod_aggregate <- aggregate(orders_nPrior$add_to_cart_order, list(orders_nPrior$order_hour_of_day, orders_nPrior$product_id
                                                                  ,orders_nPrior$order_dow), mean)

prod_aggregate <- aggregate(orders_nPrior$add_to_cart_order, list(orders_nPrior$order_dow,orders_nPrior$order_hour_of_day), mean)

#meanof add_to_cart_orders over a 24 hour day. Time Zone is unknown, but the average high is around 8.3-8.6 between 1am and 10am
  # between 1am and 10am it declines around 11AM 
prod_agg_time <- aggregate(orders_nPrior$add_to_cart_order, list(orders_nPrior$order_hour_of_day), mean)
plot(prod_agg_time$Group.1, prod_agg_time$x,  type = "b", xlab = "Hour of Day", ylab = "Avg AddtoCart")

#meanof add_to_cart_orders over a 24 hour day. Time Zone is unknown, but the average high is around 8.3-8.6 between 1am and 10am
# between 1am and 10am it declines around 11AM 
prod_agg_dow <- aggregate(orders_nPrior$add_to_cart_order, list(orders_nPrior$order_dow), mean)
plot(prod_agg_dow$Group.1, prod_agg_dow$x, type = "b", xlab = "Day of Week", ylab = "Avg AddtoCart")



########################################### User Data      ###############################################
###                                                                                                       ###


user_agg_dow <- aggregate(orders_nPrior$order_number, list(c), mean)
plot(user_agg_dow$Group.1, user_agg_dow$x, type = "p", xlab = "orders", ylab = "users")

library(plyr)

userid_no_orders <- plyr::count(orders_nPrior$user_id)

agg <- aggregate(data=orders_nPrior$order_number, type ~ color, function(x) length(unique(x)))
merge(df, agg, by="color", all=TRUE)






# aggregates sum of reorder, by product id and product name. this is boolean , does not say number of reorders 
reord_agg <- aggregate(orders_prod_nprior$reordered, list(orders_prod_nprior$product_id
                                                          ,orders_prod_nprior$product_name)
                       , sum)
names(reord_agg) <- c("product_id","product_name", "reorder")

# aggregates mean reorder, by product id and product name. this is boolean , does not say number of reorders 
reord_agg_m <- aggregate(orders_prod_nprior$reordered, list(orders_prod_nprior$product_id
                                                          ,orders_prod_nprior$product_name)
                       , mean)
names(reord_agg_m) <- c("product_id","product_name", "reorder")

ra_ramMerge <- merge(x = reord_agg, y = reord_agg_m, by = "product_id")
names(ra_ramMerge) <- c("product_id","product_name", "reordersum", "product_nameavg", "reorderavg")

prod_aggregate <- ra_ramMerge


with(mtcars, tapply(mpg, cyl, mean))
sapply(split(mtcars$mpg, mtcars$cyl), mean)
tapply(mtcars$mpg, mtcars$cyl, mean)

# gets the product aggregate plots
ggplot(prod_aggregate
       , aes(y=x, x=Group.2)
      )+geom_line(aes(group=Group.1
                          ,colour=factor(Group.1)
                      )
                  )+geom_point(aes(group=Group.1
                                    ,colour=factor(Group.1)
                                    )
                                          , size = 2
                                )




###

########    prob of time of day for add to carts. check corr with purch time of day ####
hist(prod_aggregate$x
     , xlab = "dist addtocart hr/day"
     ,prob = TRUE
     , )



a <- plyr::summarize(head(as.list(order_products__prior$add_to_cart_order),50000))


