############# from orders_users_analysis  ############

users_order_prod_prior_dept <- as.data.frame(merge(x = ordersPrior, y = order_prod_prior_dept , by = "order_id"))
#users_order_prod_prior_dept <- users_order_prod_prior_dept[order("user_id","order_id","add_to_cart_order"),]

library(plyr)
deptAisle_userCt <- plyr::ddply(users_order_prod_prior_dept,.(aisle,department),summarize
                                ,no_dist_users=(length(unique(user_id))))

#### D1 - this is to get order_user to products in the order. users increase product variety  as they return to place future orders. See product_aisles_departments_analysis.R
#   !!! Replace this in code. use the users_order_prod_prior_dept already made in code
A_ordersPriorProduct <- as.data.frame(merge(x = ordersPrior, y = order_products__prior , by = "order_id"))
ordersPriorProd <- A_ordersPriorProduct[order(A_ordersPriorProduct$user_id,A_ordersPriorProduct$order_number,A_ordersPriorProduct$order_id,A_ordersPriorProduct$add_to_cart_order),]
ordersPriorProduct <- ordersPriorProd
rm(ordersPriorProd)