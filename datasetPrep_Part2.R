
## need to convert to merge with another 2 sets in one ######### products aisle depts   #######
A_prodAislDept <- as.data.frame(merge(x = products, y = aisles, by = "aisle_id"))

prodAislDept <- as.data.frame(merge(x = A_prodAislDept, y = departments, by = "department_id"))


# nrow =  32434489
order_prod_prior_dept <- as.data.frame(merge(x = order_products__prior, y = prodAislDept, by = "product_id"))


##  A20 -  User and Orders  #############################################
ordersPrior <- as.data.frame(orders[which(orders$eval_set == "prior"),])
useridfreq <- as.data.frame(table(ordersPrior$user_id))