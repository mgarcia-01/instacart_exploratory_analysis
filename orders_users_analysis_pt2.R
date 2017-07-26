#####orders users analysis pt 2 ########
userFrqPlot <- function(x) {
  ordersPrior <- orders[which(orders$eval_set == "prior"),]
  useridfreq <- as.data.frame(table(ordersPrior$user_id))
  names(useridfreq) <- c("user_id","FrqOrders")
  # a <- hist(useridfreq$FrqOrders, freq = FALSE)
  return(hist(useridfreq$FrqOrders, freq = TRUE
              , breaks = 25
              , col = "dodgerblue"
              , main = "Users and Qty of Orders"
              , xlab = "No. of Orders Per User" 
              , ylab = "No of Users"))
}


## 6  how many different day lenghts does each user have. for example, user_id 1 has waited 2, 4, 10 ,etc, days on different occasions 

days_prior_order <- function(x){
  a <- plyr::ddply(ordersPrior,"user_id",summarize
                   ,no_days_prior_order=(length(unique(days_since_prior_order))))
  return(hist(a$no_days_prior_order
              ,freq = TRUE))
}

avg_daysPriorPerUser <- function(x){
  a <- aggregate(as.numeric(ordersPrior[which(ordersPrior$days_since_prior_order != "NA"),"days_since_prior_order"])
                 , list(ordersPrior[which(ordersPrior$days_since_prior_order!= "NA"),"user_id"]), mean)
  names(a) <-   c("user_id,","num_days_prior")
  b <- hist(a$num_days_prior, freq = TRUE
            , breaks = 100
            , col = "dodgerblue"
            , main = "Users and Qty of Orders"
            , xlab = "Avg Days to Reorder" 
            , ylab = "No of Users")
  c <- rug(a$num_days_prior, col = "blue")
  d <- abline(v = mean(a$num_days_prior), col = "red", lwd = 3)
  return(d)
}



##### number 3 days since prior order   ####
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)


reorder_days <- function(department = NULL  ,aisle = NULL){
  a <- users_order_prod_prior_dept
  a <- a[which(a$department == department & a$aisle == aisle),] 
  a %>% 
    ggplot(aes(x=days_since_prior_order)) + 
    geom_histogram(stat="count",fill="blue")
}


##### number 4 qty of prior orders   ####
########## use for all ice cream only

no_prior_orders <- function(department = NULL  ,aisle = NULL){
  a <- users_order_prod_prior_dept
  b <- a[which(a$department == department & a$aisle == aisle),] 
  
  b %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="blue", size=1)+geom_point(size=2, color="blue")
  
}


Rplot_icecream_reorderdays <- file.path(getwd(),paste("Rplot_icecream_reorderdays",".png",sep = ""))
png(filename = Rplot_icecream_reorderdays,
    width = 1000, height = 850, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)

no_prior_orders("frozen", "ice cream ice")
dev.off()

########## use for all reorders
no_prior_orders_all <- function(){
  a <- users_order_prod_prior_dept
  a %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="blue", size=1)+geom_point(size=2, color="blue")
  
}

Rplot_ALL_reorderdays <- file.path(getwd(),paste("Rplot_ALL_reorderdays",".png",sep = ""))
png(filename = Rplot_ALL_reorderdays,
    width = 1000, height = 850, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)
no_prior_orders_all()
dev.off()