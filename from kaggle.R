##### number 3 days since prior order   ####


## DataUserPrPrd() is in orders_users_analysis.R source code , does not need it since already comiited here users_order_prod_prior_dept
reorder_days <- function(department = NULL  ,aisle = NULL){
        a <- DataUserPrPrd()
        a <- a[which(a$department == department & a$aisle == aisle),] 
        a %>% 
        ggplot(aes(x=days_since_prior_order)) + 
        geom_histogram(stat="count",fill="blue")
}



reorder_days <- function(department = NULL  ,aisle = NULL){
                    
                                a <- users_order_prod_prior_dept   ###DataUserPrPrd()
                                b <- a[which(a$department == department & a$aisle == aisle),] 
                                b %>% 
                                  ggplot(aes(x=days_since_prior_order)) + 
                                  geom_histogram(stat="count",fill="blue")
                        
}


reorder_days_all <- function(){
  
                              a <- users_order_prod_prior_dept   ###DataUserPrPrd()
                            #  b <- a[which(a$department == department & a$aisle == aisle),] 
                              a %>% 
                                ggplot(aes(x=days_since_prior_order)) + 
                                geom_histogram(stat="count",fill="blue")
                              
}



reorder_days_ice <-  users_order_prod_prior_dept %>% 
                      ggplot(aes(x=days_since_prior_order)) + 
                      geom_histogram(stat="count",fill="blue")

##### number 4 qty of prior orders   ####


no_prior_orders <- function(department = NULL  ,aisle = NULL){
  a <- users_order_prod_prior_dept
  a <- a[which(a$department == department & a$aisle == aisle),] s
  
a %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="blue", size=1)+geom_point(size=2, color="blue")
  
}


no_prior_orders("frozen", "ice cream ice")
no_prior_orders()

reorder_days(department = "frozen", aisle = "ice cream ice")
reorder_days()




#### Number 5 how many items people buy
#see ![Distribution of ProductID Combinations](prodcombinations.png) needs to be merged with data to filter it. 

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

#### Number 6 how many items people buy


