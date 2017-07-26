NonUniquereordersDataFunc <- function(metricType = NULL, reorderType = NULL, department = NULL, aisle = NULL){
  
  if(metricType == "reorderTotal" & reorderType == "1"){
    noreordera <- order_prod_prior_dept[which(order_prod_prior_dept$reordered == "1" 
                                              & order_prod_prior_dept$department == department
                                              & order_prod_prior_dept$aisle == aisle),]
    noreorderb <- plyr::ddply(noreordera,.(product_name),summarize
                              ,no_noreorders=(length((order_id))))
    a <- nrow(noreorderb)
    
  } else if (metricType == "notreorderTotal" & reorderType == "0")
  {noreordera <- order_prod_prior_dept[which(order_prod_prior_dept$reordered == "0" 
                                             & order_prod_prior_dept$department == department
                                             & order_prod_prior_dept$aisle == aisle),]
  noreorderb <- plyr::ddply(noreordera,.(product_name),summarize
                            ,no_noreorders=(length((order_id))))
  noreorderb <- noreorderb[order(noreorderb$no_noreorders, decreasing = TRUE),]
  a <- nrow(noreorderb)
  }
  return(a)
}
nonuniqueReord <- NonUniquereordersDataFunc(metricType = "reorderTotal", reorderType = "1", department = "frozen", aisle = "ice cream ice")


reordersDataFunc <- function(metricType = NULL, reorderType = NULL, department = NULL, aisle = NULL){
                            
                              if(metricType == "reorderTotal" & reorderType == "1"){
                                noreordera <- order_prod_prior_dept[which(order_prod_prior_dept$reordered == "1" 
                                                                          & order_prod_prior_dept$department == department
                                                                          & order_prod_prior_dept$aisle == aisle),]
                                noreorderb <- plyr::ddply(noreordera,.(product_name),summarize
                                                        ,no_noreorders=(length(unique(order_id))))
                                a <- nrow(noreorderb)
                                
                                    } else if (metricType == "notreorderTotal" & reorderType == "0")
                                                  {noreordera <- order_prod_prior_dept[which(order_prod_prior_dept$reordered == "0" 
                                                                                             & order_prod_prior_dept$department == department
                                                                                             & order_prod_prior_dept$aisle == aisle),]
                                                                                      noreorderb <- plyr::ddply(noreordera,.(product_name),summarize
                                                                                                                ,no_noreorders=(length(unique(order_id))))
                                                                                      noreorderb <- noreorderb[order(noreorderb$no_noreorders, decreasing = TRUE),]
                                                                                      a <- nrow(noreorderb)
                                                                                      }
                            return(a)
}


reorderedTotal <- reordersDataFunc(metricType = "reorderTotal", reorderType = "1", department = "frozen", aisle = "ice cream ice")
NotorderedTotal <- reordersDataFunc(metricType = "notreorderTotal",  reorderType = "0", department = "frozen", aisle = "ice cream ice")

#Reordered_Total <- reordersDataFunc()
#NotReordered_Total <- reordersDataFunc(metricType = "notreorderTotal", reorderType = "0")


#reordersDataFunc(metricType = "reorderTotal", reorderType = "1")

reorder_days <- function(department = NULL  ,aisle = NULL){
  a <- users_order_prod_prior_dept
  a <- a[which(a$department == department & a$aisle == aisle),] 
  a %>% 
    ggplot(aes(x=days_since_prior_order)) + 
    geom_histogram(stat="count",fill="blue")
}


reorder_days(department = "frozen", aisle = "ice cream ice")



no_prior_orders <- function(department = NULL  ,aisle = NULL){
  a <- users_order_prod_prior_dept
  a <- a[which(a$department == department & a$aisle == aisle),] 
  
  a %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="blue", size=1)+geom_point(size=2, color="blue")
  
}


no_prior_orders("frozen", "ice cream ice")
no_prior_orders()








################################ how many purchase again 






