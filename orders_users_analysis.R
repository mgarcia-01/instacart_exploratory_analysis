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



############################################ User and Orders  #############################################
#  frequncy use table function. only for "prior" eval_set
# ordersPrior dataset with only prior evalset. 3,214,874 records 
#frequency of userid also means number of orders by the user in this dataset

## A20 ## moved this to datasetprep.R
##ordersPrior <- as.data.frame(orders[which(orders$eval_set == "prior"),])
## useridfreq <- as.data.frame(table(ordersPrior$user_id))
# Freq Table
names(useridfreq) <- c("user_id","FrqOrders")
#Summary of Table 
frqSummary <- function(x){
  x <- as.table(summary(useridfreq$FrqOrders))
  return(x)
  }
#names(frqSummary)

#1 how many users by day of the week to date in entire data set and barplot
#2 time of day users placed an order and line plot 






#################################### 1 Main Function for datauserprprd   ##########################################
library("plyr")
dow_userFunc <- function(x){
  dow_users <- plyr::ddply(ordersPrior,"order_dow",summarize
                           ,no_dist_users=(length(unique(user_id))))
  return(dow_users)
}


############  2  DOW BY DEPT AISLE FUNCTION to create the dataset ################
#!!!!!!    Use this  users_order_prod_prior_dept  
#DataUserPrPrd <- function(){
#  UsrPrdDpAsl <- as.data.frame(merge(x = ordersPriorProduct
#                                     , y = order_prod_prior_dept
#                                    , by = c("order_id","product_id","add_to_cart_order","reordered")
#                                     )
#                               )
#                            }




dow_userDptAslFunc <- function(department = NULL  ,aisle = NULL){
        
        as.character(department)
        as.character(aisle)
      if(is.null(department) == FALSE & is.null(aisle) == FALSE){
        aa <- users_order_prod_prior_dept
        uspda <- aa[which(aa$department == department & aa$aisle == aisle),]
                         oppd <- plyr::ddply(uspda,"order_dow",summarize
                                             ,no_dist_orders=(length(unique(order_id))))
                        #names(oppd) <- c("department", "aisle", "product_name", "no_orders")
                        #das <- oppd[which(oppd$department == department & oppd$aisle == aisle),]
                        #asort <- head(das[order(das$no_orders, decreasing= TRUE),], n = topN)
                        return(oppd)
                        } else if (is.null(department) == TRUE & is.null(aisle) == TRUE){
                                    a <- DataUserPrPrd()
                                    oppda <- plyr::ddply(a,"order_dow",summarize
                                                            ,no_dist_orders=(length(unique(order_id))))
                                   # names(oppd) <- c("department", "aisle", "product_name", "no_orders")
                                  #  das <- oppd[which(oppd$department == department & oppd$aisle == aisle),]
                                  #  asort <- head(das[order(das$no_orders, decreasing= TRUE),], n = topN)
                                    return(oppda)
                        }
        }



frozen_icecream_dow <- file.path(getwd(),paste("frozen_icecream_dow",".png",sep = ""))
png(filename = frozen_icecream_dow,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)

dow_icefrozn <- dow_userDptAslFunc(department = "frozen", aisle = "ice cream ice")
plot(x=dow_icefrozn$order_dow, y= (as.numeric(dow_icefrozn$no_dist_orders)/1000)
     , type = "b",col = "dodgerblue", lwd = 5
     ,xlab = "Day of Week", ylab = "No of Orders ('000s)", sub = "")
text(dow_icefrozn$order_dow, (as.numeric(dow_icefrozn$no_dist_orders)/1000)
     ,labels = round((as.numeric(dow_icefrozn$no_dist_orders)/1000))
     , cex=0.85, font = 4, pos= 3, col="black")


dev.off()


##################################### 3  HOUR ICE CREAM  ##################################################################
hour_userDptAslFunc <- function(department = NULL  ,aisle = NULL){
              
                              as.character(department)
                              as.character(aisle)
                                if(is.null(department) == FALSE & is.null(aisle) == FALSE){
                                  aa <- users_order_prod_prior_dept
                                  uspda <- aa[which(aa$department == department & aa$aisle == aisle),]
                                  oppd <- plyr::ddply(uspda,"order_hour_of_day",summarize
                                                      ,no_dist_orders=(length(unique(order_id))))
                                  #names(oppd) <- c("department", "aisle", "product_name", "no_orders")
                                  #das <- oppd[which(oppd$department == department & oppd$aisle == aisle),]
                                  #asort <- head(das[order(das$no_orders, decreasing= TRUE),], n = topN)
                                  return(oppd)
                                } else if (is.null(department) == TRUE & is.null(aisle) == TRUE){
                                  a <- DataUserPrPrd()
                                  oppda <- plyr::ddply(a,"order_hour_of_day",summarize
                                                       ,no_dist_orders=(length(unique(order_id))))
                                  # names(oppd) <- c("department", "aisle", "product_name", "no_orders")
                                  #  das <- oppd[which(oppd$department == department & oppd$aisle == aisle),]
                                  #  asort <- head(das[order(das$no_orders, decreasing= TRUE),], n = topN)
                                  return(oppda)
                                }
                }

frozen_icecream_hour <- file.path(getwd(),paste("frozen_icecream_hour",".png",sep = ""))
png(filename = frozen_icecream_hour,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)

hour_icefrozn <- hour_userDptAslFunc(department = "frozen", aisle = "ice cream ice")
plot(x= hour_icefrozn$order_hour_of_day, y= (as.numeric(hour_icefrozn$no_dist_orders)/1000)
     , type = "h",col = "dodgerblue", lwd = 14
     ,xlab = "Hour of Day", ylab = "No of Orders ('000s)", sub = "")
text(hour_icefrozn$order_hour_of_day, (as.numeric(hour_icefrozn$no_dist_orders)/1000)
     ,labels = round((as.numeric(hour_icefrozn$no_dist_orders)/1000))
     , cex=0.78, font = 4, pos= 1, col="black")


dev.off()



####################


# check sample :    head(deptAisle_userCt[order(-deptAisle_userCt$no_dist_users),],10)
# see product_aisles_departments_analysis.R for this variable deptAisle_OrderCt
#deptAisle_OrderCt
#UsersOrdersCt_DeptAisl <- merge(x = deptAisle_OrderCt, y = deptAisle_userCt, by = c("department","aisle"))

#ordersPriorProduct
#prodAislDept




######    0 seems to be Saturdays and 1 = Sundays  
dow_userPlot <- file.path(getwd(),paste("dow_userPlot",".png",sep = ""))
png(filename = dow_userPlot,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)

dow_userPlotFunc <- function(x){ dow_users=dow_userFunc()
plot(x=dow_users$order_dow, y= (as.numeric(dow_users$no_dist_users)/1000)
     , type = "b",col = "dodgerblue", lwd = 5
     ,xlab = "Day of Week", ylab = "No of Users", sub = "Values In '000s")
text(dow_users$order_dow, (dow_users$no_dist_users/1000)
     ,labels = round((dow_users$no_dist_users)/1000)
     , cex=0.95, font = 2, pos= 1, col="black")
}

dow_userPlotFunc()
dev.off()


##### 5 number of orders per user. see Freq Table above ###
  #OrderUser <- plyr::ddply(ordersPrior,"user_id",summarise, no_orders=length(unique(order_number)))
  #OrderFreq <- plyr::ddply(ordersPrior,"user_id",summarise, no_orders=length(unique(order_number)))
  #plot(OrderUser, type = "p",col = "red", lwd = 1
  #     ,xlab = "No Users", ylab = "Freq of Orders")
  #text(OrderUser$no_orders, dow_users$no_dist_users
  #     ,labels = round(dow_users$no_dist_users) , cex=0.6, pos=4, col="black")

## OR USE THIS ....see near the beginning
  #ordersPrior <- orders[which(orders$eval_set == "prior"),]
  #useridfreq <- as.data.frame(table(ordersPrior$user_id))
  #names(useridfreq) <- c("user_id","FrqOrders")
  #hist(useridfreq$FrqOrders)
  #hist(useridfreq$FrqOrders, freq = FALSE)





######## How Many Reorder ##########


#write.csv(a,file.path(getwd(),paste("avgDaysPriorOrders",".csv",sep = "")))


#names(ordersPrior)
#head(ordersPrior[order(ordersPrior$user_id,ordersPrior$order_number),],10)



