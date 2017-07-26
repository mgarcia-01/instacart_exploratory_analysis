################################## Orders users analysis pt 3
##### 3  Time Users Functions ### 
# example of parralel processing  aaply(seq(1,10000,100), function(x) rnorm(1, mean=x+(1:100), .parallel=TRUE)
time_users <- function(){plyr::ddply(ordersPrior,"order_hour_of_day"
                                     ,summarise,no_dist_users=(length(unique(user_id)))/1000)
}

time_dow_users <- function(){plyr::ddply(ordersPrior
                                         , .(order_hour_of_day, order_dow)
                                         ,summarise,no_dist_users=log10(length(unique(user_id))))
}


avg_time_dow_users <- function(x){
  
  p <- as.data.frame(time_dow_users())
  px <- aggregate(p$no_dist_users
                  , list(p$order_dow
                         ,p$order_hour_of_day)
                  , mean
  )
  names(px) <- c("order_dow","order_hour_of_day","no_dist_users")
  
  
  py <- transform(px, dayHour = paste(px$order_dow,px$order_hour_of_day,sep = ":"))
  pa <- aggregate(py$no_dist_users, list(py$dayHour),mean)
  names(pa) <- c("dayHour","avg_users")
  
  return(pa)  #[order(pa$dayHour),])
}



avg_time_dow_usersPlot <- function(x){
  a = as.data.frame(avg_time_dow_users())
  plot(a$dayHour, a$avg_users, pch = 21
       , type = "p",col = "dodgerblue", lwd = 2
       , ylim = c(1,max(a$avg_users)+.25))
  #text(a$dayHour, a$avg_users
  # ,labels = a$avg_users
  #    , cex=0.10,font = 1, pos=1, col="black")
}





################################## END



#transform(avg_timeday_users,daytime=paste0(x,y,z))




#avg_time_dow_users <- as.data.frame(aggregate(time_dow_users$user_id, list(time_dow_users$order_dow, time_dow_users$order_hour_of_day)
#                                , unique))
#names(avg_time_dow_users) <- c("order_dow","order_hour_of_day", "unique")






#plot(time_users, type = "h",col = "red", lwd = 10
#     ,xlab = "Time of Day", ylab = "No of Users")
# use with function above

time_usersPlot <- file.path(getwd(),paste("time_usersPlot",".png",sep = ""))
png(filename = time_usersPlot,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)

time_usersPlot <- function(){ time_users=time_users()
plot(time_users, type = "h",col = "skyblue"
     ,lwd = 14, xlab = "Hour of Day", ylab = "No of Users ('000s)")
text(time_users$order_hour_of_day, time_users$no_dist_users
     ,labels = round(time_users$no_dist_users,0) , cex=0.78,font = 6, pos=3, col="black")
# rug(time_users$order_hour_of_day, col = "red")
#abline(v = mean(time_users$order_hour_of_day), col = "red", lwd = 3)
}
time_usersPlot()
dev.off()

#3 number of unique users in the Prior set ; that are not test or train data:   206,209
#    distinctUsers <- length(unique(ordersPrior$user_id))
#4 number of unique orders prior set: 3,214,874
#  no_orders <- length(unique(ordersPrior$order_id))
#dont need this  since already menteind above#  length(unique(ordersPrior$order_number))


userCounts <- function(x) {
  no_Users <- length(unique(ordersPrior$user_id))
  no_Orders <- length(unique(ordersPrior$order_id))
  a <- as.data.frame(cbind(no_Users,no_Orders))
  return(a)
}

userCounts <- userCounts()
