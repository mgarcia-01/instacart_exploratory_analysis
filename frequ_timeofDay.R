#### time of day clusters ####
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
library(plyr)


orders_aggregate <- plyr::count(orders,"order_hour_of_day")


plot(orders_aggregate$freq,orders_aggregate$order_hour_of_day,type = "h")

########## frequency of the hour of day orders are placed  ###########
orderHourfreq <- file.path(getwd(),paste("orderHourfreq",".png",sep = ""))
png(filename = orderHourfreq,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    #type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)
hist(orders[which(orders$eval_set!= 'train' | orders$eval_set!= 'test'), "order_hour_of_day"]
     ,xlab = "hour of day"
     ,main = "freq dist for hour of day purch"
     ,col = "green"
     ,border = "black"
     ,freq = TRUE
     ,labels = TRUE
     )

dev.off()

plot(orders$order_hour_of_day, orders$order_dow)


########## probability of an order taking place at a certain hour  ###########
orderHourprob <- file.path(getwd(),paste("orderHourprob",".png",sep = ""))
ttlorderHourfreq <- file.path(getwd(),paste("ttlorderHourfreq",".png",sep = ""))
png(filename = ttlorderHourfreq,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    #type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)
hist(orders[which(orders$eval_set!= 'train'), "order_hour_of_day"]
     ,xlab = "hour of day"
     , main = "freq dist for hour of day purch 30 MIL"
     ,col = "green"
     ,border = "black"
     , freq = FALSE
     , prob = TRUE)
dev.off()





NEISCC <- merge(x = NEI, y = SCC, by = "SCC", all = TRUE)






NEISCC <- NEISCC[ which(NEISCC$fips == "24510"), ]
yearEmission3 <- aggregate(NEISCC$Emissions, list(NEISCC$type, NEISCC$year),sum)
###names(yearEmission3) <- c("year", "type","Emissions")

png(filename = plot3img,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    #type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
  )
  
              ggplot(yearEmission3
                     , aes(y=x, x=Group.2)
                      )+geom_line(aes(group=Group.1
                              ,colour=factor(Group.1)
                                      )
                                  )+geom_point(aes(group=Group.1
                               ,colour=factor(Group.1)
                              )
                            , size = 2
                            )
  dev.off()








itemTimeCluster <- function(itemList)
    {x=itemList
      timeFreq <- frequency()
      }
  
  
  











  ###

best <- function(state, outcome) {
  ## Read outcome data
  a <- read.csv(fileURL)
  aa <- a[c(2,7,11,17,23)]
  ab <- plyr::rename(aa,c("Hospital.Name"="hospital name"
                          ,"State" = "state"
                          , "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" = "heart attack"
                          ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" = "heart failure"
                          , "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" = "pneumonia"
  )
  )
  x=state
  y=outcome
  
  ## Check that state and outcome are valid
  
  if (missing(y))
    stop("missing state")
  if (missing(x))
    stop("missing outcome")
  if ((x %in% ab$state) == FALSE)
    stop("invalid state")
  if((y %in% c("heart attack", "heart failure" , "pneumonia")) == FALSE)
    stop("invalid outcome")
  
  c <- subset(ab, ab$state == x
              , select = c("hospital name", y )
  )
  myOutput <- subset(c, c[2]!= "Not Available")
  
  
  
  ## Return hospital name in that state with lowest 30-day death rate'
  hospital <- as.character(dplyr::first(myOutput$`hospital name`,order_by = myOutput[2]))
  return(hospital)
  
}