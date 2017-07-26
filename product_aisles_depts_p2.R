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


##### percent changes for basket variety plot #

av <- zoo::zoo(basketsize_trend$avg_basketsize)
basketsize_lag <- as.data.frame(lag(av,k = -1, na.pad = TRUE))
names(basketsize_lag) <- c("avg_basketsize_lag")

basketsize_prior <-  cbind(basketsize_trend[1:2], basketsize_lag[1])
basketsize_pct <-  as.data.frame(((basketsize_prior$avg_basketsize - basketsize_prior$avg_basketsize_lag)
                                  /basketsize_prior$avg_basketsize_lag)*100)
names(basketsize_pct) <- c("pct_chg")
basketsize_pct_trend <- cbind(basketsize_prior[1:3],basketsize_pct[1])




basketsize_pctChg_Plot <- file.path(getwd(),paste("basketsize_pctChg_Plot",".png",sep = ""))
png(filename = basketsize_pctChg_Plot,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)
plot( x = basketsize_pct_trend$order_number, y = basketsize_pct_trend$pct_chg, type = "p"
      ,col = "dodgerblue", lwd = 1, pch = 19
      ,xlab = "Order Number (Sequence)", ylab = "Variance", sub = "")
#abline(h = mean(basketsize_trend$avg_basketsize), col = "blue", lwd = 1)
abline(lsfit(x = basketsize_pct_trend$order_number,y = basketsize_pct_trend$pct_chg ),col = "blue",lwd = 1)

dev.off()




########## ice cream only   percent variance
### see part 1 for function


tempset <- as.data.frame(basketTrend_DepAisle(department = "frozen", aisle = "ice cream ice"))

av <- zoo::zoo(tempset$avg_basketsize)
basketsizeFilt_lag <- as.data.frame(lag(av,k = -1, na.pad = TRUE))
names(basketsizeFilt_lag) <- c("avg_basketsize_lag")

basketsize_prior_Filt <-  as.data.frame(cbind(tempset[1:2], basketsizeFilt_lag[1]))
basketsizeFilt_pct <-  as.data.frame(((basketsize_prior_Filt$avg_basketsize - basketsize_prior_Filt$avg_basketsize_lag)
                                  /basketsize_prior_Filt$avg_basketsize_lag)*100)
names(basketsizeFilt_pct) <- c("pct_chg")
basketsizeFilt_pct_trend <- as.data.frame(cbind(basketsize_prior_Filt[1:3],basketsizeFilt_pct[1]))



basketsizeFilt_pctChg_Plot <- file.path(getwd(),paste("basketsizeFilt_pctChg_Plot",".png",sep = ""))
png(filename = basketsizeFilt_pctChg_Plot,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)
plot( x = basketsizeFilt_pct_trend$order_number, y = basketsizeFilt_pct_trend$pct_chg, type = "p"
      ,col = "dodgerblue", lwd = 1, pch = 19
      ,xlab = "Order Number (Sequence)", ylab = "Variance", sub = "")
#abline(h = mean(basketsize_trend$avg_basketsize), col = "blue", lwd = 1)
abline(lsfit(x = basketsizeFilt_pct_trend$order_number,y = basketsizeFilt_pct_trend$pct_chg ),col = "blue",lwd = 1)

dev.off()
rm(tempset)

#####    Top selling products #### 
# ice cream and personal category #  personal skin and care  #  Questions # 


