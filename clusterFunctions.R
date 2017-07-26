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

#### Classification - Run Seperate
library(mclust)
install.packages("mclust", dependencies = TRUE, verbose = TRUE)

groups_userDayHour <- file.path(getwd(),paste("groups_userDayHour",".png",sep = ""))
png(filename = groups_userDayHour,
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)

fit <- Mclust(avg_time_dow_users())
plot(fit) # plot results
summary(fit) # display the best model 

dev.off()