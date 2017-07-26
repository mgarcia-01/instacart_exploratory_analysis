###### Top selling products  ####

library(ggplot2)
# this function allows a user to select depertmant or aisle of a dataset based on the top ten most products purchased

top_products <- function(department = NULL, aisle = NULL, topN = 10){
  as.character(department)
  as.character(aisle)
  
    if(is.null(department) == FALSE & is.null(aisle) == FALSE)
    {
      
      oppd <- aggregate(order_prod_prior_dept$order_id
                        , list(order_prod_prior_dept$department
                               ,order_prod_prior_dept$aisle
                               ,order_prod_prior_dept$product_name)
                        , length)
                # used ddply but it gives same result
               #a <-  plyr::ddply(order_prod_prior_dept,.(department, aisle,product_name),summarize
               #             ,no_dist_orders=(length(unique(order_id))))
      names(oppd) <- c("department", "aisle", "product_name", "no_orders")
      das <- oppd[which(oppd$department == department & oppd$aisle == aisle),]
      asort <- head(das[order(das$no_orders, decreasing= TRUE),], n = topN)
      return(asort)
    } else if(is.null(department) == TRUE & is.null(aisle) ==  TRUE )
           {oppd <- aggregate(order_prod_prior_dept$order_id
                              , list(order_prod_prior_dept$department
                                     ,order_prod_prior_dept$aisle
                                     ,order_prod_prior_dept$product_name)
                              , length)
            names(oppd) <- c("department", "aisle", "product_name", "no_orders")
            das <- oppd
            asort <- head(das[order(das$no_orders, decreasing= TRUE),], n = topN)
            return(asort)
          } 
  }


### used to get png for frozen and ice cream
#frozen_icecream <- file.path(getwd(),paste("frozen_icecream",".png",sep = ""))
#png(filename = frozen_icecream,
#    width = 480, height = 480, units = "px", pointsize = 12,
#    bg = "white",  res = NA,## ...,
#    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
#    antialias = c("default")
#)
#adf <- top_products(department = "frozen", aisle = "ice cream ice", topN = 15)
#treemap::treemap(dtf = adf, index = "product_name"
#                 ,type = "index"
#                 ,vSize = "no_orders"
#                 #,vColor = "no_orders"
#                 ,command.line.output = FALSE
#                 #,palette = "Set1"
#                 ,fun.aggregate = "sum")
#dev.off()

##### barplot








#### used to get all dept and aisles
#RColorBrewer::display.brewer.all()
all_dept <- file.path(getwd(),paste("all_dept",".png",sep = ""))
png(filename = all_dept,
    width = 1000, height = 850, units = "px", pointsize = 12,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)
allDept <- top_products(topN = 60)
treemap::treemap(dtf = allDept, index = c("department","aisle", "product_name")
                 ,type = "index"
                 , fontsize.labels=c(20,14,10)
                 , vSize = "no_orders"
                 ,vColor = "department"
                 ,command.line.output = FALSE
                 ,fontface.labels = "bold"
                 ,palette = "Set1"
                 ,fun.aggregate = "sum")
dev.off()


###### Ice Cream Only

frozen_icecream_barplt <- file.path(getwd(),paste("frozen_icecream_barplt",".png",sep = ""))
png(filename = frozen_icecream_barplt,
    width = 480, height = 580, units = "px", pointsize = 10,
    bg = "white",  res = NA,## ...,
    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
    antialias = c("default")
)
adf <- top_products(department = "frozen", aisle = "ice cream ice", topN = 12)

ggplot(data=adf, aes(x=adf$product_name, y=adf$no_orders)) +
  geom_bar(stat="sum") + theme(axis.text.x = element_text(angle = 90, hjust = 1)
                               ,axis.title.x = element_text(face="bold", size=12)
                               ,legend.position="none"
  )+labs(x="Product Name",y="No. Orders") 
dev.off()

####### used to get only departments
#RColorBrewer::display.brewer.all()
#alldept_prod <- file.path(getwd(),paste("all_dept",".png",sep = ""))
#png(filename = alldept_prod,
#    width = 480, height = 480, units = "px", pointsize = 12,
#    bg = "white",  res = NA,## ...,
#    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
#    antialias = c("default")
#)
#alldept_prod <- top_products(topN = 60)
#treemap::treemap(dtf = alldept_prod, index = c("department", "product_name")
#                 ,type = "index"
#                 , fontsize.labels=c(20,10)
#                 , vSize = "no_orders"
#                 ,vColor = "department"
#                 ,command.line.output = FALSE
#                 ,fontface.labels = "bold"
#                 ,palette = "Set1"
#                 ,fun.aggregate = "sum")
#dev.off()



####### used to get only aisles
#RColorBrewer::display.brewer.all()
#allaisle_prod <- file.path(getwd(),paste("all_dept",".png",sep = ""))
#png(filename = allaisle_prod,
#    width = 480, height = 480, units = "px", pointsize = 12,
#    bg = "white",  res = NA,## ...,
#    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
#    antialias = c("default")
#)
#allaisle_prod <- top_products(topN = 60)
#treemap::treemap(dtf = allaisle_prod, index = c("aisle", "product_name")
#                 ,type = "index"
#                 , fontsize.labels=c(20,10)
#                 , vSize = "no_orders"
#                 ,vColor = "department"
#                 ,command.line.output = FALSE
#                 ,fontface.labels = "bold"
#                 ,palette = "Set1"
#                 ,fun.aggregate = "sum")
#dev.off()