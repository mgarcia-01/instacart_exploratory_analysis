#Purchase frequency and most often reordered


# source https://www.kaggle.com/philippsp/exploratory-analysis-instacart/notebook  under Apache License

#reorderProportionPlot <- file.path(getwd(),paste("reorderProportionPlot",".png",sep = ""))
#png(filename = reorderProportionPlot,
#    width = 480, height = 480, units = "px", pointsize = 12,
#    bg = "white",  res = NA,## ...,
#    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
#    antialias = c("default")
#)

#dev.off()

#tmp <- order_prod_prior_dept[which(order_prod_prior_dept$department == "frozen" & order_prod_prior_dept$aisle == "ice cream ice"),] 




#tmp %>% 
#  group_by(reordered) %>% 
#  summarize(count = n()) %>% 
#  mutate(reordered = as.factor(reordered)) %>%
#  mutate(proportion = round((count/sum(count)),2))
#kable(tmp)

#tmp %>% 
#  ggplot(aes(x=reordered,y=count,fill=reordered))+
#  geom_bar(stat="identity",fill = c("blue", "dodgerblue"))



#################

#reorder_table <- file.path(getwd(),paste("reorder_table",".png",sep = ""))
#png(filename = reorder_table,
#    width = 480, height = 480, units = "px", pointsize = 12,
#    bg = "white",  res = NA,## ...,
#    ##type = c("cairo", "cairo-png", "Xlib", "quartz"), 
#    antialias = c("default")
#)


#tmp %>% 
#  group_by(product_id) %>% 
#  summarize(proportion_reordered = mean(reordered), n=n()) %>% 
#  filter(n>40) %>% 
#  top_n(10,wt=proportion_reordered) %>% 
#  arrange(desc(proportion_reordered)) %>% 
#  left_join(products,by="product_id")

#kable(tmp)
#dev.off()


#rm(tmp)

#order_prod_prior_dept[which(order_prod_prior_dept$department == "frozen" & order_prod_prior_dept$aisle == "ice cream ice"),] %>% 
#  ggplot(aes(x=reordered,y=count,fill=reordered))+
#  geom_bar(stat="identity")



#########



reorderedfunc <- function(x){
  tmpa <- as.data.frame(unique(order_prod_prior_dept[which(order_prod_prior_dept$reordered == "1" 
                                                           & order_prod_prior_dept$department == "frozen"
                                                           & order_prod_prior_dept$aisle == "ice cream ice"),c("product_name")]))
  names(tmpa) <- ("products")
  return(tmpa)
}


Notreorderedfunc <- function(x){
  tmpa <- as.data.frame(unique(order_prod_prior_dept[which(order_prod_prior_dept$reordered == "0" 
                                                           & order_prod_prior_dept$department == "frozen"
                                                           & order_prod_prior_dept$aisle == "ice cream ice"),c("product_name")]))
  names(tmpa) <- ("products")
  return(tmpa)
}



reorderedProportion <- function(x){
  order_prod_prior_dept[which(order_prod_prior_dept$department == "frozen"
                              & order_prod_prior_dept$aisle == "ice cream ice"),] %>% 
    group_by(reordered) %>% 
    summarize(count = n()) %>% 
    mutate(reordered = as.factor(reordered)) %>%
    mutate(proportion = count/sum(count))
}




###### most often ordered 

oftenOrderFun <- function(topNProd = 10){
              order_prod_prior_dept[which(order_prod_prior_dept$department == "frozen"
                                          & order_prod_prior_dept$aisle == "ice cream ice"),] %>% 
                group_by(product_id) %>% 
                summarize(proportion_reordered = mean(reordered), n=n()) %>% 
                filter(n>40) %>% 
                top_n(topNProd,wt=proportion_reordered) %>% 
                arrange(desc(proportion_reordered)) %>% 
                left_join(products,by="product_id")
                
}


PlotoftenOrderFun <- function(topNPrdPlot = 10){
  oftenOrderFun(topNProd = topNPrdPlot) %>% 
  ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
  geom_bar(stat="identity",fill="dodgerblue")+
  theme(axis.text.x=element_text(angle=35, hjust= 1)
        ,axis.title.x = element_blank())+coord_cartesian(ylim=c(0.65,0.85))

}



