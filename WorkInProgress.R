quantile(a$no_products)

##### use this for r files sourcing all files
names(products)
names(aisles)
names(departments)
names(ordersPrior)
names(order_products__prior)
names(prodAislDept)
names(order_prod_prior_dept)
names(basketsize_pct_trend)
names(ordersPriorProduct)
source(file.path(getwd(),paste("datasetPrep",".R",sep = "")))
source(file.path(getwd(),paste("orders_users_analysis",".R",sep = "")))



#addnew


install.packages("treemap",dependencies = TRUE)
install.packages("bsselectR", dependencies = TRUE)

## 2 How many times has the product_id been reordered  ######
library(lattice)
xyplot("no_orders",a)

ab <- a$
  plot(x = a$product_id, y= log10(a$no_orders), type = "h")
nrow(a)
quantile(a$no_orders, type = 9)

head(a[order(-a$no_orders),],10)
summary(a$no_dist_users)


### clean up unused dataset ###
cleanUp <- c("aisles","departments", "products", "orders","order_products__prior ", "order_products__train")
rm(list = cleanUp)




######  departments and aisles
# proudcts per department






#text(basketsize_trend$order_number, basketsize_trend$avg_basketsize
     #,labels =  round(basketsize_trend$avg_basketsize,0)
   #  , cex=0.60, font = 2, pos= 1, col="black")

###
############################################ Path Most Taken thru Aisle dept  ######################
##### check the usercounts with orders counts 
users_order_prod_prior_dept <- as.data.frame(merge(x = ordersPrior, y = order_prod_prior_dept , by = "order_id"))
#users_order_prod_prior_dept <- users_order_prod_prior_dept[order("user_id","order_id","add_to_cart_order"),]

deptAisle_userCt <- plyr::ddply(users_order_prod_prior_dept,.(aisle,department),summarize
                                ,no_dist_users=(length(unique(user_id))))



# check sample :    head(deptAisle_userCt[order(-deptAisle_userCt$no_dist_users),],10)
# see product_aisles_departments_analysis.R for this variable deptAisle_OrderCt
#deptAisle_OrderCt
#UsersOrdersCt_DeptAisl <- merge(x = deptAisle_OrderCt, y = deptAisle_userCt, by = c("department","aisle"))

#ordersPriorProduct
#prodAislDept
UserorderProdAisleDep <- as.data.frame(merge(ordersPriorProduct, prodAislDept, by = "product_id"))
UserorderProdAisleDep <- UserorderProdAisleDep[order(UserorderProdAisleDep$user_id
                                                     ,UserorderProdAisleDep$order_id
                                                     ,UserorderProdAisleDep$add_to_cart_order),]

UserorderProdAisleDep <- UserorderProdAisleDep[c("department_id","department","aisle_id","aisle","add_to_cart_order","user_id")]
UserorderProdAisleDep <- plyr::ddply(UserorderProdAisleDep,c("department","aisle","add_to_cart_order"),summarize
                                     ,user_id=((unique(user_id))))

library(dplyr)
dplyr::count(UserorderProdAisleDep, c("department","aisle","add_to_cart_order")) %>% ungroup()


rm(order_prod_prior_dept,order_products__prior,deptAisle_userCt)
######## END #####

###UserorderProdAisleDep <- UserorderProdAisleDep[order(user_id,add_to_cart_order),]

   ### cleanup unsued data
#rm(aisles,departments,products,order_products__prior,order_products__train,prodAislDept,order_prod_prior_dept)


UserorderProdAisleDep <- plyr::ddply(UserorderProdAisleDep,c("add_to_cart_order"),summarize
                                     ,no_dist_users=(length(unique(user_id))))

UserorderProdAisleDep <- plyr::ddply(UserorderProdAisleDep,.(department,aisle,add_to_cart_order),summarize
                                     ,no_dist_users=(length(unique(user_id))))

write.csv(UserorderProdAisleDep,"C:/Users/Michael/Desktop/depAislePrdUser.csv")

mclust::mclustModel(UserorderProdAisleDep)


abc <- split(UserorderProdAisleDep
             , list(UserorderProdAisleDep$department
             , UserorderProdAisleDep$aisle
             ,UserorderProdAisleDep$add_to_cart_order)
      ,drop = TRUE)


pathtaken1 <- 
  
  
hc = hclust(dist(UserorderProdAisleDep))
# very simple dendrogram
plot(hc)


deptAisle_userCt <- aggregate(users_order_prod_prior_dept$user_id
                               , list(users_order_prod_prior_dept$department,order_prod_prior_dept$aisle), length)
names(deptAisle_userCt) <- c("department","aisle","userCount")






#write.csv(NoOrder_DeptAisle, file = (file.path(getwd(),paste("orders_users_analysis"))))

#library(psych)
psych::describe(NoOrder_DeptAisle$orderCount)
quantile(NoOrder_DeptAisle$orderCount)
deptAisle_OrderCt[which(deptAisle_OrderCt$aisle == "candles"),]


# Determine number of clusters
wss <- (nrow(avg_time_dow_users())-1)*sum(apply(avg_time_dow_users(),2,var))
for (i in 2:15) wss[i] <- sum(kmeans(avg_time_dow_users(),
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis

xc <- avg_time_dow_users()[1]
fit <- kmeans(avg_time_dow_users(), 5) # 5 cluster solution
# get cluster means
aggregate(avg_time_dow_users(),by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(avg_time_dow_users(), fit$cluster) 


down vote


As far as Shiny is concerned, you could start with this. Does that help you?

library(shiny)

aggdata <- data.frame(
  "Experiment" = c("One","Two","Three"),
  "AnythingElse" = c(1,2,3)
)

ui <- shinyUI(
  fluidPage(
    selectInput("Experiment1","Choose the first experiment",
                choices = unique(aggdata$Experiment),
                selected = unique(aggdata$Experiment)[1]),
    tableOutput("table1")
  )
)

server <- shinyServer(function(input, output, session) {
  reactiveData <- reactive({
    return(as.data.frame(subset(aggdata, Experiment == input$Experiment1)))  
  })
  output$table1 <- renderTable({
    return( reactiveData() )
  })
})

shinyApp(ui = ui, server = server)


########## managing r markdown pages ############

## title {.tabset .tabset-fade}
content above tabbed region.

### tab 1 

tab content 1

### tab 2

tab content 2

##

content below tabbed region


###
hello world
\newpage
```{r, echo=FALSE}
1+1
```
\pagebreak
```{r, echo=FALSE}
plot(1:10)
```
                                

reorderDataFunc <- function(x){
  reordera <- order_prod_prior_dept[which(order_prod_prior_dept$reordered == "1"),]
  reorderb <- plyr::ddply(reordera,.(product_name),summarize
                          ,no_reorders=(length(unique(order_id))))
  reorderb <- reorderb[order(reorderb$no_reorders, decreasing = TRUE),]
  reorderTotal <- nrow(reorderb)
  reorderPct <- reorderTotal/(sum(reorderTotal,notReorderTotal))
  
}

reorderDataFunc <- function(x){
  noreordera <- order_prod_prior_dept[which(order_prod_prior_dept$reordered == "0"),]
  noreorderb <- plyr::ddply(noreordera,.(product_name),summarize
                            ,no_noreorders=(length(unique(order_id))))
  noreorderb <- noreorderb[order(noreorderb$no_noreorders, decreasing = TRUE),]
  notReorderTotal <- nrow(noreorderb)
  notReorderPct <- notReorderTotal/(sum(reorderTotal,notReorderTotal))
}