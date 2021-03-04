#install.packages(c("Rcpp","dply"), dependencies = TRUE)
require("dplyr")

load(file="train.RData")  ##this loads a dataframe called train which has daily sales from 45 Wal-Mart Stores

newDF = train %>% group_by(store_nbr, item_nbr) %>% summarize(totalSales = sum(units)) %>% ungroup() %>% arrange(desc(totalSales))

View(newDF)  

###modify the above to answer the question on the exam

