setwd("/Users/dylan/PycharmProjects/FSAN-830/quiz2_ggplot")


pdf("Quiz1DayuYang.pdf") ## route output to file instead of Plots pane



#### pre liminary

library(ggplot2)
library(dplyr)
set.seed(123)
ddf = diamonds %>% dplyr::sample_frac(0.1) 
# creates a blank plot
ggplot(ddf, aes(x = carat, y = price)) + 
  geom_point()

# 
ggplot(ddf, aes(x = carat, y = price)) + 
  geom_point()+
  geom_smooth()

ggplot(ddf, aes(x = carat, y = price, color = clarity) ) + 
  geom_point()+
  geom_smooth()

ggplot(ddf, aes(x = carat, y = price, color = clarity) ) + 
  geom_point(alpha=0.1)





###
###Define the data and aesthetics layers:
###map carat on the x and price on the y axis. ###Assign it to an object: diaPlot.
ggplot(ddf, aes(x = carat, y = price,color =cut))+
  geom_point()+
  ggtitle("Ideal Diamonds Become less Frequent as Diamonds Get Bigger")


ggplot(ddf, aes(x = carat, y = price))+
  geom_smooth(se=FALSE)

ggplot(ddf, aes(x = carat, y = price))+
  geom_smooth(aes(color=cut), se=FALSE)



ggplot(ddf,aes(carat,price)) + geom_point(color="darkorange")



ggplot(ddf,aes(x=clarity))+geom_bar(aes(fill=cut))

ggplot(ddf,aes(x=clarity))+geom_bar(aes(fill=cut), position = "dodge")


dev.off() ## if you are having issues, run this line repeatedly


