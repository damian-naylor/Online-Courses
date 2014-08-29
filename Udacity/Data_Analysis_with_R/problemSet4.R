# Problem Set 4

library(ggplot2)
data(diamonds)
summary(diamonds)


#Q1
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) 

ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(shape=1, alpha=0.05) +    # add alpha to counter overplotting
  geom_smooth(method=lm) 

#Q2
#Looks non-linear, need to try some variable transforms

#Q3
cor(diamonds$x, diamonds$price)
cor(diamonds$y, diamonds$price)
cor(diamonds$z, diamonds$price)

#Q4
ggplot(diamonds, aes(x=depth, y=price)) +
  geom_point(shape=1)

#Q5
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha=0.01)+
  scale_x_continuous(breaks=seq(50,70,2), lim=c(50,70))

#Q6
# 58,63

#Q7
cor(diamonds$depth, diamonds$price)

#Q8
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point(shape=1) +
  xlim(0, quantile(diamonds$carat, 0.99)) +
  ylim(0, quantile(diamonds$price, 0.99))

#Q9
ggplot(data = diamonds, aes(x = x*y*z, y = price)) + 
  geom_point(shape=1) +
  xlim(0, quantile(diamonds$x*diamonds$y*diamonds$z, 0.9999))
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

#Q10
attach(diamonds)
cor(price[volume!=0 & volume<800], volume[volume!=0 & volume<800])
detach(diamonds)
#or
cor(diamonds$price[diamonds$volume!=0 & diamonds$volume<800], diamonds$volume[diamonds$volume!=0 & diamonds$volume<800])
#or
with(subset(diamonds, volume!=0 & volume<800), cor(price, volume))

#Q11
ggplot(data = subset(diamonds, diamonds$volume<800), aes(x = volume, y = price)) + 
  geom_point(shape=1, alpha=0.2) +
  geom_smooth(method=lm) +
  ylim(0,20000)
#non-linear relationship, needs transformations

#Q12
#detach("package:plyr", unload=TRUE)
install.packages(dplyr)
library(dplyr)

