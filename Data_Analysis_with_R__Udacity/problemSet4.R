## Problem Set 4 ##

library(ggplot2)
data(diamonds)
summary(diamonds)

# Question1
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method = lm) 

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(shape = 1, alpha = 0.05) +    # add alpha to counter overplotting
  geom_smooth(method = lm) 

# Q2
# Looks non-linear, need to try some variable transforms

# Q3
cor(diamonds$x, diamonds$price)
cor(diamonds$y, diamonds$price)
cor(diamonds$z, diamonds$price)

# Q4
ggplot(diamonds, aes(x = depth, y = price)) +
  geom_point(shape = 1)

# Q5
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 0.01) +
  scale_x_continuous(breaks = seq(50, 70, 2), lim = c(50, 70))

# Q6
# 58,63

# Q7
cor(diamonds$depth, diamonds$price)

# Q8
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point(shape = 1) +
  xlim(0, quantile(diamonds$carat, 0.99)) +
  ylim(0, quantile(diamonds$price, 0.99))

# Q9
ggplot(data = diamonds, aes(x = x*y*z, y = price)) + 
  geom_point(shape = 1) +
  xlim(0, quantile(diamonds$x*diamonds$y*diamonds$z, 0.9999))
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

# Q10
attach(diamonds)  # using attach is not recommended
cor(price[volume!=0 & volume<800], volume[volume!=0 & volume<800])
detach(diamonds)
#or
cor(diamonds$price[diamonds$volume!=0 & diamonds$volume<800], diamonds$volume[diamonds$volume!=0 & diamonds$volume<800])
#or
with(subset(diamonds, volume!=0 & volume<800), cor(price, volume))

# Q11
ggplot(data = subset(diamonds, diamonds$volume<800), aes(x = volume, y = price)) + 
  geom_point(shape = 1, alpha = 0.2) +
  geom_smooth(method = lm) +
  ylim(0, 20000)
# non-linear relationship, needs transformations

# Q12
# detach("package:plyr", unload=TRUE)
# install.packages('dplyr')
library(dplyr)
clarityGroup <- group_by(diamonds, clarity)
diamondsByClarity <- summarise(clarityGroup,
                               mean_price = mean(price),
                               median_price = median(price),
                               min_price = min(price),
                               max_price = max(price),
                               n = n())

# Q13
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))
diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

clarityGraph <- ggplot(data=diamonds_mp_by_clarity, aes(x = clarity, y = mean_price)) +
                        geom_bar(stat ="identity")
colorGraph <- ggplot(data=diamonds_mp_by_color, aes(x = color, y = mean_price)) +
                      geom_bar(stat = "identity")

# install.packages('gridExtra')
library(gridExtra)
grid.arrange(clarityGraph, colorGraph)

# Q14
?diamonds
# color and clarity seem to be inversely mean price which is the opposite of what you'd expect
diamonds_by_cut <- group_by(diamonds, cut)
diamonds_mp_by_cut <- summarise(diamonds_by_cut, mean_price = mean(price))
ggplot(data = diamonds_mp_by_cut, aes(x = cut, y = mean_price)) +
  geom_bar(stat = "identity")
# same with cut

# Q15
# Continuation (of EDA on a gapminder.org dataset of the student's choice) question 15, problem set 3




