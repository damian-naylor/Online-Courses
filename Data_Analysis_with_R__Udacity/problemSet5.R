# Problem Set 5

#Question 1
ggplot(data=diamonds, aes(x=price)) +
  geom_histogram(aes(fill=cut)) +
  facet_wrap(~color) +
  scale_x_log10() +
  scale_fill_brewer(type="qual", guide=guide_legend(reverse=TRUE))

#Q2
ggplot(data=subset(diamonds, table>=50 & table<=80), aes(x=table, y=price)) +
  geom_point(shape=16, aes(color=cut), position="jitter")+
  scale_color_brewer(type="qual", guide=guide_legend(reverse=TRUE)) +
  scale_x_continuous(breaks=seq(50,80,2))

#Q3
# 53, 57; 58, 62

#Q4
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(data=subset(diamonds, volume>0 & volume<quantile(volume, 0.99)), aes(x=volume, y=price)) +
  geom_point(shape=16, aes(color=clarity)) +
  scale_y_log10() +
  scale_color_brewer(type='div', guide=guide_legend(reverse=TRUE))

#Q5
pf <- read.delim('pseudo_facebook.tsv')
pf$prop_initiated <- pf$friendships_initiated / pf$friend_count
pf$prop_initiated[is.nan(pf$prop_initiated)] <- 1

#Q6
pf$year_joined <- floor(2014 - pf$tenure/365)
pf$year_joined_bucket <- cut(pf$year_joined, c(2004,2009,2011,2012,2014))

ggplot(data=pf, aes(x=tenure, y=prop_initiated)) +
  geom_line(aes(color=pf$year_joined_bucket), stat="summary", fun.y=median))

#Q7
ggplot(data=pf, aes(x=tenure, y=prop_initiated)) +
  geom_line(aes(color=pf$year_joined_bucket), stat="summary", fun.y=median) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), size = 1)
  #fit log ###

#Q8
with(subset(pf, pf$year_joined_bucket=="(2011,2012]"), mean(pf$friendships_initiated))

mean(pf[pf$year_joined_bucket=="(2012,2014]"]$friendships_initiated)

pf[1,]$year_joined_bucket == "(2012,2014]"


#Q9