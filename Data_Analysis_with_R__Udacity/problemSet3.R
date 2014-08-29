# Problem Set 3

#the diamonds datset is part of ggplot2
library(ggplot2)
data(diamonds)
summary(diamonds)

#Question 1
dim(diamonds)[1]
dim(diamonds)[2]
#from summary(diamonds)
?diamonds

#Q2
qplot(x=diamonds$price, data=diamonds, binwidth=100)
summary(diamonds$price)

#Q3
qplot(x=diamonds$price, data=diamonds, binwidth=100)

#Q4
summary(diamonds$price)
#Price itself has an exponential shaped distribution, obviously long tailed, median 2401, mean 3933, min 326, max 18820

#Q5
#a.
dim(subset(diamonds, diamonds$price < 500))[1]
dim(diamonds[diamonds$price < 500,])[1]
#b...
dim(subset(diamonds, diamonds$price < 250))[1]
dim(subset(diamonds, diamonds$price >= 15000))[1]

#Q6
qplot(x=diamonds$price, data=diamonds, binwidth=10)+
  scale_x_continuous(lim=c(0,1500), breaks=seq(0,1500, 100))
#ggsave('testHistogram.png')

#Q7
qplot(x=diamonds$price, data=diamonds, 
      xlab = "Price ($)",
      ylab = "Number of samples")+
  facet_wrap(~cut)
  
#Q8
#a.
diamonds$cut[diamonds$price == max(diamonds$price)]
#or
subset(diamonds$cut, diamonds$price == max(diamonds$price))
#b.
diamonds$cut[diamonds$price == min(diamonds$price)]
#c.
library("plyr")
ddply(diamonds, "cut", summarise, median=median(price))
#or
ddply(diamonds, "cut", function(x){median(x$price)})

#Q9
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales="free_y")

#Q10
#scale in transformed units 
qplot(x = log10(price), data = diamonds) + facet_wrap(~cut, scales="free_y")
#x axis scale in original units although the plot is showing transformed units
ggplot(aes(x=price), data=diamonds) + geom_histogram() + scale_x_log10() + facet_wrap(~cut, scales="free_y")

#geom_freqpoly(aes(color = ##))

#Q11
ddply(diamonds, "cut", function(x){summary(x$price)})
qplot(x = cut, y= log10(price), data = diamonds, geom = 'boxplot')

#Q12
ddply(diamonds, "color", function(x){summary(x$price)})
?diamonds

#Q13
qplot(x = color, y= price/carat, data = diamonds, geom = 'boxplot')

#Q14
qplot(x=carat, data=diamonds)
qplot(x=carat, data=diamonds, binwidth=0.1)
table(diamonds$carat)
#automated answer
possibleAnswers <- c(0.1, 0.3, 0.8, 1.01, 1.6, 2.0, 3.0, 5.0)
for (x in possibleAnswers) {
  if (length(diamonds$carat[diamonds$carat == x]) > 2000) {print(x)}
}

#================================================================================================
#Q15

#open analysis question on dataset of student's choice from www.gapminder.org
#downloaded 'age at first marriage' dataset (2nd in list) and converted it to CSV in OpenOffice Calc
# to save time and avoid installing further packages

marraiges <- read.csv("indicator age of marriage.csv")
names(marraiges)[1] <- "Country"   #
#remove leading "X" from all column names except the first("Country")
names(marraiges)[-1] <- substr(names(marraiges)[-1], 2, nchar(names(marraiges)[-1]))

#boxplot of first married ages by country function
MyBoxPlot <- function(years='all', saveFileName="marriageCountries.png") {
  
#   # years = all / how many of the recent years to include (116 max)  
#   
#   #convert col num (years) to python notation, ie 0 is first year and minus notaion works, ie -1 is last year
#   if (is.double(years)){
#     if (years < 0){years <- -years}
#     if (years >= 116) {years <- -1; print('Capping years to 116')
#     } else {years <- }
#   } else if (years == 'all') {years <- -1}  
#   print(years)
  
  
  if (years == 'all'){
    years <--1
  } else {break}
  
  ###cannot get boxplot to accept a vector per label rather than a long (label, value) list ### - could look at different boxplot functions
  flatList <- list()
  for (y in 1:dim(marraiges)[1]) {
    ages <- marraiges[y,years][!is.na(marraiges[y,years])]
    if (length(ages) > 0) {
      for (x in 1:length(ages)) {
        flatList <- append(flatList, list( c(as.character(marraiges[y,1]),ages[x]) ))
      }
    }
  }
  twoEntryList <- data.frame(t(array( unlist(flatList), dim=c(2,length(flatList)) ))) 
  twoEntryList[,1] <- factor(twoEntryList[,1])
  twoEntryList[,2] <- as.double(as.character(twoEntryList[,2]))
  names(twoEntryList)[1]<-"country"
  names(twoEntryList)[2]<-"avAge"
  qplot(x = country, y= avAge, data = twoEntryList, geom = 'boxplot') +
    theme(axis.text.x = element_text(angle = -90, hjust = 0, size=2.5))
  ggsave(saveFileName)
}

MyBoxPlot()
# MyBoxPlot(years = -1, saveFileName = 'allYears.png')
# MyBoxPlot(years = all, saveFileName = 'allYears.png')
## only plot boxplot if more than z observations for country
############NEED SOME MORE GRAPHS HERE #############
#================================================================================================


#Q16
bday <- read.csv('birthdaysExample.csv')
#sort
bday$dates <- bday[order(as.Date(bday$dates, format = "%m/%d/%y")),]

bday$month <- 'unknown'
for (x in 1:dim(bday)[1]){ bday$month[x] <- months(strptime(bday$dates[x],"%m/%d/%y")) }

#day of year
#bday$day <- 'unknown'
#for (x in 1:dim(bday)[1]){ bday$day[x] <- days(strptime(bday$dates[x],"%m/%d/%y")) }

#a. March
table(bday$month)
qplot(x=month, data=bday)
qplot(x=dates, data=bday)

bday$temp <- 0
for (x in 2:dim(bday)[1]) { bday$temp[x] <- bday$dates[x]-bday$dates[x-1]}
#values in temp should not exceed 1 if part d. of the Q is correct
qplot(x=temp, data=bday)