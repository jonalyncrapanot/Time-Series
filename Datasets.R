library(dplyr)
library(ggplot2)
library(olsrr)
library(EnvStats)

#Dataset 1
attach(NYC)

##Cleaning
str(NYC)
# Replace . with 0's
NYC <- replace(NYC[,], NYC[,] == '.', '0')
#Remove NA
nona<-na.omit(NYC)
#Convert to numeric values
numeric <- lapply(nona[,c(1,5,6,7)], as.numeric)
#Combine old data with numeric data
data <- cbind(nona, numeric)
#Remove old columns that are not numeric
data <- subset(data, select = -c(1,5,6,7))
#Remove the 0's in the data frame since it is unnecessary
data <- data[data$`Death Rate` !=0,]

#Summary
summary(data$`Death Rate`)
summary(data$`Age Adjusted Death Rate`)
IQR(data$`Death Rate`)
IQR(data$`Age Adjusted Death Rate`)
sd(data$`Age Adjusted Death Rate`)
boxplot(data$`Death Rate`, data$`Age Adjusted Death Rate`)

#Empirical Rule
mean <- mean(data$`Death Rate`)
sd<-sd(data$`Death Rate`)
low <- mean - (3*sd) 
high <- mean + (3*sd) 
low_99_7_percent <- data$`Death Rate`[data$`Death Rate` > low & data$`Death Rate` < high]
low_99_7_percent


#Based on Death Rate
dr.year.sum <- data %>% group_by(Year) %>% summarize(Total = sum(`Death Rate`))
dr.year <- data %>% group_by(Year) %>% summarize(avg = mean(`Death Rate`), 
                                                  median(`Death Rate`), sd(`Death Rate`), 
                                                 IQR(`Death Rate`))
dr.sex <- data %>% group_by(Sex) %>% summarize(avg = mean(`Death Rate`), 
                                               median(`Death Rate`), sd(`Death Rate`), 
                                               IQR(`Death Rate`))
dr.cause <- data %>% group_by(`Leading Cause`) %>% summarize(avg = mean(`Death Rate`), 
                                                             median(`Death Rate`), sd(`Death Rate`), 
                                                             IQR(`Death Rate`))
View(dr.year.sum)
View(dr.year)
View(dr.sex)
View(dr.cause)

boxplot(dr.year.sum$Total)


dr.ys <- data %>% group_by(Year, Sex) %>% summarize(avg = mean(`Death Rate`))
View(dr.ys)

dr.yc <- data %>% group_by(Year, `Leading Cause`) %>% summarize(avg = mean(`Death Rate`))
View(dr.yc)

dr.ysc <- data %>% group_by(Year, Sex, `Leading Cause`) %>% summarize(avg = mean(`Death Rate`))
View(dr.ysc)

#Plots
p1 <- ggplot(dr.year, aes(x=Year, y = avg)) + geom_point() + geom_line() + 
  ggtitle("Time Series of Average Death Rate")+
  theme(plot.title = element_text(hjust = 0.5)) + labs(y= "Average")
p1

plot.drys <- ggplot(dr.ys, aes(x = Year, y = avg, col = Sex)) + geom_point() + 
              geom_line() + ggtitle("Time Series of Average Death Rate Group by Sex")+
              theme(plot.title = element_text(hjust = 0.5)) + labs(y= "Average")
plot.drys


plot.dryc <- ggplot(dr.yc, aes(x = Year, y = avg, col = `Leading Cause`)) + geom_point() + 
                   geom_line() + ggtitle("Time Series of Average Death Rate by Leading Cause")+
                   facet_wrap(~`Leading Cause`) + theme(legend.position="none")+
                   theme(plot.title = element_text(hjust = 0.5)) + labs(y= "Average")
plot.dryc

#Based on Adjusted Deaths Rate

adr.year <- data %>% group_by(Year) %>% summarize(avg = mean(`Age Adjusted Death Rate`))
adr.sex <- data %>% group_by(Sex) %>% summarize(avg = mean(`Age Adjusted Death Rate`))
adr.cause <- data %>% group_by(`Leading Cause`) %>% summarize(avg = mean(`Age Adjusted Death Rate`))
View(adr.year)
View(adr.sex)
View(adr.cause)

adr.ys <- data %>% group_by(Year, Sex) %>% summarize(avg = mean(`Age Adjusted Death Rate`))
View(adr.ys)

adr.yc <- data %>% group_by(Year, `Leading Cause`) %>% summarize(avg = mean(`Age Adjusted Death Rate`))
View(adr.yc)

adr.ysc <- data %>% group_by(Year, Sex, `Leading Cause`) %>% summarize(avg = mean(`Age Adjusted Death Rate`))
View(adr.ysc)

#Plot
plot.adrys <- ggplot(adr.ys, aes(x = Year, y = avg, col = Sex)) + geom_point() + 
  geom_line() + ggtitle("Time Series of Average Age Adjusted Death Rate")+
  theme(plot.title = element_text(hjust = 0.5)) + labs(y= "Average")

plot.drys


plot.adryc_sep <- ggplot(adr.yc, aes(x = Year, y = avg, col = `Leading Cause`)) + geom_point() + 
  geom_line() + ggtitle("Time Series of Average Age Adjusted Death Rate by Leading Cause")+
  facet_wrap(~`Leading Cause`) + theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5)) + labs(y= "Average")
plot.dryc_sep

#Dataset 2
attach(HIV)

# Remove unnecessary columns and replace
data2 <- subset(HIV, select = -c(4,5,8,9))
Year<- as.numeric(data2$Year)
data2<-cbind(Year, data2)
data2 <- subset(data2, select = -3)
summary(data2$`Value Numeric`)
sd(data2$`Value Numeric`)
IQR(data2$`Value Numeric`)
View(data2)

#Group by
year <- data2 %>% group_by(Year) %>% summarize(avg = mean(`Value Numeric`), 
                                               median(`Value Numeric`), sd(`Value Numeric`), 
                                               IQR(`Value Numeric`))
sex <- data2 %>% group_by(Sex) %>% summarize(avg = mean(`Value Numeric`))
income <- data2 %>% group_by(`World bank income group`) %>% summarize(avg = mean(`Value Numeric`))
country <- data2 %>% group_by(Country) %>% summarize(avg = mean(`Value Numeric`), 
                                                     median(`Value Numeric`), sd(`Value Numeric`), 
                                                     IQR(`Value Numeric`))
View(year)
View(sex)
View(income)
View(country)

ys <- data2 %>% group_by(Year, Sex) %>% summarize (avg = mean(`Value Numeric`))
View(ys)

yi <- data2 %>% group_by(Year, `World bank income group`) %>% summarize(avg = mean(`Value Numeric`))
View(yi)

yis <- data2 %>% group_by(Year, `World bank income group`, Sex) %>% summarize(avg = mean(`Value Numeric`))
View(yis)
yc <- data2 %>% group_by(Year, Country) %>% summarize(avg = mean(`Value Numeric`))
View(yc)
yic <- data2 %>% group_by(Year, Country, `World bank income group`) %>% summarize(avg = mean(`Value Numeric`))
View(yic)

#Plots
p2 <- ggplot(year, aes(x = Year, y=avg)) + geom_point() + geom_line() + 
  ggtitle("Time Series of Average HIV Infection by Year")+
  theme(plot.title = element_text(hjust = 0.5)) + labs(y= "Average")
p2

plot.ys <- ggplot(ys, aes(x = Year,y = avg, col = Sex)) + geom_point() + geom_line() +
                    ggtitle("Time Series of Average HIV Infection Grouped by Gender") +
                    theme(plot.title = element_text(hjust = 0.5)) + labs(y= "Average")
plot.ys

plot.yi <- ggplot(yi, aes(x = Year,y = avg, col = `World bank income group`)) + geom_point() + geom_line() +
  ggtitle("Time Series of Average HIV Infection Grouped by Income Group") +
  theme(plot.title = element_text(hjust = 0.5)) + labs(y= "Average")
plot.yi

plot.yc <- ggplot(yc, aes(x = Year, y = avg, col = Country)) + geom_point() + geom_line()+
  ggtitle("Time Series of Average HIV Infection Grouped by Country") + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(y= "Average") + facet_wrap(~Country)
plot.yc  
