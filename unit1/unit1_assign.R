##
#https://courses.edx.org/courses/course-v1:MITx+15.071x_2a+2T2015/
##
Sys.setlocale(category= "LC_ALL")
#####
## Assignment 1 : mvt
#####

MVT <- read.csv("mvtWeek1.csv")
#PR1.1, PR1.2
str(MVT)
nrow(MVT)

#PR1.3
MVT$ID[which.max(MVT$ID)]
MVT$Beat[which.min(MVT$Beat)]
#PR1.4 but 解答是用 Summary
Arrest <- subset(MVT, MVT$Arrest == TRUE)
nrow(Arrest)
summary(MVT)
#1.5
Alley <- subset(MVT, MVT$LocationDescription == "ALLEY")
str(Alley)

############ 2.1 Date 
##Date format : 12/27/12 20:00
## 2.2 Conver to Date format
#DateConvert <- as.Date(strptime(MVT$Date,"%m/%d/%y  %H:%M"))
DateConvert = as.Date(strptime(MVT$Date, "%m/%d/%y %H:%M"))
DateConvert[1]
summary(DateConvert)
months(DateConvert)
MVT$Month <- months(DateConvert)
MVT$Weekday <- weekdays(DateConvert)
MVT$Date <- DateConvert
### 2.3 ... 
table(MVT$Month)
### 2.4 
table(MVT$Weekday)
###2.5
table(MVT$Arrest,MVT$Month)
###  3.1 - VISUALIZING CRIME TRENDS
hist(MVT$Date, breaks=100)
### PROBLEM 3.2 - VISUALIZING CRIME TRENDS
boxplot(MVT$Date~MVT$Arrest)
## PROBLEM 3.3 ~3.5 , 自己把TRUE or FALSE 的值帶進去乘除 @@好奇怪
table(MVT$Arrest,MVT$Year)
## 於是 我覺得也可這樣
Year2001 <- subset(MVT, MVT$Year==2012)
NumOfArr <- nrow(Year2001Arrest <- subset(Year2001, Year2001$Arrest == TRUE))
NumOfNotArr <- nrow(Year2001Arrest <- subset(Year2001, Year2001$Arrest == FALSE))
### Ans:
NumOfArr/ (NumOfArr + NumOfNotArr)

### PROBLEM 4.1 - POPULAR LOCATIONS
##One of the locations has a much higher arrest rate than the other locations. Which is it
Top5 = subset(MVT, LocationDescription=="STREET" | 
                LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | 
                LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | 
                LocationDescription=="DRIVEWAY - RESIDENTIAL")
### PROBLEM 4.3 - POPULAR LOCATIONS  
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription,Top5$Arrest)
## HsinYi 's 解法! 
apply(table(Top5$LocationDescription,Top5$Arrest),1,function(x) x[1]/(x[1]+x[2]))

### PROBLEM 4.4 - POPULAR LOCATIONS  
##On which day of the week do the most motor vehicle thefts 
## at gas stations happen?
## Ed的解法 
table(Top5$LocationDescription, Top5$Weekday)
## My solution
which.max(table(MVT$Weekday,MVT$LocationDescription)[,"GAS STATION"])

###PROBLEM 4.5
##On which day of the week do the fewest motor vehicle thefts 
##in residential driveways happen?
## its solution
table(Top5$LocationDescription, Top5$Weekday)
## My solution
which.min(table(MVT$Weekday,MVT$LocationDescription)[,"DRIVEWAY - RESIDENTIAL"])


#####
## Assignment 2 :STOCK DYNAMICS
#####
##Clear Data
rm(list=ls()) 

### Read Data "IBM", "GE", "ProcterGamble", "CocaCola", and "Boeing"
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")
##Convert Date 1/1/70 
str(as.Date(Boeing$Date,"%m/%d/%y"))
##as.Date return an Date obj 
##把以下的Date format 改成 Date Obj 的 format
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

## PROBLEM 1.1 - SUMMARY STATISTICS  
##  How many observations are there in each data set?
str(IBM)

## What is the earliest year in our datasets?
## My solution
sort(IBM$Date)
## Edx solution
summary(IBM)
## PROBLEM 1.5 ~ 1.8
summary(GE)
summary(CocaCola)
summary(Boeing)
sd(ProcterGamble$StockPrice)

#####
## PROBLEM 2.1 - VISUALIZING STOCK DYNAMICS
####

plot(CocaCola$Date, CocaCola$StockPrice)
## type ="l", l(L) -> Line 
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
## 在 CocaCola 的 plot 裡加上 ProcterGamble 的 line  lty = 2 , dash line
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue", lty=2)
abline(v=as.Date(c("2000-03-01")), lwd=2)

## the stock prices changed from 1995-2005 for all five companies
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

## my solution 
CocaCola$Year <- format(CocaCola$Date, "%Y")
Coca1995To2005 <- subset( subset(CocaCola, CocaCola$Year >= 1995),  subset(CocaCola, CocaCola$Year >= 1995)$Year <=2005)
plot(Coca1995To2005$Date, Coca1995To2005$StockPrice, type="l", col="red", ylim=c(0,210))


### drawing 1997 年 9 - 11 月
CocaCola$Month <- months(CocaCola$Date)
a <- subset(CocaCola, subset(CocaCola$Year ==1997,  CocaCola$Month==""))
Coca97 <- subset( subset(CocaCola, CocaCola$Year ==1997),  subset(CocaCola, CocaCola$Year==1997)$Month > 9)
