##
#https://courses.edx.org/courses/course-v1:MITx+15.071x_2a+2T2015/
##
Sys.setlocale(category= "LC_ALL")
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
Top5 = subset(MVT, LocationDescription=="STREET" | 
                LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | 
                LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | 
                LocationDescription=="DRIVEWAY - RESIDENTIAL")
### PROBLEM 4.3 - POPULAR LOCATIONS  
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription,Top5$Arrest)







