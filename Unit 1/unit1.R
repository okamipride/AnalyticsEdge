### USE UTF-8 Encoding To See Chinese
Sys.setlocale(category="LC_ALL")
###Video4 
WHO <- read.csv("WHO.csv")
str(WHO)
summary(WHO)
WHO_EUROPE <- subset(WHO, Region=="Europe")
str(WHO_EUROPE)
summary(WHO_EUROPE)
write.csv(WHO_EUROPE,"WHO_Europe.csv")
ls()
rm(WHO_EUROPE)
ls()
### Video5
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
minCol <- which.min(WHO$Under15)
WHO$Country[minCol]
maxCol <-which.max(WHO$Under15)
WHO$Country[maxCol]
plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO,GNI>10000 & FertilityRate > 2.5)
nrow(Outliers)
#找出 Outliers , 离群值（mild Outliers）和极限离群值（extreme Outliers）
# http://blog.csdn.net/jia20003/article/details/6382347 有 boxplot 的介紹!
# Outlier:IQR= Q3-Q1 , outlier 就是 Q3+1.5IQR or Q1-1.5IQR
Outliers[c("Country", "GNI","FertilityRate")]
hist(WHO$CellularSubscribers)
###boxplot 中間黑線是median, box 是1st~3rd 分衛，曲線叫 whisters 秀出最大分衛與最小值分衛。小圓圈是 outlier 
boxplot(WHO$LifeExpectancy~WHO$Region)
boxplot(WHO$LifeExpectancy~WHO$Region,xlab="",ylab="LifeExpectancy", main="Life LifeExpectancies of Countires by Region")
table(WHO$Region)
### tapply 把2nd argument 的Data分群，然後使用3rd arg 的 func 做處理
tapply(WHO$Over60,WHO$Region,mean)
### Outcome 很多都是 NA , 這是因為有 NA 值
tapply(WHO$LiteracyRate, WHO$Region, min)
### 把 NA 值去掉
tapply(WHO$LiteracyRate, WHO$Region, min,na.rm=TRUE)

#####################################
## Understanding Food: Nutritional Education with Data (Recitation) 
####################################
#先把上個 section 的 variable 清掉
rm(list=ls()) 
### Prelog 
# 1990 US 14% Obesity, 2000 20% 2010 > 20% ,35% 過胖, USDA 提供了Nutrition 的Database 給使用者

USDA <- read.csv("USDA.csv")
str(USDA)
## ID Unique# of food .... 
summary(USDA)
### 從 Summary 裡發現 Sodium Max 有夠高, 所以來看看這個Item 
USDA$Sodium
maxSod <- which.max(USDA$Sodium)
minSod
names(USDA) #叫出 variable name 
USDA$Description[maxSod]
HighSodium <- subset(USDA, Sodium > 10000)
nrow(HighSodium) # 幾行 
HighSodium$Description
match("CAVIAR", USDA$Description) 
USDA$Sodium[4154]
USDA$Sodium[match("CAVIAR", USDA$Description)] ## 這麼寫也可以! *這樣才CS嘛!
summary(USDA$Sodium)## Min 1st Qu, median mean 3rd Qu max NAs , 沒有 Standard deviation
sd(USDA$Sodium) ## Return NA, 所以要把 NA 拿掉
sd(USDA$Sodium, na.rm=TRUE) # 把 NA 拿掉
plot(USDA$Protien, USDA$TotalFat)
plot(USDA$VitaminC) #Self add , take a look of VitaminC Level
hist(USDA$VitaminC, xlab="Vitamin C (mg)", ylab="Histogram of Vitamin C levels")
## 畫 0-100 mg VitaminC, 因為大部分分布在這 
hist(USDA$VitaminC, xlab="Vitamin C (mg)", ylab="Histogram of Vitamin C levels", xlim=c(0,100))
## 因為上面只出現一個大 Cell, 所以, 要把她 break 成 100 cells
## 可是圖紙有出現五格, 預期 100 cell 這是因為，原來x到 2000 , 2000/100 =20 , 1cell =20mg
## 100 mg 就五格
hist(USDA$VitaminC, xlab="Vitamin C (mg)", ylab="Histogram of Vitamin C levels", xlim=c(0,100), breaks=100)
##但其實我們要看 100 格 , 所以做 2000 breaks
hist(USDA$VitaminC, xlab="Vitamin C (mg)", ylab="Histogram of Vitamin C levels", xlim=c(0,100), breaks=2000)
## Box plot , 很多 Outlier
boxplot(USDA$Sugar, main="Boxplot of sugar levels", ylab="Sugar(g)")

###############################
##  VIDEO 5: ADDING VARIABLES
###############################
## 加欄 if sodium > average => 1 sodium < average =>0 
USDA$Sodium[1] > mean(USDA$Sodium, na.rm=TRUE) # Take a look
USDA$Sodium[50] > mean(USDA$Sodium, na.rm=TRUE) # Take a look
HighSodium <- USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE)
str(HighSodium) #注意輸出 是 logi (TRUE/FALSE), 但我們要的是 0 or 1
## 所以要把output 轉成 numeric
HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
str(HighSodium)
## 加進 DataFrame
USDA$HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
str(HighSodium)
## 檢查看有沒有加進去
names(USDA)
USDA$HighProtien <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat <- as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
USDA$HighCarbs <- as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
names(USDA)
str(USDA)
##################
## Video6 : SUMMARY TABLES
################
table(USDA$HighSodium)
# 兩個 變數, Upper Lable HighSodium, Left label 是 HighFat , 這會給 4 個值
# (0,0)低鹽低油 (0,1) 高鹽低油 .... etc 
table(USDA$HighSodium, USDA$HighFat)
## 接下來要找，Average amount of iron sorted by high low protien
## 要用到 tapply, tapply(arg1, arg2, arg3) arg1依據arg2做出一個Group, 然後丟到 arg3 
tapply(USDA$Iron, USDA$HighProtien,mean, na.rm=TRUE) 
##結果: 因為HighProtien 是 0, 1 所以分兩Group , each group 再去做 mean 所以結果是 2 個值
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)
### 結果顯示出HighCarbs 有 VitaminC , 真的是這樣嗎? 可以用 Summary 看究竟! 
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)






