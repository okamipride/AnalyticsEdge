## Linear Regression 
## SSE = sum of square of error 
## RSSE = Root- Mean - square Error , Root(SSE/N) (根號)
## SST = sum of square of base line model (每個點跟　mean 線的平方)
## R square = 1- SSE/SST
## R = 0 <- no improvement over baseline
## R = 1 <- Fit Perfect
## Multiple Regression Model
## What is the best model to use, the more may have overfitting problem

#### Start R example
WINE <- read.csv("wine.csv")
str(WINE)
summary(WINE)
## Price vs Average Growing Season Tempture linear regression
model1 <- lm(formula = Price ~ AGST, data=WINE)

##Call:
##  lm(formula = Price ~ AGST, data = WINE)
##
##Coefficients:
##  (Intercept)     AGST  
##  -3.4178       0.6351 

summary(model1)

##Residuals:
##  Min       1Q   Median       3Q      Max 
##-0.78450 -0.23882 -0.03727  0.38992  0.90318 
##
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)  -3.4178     2.4935  -1.371 0.183710    
##AGST          0.6351     0.1509   4.208 0.000335 ***
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
str(model1)
model1$residuals
SSE1 <- sum(model1$residuals^2)
SSE1

## Add another independent variable
model2 <- lm(Price~AGST+HarvestRain, data=WINE)
summary(model2)
SSE2 <- sum(model2$residuals^2)
SSE2
## Add all indenpent vairable
model3 <- lm (Price~AGST+HarvestRain+WinterRain+Age+FrancePop,data=WINE)
summary(model3)
SSE3<-sum(model3$residuals^2)
SSE3
#####
## Estimate :Coefficient of model , if approach zero ,
##            no effect on model, should eliminate
## Std Error : Coefficient 跟 estimate value 差多多
## t value : estimate / std error, 如果 t value 越大 , 很有可能 
## independent variable 的影響越顯著
## Pr : coefficient = 0 的機率多大
## Significant 的表示法
## *** (0.001) ** (0.01) * (0.05) . (005 -0.1) (almost significant)
##
####
###
### Model4 去除 FrancePop 這個 independent variable , 因為數據顯示
## 對 model 的作用不大
model4 <- lm(Price~AGST+HarvestRain+WinterRain+Age, data=WINE)
summary(model4)

##奇怪的事發生了  WinterRain & Age 之前不明顯　, 去除 FrancePop 就變明顯
## 
## Correlation : A measure of the linear relationship between 2 variables
## +1 : Perfect positive linear relationship
## 0 : no linear relationship
## -1 : perfect negtive linear relationship
## highly correlated = 絕對值接近1

## 以下兩個都沒啥相關性
plot(WINE$WinterRain, WINE$Price)
plot(WINE$HarvestRain, WINE$AGST)
## 有強烈相關性
plot(WINE$FrancePop, WINE$Age)

## Correlation 用算的 
cor(WINE$WinterRain, WINE$Price)
cor(WINE$FrancePop, WINE$Age)
## 一次看所有變數的 cor 
cor(WINE)
## Multicollinearity:refer to the situation when 2 independent variables are 
## highly correlated 
## 中文 Multicollinearity 就是在指兩個獨立變數高度相關
##

### 來看看把 FancePop 跟 Age 去除後 對  linear model 有無影響 

model5 <- lm(Price~AGST+ WinterRain + HarvestRain, data=WINE)
summary(model5)
summary(model4)
### R2 在 model 5 減少了  表示拿掉 FrancePop or Age 會影響 model 
### 只拿掉一個　就是 FrancePop, 為何都留Age 呢? 因為 Age 在直覺裡　
### 跟酒的品質比較相關


