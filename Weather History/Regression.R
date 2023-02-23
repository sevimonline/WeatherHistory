#### 1.SORU 

set.seed(1000)
data <- read.csv("C:\\Users\\User\\Desktop\\weatherHistory.csv") # veri setini a?t?k

df <- sample(96453, 1000,replace=F) 



data <- data[df,]

data_new2 <- data[,-c(1,2,5,10,12)] 

rownames(data_new2) <- 1:1000 

data_new2$Precip.Type=as.numeric(data_new2$Precip.Type)
as.numeric(data_new2$Precip.Type)
summary(data_new2$Precip.Type)



class(data_new2$Precip.Type)




#### 2.SORU



hist(data_new2$Precip.Type)
hist(data_new2$Temperature..C.)
hist(data_new2$Humidity)
hist(data_new2$Wind.Speed..km.h.)
hist(data_new2$Wind.Bearing..degrees.)
hist(data_new2$Visibility..km.)
hist(data_new2$Pressure..millibars.)


##

summary(data_new2) 

##

plot(data_new2$Precip.Type)
plot(data_new2$Temperature..C.)
plot(data_new2$Wind.Speed..km.h.)
plot(data_new2$Humidity)
plot(data_new2$Wind.Bearing..degrees.)
plot(data_new2$Visibility..km.)
plot(data_new2$Pressure..millibars.)


##

colnames(data_new2) <- c("Temperature..C.","Summary","Precip.Type","Humidity",
                         "Wind.Speed..km.h.","Wind.Bearing..degrees.","Visibility..km.","Pressure..millibars."
                         ,"Daily.Summary")
pairs(data_new2[,1:7], pch = 19, col='red', lower.panel = NULL)

cor(data_new2)


#KORELASYON MATRIX


cor(data_new2)

install.packages("Hmisc")
library("Hmisc")
rcorr(as.matrix(data_new2))

##korelasyon matrix plot
install.packages("corrplot")
library(corrplot)
corrplot(cor(data_new2))


##



#### 3.SORU



model=lm(data_new2$Temperature..C.~., data=data_new2)
summary(model)

# x1 = Precip.type (Ya??? t?r?)
# x2 = Humidity  (Nem oran?)
# x3 = Wind.Speed..km.h.  (R?zgar h?z?)
# x4 = Wind.Bearing..degrees. (R?zgar yata?? derecesi)
# x5 = Visibility..km. (G?r?n?rl?k KM)
# x6 = Pressure..millibars. (bas?n? milibar?)




qf(0.95,6,993)




confint(model)


#### 4.SORU


library(olsrr)


k=ols_step_all_possible(model)
plot(k)
ols_step_all_possible(model)[42,]
ols_step_best_subset(model)


#1.Modelde Cp(581.853)>p(2) 

#2.Modelde Cp(95.6184)>p(3) 

#3.Modelde Cp(37.2583)>p(4) 

#4.Modelde Cp(5.4465)>p(5) 

#5.Modelde Cp(5.0001)<p(6) 


ols_step_both_p(model)  
ols_step_backward_p(model) 
summary(model)


par(mfrow=c(2,2))
plot(model)

data_new3 <- data_new2

data_new3 <- data_new2[-c(746,659,949),]  

alternative.model=lm(data_new3$Temperature..C.~., data=data_new3)
summary(alternative.model)

#

par(mfrow=c(2,2))
plot(alternative.model2)

data_new3 <- data_new2

data_new3 <- data_new2[-c(949,211,161),] #outlier

alternative.model2=lm(data_new3$Temperature..C.~., data=data_new3)
summary(alternative.model2)



#### 6. SORU


hist(model$residuals) 
shapiro.test(model$residuals)


par(mfrow=c(1,2))


hist(alternative.model2$residuals)  
shapiro.test(alternative.model2$residuals)


install.packages("lmtest") 
library(lmtest)


bptest(model)



bptest(alternative.model2)

##

par(mfrow=c(2,2))
plot(model)



which(hatvalues(model)>2*mean(hatvalues(model))) 
which(hatvalues(model)>14/1000) 

which(hatvalues(model)>2*mean(hatvalues(model))) == which(hatvalues(model)>14/1000) 




which(cooks.distance(model)>4/model$df.residual) 

leverage = 2*mean(hatvalues(model))
leverage

cooksdistance=4/model$df.residual
cooksdistance

which(hatvalues(model)>leverage)

which(cooks.distance(model)>cooksdistance)

hatvalues(model)[c(4 ,8, 74, 75, 101, 124, 138 ,143 ,153,
                   164 ,180, 207, 221 ,240 ,245 ,259 ,266 ,272 ,297 ,308 ,332 ,370 ,376,
                   390 ,396 ,425, 440 ,453 ,484 ,486 ,502 ,515 ,572 ,577 ,588 ,622 ,646 ,657, 
                   659 ,706 ,722, 724 ,737 ,746 ,793 ,856 ,871 ,899 ,918 ,923 ,930 ,941 ,949, 986 )]



cooks.distance(model)[c(4 ,  8  ,11 , 75  ,84, 125, 138, 154, 207 ,211 ,245 ,249, 
                        253 ,259 ,266, 275 ,375 ,376, 384, 440 ,484, 551,574, 588 ,657, 659 ,722 ,728, 746 ,
                        775 ,795, 830, 832, 842 ,900 ,949 ,967 )]



####



###### 7.SORU

install.packages("Metrics")
install.packages("ISLR")
install.packages("caret")
install.packages("dplyr")
install.pckages("car")
library("ISLR")
library("caret")
library("dplyr")
library("car")
library(Metrics)

library(car)
vif(model)


colnames(data_new2) <- c("Temperature..C.","Summary","Precip.Type","Humidity",
                         "Wind.Speed..km.h.","Wind.Bearing..degrees.","Visibility..km.","Pressure..millibars."
                         ,"Daily.Summary")
pairs(data_new2[,1:7], pch = 19, col='red', lower.panel = NULL)



smp_size <- floor(0.75 * nrow(data_new2)) 


train_ind <- sample(nrow(data_new2), size = smp_size, replace = FALSE)
train <- data_new2[train_ind, ]
test <- data_new2[-train_ind, ]


dim(train)
dim(test)


model=lm(data_new2$Temperature..C.~., data=data_new2)
summary(model)




alternative.model=lm(data_new3$Temperature..C.~., data=data_new3)
summary(alternative.model)




alternative.model2=lm(data_new3$Temperature..C.~., data=data_new3)
summary(alternative.model2)





predictions1 <- predict(model,test)
predictions2 <- predict(alternative.model,test)
predictions3 <- predict(alternative.model2,test)

RMSE1 <-  rmse(predictions1, test$Temperature..C.)
RMSE2 <- rmse(predictions2, test$Temperature..C.)
RMSE3 <- rmse(predictions3, test$Temperature..C.)

cbind(RMSE1,RMSE2,RMSE3)


install.packages("ModelMetrics") 
library(ModelMetrics)

mae1 <- mae(predictions1, test$Temperature..C.)
mae2 <- mae(predictions2, test$Temperature..C.)
mae3 <- mae(predictions3, test$Temperature..C.)

cbind(mae1,mae2,mae3)

MSE1 <- mse(predictions1, test$Temperature..C.)
MSE2 <- mse(predictions2, test$Temperature..C.)
MSE3 <- mse(predictions3, test$Temperature..C.)


cbind(MSE1,MSE2,MSE3)







standardized.residuals<-model$residuals/sqrt(var(model$residuals))

which(standardized.residuals<(-2))
which(standardized.residuals>(2))




data_new3 <- data_new2

data_new3 <- data_new2[-c(which(standardized.residuals<(-2)),which(standardized.residuals>(2))),] 

final.model=lm(data_new3$Temperature..C.~., data=data_new3)
summary(final.model)


which(cooks.distance(model)>4/model$df.residual) 





data_new3 <- data_new2

data_new3 <- data_new2[-c(which(cooks.distance(model)>4/model$df.residual),which(hatvalues(model)>2*mean(hatvalues(model)))  ),] 

final.modeli2=lm(data_new3$Temperature..C.~., data=data_new3)
summary(final.modeli2)







data_new3 <- data_new2

data_new3 <- data_new2[-c(which(cooks.distance(model)>4/model$df.residual),which(hatvalues(model)>2*mean(hatvalues(model))),which(standardized.residuals<(-2)),which(standardized.residuals>(2))  ),] 

asil.final.modeli=lm(data_new3$Temperature..C.~., data=data_new3)
summary(asil.final.modeli)



                                                          
