##############
### set-up ###
##############

library(car)
library(corrplot)
library(arm)
library(leaps)

car_price_data<-read.csv("CarPrice.csv", header=TRUE)
numeric_vars <- car_price_data[ , sapply(car_price_data, is.numeric)]

#############################
### digging into the data ###
#############################

plot(car_price_data$carwidth, car_price_data$price) 

hist(log(car_price_data$price),main="Histogram of log prices", xlab="log transformed price") # normal histogram is right-skewed, log transformation might be useful 
qqnorm(log(car_price_data$price))
qqline(log(car_price_data$price))
boxplot(car_price_data$price)
plot(car_price_data$symboling, car_price_data$price)

# boxplot of categorical covariates
boxplot(car_price_data$price~car_price_data$car_ID) # should be redundant
boxplot(car_price_data$price~car_price_data$CarName)
boxplot(car_price_data$price~car_price_data$fueltype)
boxplot(car_price_data$price~car_price_data$aspiration)
boxplot(car_price_data$price~car_price_data$doornumber)
boxplot(car_price_data$price~car_price_data$carbody)
boxplot(car_price_data$price~car_price_data$drivewheel)
boxplot(car_price_data$price~car_price_data$enginelocation)
boxplot(car_price_data$price~car_price_data$enginetype)
boxplot(car_price_data$price~car_price_data$cylindernumber) # maybe

# continuous covariates
plot(car_price_data$wheelbase, car_price_data$price)
plot(car_price_data$carlength, car_price_data$price)
plot(car_price_data$carwidth, log(car_price_data$price))

pairs(numeric_vars)

plot(car_price_data$horsepower, log(car_price_data$price))


# partial F-test
test1<-lm(log(car_price_data$price)~car_price_data$horsepower)
test2<-lm(log(car_price_data$price)~car_price_data$horsepower+car_price_data$symboling)
summary(test1)

anova(test1,test2)

# check correlation between carwidth and carlength and perhaps enginesize/horsepower
round(cor(numeric_vars),2)
# curbweight, carlength, carwidth seem highly correlated
# enginesize and curbweight as well
# citympg and highwaympg highly correlated (one redundant)

# Variance influence factor
corrplot(cor(numeric_vars))

vif(lm(log(price)~curbweight+horsepower+carwidth,data=car_price_data))
vif(lm(log(price)~curbweight+horsepower,data=car_price_data))
vif(lm(log(price)~curbweight+horsepower+citympg,data=car_price_data))

smaller_model<-lm(log(car_price_data$price)~car_price_data$curbweight+car_price_data$horsepower+car_price_data$citympg)
vif(smaller_model)

larger_model<-lm(log(car_price_data$price)~car_price_data$curbweight+car_price_data$horsepower+car_price_data$citympg+car_price_data$wheelbase+car_price_data$carlength+car_price_data$carwidth+car_price_data$enginesize)
vif(larger_model)

anova(smaller_model, larger_model)
summary(smaller_model)
summary(larger_model)

# diagnostics
par(mfrow=c(2,2))
plot(smaller_model)

pred <- predict(smaller_model)

par(mfrow=c(1,1))

plot(log(car_price_data$price), pred,
     xlab = "Actual Price",
     ylab = "Predicted Price",
     main = "Actual vs Predicted Prices",
     pch = 19, col = "blue")

abline(0, 1, col = "red", lwd = 2)  # ideal fit line

plot(smaller_model$fitted.values, smaller_model$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

##################################################################
### try adding carbody (continuous and categorical covariates) ###
##################################################################

model_w_categorical<-lm(log(price)~curbweight+horsepower+citympg+carbody, data=car_price_data)

test_model1<-lm(log(price)~curbweight+horsepower+citympg+cylindernumber, data=car_price_data) # 0.867
summary(test_model1)

test_model2<-lm(log(price)~curbweight+horsepower+citympg+carbody, data=car_price_data) # 0.877 BEST
summary(test_model2) 

test_model3<-lm(log(price)~curbweight+horsepower+citympg+fueltype, data=car_price_data) # 0.86
summary(test_model3)

test_model4<-lm(log(price)~curbweight+horsepower+citympg+aspiration, data=car_price_data) # 0.852
summary(test_model4)

test_model5<-lm(log(price)~curbweight+horsepower+citympg+drivewheel, data=car_price_data) # 0.858
summary(test_model5)

test_model6<-lm(log(price)~curbweight+horsepower+citympg+enginetype, data=car_price_data) # 0.866
summary(test_model6)

test_model7<-lm(log(price)~curbweight+horsepower+citympg+fueltype, data=car_price_data) # 0.86
summary(test_model7)

par(mfrow=c(2,2))
plot(model_w_categorical)

par(mar = c(3, 6, 4, 3))  # bottom, left, top, right
coefplot(model_w_categorical)
car_price_data$cylindernumber <- factor(car_price_data$cylindernumber)
car_price_data$cylindernumber <- relevel(car_price_data$cylindernumber, ref = "four")

vif(model_w_categorical)

#########################################
### Backward search with "full model" ###
#########################################

step(model_w_categorical, direction = "backward", test="F")

##############
## Outliers ##
##############

car_price_data$carbody<-relevel(factor(car_price_data$carbody), ref="hatchback") # baseline
new_model_w_categorical<-lm(log(price)~curbweight+horsepower+carbody, data=car_price_data)

par(mfrow = c(2, 2))
plot(new_model_w_categorical)
par(mfrow = c(1, 1))

# cooks
cook<-cooks.distance(new_model_w_categorical)

cook_cutoff<-4/nrow(car_price_data)
outl<-which(cook > cook_cutoff)
outl
plot(cook, type = "h", main = "Cook's distance", ylab = "Cook's dist")
abline(h = cook_cutoff, col = "red")

#############################
### Test-train data split ###
#############################
regsubsets_model_w_categorical<-lm(log(price)~curbweight+horsepower+citympg+carbody+carwidth+carlength+enginesize+boreratio, data=car_price_data, x=TRUE)

X<-regsubsets_model_w_categorical$x
X<-X[, -1]

# training test split
set.seed(123)
frac<-0.7
trainind<-sample(seq_len(nrow(X)), round(frac*nrow(X)))
xx<-as.matrix(X[trainind, ])
yy<-log(car_price_data$price[trainind])
xxt<-as.matrix(X[-trainind, ])
yyt<-log(car_price_data$price[-trainind])

cat_cols <- grep("^carbody", colnames(xx))


m_sub<-regsubsets(xx, yy, force.in=cat_cols,int=T, nbest=1, nvmax=ncol(xx), method="ex", really.big=TRUE)
cleaps<-summary(m_sub, matrix=TRUE)

# reorder columns of xx/xxt to match cleaps$which
col.order<-colnames(cleaps$which)
xx<-xx[, col.order[-1], drop=FALSE]
xxt<-xxt[, col.order[-1], drop=FALSE]

# compute pMSE for each model
pMSE<-numeric(nrow(cleaps$which))
for (k in seq_len(nrow(cleaps$which))) {
  selected<-cleaps$which[k, -1]
  xx_model<-xx[, selected, drop = FALSE]
  mmr<-lm(yy ~ xx_model)
  
  X_test <- xxt[, selected, drop = FALSE]
  X_test <- cbind(1, X_test)
  
  beta <- coef(mmr)
  
  colnames(X_test)[1]
  
  y_hat<-as.vector(X_test%*%beta)
  pMSE[k]<-mean((yyt-y_hat)^2)
}

# plot
real_dim<-rowSums(cleaps$which[, -1])-length(cat_cols)+1
par(mar = c(5, 6, 4, 3))
plot(real_dim, sqrt(pMSE), xlab="Number of covariates", ylab="sqrt(pMSE)", pch=19, col="blue",type = "b")

par(mar = c(3, 6, 4, 3))
#############################################
### diagnostics of model with categorical ###
#############################################
par(mfrow=c(2,2))
plot(model_w_categorical)

pred <- predict(model_w_categorical)

par(mfrow=c(1,1))

plot(log(car_price_data$price), pred,
     xlab = "Actual Price",
     ylab = "Predicted Price",
     main = "Actual vs Predicted Prices",
     pch = 19, col = "blue")

abline(0, 1, col = "red", lwd = 2)  # ideal fit line

plot(model_w_categorical$fitted.values, model_w_categorical$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red") 


#############
### LOOCV ###
#############
set.seed(123)

n <- nrow(car_price_data)
loocv_errors <- numeric(n)

for (i in 1:n) {
  train_data<-car_price_data[-i, ]
  test_data<-car_price_data[i, ]
  
  fit<-lm(log(price)~curbweight+horsepower+carbody, data=train_data)
  
  pred<-predict(fit, newdata=test_data)
  loocv_errors[i]<-log(test_data$price)-pred
}

# LOOCV rMSE
loocv_rmse<-sqrt(mean(loocv_errors^2))
loocv_rmse
summary(loocv_errors)
sd(loocv_errors)


####################
### interactions ###
####################

# intuitively a interaction might be relevant between curbweight*horsepower as horsepower might increase 
# price more for heavier cars
# another interaction which might be logical is horsepower*carbody as horsepower might be valued differently
# in certain carbodies.

mod1<-lm(log(price)~curbweight*horsepower+carbody, data=car_price_data)
summary(mod1)
anova(new_model_w_categorical, mod1) # significant with p=0.04

mod2<-lm(log(price)~curbweight+horsepower*carbody, data=car_price_data)
summary(mod2)
anova(new_model_w_categorical, mod2) # significant with p=0.002

mod3<-lm(log(price)~horsepower+curbweight*carbody, data=car_price_data)
summary(mod3)
anova(new_model_w_categorical, mod3) # not significant with p=0.2

mod4<-lm(log(price)~horsepower*curbweight*carbody, data=car_price_data)
summary(mod4)
anova(new_model_w_categorical, mod4) # significant with p=0.004

# now LOOCV on the new interaction models (1 & 2)
set.seed(123)

n <- nrow(car_price_data)
loocv_errors_reg <- numeric(n)
loocv_errors_mod1 <- numeric(n)
loocv_errors_mod2 <- numeric(n)

for (i in 1:n) {
  train_data<-car_price_data[-i, ]
  test_data<-car_price_data[i, ]
  
  fit_reg<-lm(log(price)~curbweight+horsepower+carbody, data=train_data)
  fit_mod1<-lm(log(price)~curbweight*horsepower+carbody, data=train_data)
  fit_mod2<-lm(log(price)~curbweight+horsepower*carbody, data=train_data)
  
  pred_reg<-predict(fit_reg, newdata=test_data)
  pred_mod1<-predict(fit_mod1, newdata=test_data)
  pred_mod2<-predict(fit_mod2, newdata=test_data)
  loocv_errors_reg[i]<-log(test_data$price)-pred_reg
  loocv_errors_mod1[i]<-log(test_data$price)-pred_mod1
  loocv_errors_mod2[i]<-log(test_data$price)-pred_mod2
}
# reg
loocv_rmse_reg<-sqrt(mean(loocv_errors_reg^2))
loocv_rmse_reg
summary(loocv_errors_reg)
sd(loocv_errors_reg)
# mod1
loocv_rmse_mod1<-sqrt(mean(loocv_errors_mod1^2))
loocv_rmse_mod1
summary(loocv_errors_mod1)
sd(loocv_errors_mod1)
# mod2
loocv_rmse_mod2<-sqrt(mean(loocv_errors_mod2^2))
loocv_rmse_mod2
summary(loocv_errors_mod2)
sd(loocv_errors_mod2)


############################
### confidence intervals ###
############################

confint(new_model_w_categorical)

# centering of covariates
car_price_data$curbweight_c<-scale(car_price_data$curbweight, scale=FALSE)
car_price_data$horsepower_c<-scale(car_price_data$horsepower, scale=FALSE)

model_centered<-lm(log(price)~curbweight_c+horsepower_c+carbody, data=car_price_data)
confint(model_centered)


#################################
### Fitted values / residuals ###
#################################

pred_log <- predict(new_model_w_categorical, newdata = car_price_data)
resid_log <- residuals(new_model_w_categorical)

par(mar = c(5, 6, 4, 3))
hist(resid_log, main="Residuals of log(price)", xlab="Residuals")

plot(log(car_price_data$price), pred_log, xlab="Actual log(price)", ylab="Predicted price", col="blue")
abline(0,1, col="red")

plot(log(car_price_data$price)-pred_log, ylab="Residual", col="blue")
abline(h=0, col="red")

par(mar = c(3, 6, 4, 3))


##############################
### GLM data (second part) ###
##############################

library(betareg)
df <- read.table("controllers.dat")
colnames(df)<-c("experience","age","errors","size")

error_rate <- df$errors/df$size

df <- data.frame(df, error_rate)

fit <- glm(
  errors ~ experience * age,
  family = poisson(link = "log"),
  weights = size,
  data = df
)
plot(fit$fitted.values~df$errors)
abline(0,1)
summary(fit)

fit1 <- glm(
  error_rate ~ experience * age,
  family = binomial(link = "logit"),
  weights = size,
  data = df
)

summary(fit1)
df$experience <- factor(df$experience)
df$experience <- relevel(df$experience, ref = "5")
fit_no_int <- glm(
  error_rate ~ experience + factor(age),
  family = binomial(link = "logit"),
  weights = size,
  data = df
)

summary(fit_no_int)
anova(fit_no_int, fit1, test = "Chisq")

deviance(fit_no_int)/df.residual(fit_no_int)

plot(df$error_rate, fit_no_int$fitted.values, xlab="Error Rate (data)" , ylab ="Estimated Error Rate", xlim=c(0,0.1) , ylim=c(0,0.1))
abline(0,1)

new_data <- data.frame(experience = levels(1,1,2,2,3,3,4,4,5,5,6,6), age = levels(1,2,1,2,1,2,1,2,1,2,1,2))
predict(fit_no_int, new_data, type = "response")

fit_no_int_2 <- glm(
  error_rate ~ experience+factor(age),
  family = binomial(link = "logit"),
  weights = size,
  data = df
)

summary(fit_no_int_2)
plot(df$error_rate, fit_no_int_2$fitted.values, xlab="Error Rate (data)" , ylab ="Estimated Error Rate", xlim=c(0,0.08) , ylim=c(0,0.08))
abline(0,1)
deviance(fit_no_int_2)/df.residual(fit_no_int_2)

predict(fit_no_int_2, new_data, type = "response")

plot(fit_no_int_2$fitted.values-df$error_rate, xlab="Index" , ylab ="Predicted - Data, Error Rates", ylim=c(-0.04,0.04))
abline(h=0)

drop1(fit_no_int_2, test="F")
# backword_l1mod <- step(fit_no_int_2, direction = "backward")

fit_no_int_3 <- glm(
  error_rate ~ experience,
  family = quasibinomial(link = "logit"),
  weights = size,
  data = df
)

summary(fit_no_int_3)
plot(df$error_rate, fit_no_int_3$fitted.values, xlab="Error Rate (data)" , ylab ="Estimated Error Rate", xlim=c(0,0.08) , ylim=c(0,0.08))
abline(0,1)
deviance(fit_no_int_3)/df.residual(fit_no_int_3)



