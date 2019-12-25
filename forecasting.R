library(readxl) # for importing data
library(forecast)
library(lmtest) # for dw test

hke <- read_excel("R/forecast/hke.xlsx", skip=5)
names(hke)[2] <- "hk_export" # Rs array starts at 1

n = round(length(hke$Date)*0.8)
hke$Date <- rev(hke$Date)
hke$seq <- seq(from=1, to = length(hke$Date))
hke$hk_export <- rev(hke$hk_export)

train = head(hke, n)
test = tail(hke, -n)

#hke_ts <-ts(hke$hk_export, start = c(1999,12), end = c(2019, 10), frequency = 12)

# Linear Model with Trend Line
linear_model <- lm(hk_export ~ seq, data=train)
plot(train$seq, train$hk_export, type="l", main="Hong Kong Export", xlab="TIME", ylab="Export", xlim=c(0,235), ylim=c(90,385))

new <- data.frame(seq = train$seq)
linear_past = predict(linear_model, newdata = new, interval = "predict")
lines(train$seq, data.frame(linear_past)$fit, type="l", col="blue")
abline(v=192,col="red", lwd=3, lty=2)

# Prediction and Error
new <- data.frame(seq = test$seq)
linear_predict = predict(linear_model, newdata = new, interval = "predict")
lines(test$seq, tail(hke$hk_export,-n), type="l", col="black")
lines(test$seq, data.frame(linear_predict)$fit, type="l", col="red")
lines(test$seq, data.frame(predict(linear_model, newdata = new, interval = "confidence"))$lwr, type='l', col="orange")
lines(test$seq, data.frame(predict(linear_model, newdata = new, interval = "confidence"))$upr, type='l', col="orange")

train_error <- mean((data.frame(linear_past)$fit - train$hk_export)^2)
test_error <- mean((data.frame(linear_predict)$fit - test$hk_export)^2)

dw <- dwtest(linear_model)
aic <- AIC(linear_model)
bic <- BIC(linear_model)


# compute r squared for testing data
r_squared <- function(vals, preds){
  1-(sum((vals-preds)^2) / sum((vals-mean(preds))^2))
}

adj_r_squared <- function(vals, preds, k){
  1-((1-r_squared(vals, preds))*(length(preds)-1))/(length(preds)-k-1)
}

adj_r_squared(test$hk_export, data.frame(linear_predict)$fit, 1)

# residuals

plot(train$seq, linear_model$residuals, type='l', main="Trend Model Residuals", 
     xlab="TIME", ylab="Residuals", xlim=c(0,235), ylim=c(-140,60))
lines(test$seq, test$hk_export -data.frame(linear_predict)$fit )
abline(v=192,col="red", lwd=3, lty=2)


# Seaonality

# To create dummy variable matrix
hke_ts <- ts(hke$hk_export, start = c(1999,12), end=c(2019,10), frequency=12)
seasonal_dummy <- data.frame(seasonaldummy(hke_ts))

train_Jan <- head(seasonal_dummy$Jan, n)
train_Feb <- head(seasonal_dummy$Feb, n)
train_Mar <- head(seasonal_dummy$Mar, n)
train_Apr <- head(seasonal_dummy$Apr, n)
train_May <- head(seasonal_dummy$May, n)
train_Jun <- head(seasonal_dummy$Jun, n)
train_Jul <- head(seasonal_dummy$Jul, n)
train_Aug <- head(seasonal_dummy$Aug, n)
train_Sep <- head(seasonal_dummy$Sep, n)
train_Oct <- head(seasonal_dummy$Oct, n)
train_Nov <- head(seasonal_dummy$Nov, n)

season_train <- cbind(train, train_Jan,train_Feb,train_Mar, train_Apr,train_May,train_Jun,train_Jul,train_Aug
               ,train_Sep,train_Oct,train_Nov)

test_Jan <- tail(seasonal_dummy$Jan, -n)
test_Feb <- tail(seasonal_dummy$Feb, -n)
test_Mar <- tail(seasonal_dummy$Mar, -n)
test_Apr <- tail(seasonal_dummy$Apr, -n)
test_May <- tail(seasonal_dummy$May, -n)
test_Jun <- tail(seasonal_dummy$Jun, -n)
test_Jul <- tail(seasonal_dummy$Jul, -n)
test_Aug <- tail(seasonal_dummy$Aug, -n)
test_Sep <- tail(seasonal_dummy$Sep, -n)
test_Oct <- tail(seasonal_dummy$Oct, -n)
test_Nov <- tail(seasonal_dummy$Nov, -n)

season_test <- cbind(test, test_Jan,test_Feb,test_Mar,test_Apr,test_May,test_Jun,test_Jul,test_Aug
                      ,test_Sep,test_Oct,test_Nov)

trend_season_model <- lm(hk_export ~ seq + train_Jan + train_Feb +train_Mar 
                         + train_Apr + train_May +train_Jun + train_Jul + train_Aug + train_Sep 
                         + train_Oct + train_Nov, data = season_train)

plot(train$seq, train$hk_export, type="l", main="Trend Season", xlab="TIME", ylab="Export", 
     xlim=c(0,235), ylim=c(70,390))
lines(season_train$seq, 
      data.frame(predict(trend_season_model, newdata = season_train, interval = "predict"))$fit,
      type='l', col="blue")

#training error
new <- data.frame(seq=train$seq, train_Jan, train_Feb, train_Mar, 
                  train_Apr, train_May,train_Jun, train_Jul, 
                  train_Aug, train_Sep, train_Oct,train_Nov)
trend_season_past = predict(trend_season_model, newdata=new, interval="predict")


# The nameing is confusing for reader, please refer to https://stackoverflow.com/questions/27464893/getting-warning-newdata-had-1-row-but-variables-found-have-32-rows-on-pred
new <- data.frame(seq = test$seq, train_Jan=test_Jan, train_Feb=test_Feb, train_Mar=test_Mar, 
                  train_Apr=test_Apr, train_May=test_May,train_Jun=test_Jun, train_Jul=test_Jul, 
                  train_Aug=test_Aug, train_Sep=test_Sep, train_Oct = test_Oct,train_Nov = test_Nov )

trend_season_predict = predict(trend_season_model, newdata = new, interval = "predict")

lines(test$seq, tail(hke$hk_export,-n), type="l")
lines(test$seq, data.frame(trend_season_predict)$fit, type="l", col="red")
#lines(test$seq, data.frame(predict(trend_season_model, newdata = new, interval = "confidence"))$lwr, type="l", col="orange")
#lines(test$seq, data.frame(predict(trend_season_model, newdata = new, interval = "confidence"))$upr, type="l", col="orange")
abline(v=192,col="red", lwd=3, lty=2)

trend_season_residuals <- trend_season_model$residuals

season_train_error <- mean((data.frame(trend_season_past)$fit - train$hk_export)^2)

season_test_error <- mean((data.frame(trend_season_predict)$fit - test$hk_export)^2)

AIC(trend_season_model)
BIC(trend_season_model)

plot(train$seq, trend_season_residuals, type='l', main = "Trend Season Residuals", 
     xlab='TIME', ylab='Redisuals', ylim=c(-100,35), xlim=c(0,235))
lines(test$seq, data.frame( test$hk_export - trend_season_predict)$fit)
abline(v=192,col="red", lwd=3, lty=2)

acf(trend_season_residuals,lag.max = 229)
pacf(trend_season_residuals, lag.max = 229)

# Cycle 
max_p = 6
max_q = 6

result_list <- data.frame(matrix(ncol = 3, nrow = 0))
cat("p   q   BIC")
for (p in 0:max_p)
{
  for (q in 0:max_q)
  {
     result <- arima(trend_season_residuals, order=c(p,0,q))
     bic <- BIC(result)
     cat(p, q, bic)
     cat("\n")
     result_list <- rbind(result_list, c(p,q,bic))
  }
}
x <- c("p", "q", "BIC")
colnames(result_list) <- x

result_list[which.min(result_list[,3]),]


# Workaround
arma_model <- Arima(trend_season_residuals, order=c(1,0,1))
arma_residuals <- arma_model$residuals

full_past <- data.frame(trend_season_past)$fit + trend_season_residuals - arma_residuals
plot(train$seq, train$hk_export, type="l", main="Trend Season Cycle", xlab="TIME", ylab="Export", 
     xlim=c(0,235), ylim=c(70,390))
lines(train$seq, full_past, col='blue')

arma_predict <- data.frame(predict(arma_model, newdata = trend_season_residuals, interval = "predict", n.ahead = 48))$pred

full_predict <- data.frame(trend_season_predict)$fit + arma_predict
lines(test$seq, test$hk_export)
lines(test$seq, full_predict, col='red')
abline(v=192,col="red", lwd=3, lty=2)

full_error <- mean((train$hk_export - full_past)^2)
adj_r_squared(train$hk_export, full_past, 14)

full_predict_error <- mean((test$hk_export - full_predict)^2)
adj_r_squared(test$hk_export, full_predict, 14)

for (i in 10:30){
  temp <- Box.test( (test$hk_export - full_predict), type="Ljung-Box", lag=i, fitdf=1+1) # p+q
  cat(temp$statistic, temp$parameter, temp$p.value)
  cat("\n")
}

# forecasting
hke_1 <- read_excel("R/forecast/hke_1.xlsx", skip=5)
names(hke_1)[2] <- "hk_export" # Rs array starts at 1

hke_1$Date <- rev(hke_1$Date)
hke_1$seq <- seq(from=1, to = length(hke_1$Date))
hke_1$hk_export <- rev(hke_1$hk_export)
hke_1_ts <- ts(hke_1$hk_export, start = c(1999,12), end=c(2020,12), frequency=12)
seasonal_dummy_1 <- data.frame(seasonaldummy(hke_1_ts))

train = head(hke_1, 239)
test = tail(hke_1, -239)

n=239
train_Jan <- head(seasonal_dummy_1$Jan, n)
train_Feb <- head(seasonal_dummy_1$Feb, n)
train_Mar <- head(seasonal_dummy_1$Mar, n)
train_Apr <- head(seasonal_dummy_1$Apr, n)
train_May <- head(seasonal_dummy_1$May, n)
train_Jun <- head(seasonal_dummy_1$Jun, n)
train_Jul <- head(seasonal_dummy_1$Jul, n)
train_Aug <- head(seasonal_dummy_1$Aug, n)
train_Sep <- head(seasonal_dummy_1$Sep, n)
train_Oct <- head(seasonal_dummy_1$Oct, n)
train_Nov <- head(seasonal_dummy_1$Nov, n)

full_train <- cbind(train, train_Jan,train_Feb,train_Mar, train_Apr,train_May,train_Jun,train_Jul,train_Aug
                      ,train_Sep,train_Oct,train_Nov)

test_Jan <- tail(seasonal_dummy_1$Jan, -n)
test_Feb <- tail(seasonal_dummy_1$Feb, -n)
test_Mar <- tail(seasonal_dummy_1$Mar, -n)
test_Apr <- tail(seasonal_dummy_1$Apr, -n)
test_May <- tail(seasonal_dummy_1$May, -n)
test_Jun <- tail(seasonal_dummy_1$Jun, -n)
test_Jul <- tail(seasonal_dummy_1$Jul, -n)
test_Aug <- tail(seasonal_dummy_1$Aug, -n)
test_Sep <- tail(seasonal_dummy_1$Sep, -n)
test_Oct <- tail(seasonal_dummy_1$Oct, -n)
test_Nov <- tail(seasonal_dummy_1$Nov, -n)

full_test <- cbind(test, train_Jan=test_Jan,train_Feb=test_Feb,train_Mar=test_Mar,train_Apr=test_Apr,
                   train_May=test_May,train_Jun=test_Jun,train_Jul=test_Jul,train_Aug=test_Aug,
                  train_Sep=test_Sep,train_Oct=test_Oct,train_Nov=test_Nov)

full_model <- lm(hk_export ~ seq + train_Jan + train_Feb +train_Mar 
                         + train_Apr + train_May +train_Jun + train_Jul + train_Aug + train_Sep 
                         + train_Oct + train_Nov, data = full_train)

full_model_residual <- full_model$residuals
full_arma_model <- Arima(full_model_residual, order=c(1,0,1))

full_arma_model_residuals <- full_arma_model$residuals


full_fit <- full_model$fitted.values + full_model_residual - full_arma_model_residuals


plot(train$seq, train$hk_export, type="l", main="Predicting 2020", xlab="TIME", ylab="Export", 
     xlim=c(0,250), ylim=c(70,410))
lines(full_train$seq, full_fit, type='l', col="blue")
abline(v=239,col="red", lwd=3, lty=2)

full_predict <- predict(full_model, full_test) + predict(full_arma_model, n.ahead = 14)$pred

arma_predict_interval_up <- predict(full_arma_model, n.ahead = 14)$pred + 1.96*predict(full_arma_model, n.ahead = 14)$se
arma_predict_interval_down <- predict(full_arma_model, n.ahead = 14)$pred - 1.96*predict(full_arma_model, n.ahead = 14)$se

full_predict_up <- data.frame(predict(full_model, full_test, interval = "confidence"))$upr + as.numeric(arma_predict_interval_up)
full_predict_down <- data.frame(predict(full_model, full_test, interval = "confidence"))$lwr + as.numeric(arma_predict_interval_down)

lines(test$seq, full_predict, col='red')
lines(test$seq, full_predict_up, col='orange')
lines(test$seq, full_predict_down, col='orange')

table <- data.frame(cbind(test$seq, full_predict_up, as.numeric(full_predict), full_predict_down))

names(table)[1] <- "TIME"
names(table)[2] <- "Predicted Export Interval Up"
names(table)[3] <- "Predicted Export"
names(table)[4] <- "Predicted Export Interval Down"


#test code
# full_pass <- data.frame(predict(arma_model, newdata = trend_season_residuals, interval = "predict"))$fit
# 
# new <- data.frame(seq = test$seq, train_Jan=test_Jan, train_Feb=test_Feb, train_Mar=test_Mar, 
#                   train_Apr=test_Apr, train_May=test_May,train_Jun=test_Jun, train_Jul=test_Jul, 
#                   train_Aug=test_Aug, train_Sep=test_Sep, train_Oct = test_Oct,train_Nov = test_Nov )
# arma_residual <- arima(data.frame(trend_season_predict)$fit - test$hk_export, order=c(1,0,1))$residuals
# full_predict <- data.frame(predict(trend_season_model, newdata = new, interval = "predict"))$fit - arma_residual
# 
# 
# full_train_error <- mean((train$hk_export - full_pass)^2)
# full_test_error <- mean((test$hk_export - full_predict)^2)
# 
# 
# plot(train$seq, train$hk_export, type="l", main="Hong Kong Export", xlim=c(0,240), ylim=c(0,400))
# lines(test$seq, full_predict, type='l', col='red')
# lines(test$seq, test$hk_export, type = "l", col="blue")
