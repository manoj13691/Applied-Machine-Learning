library(pROC)

#a part
pred_m1 = c(0.73, 0.69, 0.44, 0.55, 0.67,0.47,0.08,0.15,0.45,0.35)
actual = c("+","+","-","-","+","+","-","-","+","-")
myRoc <- roc(predictor = pred_m1, response =actual , positive = '+')
plot(myRoc)

pred_m2 = c(0.61,0.03,0.68,0.31,0.45,0.09,0.38,0.05,0.01,0.04)
actual = c("+","+","-","-","+","+","-","-","+","-")
myRoc <- roc(predictor = pred_m2, response =actual , positive = '+')
plot(myRoc)
#a part ends

#b part
predict = c(1,1,0,1,1,0,0,0,0,0)
true = c(1,1,0,0,1,1,0,0,1,0)
retrieved <- sum(predict)
precision <- sum(predict & true) / retrieved
recall <- sum(predict & true) / sum(true)
Fmeasure <- 2 * precision * recall / (precision + recall)
#b part ends


#c part
predict = c(1,0,1,0,0,0,0,0,0,0)
true = c(1,1,0,0,1,1,0,0,1,0)
retrieved <- sum(predict)
precision <- sum(predict & true) / retrieved
recall <- sum(predict & true) / sum(true)
Fmeasure <- 2 * precision * recall / (precision + recall)
#c part ends


#d part
predict = c(1,1,1,1,1,1,0,1,1,1)
true = c(1,1,0,0,1,1,0,0,1,0)
retrieved <- sum(predict)
precision <- sum(predict & true) / retrieved
recall <- sum(predict & true) / sum(true)
Fmeasure <- 2 * precision * recall / (precision + recall)
myRoc <- roc(predictor = predict, response =true , positive = 1)
plot(myRoc)
#d part ends