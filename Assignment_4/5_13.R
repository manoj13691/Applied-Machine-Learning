#Logistic Regression starts here
library(MASS)
attach(Boston)
library(class)
crime =  rep(0, length(crim))
crime[crim > median(crim)] = 1
Boston =  data.frame(Boston, crime)

#First half is train
train =  1:(dim(Boston)[1] / 2)
test =  (dim(Boston)[1] / 2 + 1):length(crim)

train_data = Boston[train, ]
test_data  =  Boston[test, ]
actual_test_labels  =  crime[test]
actual_train_labels = crime[test]
#fit.glm =  glm(crime ~ . - crime - crim, data = Boston, family = binomial, subset = train)
fit.glm =  glm(crime ~ . - crime - crim, data = train_data, family = binomial)
#Predict on test data
probs =  predict(fit.glm, test_data, type = "response")
pred.glm  =  rep(0, length(probs))
pred.glm[probs > 0.5] =  1
table(pred.glm, actual_test_labels)

mean(pred.glm != actual_test_labels)



fit.glm =  glm(crime ~ . - crime - crim - zn - indus - chas - nox, data = train_data, family = binomial)
#Predict on test data
probs =  predict(fit.glm, test_data, type = "response")
pred.glm  =  rep(0, length(probs))
pred.glm[probs > 0.5] =  1
table(pred.glm, actual_test_labels)
mean(pred.glm != actual_test_labels)
#Logistic Regression ends here



#KNN begins here

model = knn(train_data[,2:14], test_data[,2:14], actual_train_labels, k=3)
table(model, actual_test_labels)
mean(model != actual_test_labels)

model = knn(train_data[,2:14], test_data[,2:14], actual_train_labels, k=5)
table(model, actual_test_labels)
mean(model != actual_test_labels)


model = knn(train_data[,2:12], test_data[,2:12], actual_train_labels, k=3)
table(model, actual_test_labels)
mean(model != actual_test_labels)

model = knn(train_data[,2:12], test_data[,2:12], actual_train_labels, k=5)
table(model, actual_test_labels)
mean(model != actual_test_labels)
#KNN ends here