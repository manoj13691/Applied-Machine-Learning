library(ISLR)
library(class)
data = Weekly
is.numeric(data$Volume)
conditon  = (data$Year < 2009)

#3 - Lag2 and 9- Direction
train  =  data[conditon,c(3,9) ]
test   =  data[!conditon,c(3,9) ]

actual_test_labels = as.matrix(test["Direction"])
actual_train_labels = as.matrix(train["Direction"])
train  =  as.matrix(data[conditon,c(3)])
test   =  as.matrix(data[!conditon,c(3)])
model =  knn(train, test, actual_train_labels, k = 1)
table(model, actual_test_labels)
mean(model != actual_test_labels)
