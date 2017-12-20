#Read data section begins*****
require("ISLR")
data = Auto
y = data["mpg"]
y = as.matrix(y)
data$mpg = NULL
data$name= NULL
data     = scale(data)
no_of_rows = dim(data)[1]
x_0 = matrix(data=1,nrow = no_of_rows, ncol = 1)
x   = cbind(x_0,as.matrix(data))
no_of_cols = dim(x)[2]
colnames(x) = NULL
#Read data section ends*****



#Paramters section begins*****
#alpha is the learning rate
alpha = 0.5
iterations = 1000
#Number of features considered will be the number of rows
#of thetha and each is initialized to zero. dim(thetha)= 2*1
thetha = matrix(data=0,nrow =dim(x)[2] ,ncol = 1)
#Paramters section ends*****



#Cost function begins*****
cost_function = function(x,y,m,thetha){
  summation_term = 0
  for(i in 1:m){
    summation_term = summation_term + ((x[i:i,] %*% thetha) - y[i])^2
  }
  
  print(summation_term/(2*m))
}
cost_function(x,y,no_of_rows,thetha)
#Cost function ends*****


#Gradient Descent begins*****
for(iter in 1:iterations){
  derivative = c()
  
  for(all_features in 1:no_of_cols){
    derivative_temp = 0
    for(i in 1:no_of_rows){
      derivative_temp = derivative_temp + ((x[i:i,] %*% thetha) - y[i])*x[i:i,all_features:all_features]
    }#for(i) ends here
    derivative_temp = derivative_temp /no_of_rows
    thetha[all_features] = thetha[all_features] - (alpha * derivative_temp)
    
  }#for(all_features) ends here
  cost_function(x,y,no_of_rows,thetha)
}

#Gradient Descent ends*****

#For 1.7 prediction. Scale the input and predict
data = Auto
y = data["mpg"]
y = as.matrix(y)
data$mpg = NULL
data$name= NULL
newRow = data.frame(cylinders=4,displacement=300,
                    horsepower=200,weight = 3500,
                    acceleration=11,year=70,origin=2 )
data = rbind(data,newRow)
#(4,300,200,3500s,11,70,2)
data = scale(data)
no_of_rows = dim(data)[1]
x_0 = matrix(data=1,nrow = no_of_rows, ncol = 1)
x_1   = cbind(x_0,as.matrix(data))
colnames(x_1) = NULL
x_1[393,]%*%thetha
#View(x)

#For 1.7 prediction. Scale the input and predict


#For 1.9
coefficients = solve(t(x) %*% x) %*% t(x) %*% y
