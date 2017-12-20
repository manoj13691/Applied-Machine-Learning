#Read data section begins*****
require("ISLR")
library(plotly)
data = Auto
no_of_rows = dim(data)[1]
no_of_cols = dim(data)[2]
y = data["mpg"]
y = as.matrix(y) 
data_horsepower = data["horsepower"]
data_horsepower = scale(data_horsepower)
data_horsepower = as.matrix(data_horsepower)
x_0 = matrix(data=1,nrow = no_of_rows, ncol = 1)
x   = cbind(x_0,data_horsepower)
colnames(x) = NULL
contour_x = c()
contour_y = c()
contour_z = c()
#Read data section ends*****



#Paramters section begins*****
#alpha is the learning rate
alpha = 1.5
iterations = 100
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
  return(summation_term/(2*m))
}
cost = cost_function(x,y,no_of_rows,thetha)
#Cost function ends*****



#Checking cost_function correctness begins*****
#This was used to check when all thethas were zero.
sum =0 
for(i in 1:length(y)){
  sum = sum +(y[i])^2
}
print(sum/(2*length(y)))
#Checking cost_function correctness ends*****



#Gradient Descent begins*****
for(iter in 1:iterations){
  derivative1 = 0
  for(i in 1:no_of_rows){
    derivative1 = derivative1 + ((x[i:i,] %*% thetha) - y[i])*x[i:i,1:1]
  }
  derivative1 = derivative1/no_of_rows
  temp1 = thetha[1] - (alpha * derivative1)
  
  derivative2 = 0
  for(i in 1:no_of_rows){
    derivative2 = derivative2 + ((x[i:i,] %*% thetha) - y[i])*x[i:i,2:2]
  }
  derivative2 = derivative2/no_of_rows
  temp2 = thetha[2] - (alpha * derivative2)
  
  
  thetha[1] = temp1
  thetha[2] = temp2
  cost = cost_function(x,y,no_of_rows,thetha)
  contour_x = c(contour_x,thetha[1])
  contour_y = c(contour_y,thetha[2])
  contour_z = c(contour_z,cost)
}
#Gradient Descent ends*****

#1.2 begins******
plot(x = x[,2], y = y,xlab = "Horsepower", ylab = "mpg")
abline(coef = thetha)
#1.2 ends******



#1.3 begins*****
data_horsepower2 = data["horsepower"]
data_horsepower2[393:393,1:1] = c(220)
data_horsepower2 = scale(data_horsepower2)
data_horsepower2 = as.matrix(data_horsepower2)
x_0_2 = matrix(data=1,nrow = no_of_rows+1, ncol = 1)
x2   = cbind(x_0_2,data_horsepower2)
colnames(x2) = NULL
x2[393,] %*% thetha
#1.3 ends*****


#1.4 begins*****
plot_ly(x = contour_x, y= contour_y, z= contour_z, type = "contour")
#1.4 ends*****

#For 1.5
#Normal equation for LR begins*****
coefficients = solve(t(x) %*% x) %*% t(x) %*% y
coefficients[1]
coefficients[2]
#Normal equation for LR ends*****

