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
#c(3,0.3,0.03,0.00003)
for(a in c(3,0.3,0.03,0.00003))
{  
alpha = a
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
cost_function(x,y,no_of_rows,thetha)
#Cost function ends*****

x_iter = c()
y_cost = c()
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
  cost = cost_function(x,y,no_of_rows,thetha)
  x_iter[iter] = iter
  print(cost)
  y_cost[iter] = cost
}
plot(x_iter,y_cost,xlab = "Iterations", ylab = "Cost",main = a)
#Gradient Descent ends*****
}#for(a in) ends here

