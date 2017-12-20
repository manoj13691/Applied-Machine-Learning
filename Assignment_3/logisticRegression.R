#Read data section begins*****
require("ISLR")
data = Auto
y = data["mpg"]
y = as.matrix(y)

#Find the median
median_y = median(y)
for(i in 1:length(y)){
  if(y[i]>median_y){
    y[i] = 1
  }
  else{
    y[i] = 0
  }
}

data$mpg = NULL
data$name= NULL
data$acceleration = NULL
data$year = NULL
data$origin = NULL
data$name = NULL
data     = scale(data)
no_of_rows = dim(data)[1]
x_0 = matrix(data=1,nrow = no_of_rows, ncol = 1)
x   = cbind(x_0,as.matrix(data))
no_of_cols = dim(x)[2]
colnames(x) = NULL
#Read data section ends*****



#Paramters section begins*****
#alpha is the learning rate
alpha = 3
iterations = 800
#Number of features considered will be the number of rows
#of thetha and each is initialized to zero. dim(thetha)= 2*1
thetha = matrix(data=0,nrow =dim(x)[2] ,ncol = 1)
#Paramters section ends*****



#Cost function begins*****
cost_function = function(x,y,m,thetha){
  summation_term = 0
  for(i in 1:m){
    h_thetha_x = exp(x[i:i,]%*% thetha)/(1 + exp(x[i:i,]%*% thetha) )
    summation_term = summation_term + (-y[i]*log(h_thetha_x) - (1-y[i])*log(1-h_thetha_x))
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
      h_thetha_x = exp(x[i:i,]%*% thetha)/(1 + exp(x[i:i,]%*% thetha) )
      derivative_temp = derivative_temp + (h_thetha_x - y[i])*x[i:i,all_features:all_features]
    }#for(i) ends here
    derivative_temp = derivative_temp /no_of_rows
    thetha[all_features] = thetha[all_features] - (alpha * derivative_temp)
    
  }#for(all_features) ends here
  cost_function(x,y,no_of_rows,thetha)
}

#Gradient Descent ends*****


#Error calculation begins*****
mis_classification_count = 0
for(i in length(y)){
  h_thetha_x = exp(x[i:i,]%*% thetha)/(1 + exp(x[i:i,]%*% thetha) )
  if(h_thetha_x >=0.5){
    h_thetha_x = 1
    
    if(h_thetha_x !=y[i]){
      mis_classification_count = mis_classification_count + 1
    }
  }
  else{
    h_thetha_x = 0
    if(h_thetha_x !=y[i]){
      mis_classification_count = mis_classification_count + 1
    }
  }
}
#Error calculation ends*****


#For 3.4
data = Auto
data$mpg = NULL
data$name= NULL
data$acceleration = NULL
data$year = NULL
data$origin = NULL
data$name = NULL
newRow = data.frame(cylinders=8, displacement=340, horsepower=200,weight=3500)
data = rbind(data,newRow)
data     = scale(data)
no_of_rows = dim(data)[1]
x_0 = matrix(data=1,nrow = no_of_rows, ncol = 1)
x_1   = cbind(x_0,as.matrix(data))
no_of_cols = dim(x_1)[2]
colnames(x_1) = NULL

ans = exp(x_1[393:393,]%*% thetha)/(1 + exp(x_1[393:393,]%*% thetha))
if(ans<0.5){
  print("0")
}
if(ans>=0.5){
  print("1")
}

#For 3.4 ends
