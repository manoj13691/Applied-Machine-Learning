x = -20:20
y = c()
sigmoid = function(x){
  numerator = exp(x)
  denominator = 1+exp(x)
  return(numerator/denominator)
}

for(i in 1:length(x)){
  y[i] = sigmoid(x[i])
}

plot(x,y,ylab = "sigmoid(x)")

