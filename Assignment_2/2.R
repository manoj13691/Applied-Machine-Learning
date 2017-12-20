#Author - Manoj Joshi
#24th Sept, 2017

#*****Data section*****
library(mvtnorm)
filename = "ionosphere.data.txt"
#filename = "dummy_data.csv"
#filename = "ringnorm_data.csv"
input = read.csv(file=filename, header=FALSE, sep=",")

if(filename=="ionosphere.data.txt")
{
  #actual_label_matrix = as.matrix((input[,35:35]))
  input = input[,-c(35)]
}

if(filename=="ringnorm_data.csv")
{
  #actual_label_matrix = as.matrix((input[,1:1]))
  input = input[,-c(1)]
}

input = as.matrix(input)
#*****Data section ends*****



#*****Initialization section*****
# mu is a data frame. sigma and prior are arrays
set.seed(2)
epsilon = 0.00001
no_of_iterations = 20
k = 2
no_of_rows     = dim(input)[1]
no_of_features = dim(input)[2]
#mu    = as.matrix(input[sample(nrow(input), k), ])
mu    = as.matrix(input[1:k,])
sig   = diag(no_of_features)
sigma = array(sig, dim=c(no_of_features,no_of_features,k))
prior = array(1/k, c(k))
weight_matrix = matrix(data = 1/k, nrow = k, ncol = no_of_rows)
#*****Initialization section ends*****





for(iter in 1:no_of_iterations){
previous_mu = mu  
#print("Previous mu:")
#print(previous_mu[1:1,])

#*****Expectation step Section*****
for(i in 1:k){
  for(j in 1:no_of_rows){
    numerator  = dmvnorm(input[j:j,],mu[i:i,],sigma[,,i]) * prior[i]
    denominator = 0
    for(a in 1:k){
      denominator = denominator + dmvnorm(input[j:j,],mu[a:a,],sigma[,,a]) * prior[a]
    }

    #if(numerator !=0 && denominator !=0 ){
     if( denominator !=0 ){
      #print(numerator)
      #print(denominator)
      weight_matrix[i:i,j:j] = (numerator) / (denominator) 
    }
  }
}
#*****Expectation step Section ends*****




#*****Maximization step section*****
for(i in 1:k){
  numerator = 0
  denominator = 0
  numerator_sigma = matrix(data = 0, nrow = no_of_features , ncol = no_of_features)
  for(j in 1:no_of_rows){
    numerator   = numerator + (weight_matrix[i:i,j:j] *input[j:j,])
    denominator = denominator + weight_matrix[i:i,j:j]
    #numerator_sigma = numerator_sigma + (weight_matrix[i:i,j:j] * ((input[j:j,]-mu[i:i,]) %*% (input[j:j,]-mu[i:i,])))
    numerator_sigma = numerator_sigma + (weight_matrix[i:i,j:j]) * (as.matrix(input[j:j,]-mu[i:i,]) %*% (t(input[j:j,]-mu[i:i,])))
  }
  mu[i:i,] = numerator/denominator
  sigma[,,i] = numerator_sigma / denominator
  prior[i] = denominator / no_of_rows
}
#*****Maximization step section ends*****




#print("current mu:")
#print(mu[1:1,])

#*****Stopping criteria section*****
if(iter !=1){
  sum = 0
  #write code for stopping criterion
  for(stopper_i in 1:k){
    sum = sum + dist(rbind(previous_mu[stopper_i:stopper_i,]
                           ,mu[stopper_i:stopper_i,]))^2
  }
  print (sum)
  if(sum < epsilon)
  {
    print(paste0("Number of iterations done:",iter))
    print (paste0("Error:",sum))
    break
  }
}
#*****Stopping criteria section ends*****

}#iteration ends


