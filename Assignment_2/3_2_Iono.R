#Author - Manoj Joshi
#24th Sept, 2017

#*****Data section*****
library(mvtnorm)
filename = "ionosphere.data.txt"
input = read.csv(file=filename, header=FALSE, sep=",")
set.seed(2)
clusters = 5
if(filename=="ionosphere.data.txt")
{
  actual_label_matrix = as.matrix((input[,35:35]))
  input = input[,-c(35)]
}
input = as.matrix(input)
#*****Data section ends*****

#For error part

error_values = matrix(data = 0, nrow = 20, ncol = 4)
iter_values  = matrix(data =0, nrow = 20, ncol = 4)

for (k in 2:clusters){
  # For each k, run for 20 onfigurations.
  # Calculate error for configuration after convergence
  for(config in 1:20){
    #*****Initialization section*****
    # mu is a data frame. sigma and prior are arrays
    epsilon = 0.00001
    #k = 2
    no_of_rows     = dim(input)[1]
    no_of_features = dim(input)[2]
    label_matrix  = matrix(data = NA, nrow = no_of_rows, ncol = 1 )
    mu    = as.matrix(input[sample(nrow(input), k), ])
    #mu    = as.matrix(input[1:k,])
    sig   = diag(no_of_features)
    sigma = array(sig, dim=c(no_of_features,no_of_features,k))
    prior = array(1/k, c(k))
    weight_matrix = matrix(data = 1/k, nrow = k, ncol = no_of_rows)
    #*****Initialization section ends*****
    
    
    
    
    iter = 0
    while(TRUE){
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
        #numerator_sigma = matrix(data = 0, nrow = no_of_features , ncol = no_of_features)
        for(j in 1:no_of_rows){
          numerator   = numerator + (weight_matrix[i:i,j:j] *input[j:j,])
          denominator = denominator + weight_matrix[i:i,j:j]
          #numerator_sigma = numerator_sigma + (weight_matrix[i:i,j:j] * ((input[j:j,]-mu[i:i,]) %*% (input[j:j,]-mu[i:i,])))
          #numerator_sigma = numerator_sigma + (weight_matrix[i:i,j:j]) * (as.matrix(input[j:j,]-mu[i:i,]) %*% (t(input[j:j,]-mu[i:i,])))
        }
        mu[i:i,] = numerator/denominator
        #sigma[,,i] = numerator_sigma / denominator
        #prior[i] = denominator / no_of_rows
      }
      #*****Maximization step section ends*****
      
      
      
      
      #print("current mu:")
      #print(mu[1:1,])
      
      #*****Stopping criteria section*****
      sum = 0
      #write code for stopping criterion
      for(stopper_i in 1:k){
        sum = sum + dist(rbind(previous_mu[stopper_i:stopper_i,]
                               ,mu[stopper_i:stopper_i,]))^2
      }
      #print (sum)
      if(sum < epsilon)
      {
        #print(paste0("Number of iterations done:",iter))
        #print (paste0("Error: ",sum))
        iter = iter+1
        break #Stop the inner iterations for a particular config
      }
      #*****Stopping criteria section ends*****
      iter = iter+1  #if stopping criteria not reached
    }#while(TRUE) ---iteration ends
    print(paste0("For k=",k))
    print(paste0("Stopping criteria reached for configuration:=",config))
    
    
    #Do hard clustering using max(weight_matrix)
    for(labeller in 1:no_of_rows){
      #take max of column for each labeller and assign to label
      label_matrix[labeller]= which.max(weight_matrix[,labeller:labeller])
    }
    
    
    #*****Error calculation section*****
    # Input matrices : label_matrix, actual_label_matrix
    # To keep a count of bag and good labels, construct error matrix of dim - (k X 2)
    # (k x 2) - 2 because of two types, good and bad.
    # 1st column is good(g) and 2nd column is bad(b)
    error_matrix = matrix(data = 0, nrow = k, ncol = 2)
    for(i in 1:dim(label_matrix)[1]){
      if(actual_label_matrix[i]== "g"){
        error_matrix[label_matrix[i]:label_matrix[i],1:1] = error_matrix[label_matrix[i]:label_matrix[i],1:1] + 1 
      }
      if(actual_label_matrix[i]== "b"){
        error_matrix[label_matrix[i]:label_matrix[i],2:2] = error_matrix[label_matrix[i]:label_matrix[i],2:2] + 1 
      }
    }
    error_sum = 0
    for(i in 1:k){
      if(error_matrix[i:i,1:1] > error_matrix[i:i,2:2]){
        error_sum = error_sum + (error_matrix[i:i,2:2]/ (error_matrix[i:i,1:1] + error_matrix[i:i,2:2]))
      }
      else{
        error_sum = error_sum + (error_matrix[i:i,1:1]/ (error_matrix[i:i,1:1] + error_matrix[i:i,2:2]))
      }
    }
    error_values[config:config,(k-1):(k-1)] = error_sum
    iter_values[config:config, (k-1):(k-1)] = iter
  } #for config 1:20
}#for (k in 2:clusters) ends

#plot(X_for_error,error_values, type = "b", pch=20, xlab = "K-G(k)", ylab = "Error-G(k)")

boxplot(error_values)
#boxplot(iter_values)
