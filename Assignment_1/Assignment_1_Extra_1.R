#*****Data and parameters section*****
filename = "breast-cancer-wisconsin.data.txt"
input = read.csv(file=filename, header=FALSE, sep=",")
#Remove NA rows
input = na.omit(input)
#Extract actual labels from breast-cancer data
if(filename=="breast-cancer-wisconsin.data.txt")
{
  actual_label_matrix = as.matrix((input[,11:11]))
  input = input[,-c(1,11)]
}
class(input)
#Data stored as a data frame
cluster = 5
max_iter = 20
tow = 0.00000000000000000000001
set.seed(2)

#For error part
X_for_error = c()
error_values = c()
#*****Data and parameters section ends*****


for(k in 2:cluster){
  #set.seed(2)  
  #*****Initializing centroids section*****
  #Dimension used in initializing centroids
  data_dimension = dim(input)[2] 
  #now create k random centroids with dimension being "data_dimension"
  ?rnorm
  all_centroids = matrix(rnorm(k*data_dimension), ncol=data_dimension)
  # Below is a way to access individual rows of k centorids and data points.
  # all_centroids[1,]
  # input[1,]
  # To access nth row to jth row and Lth col to Kth col,
  # input [n:j,L:K]
  #*****Initializing centroids section ends*****
  
  
  
  
  #*****Initialize distance_matrix and label_matrix section***** 
  # Each data point has to be measures against k cluster centroids.
  # Hence we will have (D x K) matrix called distance_matrix 
  # where D - No of data points and K - no of cluster entroids
  #?matrix
  distance_matrix = matrix(data = -1, nrow = dim(input)[1], ncol = k ) 
  label_matrix    = matrix(data = NA, nrow = dim(input)[1], ncol = 1 )
  #*****Initialize distance_matrix and label_matrix section ends*****
  
  
  
  # iter indicates the iteration.
  # A variable 'prev_all_centroids' is used to store (iter-1) centroids if iter>1.
  # Then 'prev_all_centroids' and 'all_centroids' are used for stopping criterion.
  # 'prev_all_centroids' is initialized just like 'all_centroids'
  for(iter in 1:max_iter)
  {
    prev_all_centroids = all_centroids
    
    #*****Calcuate 'K' distances for each data point section***** 
    # For every calculation, update the distance matrix, 
    # find min and update label_matrix for each point.
    for (i in 1:dim(input)[1]){
      for (j in 1:k){
        #print(input[i:i,]) 
        #print(all_centroids[j:j,]) 
        distance = dist(rbind(input[i:i,],all_centroids[j:j,]))
        distance_matrix[i:i,j:j] = (distance)
        #print(distance)
      }
    }
    
    # Go through distance matrix of each data point and find the min value 
    # ie. the index of min value will be the nearest centroid number
    for(i in 1:dim(input)[1]){
      min_value       = distance_matrix[i:i,1:1]
      min_value_index = 1
      for(j in 1:k){
        #print(distance_matrix[i:i,j:j])
        if(distance_matrix[i:i,j:j] < min_value){
          min_value       = distance_matrix[i:i,j:j]
          min_value_index = j
        }
      }
      label_matrix[i:i,1:1] = min_value_index
    }
    #*****Calcuate 'K' distances for each data point section ends*****
    
    
    
    
    #*****Update the centroid section*****
    # Inputs involved,
    # label_matrix  - labels of each data point (N x 1)
    # all_centorids - centroids of each cluster (K x D)
    # input         - input data (N x D)
    for(i in 1:k){
      current_centroid = all_centroids[i:i,1:dim(input)[2]]
      counter = 1
      for (j in 1:dim(input)[1]){
        if(label_matrix[j:j,1:1] == i){
          #Add the vector input[j:j,] to current_centroid and 
          #Increment counter
          current_centroid = colSums(rbind(current_centroid, input[j:j,]), na.rm=TRUE)
          counter = counter +1 
        }
      }
      #print(current_centroid)
      all_centroids[i:i,] = current_centroid/counter
      #print(counter)
    }
    #*****Update the centroid section ends*****
    
    
    
    #*****Stopping criteria section*****
    if(iter !=1){
      sum = 0
      #write code for stopping criterion
      for(stopper_i in 1:k){
        sum = sum + dist(rbind(prev_all_centroids[stopper_i:stopper_i,]
                               ,all_centroids[stopper_i:stopper_i,]))
      }
      #print (sum/k)
      if(sum/k < tow)
      {
        #print("Reached threshold. But continuing")
        #break
      }
    }
    #*****Stopping criteria section ends*****
  }#iteration ends
  
  
  #***** Error calculation for Ionosphere data section*****
  if(filename=="breast-cancer-wisconsin.data.txt"){
    # Input matrices : label_matrix, actual_label_matrix
    # To keep a count of bag and good labels, construct error matrix of dim - (k X 2)
    # (k x 2) - 2 because of two types, good and bad.
    # 1st column is good(g) and 2nd column is bad(b)
    error_matrix = matrix(data = 0, nrow = k, ncol = 2)
    for(i in 1:dim(label_matrix)[1]){
      if(actual_label_matrix[i]==2){
        error_matrix[label_matrix[i]:label_matrix[i],1:1] = error_matrix[label_matrix[i]:label_matrix[i],1:1] + 1 
      }
      if(actual_label_matrix[i]==4){
        error_matrix[label_matrix[i]:label_matrix[i],2:2] = error_matrix[label_matrix[i]:label_matrix[i],2:2] + 1 
      }
    }
    
    # Sum over the error_matrix. 
    # Formula = sum(bi/(bi + gi)) for i=2 to k
    error_sum = 0
    for(i in 1:k){
      denominator = error_matrix[i:i,2:2] + error_matrix[i:i,1:1]
      if(denominator == 0){
        denominator = 1
      }
      error_sum   = error_sum + (error_matrix[i:i,2:2])/ denominator
    }
    print(error_sum)
    # After running the above code for k=2,3,4,5 for 20 iterations each,
    # Plotting the errors below
    X_for_error[k-1] = k
    error_values[k-1] = error_sum
  } # if(filename=="") ends
} # for(k in 2:cluster) ends here
plot(X_for_error,error_values, type = "b", pch=20, xlab = "K", ylab = "Error")
