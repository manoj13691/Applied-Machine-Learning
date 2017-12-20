# Author - Manoj Joshi
# Sept 9th, 2017


#*****Data and parameters section*****
filename = "ionosphere.data.txt"
input = read.csv(file=filename, header=FALSE, sep=",")
#Below line -c(35) is for ionosphere only
input = input[,-c(35)]
class(input)
#Data stored as a data frame
k = 2
max_iter = 20
tow = 0.00000000000001
set.seed(2)
#*****Data and parameters section ends*****



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
?matrix
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
  print (sum/k)
  if(sum/k < tow)
  {
    break
  }
}
#*****Stopping criteria section ends*****
}#iteration ends


