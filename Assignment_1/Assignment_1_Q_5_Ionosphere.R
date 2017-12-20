#Below is the code for ionosphere dataset

input = read.csv(file="ionosphere.data.txt", header=FALSE, sep=",")
input = input[,-c(35)] 
#normalized_input = scale(input)
class(input)

#no_of_clusters and vector_of_errors will be used in plotting
no_of_clusters = c(2:10)
#error_counter is used to append values at the right index
error_counter = 1
vector_of_errors = c() 
for(i in no_of_clusters)
{
  kmeans_output <- kmeans(input,i)
  #print (kmeans_output$tot.withinss)
  vector_of_errors[error_counter] = kmeans_output$tot.withinss
  error_counter = error_counter + 1
}
plot(no_of_clusters, vector_of_errors,type = "b", pch=20, xlab = "K", ylab = "
     Sum of square error within cluster")
vector_of_errors
for (i in 2:9){
 print(vector_of_errors[i-1]-vector_of_errors[i])
}
# There is a steep drop in error from k=2 to k=3 than the error drop from k=3 to 4. 
# The drop from k=2 to k=3 is (2419.365 - 2212.872) = 206.493 units
# Hence k=3 will be the optimal number of clusters.