filename = "ionosphere.data.txt"
input = read.csv(file=filename, header=FALSE, sep=",")
if(filename=="ionosphere.data.txt")
{
  actual_label_matrix = as.matrix((input[,35:35]))
  input = input[,-c(35)]
}
#Take only first 50 data points
#input = input[1:50,]
input = input[50:99,]
actual_label_matrix = actual_label_matrix[50:99]
#input = as.matrix(input)
#*****Data section ends*****

#5.1
clusters = hclust(dist(input),method = "complete")
plot(clusters)
#abline (h=8.6, col =" red ")


#5.2
cut_tree = cutree(clusters, k = 2)
plot(cut_tree)

#Error calculation for 5.2 *********************************
clust_one_g_count = 0
clust_one_b_count = 0
clust_two_g_count = 0
clust_two_b_count = 0
for(i in 1:length(actual_label_matrix)){
  if(cut_tree[i] == 1){
    if(actual_label_matrix[i] == "g"){
      clust_one_g_count = clust_one_g_count + 1
    }
    else{
      clust_one_b_count = clust_one_b_count + 1
    }
  }
  else{
    if(actual_label_matrix[i] == "g"){
      clust_two_g_count = clust_two_g_count + 1
    }
    else{
      clust_two_b_count = clust_two_b_count + 1
    }
  }
}
error_1 = clust_one_b_count/(clust_one_b_count + clust_one_g_count)
error_2 = clust_two_b_count/(clust_two_b_count + clust_two_g_count)
error = error_1 + error_2
print(paste0("Error before PCA=",error))
#Error calculation for 5.2 ends*********************************

#cut_tree = cut(as.dendrogram(clusters), h = 8.6)
#par(mfrow=c(2,1))
#plot(cut_tree$lower[[1]])
#plot(cut_tree$lower[[2]])



#5.3
scaled_input = scale(input[1:50,3:34])
myPCA = princomp(scaled_input)
input_90_variance = myPCA$scores[,1:12]
clusters = hclust(dist(input_90_variance))
plot(clusters)
#myPCA = prcomp(scaled_input)
# = myPCA$sdev^2
#for(i in 1:length(eigs)){
 # print(eigs[i]/sum(eigs))
#} 
#summary(myPCA)


#5.4
cut_tree = cutree(clusters, k = 2)
plot(cut_tree)
#cut_tree = cut(as.dendrogram(clusters), h = 15)
#par(mfrow=c(2,1))
#plot(cut_tree$lower[[1]])
#plot(cut_tree$lower[[2]])
#*************Do the error rate calculation*************
clust_one_g_count = 0
clust_one_b_count = 0
clust_two_g_count = 0
clust_two_b_count = 0
for(i in 1:length(actual_label_matrix)){
  if(cut_tree[i] == 1){
    if(actual_label_matrix[i] == "g"){
      clust_one_g_count = clust_one_g_count + 1
    }
    else{
      clust_one_b_count = clust_one_b_count + 1
    }
  }
  else{
    if(actual_label_matrix[i] == "g"){
      clust_two_g_count = clust_two_g_count + 1
    }
    else{
      clust_two_b_count = clust_two_b_count + 1
    }
  }
}
error_1 = clust_one_b_count/(clust_one_b_count + clust_one_g_count)
error_2 = clust_two_b_count/(clust_two_b_count + clust_two_g_count)
error = error_1 + error_2
print(paste0("Error after PCA=",error))






