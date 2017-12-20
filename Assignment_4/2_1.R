#install.packages("pdist")
#library(pdist)


filename_train = "ionosphere.data.txt_train_1.csv"
filename_test = "ionosphere.data.txt_test_1.csv"

input_train = read.csv(file=filename_train, header=TRUE, sep=",")
no_rows_train = dim(input_train)[1]

input_test = read.csv(file=filename_test, header=TRUE, sep=",")
no_rows_test = dim(input_test)[1]


actual_test_labels = as.array(input_test[1:70,35:35])
actual_train_labels = as.array(input_train[1:281,35:35])

#Converting to numeric for comparison
actual_train_labels = as.numeric(actual_train_labels)
actual_test_labels = as.numeric(actual_test_labels)



#Finding k nearest neighbors section begins here
predicted_test_labels = c()
#Remember - 35th column is the label. Both in train and test
for(i in 1:no_rows_test){
  dist_vector = c()
  for(j in 1:no_rows_train){
    distance = dist(rbind(input_test[i:i,1:34],input_train[j:j,1:34]))
    dist_vector = c(dist_vector, distance)
    #print(as.matrix(pdist(as.matrix(input_test[i:i,1:34]), as.matrix(input_train[j:j,1:34]))))
  }
  min = dist_vector[1]
  min_index = 1
  for(runner in 1:length(dist_vector)){
    if(dist_vector[runner]< min){
      min = dist_vector[runner]
      min_index = runner
    }
  }
  predicted_label = input_train[min_index:min_index,35:35]
  predicted_test_labels= c(predicted_test_labels , predicted_label)
  
}
#Finding k nearest neighbors section begins here




#check the test accuracy begins here
wrong_count = 0
for(i in 1:length(actual_test_labels)){
  if(predicted_test_labels[i] != actual_test_labels[i]){
    wrong_count = wrong_count +1
  }
}
print(wrong_count/length(actual_test_labels))
#check the test accuracy ends here