#Parameters list begins here
k_list = c(1,3,5,7,9)
graph_x_k = c()
graph_y_avg_error = c()

filename_train_list = c("ionosphere.data.txt_train_1.csv", "ionosphere.data.txt_train_2.csv",
                        "ionosphere.data.txt_train_3.csv","ionosphere.data.txt_train_4.csv",
                        "ionosphere.data.txt_train_5.csv")

filename_test_list = c("ionosphere.data.txt_test_1.csv", "ionosphere.data.txt_test_2.csv",
                       "ionosphere.data.txt_test_3.csv","ionosphere.data.txt_test_4.csv",
                       "ionosphere.data.txt_test_5.csv")

#filename_train_list = c("ionosphere.data.txt_train_1.csv")
#filename_test_list = c("ionosphere.data.txt_test_1.csv")
#Parameters list ends here

for(k in k_list){
  errors_from_all_files = c()
  
  for(all_files in 1:length(filename_train_list)){
    
    filename_train = filename_train_list[all_files]
    filename_test = filename_test_list[all_files]
    
    
    input_train = read.csv(file=filename_train, header=TRUE, sep=",")
    no_rows_train = dim(input_train)[1]
    
    input_test = read.csv(file=filename_test, header=TRUE, sep=",")
    no_rows_test = dim(input_test)[1]
    
    
    actual_test_labels = as.array(input_test[,35:35])
    actual_train_labels = as.array(input_train[,35:35])
    
    #Converting to numeric for comparison
    actual_train_labels = as.numeric(actual_train_labels)
    actual_test_labels = as.numeric(actual_test_labels)
    
    
    
    #Finding k nearest neighbors section begins here
    predicted_test_labels = c()
    #min_indices_list = c()
    for(i in 1:no_rows_test){
      min_indices_list = c()
      dist_vector = c()
      for(j in 1:no_rows_train){
        distance = dist(rbind(input_test[i:i,1:34],input_train[j:j,1:34]),method = "manhattan")
        dist_vector = c(dist_vector, distance)
        #print(as.matrix(pdist(as.matrix(input_test[i:i,1:34]), as.matrix(input_train[j:j,1:34]))))
      }
      
      for(kk in 1:k){
        min = dist_vector[1]
        min_index = 1
        for(runner in 1:length(dist_vector)){
          if(dist_vector[runner]< min){
            min = dist_vector[runner]
            min_index = runner
          }
        }
        #Make the value at min_index to be very high so that it wont show up again for next kk
        dist_vector[min_index] = max(dist_vector)
        min_indices_list = c(min_indices_list, min_index)
      } 
      
      #Voting section
      one_count = 0
      two_count = 0
      for(ll in 1:length(min_indices_list)){
        if(actual_train_labels[min_indices_list[ll]] == 1){
          one_count = one_count + 1
        }
        else{
          two_count = two_count + 1
        }
      }
      
      if(one_count > two_count){
        predicted_test_labels = c(predicted_test_labels, 1)
      }
      
      else{
        predicted_test_labels = c(predicted_test_labels, 2)
      }
      
    }
    #Finding k nearest neighbors section begins here
    
    
    
    
    #check the test accuracy begins here
    wrong_count = 0
    for(i in 1:length(actual_test_labels)){
      if(predicted_test_labels[i] != actual_test_labels[i]){
        wrong_count = wrong_count +1
      }
    }
    
    errors_from_all_files = c(errors_from_all_files ,(wrong_count/length(actual_test_labels)))
    #check the test accuracy ends here
    
  }#for(all_files) ends here
  
  graph_x_k = c(graph_x_k, k)
  graph_y_avg_error = c(graph_y_avg_error, mean(errors_from_all_files))
  
}#for(k in k_list) ends here  
plot(graph_x_k, graph_y_avg_error, xlab = "K", ylab = "Error", type = "o" )

