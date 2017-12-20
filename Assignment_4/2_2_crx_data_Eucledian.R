library(dummies)
#filename_train_list = c("crx.data.txt_train_1.csv")
#data = read.csv("crx.data.txt_train_1.csv")

#df =  dummy.data.frame(data, names=c("V1","V2","V3","V4","V5","V6"), sep="_")



#Parameters list begins here
k_list = c(1,3,5,7,9)
graph_x_k = c()
graph_y_avg_error = c()

filename_train_list = c("crx.data.txt_train_1.csv", "crx.data.txt_train_2.csv",
                        "crx.data.txt_train_3.csv","crx.data.txt_train_4.csv",
                        "crx.data.txt_train_5.csv")

filename_test_list = c("crx.data.txt_test_1.csv", "crx.data.txt_test_2.csv",
                       "crx.data.txt_test_3.csv","crx.data.txt_test_4.csv",
                       "crx.data.txt_test_5.csv")


#Parameters list ends here

for(k in k_list){
  errors_from_all_files = c()
  
  for(all_files in 1:length(filename_train_list)){
    
    filename_train = filename_train_list[all_files]
    filename_test = filename_test_list[all_files]
    
    
    input_train = read.csv(file=filename_train, header=TRUE, sep=",")
    input_train$V1 = as.numeric(input_train$V1)
    input_train$V2 = as.numeric(input_train$V2)
    input_train$V4 = as.numeric(input_train$V4)
    input_train$V5 = as.numeric(input_train$V5)
    input_train$V6 = as.numeric(input_train$V6)
    input_train$V7 = as.numeric(input_train$V7)
    input_train$V9 = as.numeric(input_train$V9)
    input_train$V10 = as.numeric(input_train$V10)
    input_train$V12 = as.numeric(input_train$V12)
    input_train$V13 = as.numeric(input_train$V13)
    input_train$V14 = as.numeric(input_train$V14)
    
    no_rows_train = dim(input_train)[1]
    
  input_test = read.csv(file=filename_test, header=TRUE, sep=",")
  input_test$V1 = as.numeric(input_test$V1)
  input_test$V2 = as.numeric(input_test$V2)
  input_test$V4 = as.numeric(input_test$V4)
  input_test$V5 = as.numeric(input_test$V5)
  input_test$V6 = as.numeric(input_test$V6)
  input_test$V7 = as.numeric(input_test$V7)
  input_test$V9 = as.numeric(input_test$V9)
  input_test$V10 = as.numeric(input_test$V10)
  input_test$V12 = as.numeric(input_test$V12)
  input_test$V13 = as.numeric(input_test$V13)
  input_test$V14 = as.numeric(input_test$V14)
  no_rows_test = dim(input_test)[1]
    
    
    actual_test_labels = as.array(input_test[,16:16])
    actual_train_labels = as.array(input_train[,16:16])
    
    #Converting to numeric for comparison
    actual_train_labels = as.numeric(actual_train_labels)
    actual_test_labels = as.numeric(actual_test_labels)
    
    
    #a =colnames(input_train)
    #for (i in a){
     # if(i %in% colnames(input_test) ){
        #print("Good")
      #}
      #else{
       # input_test[i] = rep(0, no_rows_test)
      #}
    #}
    
    
    
    #Finding k nearest neighbors section begins here
    predicted_test_labels = c()
    #min_indices_list = c()
    for(i in 1:no_rows_test){
      min_indices_list = c()
      dist_vector = c()
      for(j in 1:no_rows_train){
        distance = dist(rbind(input_test[i:i,1:15],input_train[j:j,1:15]))
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
