library(class)
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
    
    input_train = input_train[,1:15]
    input_test = input_test[,1:15]
    
    
    #Converting to numeric for comparison
    actual_train_labels = as.numeric(actual_train_labels)
    actual_test_labels = as.numeric(actual_test_labels)
    
    
    
    predicted_test_labels = knn(train = input_train, test = input_test,
                                cl =actual_train_labels, k=k)
    
    predicted_test_labels = as.numeric(predicted_test_labels)
    
    
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

