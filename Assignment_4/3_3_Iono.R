#For Naive Bayes
library(e1071)

k = 1
graph_x_k = c()
graph_y_error = c()

filename_train_list = c("ionosphere.data.txt_train_1.csv", "ionosphere.data.txt_train_2.csv",
                        "ionosphere.data.txt_train_3.csv","ionosphere.data.txt_train_4.csv",
                        "ionosphere.data.txt_train_5.csv")

filename_test_list = c("ionosphere.data.txt_test_1.csv", "ionosphere.data.txt_test_2.csv",
                       "ionosphere.data.txt_test_3.csv","ionosphere.data.txt_test_4.csv",
                       "ionosphere.data.txt_test_5.csv")
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
  
  #input_train$V35 = as.numeric(input_train$V35)
  #input_test$V35 = as.numeric(input_test$V35)
  
  input_train = input_train[-c(1,2)]
  input_test  = input_test[-c(1,2)]
  
  
  #Converting to numeric for comparison
  actual_train_labels = as.numeric(actual_train_labels)
  actual_test_labels = as.numeric(actual_test_labels)
  
  
  model = naiveBayes( V35~.,data = input_train)
  nb_test_predict =  predict(model,input_test[,-33])
  #confusion matrix
  #table(pred=nb_test_predict,true=input_test$V35)
  predicted_test_labels = as.numeric(nb_test_predict)
  
  
  #check the test accuracy begins here
  wrong_count = 0
  for(i in 1:length(actual_test_labels)){
    if(predicted_test_labels[i] != actual_test_labels[i]){
      wrong_count = wrong_count +1
    }
  }
  
  errors_from_all_files = c(errors_from_all_files ,(wrong_count/length(actual_test_labels)))
  #check the test accuracy ends here
  
  #Put errors in graph
  graph_x_k = c(graph_x_k, all_files)
  graph_y_error = c(graph_y_error, (wrong_count/length(actual_test_labels)))
  
}#for(all_files) ends here
plot(graph_x_k, graph_y_error, xlab = "Test Files", ylab = "Error", type = "o" )  
