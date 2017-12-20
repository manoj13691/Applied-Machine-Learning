#For KNN
library(class)
#For Naive Bayes
library(e1071)

#For one-hot encoding
library(dummies)

k = 3
graph_x_k = c()
graph_y_error = c()

filename_train_list = c("car.data.txt_train_1.csv", "car.data.txt_train_2.csv",
                        "car.data.txt_train_3.csv","car.data.txt_train_4.csv",
                        "car.data.txt_train_5.csv")

filename_test_list = c("car.data.txt_test_1.csv", "car.data.txt_test_2.csv",
                       "car.data.txt_test_3.csv","car.data.txt_test_4.csv",
                       "car.data.txt_test_5.csv")


  errors_from_all_files = c()
  
  for(all_files in 1:length(filename_train_list)){
    
    filename_train = filename_train_list[all_files]
    filename_test = filename_test_list[all_files]
    
    
    input_train = read.csv(file=filename_train, header=TRUE, sep=",")
    input_train = dummy.data.frame(input_train, names=c("V1","V2","V3","V4","V5","V6"), sep="_")
    no_rows_train = dim(input_train)[1]
    
    
    input_test = read.csv(file=filename_test, header=TRUE, sep=",")
    input_test = dummy.data.frame(input_test, names=c("V1","V2","V3","V4","V5","V6"), sep="_")
    no_rows_test = dim(input_test)[1]
    
    
    actual_test_labels = as.array(input_test[,22:22])
    actual_train_labels = as.array(input_train[,22:22])
    
    input_train = input_train[,1:21]
    input_test = input_test[,1:21]
    
    
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
    
    #Put errors in graph
    graph_x_k = c(graph_x_k, all_files)
    graph_y_error = c(graph_y_error, (wrong_count/length(actual_test_labels)))
    
  }#for(all_files) ends here
  
  
  
  
  
  
  
#Naive Bayes begins here
  
  errors_from_all_files = c()
  
  for(all_files in 1:length(filename_train_list)){
    
    filename_train = filename_train_list[all_files]
    filename_test = filename_test_list[all_files]
    
    
    input_train = read.csv(file=filename_train, header=TRUE, sep=",")
    #input_train = dummy.data.frame(input_train, names=c("V1","V2","V3","V4","V5","V6"), sep="_")
    no_rows_train = dim(input_train)[1]
    
    
    input_test = read.csv(file=filename_test, header=TRUE, sep=",")
    #input_test = dummy.data.frame(input_test, names=c("V1","V2","V3","V4","V5","V6"), sep="_")
    no_rows_test = dim(input_test)[1]
    
    
    actual_test_labels = as.array(input_test[,7])
    actual_train_labels = as.array(input_train[,7])
    
    #input_train = input_train[,1:21]
    #input_test = input_test[,1:21]
    
    
    #Converting to numeric for comparison
    actual_train_labels = as.numeric(actual_train_labels)
    actual_test_labels = as.numeric(actual_test_labels)
    
    
    
    model = naiveBayes( V7~.,data = input_train)
    nb_test_predict =  predict(model,input_test[,-7])
    #confusion matrix
    #table(pred=nb_test_predict,true=input_test$V7)
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
  
  
  x = graph_x_k
  y = graph_y_error
  z = rep(c("KNN","Naive-Bayes"),each=5)
  ggplot(data.frame(x,y,z),aes(x=x,y=y,colour=z))+geom_point(aes(colour=z))+geom_line(aes(colour=z))+xlab("Test Files")+ylab("Error")
  