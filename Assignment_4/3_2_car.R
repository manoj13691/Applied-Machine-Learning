#For dmvnorm
library(mvtnorm)
library(dummies)
library(reshape2)
graph_x = c()
graph_y_avg_error = c()

filename_train_list = c("car.data.txt_train_1.csv", "car.data.txt_train_2.csv",
                         "car.data.txt_train_3.csv","car.data.txt_train_4.csv",
                        "car.data.txt_train_5.csv")

filename_test_list = c("car.data.txt_test_1.csv", "car.data.txt_test_2.csv",
                       "car.data.txt_test_3.csv","car.data.txt_test_4.csv",
                       "car.data.txt_test_5.csv")

#filename_train_list = c("car.data.txt_train_2.csv")
#filename_test_list = c("car.data.txt_test_2.csv")
#Parameters list ends here


errors_from_all_files = c()
for(all_files in 1:length(filename_train_list)){
  
  filename_train = filename_train_list[all_files]
  filename_test = filename_test_list[all_files]
  
  
  input_train = read.csv(file=filename_train, header=TRUE, sep=",")
  no_rows_train = dim(input_train)[1]
  
  input_test = read.csv(file=filename_test, header=TRUE, sep=",")
  no_rows_test = dim(input_test)[1]
  
  
  actual_test_labels = as.array(input_test[,7:7])
  actual_train_labels = as.array(input_train[,7:7])
  
  #Converting to numeric for comparison
  actual_train_labels = as.numeric(actual_train_labels)
  actual_test_labels = as.numeric(actual_test_labels)
  
  input_train$V7 = as.numeric(input_train$V7)
  input_test$V7 = as.numeric(input_test$V7)
  
  predicted_test_labels = c()
  unique_train_labels = sort(unique(actual_train_labels))
  prior = rep(1/length(unique_train_labels),4 )
  
  
  #Build probability table begins
  #table will be a Cx(# of unique_lables) matrix
  #where c is the number of features
  #This has to be updated for each test sample
  table_for_nc = matrix(data = 0, nrow = dim(input_train)[2]-1, ncol = length(unique_train_labels))
  m = no_rows_train
  each_class_occurance_train = rep(0,length(unique_train_labels))
  
  for(i in 1:no_rows_train){
    each_class_occurance_train[input_train[i:i,7:7]] = each_class_occurance_train[input_train[i:i,7:7]] + 1
  }
  
  for(ii in 1:dim(input_test)[1]){
    table_for_nc = matrix(data = 0, nrow = dim(input_train)[2]-1, ncol = length(unique_train_labels))
    for(jj in 1:(dim(input_test[ii:ii,])[2]-1)){
      for(kk in 1:dim(input_train)[1]){
        #print(kk)
        if(input_train[kk:kk,jj:jj] == input_test[ii:ii,jj:jj]){
          #print("hello")
          class_label = input_train[kk:kk,7:7]
          table_for_nc[jj:jj,class_label:class_label] = table_for_nc[jj:jj,class_label:class_label] +1
        }
      }
    }
    #nc table is ready for a test sample
    prob_table = table_for_nc
    
    for(prob_table_runner in 1:dim(prob_table)[1]){
      for(class_runner in 1:dim(prob_table)[2]){
        num = prob_table[prob_table_runner:prob_table_runner,class_runner:class_runner] + (m * prior[class_runner])
        den = each_class_occurance_train[class_runner] + m
        prob_table[prob_table_runner:prob_table_runner,class_runner:class_runner] = num/den
      }
    }
      
    
    #get the max probabilities now from prob_table
    class_probs = c()
    
    for( prob_table_runner in 1:dim(prob_table)[2]){ #Run through all classes
      prod = 1
      for(row_runner in 1:dim(prob_table)[1]){
        prod = prod * prob_table[row_runner:row_runner,prob_table_runner:prob_table_runner]
      }
      answer = prod  * each_class_occurance_train[prob_table_runner]/sum(each_class_occurance_train)
      class_probs = c(class_probs, answer)
    }
    
    #Get the max prob and put it in predicted labels
    for(class_runner in 1:length(class_probs)){
      if(max(class_probs) == class_probs[class_runner]){
        predicted_test_labels = c(predicted_test_labels,class_runner)
        break
      }
    }
      
  }#for(ii in 1:dim(input_test)[1]) ends here
  
  
  
  #Build probability table ends
  
  
  
  #Check error begins
  wrong_count  = 0 
  for(i in 1:length(predicted_test_labels)){
    if(predicted_test_labels[i] != actual_test_labels[i]){
      wrong_count = wrong_count + 1
    }
  }
  errors_from_all_files = c(errors_from_all_files, wrong_count/length(predicted_test_labels))
  #print(wrong_count/length(predicted_test_labels))
  #Check error ends
  
  graph_x = c(graph_x, all_files)
}
plot(graph_x, errors_from_all_files, xlab = "Test Files", ylab = "Error", type = "o" )



