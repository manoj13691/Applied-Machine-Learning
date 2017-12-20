#For dmvnorm
library(mvtnorm)

graph_x = c()
graph_y_avg_error = c()

filename_train_list = c("crx.data.txt_train_1.csv", "crx.data.txt_train_2.csv",
                        "crx.data.txt_train_3.csv","crx.data.txt_train_4.csv",
                        "crx.data.txt_train_5.csv")

filename_test_list = c("crx.data.txt_test_1.csv", "crx.data.txt_test_2.csv",
                       "crx.data.txt_test_3.csv","crx.data.txt_test_4.csv",
                       "crx.data.txt_test_5.csv")

#filename_train_list = c("ionosphere.data.txt_train_1.csv")
#filename_test_list = c("ionosphere.data.txt_test_1.csv")
#Parameters list ends here


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
  
  input_train$V16 = as.numeric(input_train$V16)
  input_test$V16 = as.numeric(input_test$V16)
  
  
  unique_train_labels = sort(unique(actual_test_labels))
  
  #Naives Bayes training begins here
  delta = list()
  for(i in unique_train_labels){
    empty_df = input_train[FALSE,]
    for(j in 1:no_rows_train){
      if(input_train[j:j,dim(input_train)[2]] == i){
        empty_df = rbind(empty_df, input_train[j:j,1:15])
      }
    }
    delta[[i]] = empty_df
  }
  
  
  
  #Size of subsets begins
  m = list()
  for(i in 1:length(delta)){
    m[i] = dim(delta[[i]])[1]
  }
  
  #Size of subsets ends
  
  
  #Prior begins here
  prior = list()
  for(i in 1:length(m)){
    prior[i] = m[[i]]/no_rows_train
  }  
  #Prior ends here
  
  
  #mu begins here
  mu = list()
  for(i in 1:length(delta)){
    temp_df = delta[[i]]
    sum  = 0
    for(j in 1:dim(temp_df)[1]){
      sum = sum + temp_df[j:j,1:15]
    }
    sum = sum/dim(temp_df)[1]
    mu[[i]] = sum
  }
  #mu ends here
  
  
  #var begins here
  
  
  z = list()
  for(i in 1:length(delta)){
    I = matrix(data=1, nrow = dim(delta[[i]])[1], ncol =1 )
    z[[i]] = as.data.frame(as.matrix(delta[[i]]) - I %*% as.matrix(mu[[i]]))
  }
  
  
  
  sigma = list()
  for(i in 1:length(z)){
    numerator = t(as.matrix(z[[i]])) %*% as.matrix(z[[i]])
    denominator = dim(z[[i]])[1]
    sigma[[i]] = as.data.frame(numerator/denominator)
  }
  #var ends 
  
  
  #Check probability using dmvnorm begins
  predicted_test_labels = c()
  for(sample in 1:dim(input_test)[1]){
    class_1 = dmvnorm(as.matrix(input_test[sample:sample,1:15]), as.matrix(mu[[1]]), 
                      as.matrix(sigma[[1]])) * prior[[1]]
    class_2 = dmvnorm(as.matrix(input_test[sample:sample,1:15]), as.matrix(mu[[2]]), 
                      as.matrix(sigma[[2]])) * prior[[2]]
    
    if(class_1 > class_2){
      predicted_test_labels = c(predicted_test_labels, 1)
    }
    else{
      predicted_test_labels = c(predicted_test_labels, 2)
    }
  }
  
  #Check probability using dmvnorm ends
  
  
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



