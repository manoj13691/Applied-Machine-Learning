x <- read.csv("/Users/manekbahl/Downloads/train_with_smote31.csv")
test <- read.csv("/Users/manekbahl/Downloads/test_without_smote_mj.csv")
#nrFolds <- 5
input <- input[,-1]
id <- test[,1]
test <- test[,-1]
a <- colnames(test)
a <- c(a,'target')
colnames(input) <- a
# generate array containing fold-number for each sample (row)
#folds <- rep_len(1:nrFolds, nrow(x))


# actual split of the data
#fold <- which(folds == 1)
#input.train<- x[-fold,]
#input.test <- x[fold,]

input$target=as.factor(input$target)
model <- glm(target ~.,family=binomial(link='logit'),data=input)
pred=predict(model,test,type="response")
pred
sum(pred>0.5)
probs=cbind(id,pred)
colnames(probs) = c("id","target")

write.csv(probs, "logistic_SMOTED.csv")
