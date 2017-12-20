input=read.csv("C:\\Users\\Sohan Rai\\Documents\\project_dataset\\train.csv")
input$target=as.factor(input$target)
train=input[,-1]
model <- glm(target ~.,family=binomial(link='logit'),data=train)
summary(model)
# Note : We noted down the column names with p-value less than 0.05 and retained only those predictors.
