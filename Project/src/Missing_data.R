input=read.csv("C:\\Users\\Sohan Rai\\Documents\\project_dataset\\train.csv")
input=inputor
#############################
ones=input[(which(input$target==1)),]
zeros=input[(which(input$target==0)),]
nz=nrow(zeros)
k=nrow(ones)
zeros=zeros[,!(colnames(inputor)%in%c("target","id"))]
km=kmeans(zeros, k, nstart = 2)
centers=km$centers
ncol(centers)
inputor=cbind(c(1:k),rep(0,k),centers)
colnames(inputor)=colnames(ones)
inputor=rbind(ones,inputor)
#############################
id=inputor$id
target=inputor$target
cols=c("id","target","ps_car_03_cat","ps_car_05_cat")
inputor=inputor[,!(colnames(inputor)%in%cols)]
cont=c("ps_reg_03","ps_car_12","ps_car_13","ps_car_14","ps_car_15")
disc=setdiff(colnames(inputor),cont)
inputor[inputor==-1]<-NA
cM <- colMeans(inputor, na.rm=TRUE)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
cmode=apply(inputor, 2, getmode)
for (col in cont)
{
  ind = match(col,names(inputor))
  inputor[,ind][is.na(inputor[,ind])] <- cM[ind]  
}
for (col in disc)
{
  ind = match(col,names(inputor))
  inputor[,ind][is.na(inputor[,ind])] <- cmode[ind]  
}
indx <- which(is.na(inputor), arr.ind=TRUE)
#inputor=scale(inputor)
output=cbind(inputor,id,target)
write.csv(output, file = "C:\\Users\\Sohan Rai\\Documents\\project_dataset\\train_balanced_unscaled.csv",row.names=FALSE)
#write.csv(output, file = "C:\\Users\\Sohan Rai\\Documents\\project_dataset\\train_filtered.csv",row.names=FALSE)