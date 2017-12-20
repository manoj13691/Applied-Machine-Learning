x1 = c(1,1,0,5,6,4)
x2 = c(4,3,4,1,2,0)
plot(x1,x2)

x_include_centroid = c(1,1,0,5,6,4,2/3,5)
y_include_centroid = c(4,3,4,1,2,0,11/3,1)
plot(x_include_centroid,y_include_centroid)

data_frame = data.frame(X= x1, Y=x2)
data_frame
#Assigning color to each class by adding the class label.
plot(data_frame$X, data_frame$Y, col=c(1,1,1,2,2,2), xlab = 'X', ylab = 'Y')

