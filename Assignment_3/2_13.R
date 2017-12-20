set.seed(1)
#a
x=rnorm(100)
x
#b
eps=rnorm(100,0,0.5)

#c
y = -1 + 0.5 * x + eps
length(y)
#theta0= -1 and theta1=0.5

#d
plot(x,y)

#e
as.matrix(x)
as.matrix(y)
model =lm(y~x)
parameters=model$coefficients

#f
abline(model, col="red")
abline(-1,0.5,col="black")
legend("topleft",c( "least square","regression"),col=c("red","black"),lty = c(1,1))

#g
plot(x,y)
model=lm(y~x+I(x^2))
summary(model)
abline(model)
#h
eps2=rnorm(100,0,0.02)
y=-1+0.5*x+eps2
plot(x,y)
model=lm(y~x)
model
abline(lm.fit2, col="red")
abline(-1,0.5,col="blue")
legend("topleft",c( "least square","regression"),col=c("red","blue"),lty = c(1,1))

#i
eps2=rnorm(100,0,0.9)
y=-1+0.5*x+eps2
plot(x,y)
model=lm(y~x)
model
abline(lm.fit2, col="red")
abline(-1,0.5,col="blue")
legend("topleft",c( "least square","regression"),col=c("red","blue"),lty = c(1,1))

#j

set.seed(1)
x=rnorm(100)
eps=rnorm(100,0,0.5)
y=-1+0.5*x+eps
model_original=lm(y~x)
confint(model_original)


eps2=rnorm(100,0,0.02)
y2=-1+0.5*x+eps2
model_noise=lm(y2~x)
confint(model_noise)


eps3=rnorm(100,0,0.9)
y3=-1+0.5*x+eps3
model_less_noise=lm(y3~x)
confint(model_less_noise)
