#14.a begins*****
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
#14.a ends*****

#14.b begins*****
cor(x1, x2)
plot(x1,x2)
#14.b ends*****

#14.c begins*****
model = lm(y~x1+x2)
model
summary(model)
#14.c ends*****

#14.d begins*****
model= lm(y ~ x1)
model
summary(model)
#14.d ends*****

#14.e begins*****
model= lm(y ~ x2)
model
summary(model)
#14.e ends*****

#14.g begins*****
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y  = c(y, 6)

model1 = lm(y ~ x1 + x2)
model2 = lm(y ~ x1)
model3 = lm(y ~ x2)
summary(model1)
summary(model2)
summary(model3)
plot(model1)
plot(model2)
plot(model3)
#14.g ends*****