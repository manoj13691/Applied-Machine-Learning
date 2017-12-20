#15.a begins*****
library(MASS)
attach(Boston)
fit_chas = lm(crim ~ chas)
summary(fit_chas)
#15.a ends*****


#15.b begins*****
fit_all = lm(crim ~ ., data = Boston)
summary(fit_all)
#15.b ends*****

#15.c begins*****
uni_reg  =  vector("numeric",0)
uni_reg  = c(uni_reg, lm(crim~zn,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~indus,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~chas,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~nox,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~rm,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~age,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~dis,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~rad,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~tax,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~ptratio,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~black,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~lstat,data=Boston)$coefficients[2])
uni_reg  = c(uni_reg, lm(crim~medv,data=Boston)$coefficients[2])
multi_reg = vector("numeric", 0)
multi_reg = c(multi_reg, lm(crim~.,data=Boston)$coefficients)
multi_reg = multi_reg[-1]
plot(uni_reg, multi_reg, col = "blue")
#15.c ends*****