#######################
#####SUPER LEARNER#####
#######################

install.packages("SuperLearner")
library(SuperLearner)

##Generate simulated data##

set.seed(27)
n<-500
data <- data.frame(W1=runif(n, min = .5, max = 1),
W2=runif(n, min = 0, max = 1),
W3=runif(n, min = .25, max = .75),
W4=runif(n, min = 0, max = 1))
data <- transform(data, #add W5 dependent on W2, W3
W5=rbinom(n, 1, 1/(1+exp(1.5*W2-W3))))
data <- transform(data, #add Y
Y=rbinom(n, 1,1/(1+exp(-(-.2*W5-2*W1+4*W5*W1-1.5*W2+sin(W4))))))


##Specify a library of algorithms##

SL.library <- c("SL.glm", "SL.mean", "SL.randomForest", "SL.glmnet")


##Run the super learner to obtain final predicted values for the super learner as well as CV risk for algorithms in the library##

set.seed(27)
fit.data.SL<-SuperLearner(Y=data[,6],X=data[,1:5],
	SL.library=SL.library, family=binomial(),
	method="method.NNLS", verbose=TRUE)


##Run the cross-validated super learner to obtain its CV risk##

set.seed(27)
fitSL.data.CV <- CV.SuperLearner(Y=data[,6],X=data[,1:5], V=10, 
	SL.library=SL.library, verbose = TRUE, 
	method = "method.NNLS", family = binomial())


##Cross-validated risks##

#CV risk for super learner
mean((data[,6]-fitSL.data.CV$SL.predict)^2) 

#CV risks for algorithms in the library
fit.data.SL 



#######################
#####     TMLE    #####
#######################


##Code lightly adapted from Schuler & Rose, 2017, AJE## 
library(tmle)
set.seed(1)
N <- 1000

##Generate simulated data##

#X1=Gender; X2=Therapy; X3=Antidepressant use
X1 <- rbinom(N, 1, prob=.55)
X2 <- rbinom(N, 1, prob=.30)
X3 <- rbinom(N, 1, prob=.25)
W <- cbind(X1,X2,X3)

#Exposure=regular physical exercise
A <- rbinom(N, 1, plogis(-0.5 + 0.75*X1 + 1*X2 + 1.5*X3))

#Outcome=CES-D score 
Y <- 24 - 3*A + 3*X1 -4*X2 - 6*X3 - 1.5*A*X3 + rnorm(N,mean=0,sd=4.5) 

##Specify a library of algorithms##

SL.library <- c("SL.glm","SL.step.interaction","SL.glmnet", 
     "SL.randomForest","SL.gam","SL.rpart" )


##TMLE approach: Super Learning##

tmleSL1 <- tmle(Y, A, W, Q.SL.library = SL.library, g.SL.library = SL.library)
tmleSL1


##TMLE approach: GLM, MT misspecification of outcome##
#Misspecified outcome regression:  Y ~ A + X1 + X2 + X3#
 
tmleGLM1 <- tmle(Y, A, W, Qform=Y~A+X1+X2+X3, gform=A~X1+X2+X3)
tmleGLM1


##TMLE approach: GLM, OV misspecification of outcome##
#Misspecified outcome regression:  Y ~ A + X1 + X2# 
  
tmleGLM2 <- tmle(Y, A, W, Qform=Y~A+X1+X2, gform=A~X1+X2+X3)
tmleGLM2


##TMLE approach: GLM, OV misspecification of exposure##
#Misspecified exposure regression:  A ~ X1 + X2# 

tmleGLM3 <- tmle(Y, A, W, Qform=Y~A+X1+X2+X3+A:X3, gform=A~X1+X2)
tmleGLM3
  
  

