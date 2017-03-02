#debugging_practice.R
#170224
library(nnet)
X <- iris[,1:4]
Y <- iris[,5]
mod <- nnet(X,Y,size=2)