library(methods)
library(Matrix)

setClass("distribution", 
         representation(description="character", numDims="numeric", "VIRTUAL"))

# binary vectors
setClass("binaryVectorDistribution", 
         representation("VIRTUAL"),
         contains=c("distribution"))

setClass("multivariateBernoulli", 
         representation(componentProbs="sparseVector", expNum1s="numeric"),
         contains=c("binaryVectorDistribution"))

