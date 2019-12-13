library(methods)
library(Matrix)  # for sparse representations


# Usage examples:
# > a = new("multivariateBernoulli", componentProbs=c(.5, .7, 1.3))
# > generateItem(a)
# > scoreItem(a, data=c(1,-2))  # also works on matrices in which each row is a point
# Test that the frequencies came out as expected
# > apply(generateItem(a, 5000), 2, mean) 
# > mean(apply(generateItem(a, 5000), 1, sum))


# This type of distribution will inherit from the plain distribution
#setClass("distribution", 
#          representation(description="character", numDims="numeric", "VIRTUAL"))

# class "binaryVectorDistribution"
#setClass("binaryVectorDistribution", 
#         representation("VIRTUAL"),
#         contains=c("distribution"))

# class "multivariateBernoulli"
#setClass("multivariateBernoulli", 
#         representation(componentProbs="vector", expNum1s="numeric"),
#         contains=c("binaryVectorDistribution"))

# initialization for multivariateBernoulli
setMethod("initialize", "multivariateBernoulli", 
          # note: default expNum1s used to be 1.
          function(.Object, componentProbs=NULL, numDims=2) {
              #print("hi, initializing a multivariateBernoulli now")
              if (is.null(componentProbs)) {
                  # default: all .5's, so as not to favor 0 vs. 1
                  componentProbs = rep(.5, numDims) 
              }
              .Object@numDims = length(componentProbs)
              .Object@componentProbs = as(componentProbs, "sparseVector")

			  .Object@expNum1s = sum(.Object@componentProbs)
                                          
              return(.Object)
          }
)

setMethod("scoreItem", "multivariateBernoulli", 
          function(obj, data, returnLogScore) {
              
              # Must be a matrix to use apply() below
              if (is.null(dim(data))) {     # a vector or a sparseVector
                  data = t(as.matrix(as.vector(data)))
              }
              #print("I'm a multivariateBernoulli looking at some data to score")
              #print(paste("ncol(data)=", ncol(data), ", and length(data)=", length(data)))
              #print(paste("my log variable is: ", returnLogScore))
              if (sum(data == 1 | data == 0) != prod(dim(data))) {
                  warning("in multivariateBernoulli.scoreItem, not all data elements were binary")
              }
              if (returnLogScore) {
                  # need as.vector because otherwise we have to explicitly convert some things back and forth 
                  # (both ways) inside the function
                  probs = apply(data, 1, logScoreVectorBernoulli, as.vector(obj@componentProbs))
              } else {
                  probs = apply(data, 1, scoreVectorBernoulli, obj@componentProbs)
              }                  
              return(probs)
          }
)

scoreVectorBernoulli = function(binaryVector, componentProbs) {
    return(prod(componentProbs**binaryVector * (1 - componentProbs)**(1-binaryVector)))
}

logScoreVectorBernoulli = function(binaryVector, componentProbs) {
    # we get NaNs if we try to take the log of componentProbs that are 0
    # In non-log version, it worked out because 0**0 is defined as 1, but here we get 0 * log(0) = NaN

    # fix: remove those elements from the vectors first. They would not change the score (when done right) anyway--they're 0's.
    # We'll remove elements as long as they're zero in both vectors. If componentProb==0 but binaryVector does not,
    # then an NaN is actually correct.
    binaryVector2 = binaryVector[binaryVector>0 | componentProbs>0]
    componentProbs2 = componentProbs[binaryVector>0 | componentProbs>0]
    
    # similarly, the reverse problem for the (1-x) part
    binaryVector3 = binaryVector2[(1-binaryVector2)>0 | (1-componentProbs2)>0]
    componentProbs3 = componentProbs2[(1-binaryVector2)>0 | (1-componentProbs2)>0]
    
    return(sum(binaryVector3 * log(as.vector(componentProbs3)) + (1-binaryVector3) * log(1 - as.vector(componentProbs3))))
}

setGeneric("scaleExpNum1s", function(obj, newExpNum1s, ...) { standardGeneric("scaleExpNum1s") })
setMethod("scaleExpNum1s", "multivariateBernoulli",
          function(obj, newExpNum1s) {
              #if (newExpNum1s >= obj@numDims) {
              #    warning("in multivariateBernoulli.scaleExpNum1s, newExpNum1s >= numDims. Will get all 1's.")
              #    newExpNum1s = obj@numDims
              #}
              
              # more stringent constraint: make sure the relative weights are always respected
              if (max(obj@componentProbs * newExpNum1s / sum(obj@componentProbs)) > 1) {
                  warning("in multivariateBernoulli.scaleExpNum1s, newExpNum1s was too high. Can't do it.")
                  return(obj)
              }
              
              currSum = sum(obj@componentProbs)
              obj@componentProbs = obj@componentProbs * (newExpNum1s / currSum)
              obj@expNum1s = newExpNum1s
              return(obj)
          }
)

setGeneric("zeroOutEntries", function(obj, binaryVectorToZero) { standardGeneric("zeroOutEntries") })
setMethod("zeroOutEntries", "multivariateBernoulli",
          function(obj, binaryVectorToZero) {
              obj@componentProbs[which(binaryVectorToZero==1)] = 0
              obj@expNum1s = sum(obj@componentProbs)
              return(obj)
          }
)

setMethod("generateItem", "multivariateBernoulli", 
          function(obj, numPoints=1) {
              # for one point:
              #sampledProbs = runif(length(obj@componentProbs))
              #value = as.integer(sampledProbs < obj@componentProbs)
              
              # for multiple points
              sampledProbs = matrix(runif(length(obj@componentProbs) * numPoints), nrow=numPoints)
              #keepElement = sampledProbs < matrix(rep(obj@componentProbs, numPoints), nrow=numPoints, byrow=T)
              #mode(keepElement) = "integer"
              # using sparse vectors & matrices
              keepElement = sampledProbs < 
                            Matrix(rep(obj@componentProbs, numPoints), nrow=numPoints, 
                                   ncol=length(obj@componentProbs), byrow=T)
              # make sure it ends up numeric, not logical
              keepElement = as(keepElement, "dMatrix")

              return(keepElement)
          }
)

# Utility function needed for certain operations involving lists of things with sparseVectors/Matrices inside

# from stackoverflow: http://stackoverflow.com/questions/8843700/creating-sparse-matrix-from-a-list-of-sparse-vectors
vectorList2Matrix<-function(vectorList){
    nzCount<-lapply(vectorList, function(x) length(x@i));  # fixed typo. Length of each sparseVector
    nz<-sum(do.call(rbind,nzCount));                       # total number of (non-zero) entries in sparse vectors
    r<-vector(mode="integer",length=nz);
    c<-vector(mode="integer",length=nz);
    v<-vector(mode="integer",length=nz);
    ind<-1;
    for(i in 1:length(vectorList)){
        ln<-length(vectorList[[i]]@i);
        if(ln>0){
            r[ind:(ind+ln-1)]<-i;
            c[ind:(ind+ln-1)]<-vectorList[[i]]@i         # why did it say @j+1 ? I think @i seems right.
            v[ind:(ind+ln-1)]<-vectorList[[i]]@x
            ind<-ind+ln;
        }
    }
    return (sparseMatrix(i=r,j=c,x=v, dims=c(length(vectorList), vectorList[[i]]@length)));
}
