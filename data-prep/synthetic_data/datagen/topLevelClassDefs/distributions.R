library(methods)
#source("distributionClassDeclarations.R")  # in same directory
# Usage examples:
# > a = new("normalDistribution", numDims=2, mean=c(10,15))
# > generateItem(a)
# > scoreItem(a, data=c(1,-2))  # also works on matrices in which each row is a point
# > a = estimateDistribution(a, cbind(obsx, obsy))

# virtual class "distribution"
# Since our initialization is more than just setting fields, we use an initialize() function
# instead of prototype().
#setClass("distribution", 
#         representation(description="character", numDims="numeric", "VIRTUAL"))

setMethod("initialize", "distribution", 
          function(.Object, numDims=2) {
              .Object@numDims = numDims
              return(.Object)
          }
)

# here are the functions we'd like distributions to implement
setGeneric("scoreItem", function(obj, data, returnLogScore=FALSE) { standardGeneric("scoreItem") })
setGeneric("generateItem", function(obj, numPoints=1, ...) standardGeneric("generateItem"))
setGeneric("estimateDistribution", function(obj, data) standardGeneric("estimateDistribution"))

# Make this method work whether the input is a single item (the usual), a vector, or a list
setMethod("scoreItem", 
          signature=signature(obj="distribution", data="list"),
          function(obj, data, returnLogScore) {
              scores = vector(length=length(data))
              for (i in 1:length(data)) {
                  score = scoreItem(obj, data[[i]], returnLogScore)
                  scores[i] = score
              }
              return(scores)
          }
)

# Some classes will let us send the log flag further into R. For the others, take the log here.
setMethod("scoreItem", "distribution",
          function(obj, data, returnLogScore) {
              regularScore = scoreItem(obj, data)
              if (returnLogScore) {
                  return(log(regularScore))
              } else {
                  return(regularScore)
              }
          }
)


