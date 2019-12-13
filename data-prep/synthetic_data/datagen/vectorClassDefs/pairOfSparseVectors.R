library(methods)
library(Matrix)

# note: pairOfBinaryVectors is the base class. Might have been more apt to call it pairOfSparseVectors.
# Child class, pairOfCountVectors, extends it by also storing counts in separate fields.

setMethod("initialize", "pairOfBinaryVectors", 
          function(.Object, obs1, obs2) {
              .Object@obs1 = as(obs1, "sparseVector")
              .Object@obs2 = as(obs2, "sparseVector")
              # compute numDims; check it's same obs1 and 2
              numDims = length(obs1)
              if (length(obs2) != numDims) {
                  warning("obs1 and obs2 weren't of same length")
              }
              .Object@numDims = 2 * numDims
              
              .Object@m = as(as.numeric(.Object@obs1 & .Object@obs2), "sparseVector")  # bit-wise AND
              .Object@d1 = as(.Object@obs1 - .Object@m, "sparseVector")
              .Object@d2 = as(.Object@obs2 - .Object@m, "sparseVector")
              return(.Object)
          }
)

