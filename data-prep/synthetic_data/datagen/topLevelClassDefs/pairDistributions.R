library(methods)

#setClass("pairOfBinaryVectors",
 #        representation(d1="vector", d2="vector"),
  #       contains="pairOfPoints")

# class negativePairDistribution
#setClass("negativePairDistribution", 
 #        representation(phi="distribution"),
  #       contains=c("distribution"))

setMethod("initialize", "negativePairDistribution", 
          function(.Object, phi) {
              .Object@phi = phi
              .Object@numDims = 2 * phi@numDims
              return(.Object)
          }
)


setMethod("scoreItem", 
          signature=signature(obj="negativePairDistribution", data="pairOfBinaryVectors"),
          function(obj, data, returnLogScore) {
              #print("I'm a negativePairDistribution looking at a pairOfBinaryVectors to score")
              if (! data@numDims == obj@numDims) {
                  warning("scoreItem.negativePairDistribution called with a pairOfBinaryVectors having a different number of dimensions")
                  return
              }
              score1 = scoreItem(obj@phi, data@obs1, returnLogScore)
              score2 = scoreItem(obj@phi, data@obs2, returnLogScore)
              if (returnLogScore) {
                  return(score1 + score2)
              } else {
                  return(score1 * score2)
              }
          }
)

setMethod("scoreItem", 
          signature=signature(obj="negativePairDistribution", data="list"),
          function(obj, data, returnLogScore) {
              #print("I'm a negativePairDistribution looking at a whole list of data to score")
              if (length(data) > 0 && !isClass("pairOfPoints", data[[1]])) {
                  warning("uh-oh. Received a list to score, but it didn't contain pair(s)OfPoints. Giving up.")
                  return
              }
              obs1s = mapply(function(obj) obj@obs1, data)
              obs2s = mapply(function(obj) obj@obs2, data)
              if (is.list(obs1s)) {      # special handling in case obj@obs1 were sparseVectors
                  obs1s = vectorList2Matrix(obs1s)
                  obs2s = vectorList2Matrix(obs2s)
              } else if (obj@numDims > 2) {
                  obs1s = t(obs1s)
                  obs2s = t(obs2s)
              }
              
              # do like when there's only 1 thing to score
              score1 = scoreItem(obj@phi, obs1s, returnLogScore)
              score2 = scoreItem(obj@phi, obs2s, returnLogScore)
              if (returnLogScore) {
                  return(score1 + score2)
              } else {
                  return(score1 * score2)
              }

          }
)


