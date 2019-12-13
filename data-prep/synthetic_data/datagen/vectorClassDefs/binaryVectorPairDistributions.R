library(methods)
library(Matrix)

# File contains 2 classes:
# negativeBinaryPairDistribution is a subclass of negativePairDistribution
# positiveBinaryPairDistribution5

# example usage:
# a = new("multivariateBernoulli", componentProbs=c(.1, .1, .1, .1, .1))
# negDist = new("negativeBinaryPairDistribution", phi=a)
# posDist = new("positiveBinaryPairDistribution5", t=.5)


# class negativeBinaryPairDistribution
#setClass("negativeBinaryPairDistribution", 
 #        representation(phi="binaryVectorDistribution"),
  #       contains=c("negativePairDistribution"))
# can use parent class's initialize(), scoreItem(), generateItem(). Except generateItem() now needs to return a pairOfBinaryVectors:

setMethod("generateItem", "negativeBinaryPairDistribution", 
          function(obj, numPoints=1) {
              callNextMethod(obj, numPoints, typeOfPair="pairOfBinaryVectors")
          }
)

# class positiveBinaryPairDistribution5
# Mixture model: obs2 ~ t (obs1) + (1 - t) phi
# P(draw2 = 1 | draw1 = 1) = t + (1 - t) p_i
# P(draw2 = 1 | draw1 = 0) = (1 - t) p_i
# As above, no need to randomize the assignment of 0/1 vs. 1/0's, at least as long as phi's components are independently assigned.
# That means draw1 is always obs1. As opposed to randomly deciding, for each component with 0/1 or 1/0, which vector gets the 1.
# (Technically, we should randomize whether draw1 becomes obs1 vs. obs2. But the point is that draw1 gets taken, wholesale,
# to be one of the observations. The math at inference doesn't need to decide which is which.)
setMethod("initialize", "positiveBinaryPairDistribution5", 
          function(.Object, singleton_phi, t, ...) {
              .Object@singleton_phi = singleton_phi
              .Object@numDims = 2 * .Object@singleton_phi@numDims              
              .Object@t = t              
              return(.Object)
          }
)

# note: returns a list, not a vector, of pair[s]OfPoints
setMethod("generateItem", "positiveBinaryPairDistribution5", 
          function(obj, numPoints=1) {
              
              pairs = vector("list", length=numPoints)
              for (i in 1:numPoints) {
                  
                  # 1. Draw full vector from singleton_phi
                  draw1 = as.vector(generateItem(obj@singleton_phi))
                  
                  # 2. Draw to find out which will be the 0/0, 1/1, or 1/0 / 0/1's.
                  #    P(1 | draw1 = 1) = t + (1 - t) p_i
                  #    P(1 | draw1 = 0) = (1 - t) p_i
                  draw2_rands = runif(length(draw1))
                  # sparse vectors wreak havoc when subsetting below, if not converted now
                  draw2_cutoffs = as.vector(obj@t + (1 - obj@t) * obj@singleton_phi@componentProbs)  # cutoff to compare flips_rands to when draw1 == 1
                  draw2_cutoffs0s = as.vector((1 - obj@t) * obj@singleton_phi@componentProbs)
                  draw2_cutoffs[draw1 == 0] = draw2_cutoffs0s[draw1 == 0]
                  
                  draw2 = as.numeric(draw2_rands < draw2_cutoffs)         
                  
                  obs1 = draw1
                  obs2 = draw2
                  
                  pairOfVectors = new("pairOfBinaryVectors", obs1=obs1, obs2=obs2)
                  
                  pairs[[i]] = pairOfVectors
              }
              return(pairs)
          }
)

# semi-synthetic data: given one item, generate a pair for it, using the pure mixture model.
# For each element, copy with probability t, or generate newly with probability (1-t).
setGeneric("generatePairFromItem", function(obj, item1, ...) { standardGeneric("generatePairFromItem") })
setMethod("generatePairFromItem", "positiveBinaryPairDistribution5",
          function(obj, item1) {
              
              # check that item1 is compatible with phi
              if (obj@singleton_phi@numDims != length(item1)) {
                  warning("generatePairFromItem: item1 is a different dimension than phi")
                  return(NULL)
              }
              
              itemFromPhi = as.vector(generateItem(obj@singleton_phi))
              whichToKeep = (runif(length(item1)) <= obj@t)
              item2 = as.vector(item1)
              item2[!whichToKeep] = itemFromPhi[!whichToKeep]
              return(new("pairOfBinaryVectors", obs1=item1, obs2=item2))
          }
)


setMethod("scoreItem", 
          signature=signature(obj="positiveBinaryPairDistribution5", data="pairOfBinaryVectors"),
          function(obj, data, returnLogScore) {
              #print("I'm a positiveBinaryPairDistribution5 looking at a pairOfBinaryVectors to score")
              
              # P(obs1, obs2) = product, over each component: 
              #                     p_i (t + (1 - t) p_i) if 1/1 (p_i = obj@singleton_phi@componentProbs[i])
              #                     (1 - p_i) (1 - p_i + p_i t) if 0/0
              #                     p_i (1 - p_i) (1 - t) if 1/0 or 0/1
              
              # Not seeing any clever dot products or helper functions here. Just create vector 
              # of components, then take (log and sum) or product.
              #positions_11 = (data@obs1 == 1 & data@obs1 == data@obs2)     # unnecessary to compute
              positions_10 = as.vector(data@obs1 != data@obs2)
              positions_00 = as.vector(data@obs1 == 0 & data@obs1 == data@obs2)
              
              # sparse vectors wreak havoc when subsetting below, if not converted now
              terms_for_11 = as.vector(obj@singleton_phi@componentProbs * (obj@t + (1 - obj@t) * obj@singleton_phi@componentProbs))
              terms_for_10 = as.vector(obj@singleton_phi@componentProbs * (1 - obj@singleton_phi@componentProbs) * (1 - obj@t))

			  # bug fixed 6/24/2014:
              #terms_for_00 = as.vector((1 - obj@singleton_phi@componentProbs) * (1 - obj@singleton_phi@componentProbs - obj@singleton_phi@componentProbs * obj@t))
              terms_for_00 = as.vector((1 - obj@singleton_phi@componentProbs) * (1 - obj@singleton_phi@componentProbs + obj@singleton_phi@componentProbs * obj@t))
              
              all_terms = terms_for_11
              all_terms[positions_10] = terms_for_10[positions_10]
              all_terms[positions_00] = terms_for_00[positions_00]
              
              if (returnLogScore) {
                  retVal = sum(log(all_terms))
              }
              else {
                  retVal = prod(all_terms)
              }
              
              return(retVal)
          }
)

