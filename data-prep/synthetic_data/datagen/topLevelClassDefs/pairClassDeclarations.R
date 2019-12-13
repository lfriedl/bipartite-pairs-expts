library(methods)

# Moved these to a separate file to avoid errors that occur when a parent class's initialize()
# function is defined before the subclass is declared.

setClass("pairOfBinaryVectors",
         representation(obs1="sparseVector", obs2="sparseVector", numDims="numeric", m="sparseVector",
                        d1="sparseVector", d2="sparseVector"))

#---

setClass("negativePairDistribution", 
         representation(phi="distribution"),
         contains=c("distribution"))

setClass("negativeBinaryPairDistribution", 
         representation(phi="binaryVectorDistribution"),
         contains=c("negativePairDistribution"))

#---

setClass("positiveBinaryPairDistribution5", 
         representation(singleton_phi="multivariateBernoulli", t="numeric"),
         contains=c("distribution"))

