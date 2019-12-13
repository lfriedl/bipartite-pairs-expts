
library(Matrix)
source("reloadClassDefns.R")

# note: in this version, constructAllPairsFromMDocs must be specified as a number.
# Doing stuff in a roundabout manner (construct pairs, then extract points from it) so as to re-use tried and true code.
genSynthData = function(pi_vector, numPositivePairs, constructAllPairsFromMDocs=0, tForModel5, 
                    outputDir, outfileNum=0) {
    
    # create phi
    phi = new("multivariateBernoulli", componentProbs=pi_vector)
    
    # housekeeping
    tic=proc.time()[3]
    print(paste("generating synthetic data with params: pi_vector of length", length(pi_vector), 
                ", t =", tForModel5, ",", numPositivePairs, "positives"))
    
    # generate labeled data: 
    # some singletons and some positives --> all pairs from that set of points
    
    # create positive distribution object and pairs from it
    posDist = new("positiveBinaryPairDistribution5", singleton_phi=phi, t=tForModel5)
    listOfPosPairs = generateItem(posDist, numPositivePairs)
    
    # create negative pairs
    # assemble lists of all pairs and labels
    if (constructAllPairsFromMDocs) {
        # contains components $listOfPairs and $labels (can be 1, 0, or -1)
        pairsAndLabels = constructNegPairs(phi, constructAllPairsFromMDocs, listOfPosPairs) 
        if (is.null(pairsAndLabels)) {
            return()
        }
        
        listOfPairsToScore = pairsAndLabels$listOfPairs
        trueLabels = pairsAndLabels$labels
        trueLabels[trueLabels != 1] = rep(0, sum(trueLabels != 1))
        
    } else {
        warning("constructAllPairsFromMDocs must be specified")
        return()
    }
    
    print(paste("generated data; tot time = ", proc.time()[3] - tic, "secs"))
    
    # change listOfPairs into a matrix of dataItems, which is what we want to store
    dataItems = Matrix(0, nrow=constructAllPairsFromMDocs, ncol=length(listOfPairsToScore[[1]]@obs1))
    dataItems[1,] = listOfPairsToScore[[1]]@obs1
    for (i in 1:(constructAllPairsFromMDocs-1)) {
        dataItems[i+1,] = listOfPairsToScore[[i]]@obs2
    }        
    
    # save it!    
    save(dataItems, numPositivePairs, phi, tForModel5, 
         file=paste(outputDir, "/data", outfileNum, ".Rdata", sep=""))
}




# We want: a list of positive & negative pairs with labels. 
# Input is the positive pairs, phi and the total num docs wanted. Function finishes the rest.
# returns list with components $listOfPairs and $labels (distinguishing among types of negs)
constructNegPairs = function(phi, totNumPts, listOfPosPairs) {
    
    numPosPts = 2 * length(listOfPosPairs)
    numNegPts = totNumPts - numPosPts
    
    if (numNegPts <= 0) {
        warning("incompatible combo of MDocs and numPositivePairs")
        return()
    }
    
    # construct the singletons
    # This call returns a matrix, where each point is a row
    singletons = generateItem(phi, numNegPts)
    
    totNumPairs = totNumPts * (totNumPts - 1) / 2
    listOfPairs = vector("list", length=totNumPairs)
    labels = vector("numeric", length=totNumPairs)
    currIndex = 1
    
    # construct all pairs and labels.
    # main complexity: different cases depending on whether points i and j are part of existing pairs
    for (i in 1:(totNumPts-1)) {
        if (i > numPosPts) {
            pointI = singletons[i - numPosPts,]
        } else {
            pairIndexWithI = ceiling(i / 2)
            if (i %% 2) {
                pointI = listOfPosPairs[[pairIndexWithI]]@obs1
            } else {
                pointI = listOfPosPairs[[pairIndexWithI]]@obs2
            }
        }
        
        for (j in (i+1):totNumPts) {
            # construct the pair of points from (i, j)
            
            if (j > numPosPts) {
                pointJ = singletons[j - numPosPts,]
            } else {
                pairIndexWithJ = ceiling(j / 2)
                if (j %% 2) {
                    pointJ = listOfPosPairs[[pairIndexWithJ]]@obs1
                } else {
                    pointJ = listOfPosPairs[[pairIndexWithJ]]@obs2
                }
            }
            
            # construct pair
            # are they already a positive pair?
            if (i <= numPosPts && j <= numPosPts    # need this test b/c pairIndex variables don't go out of scope when invalid
                && pairIndexWithI == pairIndexWithJ) {
                newPair = listOfPosPairs[[pairIndexWithI]]
            } else {
                newPair = new("pairOfBinaryVectors", obs1=pointI, obs2=pointJ)
            }
            listOfPairs[[currIndex]] = newPair
            
            # construct label
            # 1 = positive, 0 = pure negative, -1 = non-modeled negative
            if (i <= numPosPts || j <= numPosPts) {
                if (i <= numPosPts && j <= numPosPts    # need this test b/c pairIndex variables don't go out of scope when invalid
                    && pairIndexWithI == pairIndexWithJ) {
                    newLabel = 1
                } else {
                    newLabel = -1
                }
            } else {
                newLabel = 0
            }
            labels[currIndex] = newLabel
            
            
            currIndex = currIndex + 1
        }
    }
    return(list(listOfPairs=listOfPairs, labels=labels))
    
}


    
