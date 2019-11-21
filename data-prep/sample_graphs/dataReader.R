
# Routines to read data from outside sources

library(Matrix)
library(hash)
library(stringi)

# returns a binary Matrix in which each record is a row
# (if binary=FALSE, this gives word counts per doc)
# If we want to read more data at once, pass around the docs as hashes, then change them 
# into a Matrix only once we've seen all the data.
# example: 
# pathToData = "/Users/lfriedl/Documents/dissertation/real-data/20_newsgroup"
# whichGroup = "sci.med"
readOneNewsGroup = function(pathToData, whichGroup, binary=TRUE, maxFiles=NULL) {
    # list files
    filesInDir = list.files(path=paste(pathToData, whichGroup, sep="/"))  # skips dirs and hidden files
    if (! is.null(maxFiles)) {
	filesInDir = filesInDir[1:min(maxFiles, length(filesInDir))]
    }
    
    # make hash to store vocabulary 
    vocab = hash()  # useful fns: x$a = 3, keys(x), values(x) -- just like Perl (by intent)
    docs = list()   # each doc will be its own hash
    
    tic=proc.time()[3]
    # for each file
    for (fileNum in 1:length(filesInDir)) {
        #print(paste("reading file #", fileNum, sep=""))
        file = filesInDir[fileNum]
    
        # open it, read text
        text = scan(paste(pathToData, whichGroup, file, sep="/"), character(0), sep = "\n", 
                    blank.lines.skip = FALSE)
        
        # trim off headers -- everything before "Lines:". 
        # Now instead: everything to the first blank link.
        textBegins = 0  # initialize
        for (i in 1:length(text)) {  # i = line num
            #if (grepl("^Lines:", text[i])) {
            if (nchar(text[i]) == 0 && nchar(text[i+1]) > 0) {
                textBegins = i + 1
                break
            }
        }
        
        # store doc as a hash of word counts
        thisDoc = hash()
        
        # splits on spaces, puts in lower case, then removes punctuation
        words = gsub("[[:punct:]]", "", tolower(
                    unlist(strsplit(text[textBegins:length(text)], split="[[:space:]]+"))))
        words = words[grepl(".", words)]   # remove empty array entries 
        for (word in words) {
            if (is.null(thisDoc[[word]])) {
                thisDoc[[word]] = 1
            } else {
                thisDoc[[word]] = thisDoc[[word]] + 1
            }
            
            # make sure it's in the overall vocabulary
            vocab[[word]] = 1
        }
        docs[[fileNum]] = thisDoc
        
    }    
	print(paste("read all files into hashes; time = ", proc.time()[3] - tic, "secs in subroutine"))

    names(docs) = filesInDir
    retMat = changeDocHashesToMatrix(docs, vocab, binary)

    # return matrix
    return(retMat)
}

# Input params: docs = list of hashes, whose names will be the matrix rownames. Hashes contain word-->count.
#               vocab = hash containing all words-->1.
#               binary: is matrix binary?
changeDocHashesToMatrix = function(docs, vocab, binary=T) {  
    
    # pick an ordering to the vocab (via the hash). keys(hash) is sorted already.
    allWords = keys(vocab)
    
    # change all file hashes into Matrix rows
    retMat = Matrix(0, nrow=length(docs), ncol=length(allWords), dimnames=list(names(docs), allWords))
    for (fileNum in 1:length(docs)) {
        wordsThisDoc = keys(docs[[fileNum]])
        positionsOfWords = match(wordsThisDoc, allWords)
        # I think values(docs[[fileNum]]) would be identical, but just to be on the safe side:
        countsOfWords = values(docs[[fileNum]][wordsThisDoc])   
            
        # set the row
        if (binary) {
            retMat[fileNum, positionsOfWords] = rep(1, length(countsOfWords))
        } else {
            if (length(countsOfWords > 0)) {
                retMat[fileNum, positionsOfWords] = countsOfWords
            }
        }        
    }
    
    # return matrix
    return(retMat)
}


# Returns a subset of allItems in which the first 2 * numPairsWanted rows are the pairs, and the rest
# are the singletons.
# Flag addExtraSingletonsAsNeeded will compensate if we can't get as many positive pairs as desired.
# If it's active, we also return the numPositivePairs sampled.
sampleNewsgroupData = function(allItems, truePositivesFile, numPairsWanted, numSingletonsWanted,
                               addExtraSingletonsAsNeeded = F) {
    
    okRowsToAdd = rep(T, nrow(allItems))     # we'll mark these false as we exclude them
    posPairOptions = read.table(truePositivesFile)
    posRowsChosen = c()
    negRowsChosen = c()
    
    # Positives! Until we have enough: pick a row. If both items are ok, take both and exclude all their pairs.
    samplingOrderPosOptions = sample(1:nrow(posPairOptions), nrow(posPairOptions))  # random permutation of row nums
    for (i in 1:length(samplingOrderPosOptions)) {
        # (note to self: if numPairsWanted == 0, we'll actually enter this loop, but break right out)
        if (length(posRowsChosen) >= numPairsWanted * 2) {
            break
        }
        
        # Consider row samplingOrderPosOptions[i]
        article1Id = as.character(posPairOptions[samplingOrderPosOptions[i], 1])     # need as.character() for congress row ids. 
        article2Id = as.character(posPairOptions[samplingOrderPosOptions[i], 2])     # (it doesn't break NGs)
        # convert article IDs to row nums of allItems
        pairRowNums = which(rownames(allItems) %in% c(article1Id, article2Id))
        
        # if both rows are still available
        if (sum(okRowsToAdd[pairRowNums]) == 2) {
            # take them, mark them as taken
            posRowsChosen = c(posRowsChosen, pairRowNums)
            okRowsToAdd[pairRowNums] = c(F, F)
            
            # find out which rows are paired with each of these
            pairsOf1 = getRowIdsForNGPairsOf(pairRowNums[1], allItems, posPairOptions)
            pairsOf2 = getRowIdsForNGPairsOf(pairRowNums[2], allItems, posPairOptions)
            okRowsToAdd[pairsOf1] = rep(F, length(pairsOf1))
            okRowsToAdd[pairsOf2] = rep(F, length(pairsOf2))
        }
    }
    print(paste("Sampled", length(posRowsChosen), "items in positive pairs"))
    
    # Negatives! Until we have enough: pick a row. If it's available, mark as taken and exclude all its pairs.
    if (addExtraSingletonsAsNeeded) {
        numSingletonsWanted = numSingletonsWanted + (2*numPairsWanted - length(posRowsChosen))
    }
    samplingOrderNegOptions = sample(which(okRowsToAdd), sum(okRowsToAdd)) # random permutation of remaining data items
    for (i in 1:length(samplingOrderNegOptions)) {
        if (length(negRowsChosen) >= numSingletonsWanted) {
            break
        }
        # consider item samplingOrderNegOptions[i]
        rowNum = samplingOrderNegOptions[i]
        if (okRowsToAdd[rowNum]) {
            # take it and mark as taken
            negRowsChosen = c(negRowsChosen, rowNum)
            okRowsToAdd[rowNum] = F
            
            # mark all of its pairs as unavailable
            itsPairs = getRowIdsForNGPairsOf(rowNum, allItems, posPairOptions)
            okRowsToAdd[itsPairs] = rep(F, length(itsPairs))
        }
    }
    print(paste("Sampled", length(negRowsChosen), "singletons"))
    
    dataSample = allItems[c(posRowsChosen, negRowsChosen),]
    if (addExtraSingletonsAsNeeded) {
        return(list(data=dataSample, numPairsSampled=length(posRowsChosen)/2))
    } else {
        return(dataSample)
    }
    
}

# input: rowNum
# work: map it to an article ID (stored in the rowname), get its pairs, map them to other rowNums
# output: paired rowNums (to exclude)
getRowIdsForNGPairsOf = function(rowIndexInData, allItems, posPairOptions) {
    articleId = rownames(allItems)[rowIndexInData]
    # fix so that this works with Congress data too:
    if (is.factor(articleId)) {
        articleId = as.character(articleId)
    }
    
    # look for matches in both columns 
    # (These sets won't intersect because one is strictly smaller, the other strictly larger, than articleId.)
    pairedArticles1 = posPairOptions[(posPairOptions[,1] == articleId),2]
    pairedArticles2 = posPairOptions[(posPairOptions[,2] == articleId),1]
    if (is.factor(pairedArticles1)) {
        pairedArticles1 = as.character(pairedArticles1)
        pairedArticles2 = as.character(pairedArticles2)
    }
    
    # map back to rowNums
    rowNums= which(rownames(allItems) %in% c(pairedArticles1, pairedArticles2))
    return(rowNums)
    
}

# returns a Matrix of all data items, and a vector of person ids (would have been rownames, 
# except they're not unique).
# Note: some affils have been changed from strings to scientific notation (e.g., 8.79648109672e+12). This was already 
# the case in the stored data files.
readAllRealityData = function(pathToData) {
    tic=proc.time()[3]
    
    filesInDir = list.files(path=pathToData)
    
    # make hash to store affils
    allAffilsHash = hash()  # if x = hash(), can write x$a = 3, keys(x), values(x) -- just like Perl (by intent)
    allItems = list()   # each item will have its own hash
    allRowIds = c()
    allPersonIds = c()
    
    # for each file
    itemCounter = 0
    for (file in filesInDir) {
        #print(paste("reading file", file))
        
        # open it, read text. (Using read.delim b/c read.table had trouble...)
        fileData = read.delim(paste(pathToData, file, sep="/"), header=T, colClasses=c("integer", "character", "character"))
        affils = strsplit(fileData$affils, ",")     # returns a list, with one entry per row
        # rowids aren't strictly necessary any more, but will use them as rownames for matrix
        rowids = paste(fileData$person, fileData$date, sep="_")
        
        # save each data row--at this point, save the hash for this item into the list allItems, 
        # and the row id into the vector allRowIds. While storing all affils ever seen into allAffilsHash.
        for (lineNum in 1:nrow(fileData)) {
            
            # store item as a hash of affils
            thisItem= hash()
            itemCounter = itemCounter + 1
            
            for (affil in affils[[lineNum]]) {
                thisItem[[affil]] = 1
                # make sure it's in the overall vocabulary
                allAffilsHash[[affil]] = 1
            }
            allItems[[itemCounter]] = thisItem
            
            allRowIds = c(allRowIds, rowids[lineNum])
        }
        allPersonIds = c(allPersonIds, fileData$person)
    }
    
    print(paste("read", itemCounter, "items into hashes; time = ", proc.time()[3] - tic, "secs in subroutine"))
    
    # once all items are read, pick an ordering to the vocab (via the hash). keys(hash) is sorted already.
    allAffils = keys(allAffilsHash)
    
    # change all item hashes into Matrix rows
    dataMat = Matrix(0, nrow=itemCounter, ncol=length(allAffils), dimnames=list(allRowIds, allAffils))
    for (itemNum in 1:itemCounter) {
        affilsThisDoc = keys(allItems[[itemNum]])
        positionsOfAffils = match(affilsThisDoc, allAffils)     
        
        dataMat[itemNum, positionsOfAffils] = rep(1, length(affilsThisDoc))
    }
    print(paste("transformed hashes into a Matrix; time = ", proc.time()[3] - tic, "secs in subroutine"))
    
    return(list(dataMatrix=dataMat, personIds=allPersonIds))
}


# Returns a Matrix in which the first 2 * numPairsWanted rows are the pairs, and the rest
# are the singletons.
sampleRealityMiningDataFromMatrix = function(allItems, personIds, numPairsWanted, numSingletonsWanted) {

    # Choose files that will constitute true pairs
    distinctPeople = unique(personIds)
    pairPersonIds = sort(sample(distinctPeople, numPairsWanted))
    # Choose files that will constitute singletons
    singletonPersonOptions = (distinctPeople)[! (distinctPeople %in% pairPersonIds)]
    singletonPersonIds = sort(sample(singletonPersonOptions, numSingletonsWanted))
    
    # choose actual rownums that will constitute pairs and singletons
    posRowsChosen = c()
    negRowsChosen = c()
    for (pairPerson in pairPersonIds) {
        pairRowIds = sample(x = which(personIds==pairPerson), size=2)
        posRowsChosen = c(posRowsChosen, pairRowIds)
    }
    for (singPerson in singletonPersonIds) {
        singRowId = sample(x = which(personIds==singPerson), size=1)
        negRowsChosen = c(negRowsChosen, singRowId)
    }
    
    # Rearrange data from original matrix
    retMat = Matrix(0, nrow=2*numPairsWanted + numSingletonsWanted, ncol=ncol(allItems), 
                    dimnames=list(NULL, colnames(allItems)))
    origRowIds = rownames(allItems)
    # permute the rows
    if (length(posRowsChosen)) {
        retMat[1:length(posRowsChosen),] = allItems[posRowsChosen,]
    }
    if (length(negRowsChosen)) {
        retMat[(1 + length(posRowsChosen)):nrow(retMat),] = allItems[negRowsChosen,]
    }
    # fix the rownames (they need to be permuted the same way -- doesn't happen automatically)
    fixedRowIds = c(origRowIds[posRowsChosen], origRowIds[negRowsChosen])
    rownames(retMat) = fixedRowIds
    
    return(retMat)    
}

