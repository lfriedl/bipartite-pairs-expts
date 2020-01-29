# Parameters for the synthetic data settings

The parameters were generated using [`params/createPiVectorsForExpts.R`](params/createPiVectorsForExpts.R). The other two files are the resulting parameters, saved in two formats.

# Generating synthetic data

Call the function [`datagen/prepSynthData.R`](datagen/prepSynthData.R):`genSynthData()`. E.g., from the R prompt:

```
load("../params/pi_vectors_forExpts.Rdata")
source("prepSynthData.R")
genSynthData(pi_vector=pi_vectors[69,], numPositivePairs=1, constructAllPairsFromMDocs=10, 
        tForModel5=.2, outputDir="example_data", outfileNum=2)
```

(The other `.R` files under `datagen` are class definitions. These are remnants of my previous codebase in R.)
