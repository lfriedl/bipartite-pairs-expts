# Uses data downloaded from https://snap.stanford.edu/data/loc-Brightkite.html in Sept 2015

library(data.table)
df = fread("loc-brightkite_totalCheckins.txt")

# I'll use every location people ever check into (one or more times)
# Construct rows of adjacency matrix:
byrow = df[, .(checkins = list(unique(V5))), by=V1]
fwrite(byrow, file="bipartite_adj.txt")




##############
# some stats
#> length(unique(df$V1))
#[1] 51406              --> number of people. Note that the webpage's 58228 matches the number in the edges file.
#> length(unique(df$V5))
#[1] 772967             --> number of affils
#> nrow(df)
#[1] 4747287    --> checkins in the data (!= the 4,491,143 the webpage says)
#> nrow(unique(df))
#[1] 4702733    --> after removing duplicates

# note that a person may hit each location multiple times
#> stats = df[, .(cnt=.N, cntUniq = length(unique(V5))), by=.(V1)]
#> head(stats)
#   V1  cnt cntUniq
#1:  0 2100     623
#2:  1 1210     112
#3:  2 2100     495
#4:  3 1807     829
#5:  4  779     229
#6:  5  407      81
#> summary(stats$cnt / stats$cntUniq)                   --> how many times people check in to each place (loosely -- summary)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   1.000    1.221    2.000   14.870    3.250 2100.000 
##############


# Ditto for gowalla, from https://snap.stanford.edu/data/loc-gowalla.html
df = fread("loc-gowalla_totalCheckins.txt")
byrow = df[, .(checkins = list(unique(V5))), by=V1]
fwrite(byrow, file="bipartite_adj.txt")

##############
# gowalla stats
#> length(unique(df$V1))
#[1] 107092              --> number of people
#> length(unique(df$V5))
#[1] 1280969             --> number of affils
#> nrow(df)
#[1] 6442892    --> checkins in the data (only differs from the webpage by 2)
#> nrow(unique(df))
#[1] 6442291    --> after removing duplicates
##############
