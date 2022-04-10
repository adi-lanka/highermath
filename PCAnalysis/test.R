COVID <- read.csv("COVIDcases.csv")
nRegion <- nrow(COVID)/84; nRegion
ndays <- 45
COVID[1:ndays,4]
ETH <- read.csv("Ethereum.csv")
ETH
