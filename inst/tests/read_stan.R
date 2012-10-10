csvfiles <- dir(system.file("line/", package="mcmc4"), pattern="chain_[0-9]+\\.csv",
                full.name=TRUE)
file <- csvfiles[1]


