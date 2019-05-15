
###
outdir <- "data/"
n.cores <- 5


trainjjac.nona <- removeNA(trainjjac)
traindjfc.nona <- removeNA(traindjfc)


wilks.model <-  wilksTrain(data = data, accuracy = 1000000,
                           max.error = 0.001)


wilksGenerateSeries()