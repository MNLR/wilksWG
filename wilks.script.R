
###
outdir <- "data/"
n.cores <- 5
reps <- 250
size <- 30
accuracy <- 1000


trainjjac.nona <- removeNA(trainjjac)
traindjfc.nona <- removeNA(traindjfc)


wilks.model <-  wilksTrain(data = data)


wilksGenerateSeries()