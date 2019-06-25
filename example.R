
load(file = "data/data.rda")

# Train model:
wilks.model <-  wilksTrain(data = data)

# More accurate model, requires much more time,
# uses parallel to speed up computation:
wilks.model.2 <-  wilksTrain(data = data, 
                             accuracy = 10000000,
                             max.error = 0.01,
                             parallelize = TRUE, 
                             n.cores = NULL, 
                             
                            )

# Generate series with the model:
wilksGenerateSeries(wilks = wilks.model.2, 
                    n = 100
                    )
