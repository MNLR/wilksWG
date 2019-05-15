
load(file = "data/data.rda")

# Train model:
wilks.model <-  wilksTrain(data = data, accuracy = 100000,
                           max.error = 0.05)

# More accurate model, requieres much more time, uses parallel to speed up computation:
wilks.model.2 <-  wilksTrain(data = data, 
                             accuracy = 1000000,
                             max.error = 0.01,
                             parallelize = TRUE, 
                             n.cores = NULL 
                            )

# Generate series:
wilksGenerateSeries(wilks = wilks.model.2, 
                    n = 100
                    )
