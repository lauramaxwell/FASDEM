install.packages("bnstruct")
library(bnstruct)
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(Rgraphviz)
asia <- asia()
child <- child()

net <- learn.network(child)
layers <- c(1,2,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5)
net.layer <- learn.network(child, layering = layers)
plot(net)
plot(net.layer)


engine <- InferenceEngine(net)
results <- em(engine, child)


##dynamic bayes network

dbn_data <- BNDataset("evolving_system.data",
                      "evolving_system.header",
                      num.time.steps = 4)

dbn <- learn.dynamic.network(dbn_data, num.time.steps = 4)

