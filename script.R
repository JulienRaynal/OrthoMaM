library(ape)

homoIndex <- vector()
macacaIndex <- vector()
rattusIndex <- vector()
musIndex <- vector()

mrcaSinge <- vector()
mrcaSouris <- vector()
mrcaCommon <- vector()

files <- list.files(path="./trees", pattern="*.rootree", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
				 tree <- read.tree(x)

				 homoIndex <<- append(homoIndex, which(tree$tip.label=="Homo_sapiens")) 
				 macacaIndex <<- append(macacaIndex, which(tree$tip.label=="Macaca_fascicularis"))
				 rattusIndex <<- append(rattusIndex, which(tree$tip.label=="Rattus_norvegicus"))
				 rattusIndex <<- append(musIndex, which(tree$tip.label=="Mus_musculus"))

				 mrcaSinge <<- getMRCA(tree, c("Homo_sapiens", "Macaca_fascicularis"))
				 mrcaSouris <<- getMRCA(tree, c("Rattus_norvegicus", "Mus_musculus"))

				 mrcaCommon <<- getMRCA(tree, c(mrcaSinge, mrcaSouris))
				 dist <- dist.nodes(tree)[mrcaCommon, mrcaSouris] 
})

