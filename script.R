library(ape)

homoIndex <- vector()
macacaIndex <- vector()
rattusIndex <- vector()
musIndex <- vector()

mrcaApe <- vector()
mrcaMouse <- vector()
mrcaCommon <- vector()

files <- list.files(path="./trees", pattern="*.rootree", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
				 tree <- read.tree(x)
        print(tree)
				 homoIndex <<- append(homoIndex, which(tree$tip.label=="Homo_sapiens")) 
				 macacaIndex <<- append(macacaIndex, which(tree$tip.label=="Macaca_fascicularis"))
				 rattusIndex <<- append(rattusIndex, which(tree$tip.label=="Rattus_norvegicus"))
				 musIndex <<- append(musIndex, which(tree$tip.label=="Mus_musculus"))

				 mrcaApe <<- getMRCA(tree, c("Homo_sapiens", "Macaca_fascicularis"))
				 mrcaMouse <<- getMRCA(tree, c("Rattus_norvegicus", "Mus_musculus"))

				 mrcaCommon <<- getMRCA(tree, c(mrcaApe, mrcaMouse))
				 distCM <- dist.nodes(tree)[mrcaCommon, mrcaMouse]
				 distCA <- dist.nodes(tree)[mrcaCommon, mrcaApe]
				 distCh <- dist.nodes(tree)[mrcaCommon, homoIndex]
				 distCma <- dist.nodes(tree)[mrcaCommon, macacaIndex]
				 distCr <- dist.nodes(tree)[mrcaCommon, rattusIndex]
				 distCmu <- dist.nodes(tree)[mrcaCommon, musIndex]
})
print(rattusIndex)
