library(ape)

files <- list.files(path="./trees", pattern="*.rootree", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
				 tree <- read.tree(x)
				 print(tree$edge)
				 mrcaSinge <- getMRCA(tree, c("Homo_sapiens", "Macaca_fascicularis"))

				 mrcaSouris <- getMRCA(tree, c("Rattus_norvegicus", "Mus_musculus"))

				 mrcaCommon <- getMRCA(tree, c(mrcaSinge, mrcaSouris))
				 dist <- dist.nodes(tree)[mrcaSinge, mrcaSouris] 
})


