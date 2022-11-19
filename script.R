setwd("C:\\Users\\samia\\Documents\\bill")
library(ape)

df = data.frame(
  id_tree=integer(),
  distCM=integer(),
  distCA=integer(),
  distCh=integer(),
  distCma=integer(),
  distCr=integer(),
  distCmu=integer()
)

i=0

files <- list.files(path="./trees", pattern="*.rootree", full.names=TRUE, recursive=FALSE)
lapply(files, function(x) {
				 tree <- read.tree(x)
				 homoIndex <- which(tree$tip.label=="Homo_sapiens")
				 macacaIndex <- which(tree$tip.label=="Macaca_fascicularis")
				 rattusIndex <- which(tree$tip.label=="Rattus_norvegicus")
				 musIndex <- which(tree$tip.label=="Mus_musculus")

				 mrcaApe <- getMRCA(tree, c("Homo_sapiens", "Macaca_fascicularis"))
				 mrcaMouse <- getMRCA(tree, c("Rattus_norvegicus", "Mus_musculus"))

				 mrcaCommon <- getMRCA(tree, c(mrcaApe, mrcaMouse))
				 distCM <- dist.nodes(tree)[mrcaCommon, mrcaMouse]
				 distCA <- dist.nodes(tree)[mrcaCommon, mrcaApe]
				 distCh <- dist.nodes(tree)[mrcaCommon, homoIndex]
				 distCma <- dist.nodes(tree)[mrcaCommon, macacaIndex]
				 distCr <- dist.nodes(tree)[mrcaCommon, rattusIndex]
				 distCmu <- dist.nodes(tree)[mrcaCommon, musIndex]
				 
				 df2 <- data.frame(
				   i,
				   distCM,
				   distCA,
				   distCh,
				   distCma,
				   distCr,
				   distCmu
				 )

				 names(df2) = c(
				   "id_tree",
				   "distCM",
				   "distCA",
				   "distCh",
				   "distCma",
				   "distCr",
				   "distCmu"
				 )

				 df <<- rbind(df,df2)

				 i <<- i+1
})
head(df)
stat <- function() {
  #question B
  median(distCM)
  median(distCA)
  median(distCh)
  median(distCma)
  median(distCr)
  median(distCmu)
  
  quantile(distCM, probs = c(0.25,0.95))
  quantile(distCA, probs = c(0.25,0.95))
  quantile(distCh, probs = c(0.25,0.95))
  quantile(distCma, probs = c(0.25,0.95))
  quantile(distCr, probs = c(0.25,0.95))
  quantile(distCmu, probs = c(0.25,0.95))
}
