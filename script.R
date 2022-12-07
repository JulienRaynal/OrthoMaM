setwd("/home/penpen/Downloads/68/")
library(ape)
library(ggplot2)
library(dplyr)
library(reshape2)

df <- data.frame(
  id_tree = integer(),
  distCM = integer(),
  distCA = integer(),
  distCh = integer(),
  distCma = integer(),
  distCr = integer(),
  distCmu = integer()
)

i <- 0

qA <- function() {
  files <- list.files(path = "./trees", pattern = "*.rootree", full.names = TRUE, recursive = FALSE)
  lapply(files, function(x) {
    tree <- read.tree(x)
    homoIndex <- which(tree$tip.label == "Homo_sapiens")
    macacaIndex <- which(tree$tip.label == "Macaca_fascicularis")
    rattusIndex <- which(tree$tip.label == "Rattus_norvegicus")
    musIndex <- which(tree$tip.label == "Mus_musculus")

    mrcaApe <- getMRCA(tree, c("Homo_sapiens", "Macaca_fascicularis"))
    mrcaMouse <- getMRCA(tree, c("Rattus_norvegicus", "Mus_musculus"))

    mrcaCommon <- getMRCA(tree, c(mrcaApe, mrcaMouse))
    distCM <- dist.nodes(tree)[mrcaCommon, mrcaMouse] / 75
    distCA <- dist.nodes(tree)[mrcaCommon, mrcaApe] / 90
    distCh <- dist.nodes(tree)[mrcaCommon, homoIndex] / 90
    distCma <- dist.nodes(tree)[mrcaCommon, macacaIndex] / 90
    distCr <- dist.nodes(tree)[mrcaCommon, rattusIndex] / 90
    distCmu <- dist.nodes(tree)[mrcaCommon, musIndex] / 90

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

    df <<- rbind(df, df2)

    i <<- i + 1
  })
}

# A
invisible(qA())
summary(df[c("distCM", "distCA", "distCh", "distCma", "distCr", "distCmu")])


results <- data.frame(
  name = character(),
  CM = integer(),
  CA = integer(),
  Ch = integer(),
  Cma = integer(),
  Cr = integer(),
  Cmu = integer()
)

qB <- function() {
  #question B
  mCm <- median(df$distCM)
  mCa <- median(df$distCA)
  mCh <- median(df$distCh)
  mCma <- median(df$distCma)
  mCr <- median(df$distCr)
  mCmu <- median(df$distCmu)

  qCm <- quantile(df$distCM, probs = c(0.25, 0.95))
  qCa <- quantile(df$distCA, probs = c(0.25, 0.95))
  qCh <- quantile(df$distCh, probs = c(0.25, 0.95))
  qCma <- quantile(df$distCma, probs = c(0.25, 0.95))
  qCr <- quantile(df$distCr, probs = c(0.25, 0.95))
  qCmu <- quantile(df$distCmu, probs = c(0.25, 0.95))

  results[nrow(results) + 1,] <<- c("Median", mCm, mCa, mCh, mCma, mCr, mCmu)
  results[nrow(results) + 1,] <<- c("q25", qCm[[1]], qCa[[1]], qCh[[1]], qCma[[1]], qCr[[1]], qCmu[[1]])
  results[nrow(results) + 1,] <<- c("q95", qCm[[2]], qCa[[2]], qCh[[2]], qCma[[2]], qCr[[2]], qCmu[[2]])


  dfresults <- melt(df[c("distCM", "distCA", "distCh", "distCma", "distCr", "distCmu")])
  boxplot(dfresults$value ~ dfresults$variable)
}

qB()
head(results)


# Question C
qC <- function() {
  dfmelt <- melt(df[c("distCh", "distCmu")])

  p <- dfmelt %>%
    ggplot(aes(x = value, fill = as.character(variable))) +
    geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity') +
    labs(fill = "")
  p
}

qC()
