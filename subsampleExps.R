library(DESeq2)
library(edgeR)
library(preprocessCore)
library(poibm)

exps <- list(readRDS("mccaskie_exps/batch3_corrected.RDS"), readRDS("mccaskie_exps/batch2_corrected.RDS"))
uniqueGenes <- readRDS("zmean_genes.RDS")

subsampleExps <- function(exps, uniqueGenes, normalisation) {

  # Full expression matrix of both batches
  genes <- lapply(exps, rownames)
  similarGenes <- Reduce(intersect, genes)
  similarExps <- lapply(exps, function(exp) exp[similarGenes,])
  fullExpression <- data.matrix(do.call("cbind", similarExps))
  
  # List of unselected genes
  unselectedGenes <- setdiff(rownames(fullExpression), uniqueGenes)
  
  message("normalising")
  # Perform normalisation on selected genes
  if (normalisation == "quantile") {
    fullExpression[uniqueGenes,] <- normalize.quantiles(fullExpression[uniqueGenes,])
  } else if (normalisation == "DESeq2") {
    fullExpression[uniqueGenes,] <- estimateSizeFactorsForMatrix(fullExpression[uniqueGenes,])
  } else if (normalisation == "tmm") {
    fullExpression[uniqueGenes,] <- calcNormFactors(fullExpression[uniqueGenes,])
  }
  message("normalised!")
  
  # Poisson correction
  tExpression <- t(fullExpression)
  fit <- poibm.fit(tExpression[,uniqueGenes], tExpression[,unselectedGenes], verbose=T)
  tExpression[,unselectedGenes] <- poibm.apply(tExpression[,unselectedGenes], fit)
  
  # Combat correction (negative binomial)
  tExpression <- t(fullExpression)
  batch <- as.matrix(rep(1, ncol(tExpression)))
  rownames(batch) <- colnames(tExpression)
  batch[uniqueGenes,] <- 2
  batch <- batch[,1]
  tExpression <- ComBat_seq(tExpression, batch=batch)
  
  fullExpression <- t(tExpression)
  return(fullExpression)
}

fullExpression <- preprocessExpressionMatrix(fullExpression)
metadata <- readRDS("metadata_full.RDS")
fullExpression <- fullExpression[,metadata$id]

pca <- plot_pca(fullExpression, metadata,5,show.label=T)
ggsave("pca_label.pdf", pca, width=11, height=8.5)

tExpression <- t(fullExpression)
tMetadata <- data.frame(gene=colnames(tExpression))
tMetadata$batch <- unlist(lapply(colnames(tExpression), function(gene) gene %in% uniqueGenes))
pca <- plot_pca_z(tExpression, tMetadata, 2)
ggsave("pca_z.pdf", pca, width=11, height=8.5)
