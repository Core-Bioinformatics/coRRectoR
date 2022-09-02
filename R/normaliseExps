library(preprocessCore)
library(poibm)

#' Creates a normalised expression matrix of all samples
#' 
#' @description Merges all expression matrices (from each batch) into one and then
#' normalises the selected genes using quantile normalisation before using POIBM
#' to correct the difference between the selected and unselected genes.
#' @param exps List of raw expression matrices where each item in list is
#' an expression matrix where the columns are the samples in that batch and
#' the rows are the genes. Default: none
#' @param uniqueGenes List of selected genes, as calculated by coRRectoR::intersectGenes.
#' Default: none
#' @export
normaliseExps <- function(exps, uniqueGenes) {

  # Full expression matrix of all batches
  genes <- lapply(exps, rownames)
  similarGenes <- Reduce(intersect, genes)
  similarExps <- lapply(exps, function(exp) exp[similarGenes,])
  fullExpression <- data.matrix(do.call("cbind", similarExps))
  
  # Perform quantile normalisation on selected genes
  fullExpression[uniqueGenes,] <- normalize.quantiles(fullExpression[uniqueGenes,])
  
  # Transpose expression matrix
  tExpression <- t(fullExpression)
  
  # Run poibm on genes
  unselectedGenes <- setdiff(colnames(tExpression), uniqueGenes)
  uniqueGenes <- intersect(uniqueGenes, colnames(tExpression))
  fit <- poibm.fit(tExpression[,uniqueGenes], tExpression[,unselectedGenes], max.resets=20, verbose=T)
  tExpression[,unselectedGenes] <- poibm.apply(tExpression[,unselectedGenes], fit)
  
  # Un-transpose
  fullExpression <- t(tExpression)
  
  return(fullExpression)
}
