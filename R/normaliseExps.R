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
  genes <- base::lapply(exps, base::rownames)
  similarGenes <- base::Reduce(base::intersect, genes)
  similarExps <- base::lapply(exps, function(exp) exp[similarGenes,])
  fullExpression <- base::data.matrix(base::do.call("cbind", similarExps))
  
  # Perform quantile normalisation on selected genes
  fullExpression[uniqueGenes,] <- preprocessCore::normalize.quantiles(fullExpression[uniqueGenes,])
  
  # Transpose expression matrix
  tExpression <- base::t(fullExpression)
  
  # Run poibm on genes
  unselectedGenes <- base::setdiff(base::colnames(tExpression), uniqueGenes)
  uniqueGenes <- base::intersect(uniqueGenes, base::colnames(tExpression))
  fit <- poibm::poibm.fit(tExpression[,uniqueGenes], tExpression[,unselectedGenes], max.resets=20, verbose=T)
  tExpression[,unselectedGenes] <- poibm::poibm.apply(tExpression[,unselectedGenes], fit)
  
  # Un-transpose
  fullExpression <- base::t(tExpression)
  
  return(fullExpression)
}
