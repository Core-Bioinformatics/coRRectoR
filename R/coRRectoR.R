#' coRRectoR wrapper function
#' 
#' @description Performs all stages of coRRectoR pipeline by calling subsequent
#' functions. Pipeline is:
#' 1. Inference of GRNs
#' 2. Calculation of covariation matrices
#' 3. Calculation of intersection genes
#' 4. Normalisation selected genes and unselected genes
#' @param exps List of raw expression matrices where each item in list is
#' an expression matrix where the columns are the samples in that batch and
#' the rows are the genes. Default: none
#' @param neighbours The number of nearest neighbours considered before the
#' intersection across genes is calculated. Default: 20
#' @param normalisation The normalisation method used (one of "quant",
#' "tmm", "deseq"). Default: "quant"
#' @param par Logical indicating whether to process the GRN inference and 
#' covariation calculations for each batch in parallel. Default: F
#' @export
coRRectoR <- function(exps, neighbours=20, normalisation="quant", par=F) {
  grns <- generateGRNS(exps, par)
  covs <- calculateCovs(grns, par)
  uniqueGenes <- intersectGenes(covs, neighbours)
  exps <- subsampleExps(exps, uniqueGenes, normalisation)
  return(exps)
}