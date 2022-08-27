#' Calculates the unique genes
#' 
#' @description Finds the top 'neighbours' highest covariance neighbours to
#' each gene and finds the intersection of these lists across every batch for
#' each given gene. Finally, takes the union of all of these genes to get a
#' set of unique genes
#' @param covs List of covariation matrices, as calculated by 'calculateCovs'.
#' One for each batch. Default: None
#' @param neighbours Number of highest covariance neighbours to consider for 
#' each gene. Default: 20
#' @export
intersectGenes <- function(covs, neighbours=20) {
  
  # Finds top N values in vector (bar top)
  topN <- function(vec) {
    return(names(head(tail(sort(vec), neighbours + 1), neighbours)))
  }
  
  # Find highest covariance neighbours
  message("finding nearest neighbours for each gene")
  highestCovs <- lapply(covs, function(cov) as.data.frame(apply(cov, 2, topN)))
  
  # Intersect corresponding genes across batches
  message("found nearest neighbours, taking intersection across each gene")
  similarGenes <- colnames(covs[[1]])
  colIntersections <- lapply(similarGenes, function(gene) Reduce(intersect, lapply(highestCovs, function(cov) unlist(cov[gene]))))
  
  # Take union of all genes
  message("intersection complete, taking union of selected genes")
  uniqueGenes <- unique(unlist(colIntersections))
  
  return(uniqueGenes)
}