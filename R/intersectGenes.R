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
    return(base::names(utils::head(utils::tail(base::sort(vec), neighbours + 1), neighbours)))
  }
  
  # Find highest covariance neighbours
  base::message("finding nearest neighbours for each gene")
  highestCovs <- base::lapply(covs, function(cov) base::as.data.frame(base::apply(cov, 2, topN)))
  
  # Intersect corresponding genes across batches
  base::message("found nearest neighbours, taking intersection across each gene")
  similarGenes <- base::Reduce(intersect, base::lapply(covs, base::rownames))
  colIntersections <- base::lapply(similarGenes, function(gene) base::Reduce(intersect, base::lapply(highestCovs, function(cov) base::unlist(cov[gene]))))
  
  # Take union of all genes
  base::message("intersection complete, taking union of selected genes")
  uniqueGenes <- base::unique(base::unlist(colIntersections))
  
  return(uniqueGenes)
}