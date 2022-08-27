library(pcaPP)
library(parallel)

#' Generates covaration matrix from each batch's GRN
#' 
#' @description Calls base::cov on each of the GRNs and returns a list of
#' covariation matrices
#' @param par Logical indicating whether to run cov on all batches in parallel. 
#' Default: F
#' @export
calculateCovs <- function(grns, par) {
  
  # Calculate covariance matrices
  message("calculating covariances")
  if (par) {
    covs <- mclapply(grns, cov)
  } else {
    covs <- lapply(grns, cov)
  }
  message("covariances calculated")
  return(covs)
}