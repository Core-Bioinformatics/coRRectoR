#' Generates covaration matrix from each batch's GRN
#' 
#' @description Calls stats::cov on each of the GRNs and returns a list of
#' covariation matrices
#' @param grns List of matrices representing GRNs inferred by MEANIE3
#' @param par Logical indicating whether to run cov on all batches in parallel. 
#' Default: F
#' @export
calculateCovs <- function(grns, par) {
  
  # Calculate covariance matrices
  base::message("calculating covariances")
  if (par) {
    covs <- parallel::mclapply(grns, stats::cov)
  } else {
    covs <- base::lapply(grns,  stats::cov)
  }
  base::message("covariances calculated")
  return(covs)
}