library(parallel)

#' Generates GRN for each batch's expression matrix
#' 
#' @description Calls MEANIE3 on each of the batches and returns a list of
#' GRNs
#' @param par Logical indicating whether to run GRN inference on all batches
#' in parallel. Default: F
#' @export
generateGRNs <- function(exps, par=F) {
  
  # Create GRNs
  message("inferring GRNs")
  if (par) {
    grns <- mclapply(preprocessed, MEANIE3)
  } else {
    grns <- lapply(preprocessed, MEANIE3)
  }
  message("GRNs inferred")
  return(grns)
}
