#' Generates GRN for each batch's expression matrix
#' 
#' @description Calls MEANIE3 on each of the batches and returns a list of
#' GRNs
#' @param par Logical indicating whether to run GRN inference on all batches
#' in parallel. Default: F
#' @export
generateGRNs <- function(exps, par=F) {
  
  # Create GRNs
  base::message("inferring GRNs")
  if (par) {
    grns <- parallel::mclapply(exps, coRRectoR::MEANIE3)
  } else {
    grns <- base::lapply(exps, coRRectoR::MEANIE3)
  }
  base::message("GRNs inferred")
  return(grns)
}