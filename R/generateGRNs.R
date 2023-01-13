#' Generates GRN for each batch's expression matrix
#' 
#' @description Calls MEANIE3 on each of the batches and returns a list of
#' GRNs
#' @param par Logical indicating whether to run GRN inference on all batches
#' in parallel. Default: F
#' @export
generateGRNs <- function(exps, par=F) {
  
  # Remove zero-variance genes
  vars <- base::lapply(exps, function(exp) base::apply(exp, 1, stats::var))
  pos_var <- lapply(vars, function(var) var > 0)
  for (i in 1:base::length(exps)) {
    exps[[i]] <- exps[[i]][pos_var[[i]],]
  }

  # Create GRNs
  base::message("inferring GRNs")
  if (par) {
    grns <- parallel::mclapply(exps, GENIE3::GENIE3)
  } else {
    grns <- base::lapply(exps, GENIE3::GENIE3)
  }
  base::message("GRNs inferred")
  return(grns)
}
