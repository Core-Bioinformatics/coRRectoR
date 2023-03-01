# reintegrate two subsets
reintegrate <- function(fullExpression, small_subset, large_subset) {
  set_diff = setdiff(large_subset, small_subset)
  fullExpression[small_subset,] <- preprocessCore::normalize.quantiles(fullExpression[small_subset,])
  tExpression <- base::t(fullExpression)
  fit <- poibm::poibm.fit(tExpression[,small_subset], tExpression[,set_diff], max.resets=20, verbose=T)
  tExpression[,set_diff] <- poibm::poibm.apply(tExpression[,set_diff], fit)
  fullExpression <- base::t(tExpression)
  return(fullExpression)
}

# generate genes for successive numbers of neighbours
neighbours <- c(50, 60, 75, 100, 130, 180, 250, 350, 500)
genes <- mclapply(neighbours, intersectGenes, covs=covs, mc.cores=length(neighbours))

# iteratively integrate each larger set of genes onto the current exp
for (i in 1:(length(neighbours) - 1)) {
  exp <- reintegrate(exp, genes[[i]], genes[[i + 1]])
}
