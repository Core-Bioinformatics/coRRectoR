library(GENIE3)

#' Modified GENIE3
#' 
#' @description Modified version of GENIE3 that translates each column by its
#' mean and scales by its standard deviation
#' @param exprMatrix Expression matrix where columns refer to samples and rows
#' to genes
#' @param verbose Logical indicating whether to print detailed output of stage
#' of computation. Default: F
#' @export
MEANIE3 <- function(exprMatrix, verbose=F) {
  exprMatrixT <- t(exprMatrix)
  rm(exprMatrix)
  num.samples <- nrow(exprMatrixT)
  allGeneNames <- colnames(exprMatrixT)
  regulatorNames <- allGeneNames
  targetNames <- allGeneNames
  nGenes <- length(targetNames)
  ret<-list()
  weightMatrix <- matrix(0.0, nrow=length(regulatorNames), ncol=length(targetNames))
  rownames(weightMatrix) <- regulatorNames
  colnames(weightMatrix) <- targetNames
  for(targetName in targetNames)
  {
    if(verbose) message(paste("Computing gene ", which(targetNames == targetName), "/", nGenes, ": ",targetName, sep=""))
    theseRegulatorNames <- regulatorNames
    theseRegulatorNames <- setdiff(theseRegulatorNames, targetName)
    numRegulators <- length(theseRegulatorNames)
    mtry <- round(sqrt(numRegulators))
    x <- exprMatrixT[,theseRegulatorNames]
    y <- exprMatrixT[,targetName]
    im <- .C("BuildTreeEns",as.integer(num.samples),as.integer(numRegulators),
             as.single(c(x)),as.single(c(y)),as.integer(1),
             as.integer(0),as.integer(1),
             as.integer(mtry),as.integer(1000),
             as.integer(1),as.integer(0),
             as.double(vector("double",numRegulators)))[[12]]
    weightMatrix[theseRegulatorNames, targetName] <- im
  }
  ret <- apply(weightMatrix, 2, function(col) (col - mean(col) / sd(col)))
  return(ret)
}