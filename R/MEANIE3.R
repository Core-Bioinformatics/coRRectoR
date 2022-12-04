#' Modified GENIE3
#' 
#' @description Modified version of GENIE3 that translates each column by its
#' mean and scales by its standard deviation
#' @param exprMatrix Expression matrix where columns refer to samples and rows
#' to genes
#' @export
MEANIE3 <- function(exprMatrix) {
  exprMatrixT <- base::t(exprMatrix)
  base::rm(exprMatrix)
  num.samples <- base::nrow(exprMatrixT)
  allGeneNames <- base::colnames(exprMatrixT)
  regulatorNames <- allGeneNames
  targetNames <- allGeneNames
  nGenes <- base::length(targetNames)
  ret<-base::list()
  weightMatrix <- base::matrix(0.0, nrow=base::length(regulatorNames), ncol=base::length(targetNames))
  base::rownames(weightMatrix) <- regulatorNames
  base::colnames(weightMatrix) <- targetNames
  for(targetName in targetNames)
  {
    theseRegulatorNames <- regulatorNames
    theseRegulatorNames <- base::setdiff(theseRegulatorNames, targetName)
    numRegulators <- base::length(theseRegulatorNames)
    mtry <- base::round(base::sqrt(numRegulators))
    x <- exprMatrixT[,theseRegulatorNames]
    y <- exprMatrixT[,targetName]
    im <- base::.C("BuildTreeEns", base::as.integer(num.samples), base::as.integer(numRegulators),
             base::as.single(base::c(x)), base::as.single(base::c(y)),
             base::as.integer(1), base::as.integer(0), base::as.integer(1),
             base::as.integer(mtry), base::as.integer(1000),
             base::as.integer(1), base::as.integer(0),
             base::as.double(base::vector("double",numRegulators)),
             PACKAGE="GENIE3")[[12]]
    weightMatrix[theseRegulatorNames, targetName] <- im
  }
  ret <- base::apply(weightMatrix, 2, function(col) (col - base::mean(col) / stats::sd(col)))
  return(ret)
}
