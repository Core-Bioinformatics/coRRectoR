% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intersectGenes.R
\name{intersectGenes}
\alias{intersectGenes}
\title{Calculates the unique genes}
\usage{
intersectGenes(covs, neighbours = 20)
}
\arguments{
\item{covs}{List of covariation matrices, as calculated by 'calculateCovs'.
One for each batch. Default: None}

\item{neighbours}{Number of highest covariance neighbours to consider for
each gene. Default: 20}
}
\description{
Finds the top 'neighbours' highest covariance neighbours to
each gene and finds the intersection of these lists across every batch for
each given gene. Finally, takes the union of all of these genes to get a
set of unique genes
}
