% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotClusterUMAP.R
\name{plotClusterUMAP}
\alias{plotClusterUMAP}
\title{Taking a SingleCellExperiment object, with clustering result and UMAP  
dimension reduction embeddings, make a UMAP plot colored by categorical 
clustering  result.}
\usage{
plotClusterUMAP(
  sce,
  cluster_name = "spatial.cluster",
  facet = NULL,
  dim_red_name = "UMAP.HARMONY"
)
}
\arguments{
\item{sce}{A SingleCellExperiment object with clustering result and reduced
dimension UMAPs in colData.}

\item{cluster_name}{The name of the clusters stored in colData, default is
the result from BayesSpace `"spatial.cluster"`}

\item{facet}{a string to specify the facet, e.g. `"orig.ident`.}

\item{dim_red_name}{default is harmony batch corrected embeddings
`"UMAP.HARMONY"`, where the first two dimensions will be plotted on x and y 
axis.}
}
\value{
a UMAP plot colored by categorical clustering result.
}
\description{
Taking a SingleCellExperiment object, with clustering result and UMAP  
dimension reduction embeddings, make a UMAP plot colored by categorical 
clustering  result.
}
\examples{
\dontrun{
## Visium DLPFC Example
library(BayesSpace)
sce <- getRDS("2020_maynard_prefrontal-cortex", "151673")
set.seed(123)
sce <- spatialPreprocess(sce, n.PCs = 15)
set.seed(123)
sce <- runUMAP(sce, dimred = "PCA")
p <- plotClusterUMAP(sce, cluster_name = "Cluster", facet = NULL,
                          dim_red_name = "UMAP")
}
}
