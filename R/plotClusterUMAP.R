#' Taking a SingleCellExperiment object, with clustering result and UMAP  
#' dimension reduction embeddings, make a UMAP plot colored by categorical 
#' clustering  result. 
#'
#' @param sce A SingleCellExperiment object with clustering result and reduced
#' dimension UMAPs in colData.
#' @param cluster_name The name of the clusters stored in colData, default is  
#' the result from BayesSpace `"spatial.cluster"`
#' @param facet a string to specify the facet, e.g. `"orig.ident`.
#' @param dim_red_name default is harmony batch corrected embeddings
#' `"UMAP.HARMONY"`, where the first two dimensions will be plotted on x and y 
#' axis. 
#'
#' @return a UMAP plot colored by categorical clustering result. 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Visium DLPFC Example
#' library(BayesSpace)
#' sce <- getRDS("2020_maynard_prefrontal-cortex", "151673")
#' set.seed(123)
#' sce <- spatialPreprocess(sce, n.PCs = 15)
#' set.seed(123)
#' sce <- runUMAP(sce, dimred = "PCA")
#' p <- plotClusterUMAP(sce, cluster_name = "Cluster", facet = NULL,
#'                           dim_red_name = "UMAP")
#' }
plotClusterUMAP <- function(sce, cluster_name = "spatial.cluster", facet = NULL,
                            dim_red_name = "UMAP.HARMONY"){
  meta <- as.data.frame(colData(sce))
  dim.red <- as.data.frame(reducedDim(sce, dim_red_name))
  CD <- cbind(meta, dim.red)
  
  p <- ggplot(CD, aes(x = UMAP1, y = UMAP2, color = as.factor(get(cluster_name)))) +
    geom_point(size = 0.3) +
    labs(color = NULL) +
    theme_bw() +
    theme(legend.position="right", panel.border = element_blank()) +
    scale_color_manual(name = cluster_name,
                       values = scales::hue_pal()(length(unique(sce[[cluster_name]])))) +
    ggtitle(cluster_name) +
    guides(colour = guide_legend(override.aes = list(size=3)))
  
  if(!is.null(facet)){
    p <- p + facet_wrap(~get(facet))
  }
  
  p
}
