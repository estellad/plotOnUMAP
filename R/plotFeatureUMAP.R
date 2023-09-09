#' Taking a SingleCellExperiment object with UMAP dimension reduction 
#' embeddings, make a UMAP plot colored by categorical clustering result. 
#'
#' @param sce A SingleCellExperiment (SCE) object with reduced dimension UMAPs 
#' in colData.
#' @param feat_name The name of the feature to plot, which can be any one in the 
#' rownames of the SCE object, e.g. `"ADIPOQ"`.
#' @param facet a string to specify the facet, e.g. `"orig.ident`.
#' @param dim_red_name default is harmony batch corrected embeddings
#' `"UMAP.HARMONY"`, where the first two dimensions will be plotted on x and y 
#' axis. 
#' 
#' @return a UMAP plot colored by continuous feature expression level. 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Visium DLPFC Example
#' library(BayesSpace)
#' sce <- getRDS("2020_maynard_prefrontal-cortex", "151673")
#' sce <- 
#' set.seed(123)
#' sce <- spatialPreprocess(sce, n.PCs = 15)
#' set.seed(123)
#' sce <- runUMAP(sce, dimred = "PCA")
#' p <- plotFeatureUMAP(sce, feat_name = "ENSG00000238009", facet = NULL,
#'                           dim_red_name = "UMAP")
#' p                         
#' }
plotFeatureUMAP <- function(sce, feat_name = "ADIPOQ", facet = NULL,
                            dim_red_name = "UMAP.HARMONY"){
  mat <- as.data.frame(as.matrix(t(logcounts(sce))))
  meta <- as.data.frame(colData(sce))
  dim.red <- as.data.frame(reducedDim(sce, dim_red_name))
  CD <- cbind(mat, meta, dim.red)

  p <- ggplot(CD, aes(x = UMAP1, y = UMAP2, color = get(feat_name))) +
    geom_point(size = 0.3) +
    labs(color = NULL) +
    theme_bw() +
    theme(legend.position="right", panel.border = element_blank()) +
    ggtitle(feat_name)

  if(!is.null(facet)){
    p <- p + facet_wrap(~get(facet))
  }

  p
}
