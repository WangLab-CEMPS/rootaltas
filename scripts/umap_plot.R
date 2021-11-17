umap_plot <- function(umap_df, expr_df, gene, pt.size){

  if ( gene %in% rownames(expr_df) ){
    umap_df$expr <- expr_df[gene,]
  } else{
    umap_df$expr <- 0
  }

  p1 <- ggplot2::ggplot(umap_df, ggplot2::aes(x=UMAP_1, y=UMAP_2)) +
    ggplot2::geom_point(ggplot2::aes(color=expr), size=pt.size) +
    ggplot2::theme_bw()  +
    ggplot2::ggtitle(paste0(gene, " Expression in UMAP Clusters")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5))
  
  if ( gene %in% rownames(expr_df))(
    p2 <- p1 +  ggplot2::scale_color_gradientn(
      colours = c("#c8c6c3","#f6ee6c","#f9a432","#eb212f","#88181d"),
      name = "Seurat scale" ) 
  ) else{
    p2 <- p1 + ggplot2::scale_color_manual(values= "#c8c6c3") 
  }
  p2
} 
