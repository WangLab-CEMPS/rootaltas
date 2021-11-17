tsne_plot <- function(tsne_df, expr_df, gene, pt.size){
  
  if ( gene %in% rownames(expr_df) ){
    tsne_df$expr <- expr_df[gene,]
  } else{
    tsne_df$expr <- 0
  }
  
  
  ggplot2::ggplot(tsne_df, ggplot2::aes(x=tSNE_1, y=tSNE_2)) +
    ggplot2::geom_point(ggplot2::aes(color=expr), size=pt.size) +
    ggplot2::scale_color_gradientn(
      colours = c("#c8c6c3","#f6ee6c","#f9a432","#eb212f","#88181d"),
      name = "Seurat scale"
    ) +
    ggplot2::theme_bw()  +
    ggplot2::ggtitle(paste0(gene, " Expression in t-SNE Clusters")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5))
}
