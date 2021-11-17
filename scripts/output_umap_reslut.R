# plot UMAP and provide UMAP plot download link


output$umapplot <- renderPlot({
  

  if ( ! is.null(global_value$gene_list)){
    gene <- global_value$gene_list[1]
    #print(gene)
    plot <- umap_plot(umap_df, 
                      expr_df, gene, 
                      pt.size = dotsize )
    return(plot)
  }
  
  if ( ! is.null(global_value$gene)){
    gene <- global_value$gene
    plot <- umap_plot(umap_df, 
                      expr_df, gene, 
                      pt.size = dotsize )
    return(plot)
    
  } 
}, width = 700, height=700)

output$download_umap <- downloadHandler(
  
  filename = "uamp.pdf",
  
  content = function(file){
    pdf(file =  file, width = pdf_width, height = pdf_height)
    
    if ( ! is.null(global_value$gene_list)){
      genes <- global_value$gene_list
    }
    if ( ! is.null(global_value$gene)){
      genes <- global_value$gene 
    }
    for (gene in genes){
      plot_out <- umap_plot(umap_df, 
                            expr_df, 
                            gene, 
                            pt.size = dotsize )
      print(plot_out)
      
    }
    dev.off()
    
  },
  contentType = "application/pdf"
)
