

output$violinplot <- renderPlot({
  

  if ( ! is.null(global_value$gene_list)){
    gene <- global_value$gene_list[1]
    #print(gene)
    plot <- vlnplot(expr_df, idents=ident_list, 
            features = gene,
            pt.size = dotsize,
            cols = colRainbow)
    return(plot)
  }
  
  if ( ! is.null(global_value$gene)){
    gene <- global_value$gene
    plot <- vlnplot(expr_df, idents=ident_list, 
                    features = gene,
                    pt.size = dotsize,
                    cols = colRainbow)
    return(plot)
    
  } 
}
)


output$download_vln <- downloadHandler(
  
  filename = "violin.pdf",
  
  content = function(file){
    pdf(file =  file, width = pdf_width, height = pdf_height)
    
    if ( ! is.null(global_value$gene_list)){
      genes <- global_value$gene_list
    }
    if ( ! is.null(global_value$gene)){
      genes <- global_value$gene 
    }
    for (gene in genes){
      plot_out <- vlnplot(expr_df, 
                          idents=ident_list, 
                          features = gene,
                          pt.size = dotsize,
                          cols = colRainbow)
      
      print(plot_out)
    }
    dev.off()
    
  },
  contentType = "application/pdf"
)