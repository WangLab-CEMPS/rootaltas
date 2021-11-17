
output$dataset <- DT::renderDataTable({
  if ( ! is.null(global_value$gene_list)){
    genes <- global_value$gene_list
  }
  if ( ! is.null(global_value$gene)){
    genes <- global_value$gene 
  }
  
  #genes <- as.character(genes)
  output_df <- as.data.frame(as.matrix(t(expr_df[genes,,drop=FALSE])))
  output_df$cluster <- ident_list[rownames(output_df)]
  #rownames(output_df) <- NULL
  output_df
}
)


output$download_dataset <- downloadHandler(
  
  filename = function(){
    paste("date", Sys.Date(), "expression.csv", sep="-")
  },
  
  content = function(file){
    
    if ( ! is.null(global_value$gene_list)){
      genes <- global_value$gene_list
    }
    if ( ! is.null(global_value$gene)){
      genes <- global_value$gene 
    }
    
    #genes <- as.character(genes)
    output_df <- as.data.frame(as.matrix(t(expr_df[genes,,drop=FALSE])))
    output_df$cluster <- ident_list[rownames(output_df)]
    #rownames(output_df) <- NULL
    write.csv(output_df, file = file)
    
  }
)
