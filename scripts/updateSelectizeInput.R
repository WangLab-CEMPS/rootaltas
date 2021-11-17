
# observe({
#   print("Start: running observe")
#   #gene_choices <- global_value$select_gene_list
#   #select_gene_list
#   # update the select input by gene ID
#   updateSelectizeInput(
#     session = session,
#     inputId = 'input_gene',
#     label = "Gene ID",
#     server = TRUE,
#     choices = data.frame(label=gene_select_list,
#                          value=gene_select_list),
#     options = list(
#                                render = I(
#                                  "{
#                                  option: function(item, escape) {
#                                  return '<div> <strong>' + item.label </strong> + '</div>';
#                                  }
#                                  }"
#                                )
#                              )
#   )
#   print("End: running observe")
# })




# 
#   # update the select input by gene ID
#   updateSelectizeInput(
#     session = session,
#     inputId = 'input_symbol',
#     label = "Gene symbol",
#     server = TRUE,
#     choices = data.frame(label=alias_df$symbol,
#                          value=alias_df$name,
#                          name=alias_df$full_name),
#     options = list(
#       render = I(
#         "{
#         option: function(item, escape) {
#         return '<div> <strong>' + item.label + '</strong> - ' +
#         escape(item.name) + '</div>';
#         }
#         }"
#       )
#     )
#   )
