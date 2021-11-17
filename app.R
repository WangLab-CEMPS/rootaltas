
# devtools::install_github("nik01010/dashboardthemes")

library(ggplot2)
library(shiny)
library(shinythemes)
#library(dashboardthemes)
library(data.table)
library(Matrix)
library(rlang)
library(cowplot)
library(DT)

# Load the plot scripts

source("scripts/umap_plot.R")
source("scripts/vlnplot.R")
source("scripts/tsne_plot.R")

## Set parameters
colRainbow <- c("#136bfb","#b4b4b5","#eb9aeb","#1175b9","#fd38fc","#26e99b","#e8b964","#cb4528","#465ca8","#178cf9","#dacd50","#b80610","#0b6a24","#fc0d1b","#66c4cc","#4bace0","#907bb6","#5bb449","#dc4722","#4957a5","#c5da6b","#ef9d3b","#86c69e","#6f6f70")
dotsize <- 1.5
png_width <- 1000
png_height <- 800
pdf_width <- 10
pdf_height <- 8

# load data

## Load the alias relationship and gene ID list
alias_df <- readRDS("data/alias_data_frame.rds")
gene_select_list <- readRDS("data/gene_list.rds")

## Load coordinate of UMAP and t-SNE 
umap_df <- readRDS("data/umap_coordinate.rds")
umap_df <- as.data.frame(umap_df) # UMAP data.frame
tsne_df <- readRDS("data/tsne_coordinate.rds")

## Load the expression matrix and cluster 
expr_df <- readRDS("data/norm_data.Rds") # expression data.frame
#expr_df <- as.data.frame(as.matrix(expr_df))
#names(gene_select_list) <- gene_select_list
ident_list <- readRDS("data/ident_list.rds") # cluster 


#print(gene_select_list)
#print("Data Loading Finished")

# User Interface
ui <- fluidPage(
  tags$head(
    includeCSS("www/bootstrap.min.css"),
    includeCSS("www/bootstrap-theme.min.css"),
    includeScript("www/bootstrap.min.js"),
    includeScript("www/baidu-analysis.js")
  ),
  
  style = "padding: 0px",
	theme = shinytheme("readable"),
	tags$title("Arabidopsis Root Cell Atlas"),
	#br()
	fluidPage(
			  style = "background-color:#181717; padding-top:20px; padding-bottom:20px",
			  column(8,
			  	tags$a(href="#","Arabidopsis Root Cell Atlas",
				 style = "color: #ffffff; margin-left: 100px;font-size: 30px"
			  )),
			  column(4,
			 	tags$a(href="http://wanglab.sippe.ac.cn","WangLab",
				style = "color:#ffffff; font-size: 30px")
			  )
			  ),
	fluidPage(
	style = "margin-left: 100px; margin-right: 100px",		  
	h2("A single-cell RNA sequencing pofiles the developmental landscape of Arabidopsis root"),
	h3("Supplementary website"),
	br(),
	sidebarLayout(
	  
	  # Panel for interaction
		sidebarPanel(

			fluidRow(
				selectizeInput("input_gene",
							label = "Gene ID",
							choices = NULL,
							options = list(
							  placeholder = "eg: AT3G20840",
							  maxOptions = 1000
							)
					 ),
				actionButton("submit", "Submit")
			),
			
			tags$hr(),
			
			#fluidRow(
			# selectizeInput("input_symbol",
			#                 label = "Gene symbol",
			#                choices = NULL,
			#                 options = list(placeholder = "eg: WUS",
			#                                maxOptions = 1000)
			#  ),
			#  actionButton("submit2", "Submit")
			#  
			#),
			#tags$hr(),
			fluidRow(
				       fileInput("upload_gene", "Upload gene list",
				                 accept = c("text/plain",
				                            ".txt"))
			)
		),
		# Data Ouput Panel
		mainPanel(
			tabsetPanel(
			  # violin plot  
				tabPanel("Violin Plot", 
				         plotOutput("violinplot"),
				         br(),
				         tags$p("For batch plotting, only the first gene will be displayed above. Please download the file for all plots. "),
				         downloadLink("download_vln", "Violin plot download")
				         ),
			
				# UMAP plot	
				tabPanel(
				  title = "UMAP Plot", 
				  fluidPage(
				  fluidRow( plotOutput("umapplot", width = 700, height=700 )),
				  fluidRow(
				    tags$p("For batch plotting, only the first gene will be displayed above. Please download the file for all plots. "),
				    downloadLink("download_umap","UMAP plot download"))
				  )),
				
			  # t-SNE plot
				tabPanel("t-SNE Plot", 
				         fluidPage(
				           fluidRow( plotOutput("tsneplot", width = 700, height=700 )),
				           fluidRow(
				             tags$p("For batch plotting, only the first gene will be displayed above. Please download the file for all plots. "),
				             downloadLink("download_tsne","t-SNE plot download"))
				         )
				         
				),
				
				# Cluster of in UMAP coordinate	
				tabPanel("Cluster Legend", 
						 tags$img(src="UMAP_legend.png", height=700, width=1080)),
				
				# show the results after user selecting the gene
				tabPanel("Dataset", 
				         DT::dataTableOutput("dataset"),
				         br(),
				         downloadLink("download_dataset","Download csv")),
				tabPanel("Contact",
				         fluidPage(
				           fluidRow(
				             tags$div( class = "text-xl-left", 
				                       style = "font-size: 20px; margin-top:30px",
				             p("Contact: ", br(),"xuzhougeng@163.com"))
				           )
				         )
				)
				         
			
			)	
		)
	)
	),
    fluidPage(
        fluidRow()
    )
 # ,
 #  fluidPage(
 #    style = "
 #    position:absolute;
 #    bottom:0;
 #    width:100%;
 #    height:50px; /* Height of the footer */
 #    color: white;
 #    padding: 10px;
 #    background-color: black;
 #    z-index: 1000;",
 #    tags$footer(title="wanglab", align="center",
 #      tags$p("Contact: xuzhougeng@163.com")
 #  )
 # )
)

# Server logical
server <- function(input, output, session){
  
  observe({
    updateSelectizeInput(session, 
                         inputId = "input_gene",
                         choices = gene_select_list,
                         server = TRUE
    )
    
  })


  global_value <- reactiveValues(
    gene = NULL,
    gene_list = NULL
  )

  # update the input
  #source("scripts/updateSelectizeInput.R", local = TRUE)$value

  # single gene input
  observeEvent(input$submit, {
    global_value$gene <- input$input_gene
    global_value$gene_list <- NULL
  })

  # upload gene list
  observeEvent(input$upload_gene, {
    global_value$gene_list <- readLines(input$upload_gene$datapath)
    global_value$gene <- NULL
  })



  # plot violin and provide download link
  source("scripts/output_violin_reslut.R", local = TRUE)

  # plot UMAP and provide UMAP plot download link
  source("scripts/output_umap_reslut.R", local = TRUE)

	# plot t-SNE and provide t-SNE plot download link
  source("scripts/output_tsne_reslut.R", local = TRUE)

  # data table output
  source("scripts/output_dataset_reslut.R", local = TRUE)

}

shinyApp(ui = ui, server = server)
