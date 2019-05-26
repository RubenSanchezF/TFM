
#library(shinyTable)
library(DT)
options(shiny.maxRequestSize=1000*1024^2) #1GB

#################################################################################################
###                            Define UI                                                      ###
#################################################################################################

pageAnalysisUI <- function(id) {
  
  fluidPage(
    
    titlePanel("Differential analysis"),
  
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        useShinyjs(),
        selectInput("normalization", "Select a normalization method", list("GC-RMA" = "gcrma", 
                                                                           "RMA" = "rma", "M.A.S. 5.0" = "mas",
                                                                           "none" = "none"),
                    multiple = FALSE),
        selectInput("normalization2", "Select a normalization method", list("TMM"="TMM",
                                                                            "RLE"="RLE",
                                                                           "none" = "none"),
                    multiple = FALSE),
        
        verbatimTextOutput("normalization_message"),
        br(),
        
        
        selectInput("analysis", "Select a statistical method", list("Linear model (EBayes)" = "linear", 
                                                                           "Significance Microarray Analysis" = "sam"),
                    multiple = FALSE, selected = "linear"),
        
        selectInput("analysis2", "Select a statistical method", list("VOOM+EBayes" = "VOOM+EBayes"), 
                                                                    
                    multiple = FALSE, selected = "VOOM+EBayes"),
        
        br(),
        
        selectInput("column", "Select the column containing conditions to contrast", list("."="."),
                    multiple = FALSE, selected = NULL),
        selectInput("column2", "Select the column containing conditions to contrast", list("."="."),
                    multiple = TRUE, selected = NULL),
        br(),
        
        selectInput("conditions", "Select the conditions to contrast", list("."="."),
                    multiple = TRUE, selected = NULL),
        
        textInput("fdr","Introduce a value for FDR", value = "0.1"),
        
        br(),
        
        actionButton("analysis_button", "Calculate", width = 100),
        
        hidden(
          p(id = "calculating", "Calculating...")
        )
      ),
      
      mainPanel(
        
        selectInput("contrast_selection", "Select contrast", list("."="."),
                    multiple = FALSE, selected = NULL),
        
        tabsetPanel(type = "tabs",
                    tabPanel("Table", dataTableOutput("analysis_table")),
                    tabPanel("Plot", plotOutput("plot_diff_analysis"))),
                    
                    
        
        
        br(),
        
        downloadButton('download1',"Download results")
    ))
  )
  

}