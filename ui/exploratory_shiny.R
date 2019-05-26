
#library(shinyTable)
options(shiny.maxRequestSize=1000*1024^2) #1GB

#################################################################################################
###                            Define UI                                                      ###
#################################################################################################



pageThreeUI <- function(id) {
  
  fluidPage(
    
    h1("Exploratory analysis"),
    br(),
    sidebarLayout(
      
      sidebarPanel(
        
        useShinyjs(),
        
        verbatimTextOutput("description"),
        
        br(),
        
        selectInput("column_explo", "Select the column containing conditions to contrast", list("."="."),
                    multiple = FALSE, selected = NULL),
        
        actionButton("exploratory", "Show", width = 100),
        
        verbatimTextOutput("warning"),
        hidden(
          p(id = "processing", "Processing...")
        )
      ),
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Boxplot", plotOutput("boxplot")),
                    tabPanel("PCA/MDS", plotOutput("pca")),
                    tabPanel("Dendogram/Heatmap", plotOutput("dendogram"))
        )))
    
  )}
  
      
    
    
                
                
                
              
    



















