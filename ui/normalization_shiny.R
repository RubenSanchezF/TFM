
#library(shinyTable)
library(DT)
options(shiny.maxRequestSize=1000*1024^2) #1GB

#################################################################################################
###                            Define UI                                                      ###
#################################################################################################

pageNormalizationcUI <- function(id) {
  
  fluidPage(
    
    titlePanel("Synthetic data"),
  
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        hr(),
        
        h4("Download a random mixed sample of original
           and synthetic data (50/50)"),
        
        hr(),
      
        downloadButton('download',"Download")
        
        
        
      ),
      
      mainPanel(
        
        h3("Generated data"),
        
        dataTableOutput("generated"))
    ))
  

}