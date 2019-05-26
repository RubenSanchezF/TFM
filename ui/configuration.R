options(shiny.maxRequestSize=1000*1024^2) #1GB

#################################################################################################
###                            Define UI                                                      ###
#################################################################################################



configurationUI <- function(id) {
  
  fluidPage(
    
    h1("Data upload"),
    br(),
    fluidRow(
      column(4,
             radioButtons(inputId="data", label=h3("Select the type of data"), 
                          choices=c("Affymetrix" = "Affymetrix",
                                    "RNA-seq" = "RNA-seq"),
                          selected = "Affymetrix"),
             useShinyjs(),
             fileInput("file3", h3('Upload txt file containing count data'),
                       accept = c(".txt"
                       )),
             br(),
             br(),
             fileInput("file1", h3('Upload zip file'),
                       accept = c(".zip"
                       )),
             textInput("arraycode", "Enter the Array Express code", value = "", width = NULL,
                       placeholder = NULL),
             
             
             br(),
             
             actionButton("button", "Load the data", width = 150),
             br(),
             hidden(
               p(id = "loading", "Loading...")
             ),
             verbatimTextOutput("message")
      ),
      column(8,
             radioButtons(inputId="data_affymetrix", label=h3("How do you want to load your Affymetrix data?"), 
                          choices=c("Upload your own file" = "Upload your own file",
                                    "Use ArrayExpress code" = "Use ArrayExpress code")),
             br(),
             br(),
             fileInput("file2", h3('Upload txt file'),
                       accept = c(".txt"
                       )),
             br(),
             br(),
             br(),
             br(),
             fileInput("file4", h3('Upload txt file containing phenotypic information'),
                       accept = c(".txt"
                       )),
             br(),
             verbatimTextOutput("note"),
             verbatimTextOutput("note2"),
             br(),
             br(),
             dataTableOutput("txt_file"),
             dataTableOutput(("txt_file_AE")),
             dataTableOutput("txt_file2")
      
    )
  )
  )
  
    
  
}