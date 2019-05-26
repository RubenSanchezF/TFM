#library(shinyTable)
library(DT)
options(shiny.maxRequestSize=1000*1024^2) #1GB

#################################################################################################
###                            Define UI                                                      ###
#################################################################################################

pageEnrichmentUI <- function(id) {
  
  fluidPage(
    
    titlePanel("Annotation and enrichment analysis"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        useShinyjs(),
        
        textInput("chip", "Enter microarray chip name", value = "", width = NULL,
                  placeholder = "hgu95av2.db"),
        tags$div(id="help_text",helpText(a("You can obtain your chip name in here", target="_blank",href="https://bioconductor.org/packages/release/data/annotation/"))),
        
        selectInput("contrast_selection2", "Select contrast", list("."="."),
                    multiple = FALSE, selected = NULL),
        
        
        
        br(),
        
        radioButtons(inputId="choice_enrichment", label="Do you want to perform enrichment analysis?", 
                     choices=c("Yes" = "Yes",
                               "No" = "No"),
                     selected = "No"),

        br(),
        selectInput("species_rnaseq", "Select the species", list("Anopheles" = "Anopheles", 
                                                                 "Arabidposis" = "Arabidposis",
                                                                 "Bovine"="Bovine", "Canine"="Canine",
                                                                 "Chicken" = "Chicken","Chimp"="Chimp",
                                                                 "E.coli strain K12"="E.coli strain K12",
                                                                 "E.coli strain Sakai"="E.coli strain Sakai",
                                                                 "Fly"="Fly","Human"="Human","Malaria"="Malaria",
                                                                 "Mouse"="Mouse","Pig"="Pig","Rat"="Rat",
                                                                 "Rhesus"="Rhesus","Worm"="Worm","Xenopus"="Xenopus",
                                                                 "Yeast"="Yeast","Zebrafish"="Zebrafish"
        ),
        multiple = FALSE, selected = "Human"),
        
        selectInput("species", "Select the species", list("Anopheles" = "Anopheles", 
                                                          "Arabidposis" = "Arabidposis",
                                                          "Bovine"="Bovine", "Canine"="Canine",
                                                          "Chicken" = "Chicken","Chimp"="Chimp",
                                                          "E.coli strain K12"="E.coli strain K12",
                                                          "E.coli strain Sakai"="E.coli strain Sakai",
                                                          "Fly"="Fly","Human"="Human","Malaria"="Malaria",
                                                          "Mouse"="Mouse","Pig"="Pig","Rat"="Rat",
                                                          "Rhesus"="Rhesus","Worm"="Worm","Xenopus"="Xenopus",
                                                          "Yeast"="Yeast","Zebrafish"="Zebrafish"
                                                          ),
                    multiple = FALSE, selected = "Human"),
        
        br(),
        
        textInput("pvalue", "Enter a p-value", value = 0.01, width = NULL,
                  placeholder = NULL),
        
        actionButton("button_annotation", "Calculate", width = 150),
        br(),
        
        hidden(
          p(id = "calculating2", "Calculating...")
        )
      ),
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Annotated data", dataTableOutput("annot"), downloadButton('download2',"Download annotation table")),
                    tabPanel("GO", dataTableOutput("go"),downloadButton('download3',"Download GO table")),
                    tabPanel("KEGG", dataTableOutput("kegg"), downloadButton('download4',"Download KEGG table")))
        )
  ))
  
  
}