library(shiny)



#################################################################################################
###                            Call all source codes                                          ###
#################################################################################################
# Call html functions
source("functions/html_functions.r")
# Call modules
source("ui/enrichment_shiny.R")
source("ui/exploratory_shiny.R")
source("ui/diff_analysis_shiny.R")
source("ui/configuration.R")
# Call functions
source("functions/reading_files.R")
source("functions/library_checking.R")
source("functions/reading_AE.R")
source("functions/exploratory_analysis.R")
source("functions/normalization.R")
source("functions/gene_analysis.R")
source("functions/gene_annotation.R")
source("functions/enrichment.R")
source("functions/gene_analysis_rnaseq.R")


##### Create main structure for the app (sidebar, header, footer, body) #####

# Define sidebar
sidebar <- dashboardSidebar(
  div(actionLink(inputId="toggle_sb",class="toggle-sidebar",HTML("<i class='fa fa-bars'></i>")),HTML("<hr>")),
  
  #### ADD YOUR MODULES HERE ###
  sidebarMenu(
    menuItem("Configuration", icon = icon("desktop"), tabName = "Tab-1"),
    menuItem("Exploratory analysis", icon = icon("desktop"), tabName = "Tab-2"),
    menuItem("Differential analysis", icon = icon("desktop"), tabName = "Tab-3"),
    menuItem("Annotation and enrichment", icon = icon("desktop"), tabName = "Tab-4")
  )
)

header<- dashboardHeader(
  title = setheader(img="Images/logo.png")
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),
  useShinyjs(),
  useShinyalert(),
  tabItems(
    tabItem(tabName = "Tab-1",
            configurationUI("configuration")),
    tabItem(tabName = "Tab-2",
            pageThreeUI("exploratory_shiny")),
    tabItem(tabName = "Tab-3",
            pageAnalysisUI("diff_analysis_shiny")),
    tabItem(tabName = "Tab-4",
            pageEnrichmentUI("enrichment_shiny"))
  ),
  footerBox(text="Software for microarray differential analysis",text2="Copyright 2019",company="Ruben Sanchez Fernandez",text3="All rights reserved")
)


dashboardPage(title=titlePanel(title="Software for microarray differential analysis"),
              header,
              sidebar,
              body,
              skin = "black")
