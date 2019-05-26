
library_checking <- function(libraries){
  
  #checking if variables are already installed
  check.libraries <- is.element(libraries, installed.packages()[, 1])==FALSE
  libraries.to.install <- libraries[check.libraries]
  if (length(libraries.to.install!=0)) {
    install.packages(libraries.to.install)
    BiocManager::install(libraries.to.install)
  }
  
  #checking if the installation was succesfull
  success <- sapply(libraries,require, quietly = FALSE,  character.only = TRUE)
  if(length(success) != length(libraries)) {stop("A package failed to return a success in require() function.")}
  rm(libraries, libraries.to.install, check.libraries)
}


# Checking and loading libraries
#install.packages("BiocManager")
libraries <- c("affy","oligo","ArrayExpress","Biobase","edgeR","gcrma","ggplot2","ggrepel","pcaMethods","limma","EMA","annotate","GOstats",
               "ggdendro","RColorBrewer","gplots","shinydashboard","shinyjs", "shinyBS", "DT","shinyalert","shinythemes")
library_checking(libraries)

