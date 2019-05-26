#################################################################################################################################################
# Function to identify genes from differential analysis model
# Author: Rubén Sánchez Fernández
# Inputs: fit.main: data frame with differentially expressed genes returned by gene_analysis function
# chip: bioconductor annotation package
#################################################################################################################################################

gene_annotation <- function(fit.main, chip){
  
  # Loading annotation library
  require(chip, character.only = TRUE)
  
  
  # Looking for genes that change under condition
  fit.Symbols <- getSYMBOL (rownames(fit.main), chip)
  
  # # Check if chip db is already installed
  # bool <- require(chip, character.only = TRUE)
  # 
  # if(bool == FALSE){
  #   BiocManager::install(chip)
  # 
  # }

  # Getting name of annotation
  annotation_name <- unlist(strsplit(chip, split='.db', fixed=TRUE))[1]
  
  
  # Looking for the names of the genes differentially expressed
  geneSymbols<-unlist(as.list(get(paste0(annotation_name, "SYMBOL"))))
  geneNames<-unlist(as.list(get(paste0(annotation_name, "GENENAME"))))
  geneNames<-substring(geneNames,1,40)
  genelist<-data.frame(GeneSymbol=geneSymbols,GeneName=geneNames)
  
  index <- vector()
  for(i in 1:nrow(fit.main)){
    
    index <- which(rownames(genelist) == rownames(fit.main[i,]))
    fit.main$GeneSymbol[i] <- as.character(genelist$GeneSymbol[index])
    fit.main$GeneName[i] <- as.character(genelist$GeneName[index])
    
  }
  
  results <- list(annotated_data = fit.main, chip = chip)
  
  return(results)

}
  



