#################################################################################################################################################
# Function to perform simple GO and KEGG enrichment analysis
# Author: Rubén Sánchez Fernández
# Inputs: table: data frame containing diferentially expressed genes; organism: organism name
# chip : chip annotation package
#################################################################################################################################################

enrichment <- function(table, organism, chip, p_value){
  
  # Selecting table we want to use for enrichment analysis
  #topTab <- tables[[1]]
  
  # Getting annotation from function
  source("functions/get_annotation.R")
  organism_db <- get_annotation(organism)
  
  # Loading libraries
  require(organism_db, character.only = TRUE)
  
  entrezUniverse = unique(getEG(as.character(rownames(table)), chip))
  
  if(is.null(table$adj.P.Val)){
    whichGenes <- table$RawpValue<p_value
  }else{
    whichGenes <- table$adj.P.Val<p_value
  }
  
  if (length(whichGenes == TRUE)==0){
    return("No results met the specified criteria. Try to adjust the p-value")
  } 
  
  geneIds <- unique(getEG(as.character(rownames(table)[whichGenes]), chip))
  
  
  GOparams = new("GOHyperGParams",geneIds=geneIds, universeGeneIds=entrezUniverse,
                 annotation=organism_db, ontology="BP", pvalueCutoff=as.numeric(p_value), 
                 conditional=FALSE,testDirection="over")
  
  KEGGparams = new("KEGGHyperGParams", geneIds=geneIds, universeGeneIds=entrezUniverse,
                   annotation=organism_db, pvalueCutoff=as.numeric(p_value), testDirection="over")
  
  GOhyper = hyperGTest(GOparams)
  
  go_results <- summary(GOhyper)
  
  KEGGhyper = hyperGTest(KEGGparams)
  
  kegg_results <- summary(KEGGhyper)
  
  return(list(GO_results = go_results, KEGG_results = kegg_results, annotated_data = table))
}
