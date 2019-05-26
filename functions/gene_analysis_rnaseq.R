#################################################################################################################################################
# Function to perform differential statistical analysis with LIMMA package in RNA-Seq data
# Author: Rubén Sánchez Fernández
# Inputs: counts: data.frame returned by reading_files function; sampleInfo: data frame containing experimental information
# method: for now only "tmm" avalaible
#################################################################################################################################################

analysis_rnaseq <- function(counts, sampleInfo, norm_method, fdr = 0.1, column_conditions, organism){
  
  # Normalizing data if user requires to
  source("functions/normalization.R")
  dge <- normalization(counts, norm_method)
  
  form <- as.formula(paste("~", paste(column_conditions, collapse="+")))
  
  design <- model.matrix(form, data = sampleInfo)
  #colnames(design) <- c("Int", column_conditions)
  
  # Voom
  v <- voom(dge,design,plot=FALSE)
  
  # Adjust and evaluate the model
  fit <- lmFit(v,design)
  fit <- eBayes(fit)
  results <- decideTests(fit)
  

  
  # If no organism is selected, returning table without annotated results
  if(organism == "none"){
    tables <- list()
    for( i in 1:ncol(fit$coefficients)){
      
      tables[[i]] <- topTable(fit, number=nrow(fit), coef=i, adjust = "fdr", p.value = fdr)
      
    }
    
    #MA plot
    plot <- list()
    for( i in 1:ncol(fit$coefficients)){
      
      plotMA(fit, coef = i, colnames(fit)[i],xlab = "Average log-expression",
             ylab = "log-fold-change", status = ifelse(p.adjust(fit$p.value[,i], method="fdr")<fdr,"red","black"), legend = FALSE)
      
      plot[[i]] <- recordPlot()}
    
    return(list(results = tables, plots = plot, contrasts = colnames(design)))
    
  }

  
  # Annotation
  # Getting annotation from function
  source("functions/get_annotation.R")
  organism_db <- get_annotation(organism)
  
  # Loading library
  require(organism_db, character.only = TRUE)
  
  
  if(organism == "Fly"){
    ann <- select(get(organism_db),keys=rownames(fit),columns=c("ENTREZID","SYMBOL","GENENAME"),keytype = "FLYBASE")
    
  } else {
    
    ann <- tryCatch(select(get(organism_db),keys=rownames(fit),columns=c("ENTREZID","SYMBOL","GENENAME"), keytype = "ENTREZID"),error=function(e) e)
    
    if(is(ann,"error")){
      
      ann <- tryCatch(select(get(organism_db),keys=rownames(fit),columns=c("ENTREZID","SYMBOL","GENENAME"), keytype = "ENSEMBL"),error=function(e) e)
    }
  }
  
  
  
  fit$genes <- ann
  
  #table
  tables <- list()
  for( i in 1:ncol(fit$coefficients)){
    
    tables[[i]] <- topTable(fit, number=nrow(fit), coef=i, adjust = "fdr", p.value = fdr)
    
  }
  
  
  
  species_name <- unlist(strsplit(organism_db, split='.', fixed=TRUE))[2]
  
  # Enrichment analysis
  # GO
  go <- tryCatch(goana(fit, coef = colnames(design)[2],species = species_name,geneid = "ENTREZID"),error=function(e) e)
  
  if(is(go,"error")){
   go <- "GO not found" 
  } else {
    go <- topGO(go, n=100)
  }
  
  
  # KEGG
  kegg <- tryCatch(kegga(fit, coef = colnames(design)[2],species = species_name,geneid = "ENTREZID"),error=function(e) e)
  
  if(is(kegg,"error")){
    kegg <- "KEGG not found" 
  } else {
    kegg <- topKEGG(kegg, n=100)
  }

  results <- list(annotated_data = tables, GO_results = go, KEGG_results = kegg)
  
  return(results)


}