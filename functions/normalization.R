#################################################################################################################################################
# Function to normalize microarray data using GC-RMA and RMA
# Author: Rubén Sánchez Fernández
# Input : AffyBatch dataset or oligo Set dataset
#################################################################################################################################################

normalization <- function(input, method){
  
  if(method == "none"){
    
    eset <- input
    
    return(eset)
    
  }
  if(method == "TMM"){
    dge <- DGEList(counts=input)
    
    dge <- calcNormFactors(dge, method = "TMM")
    
    return(dge)
  }
    
  if(method == "RLE"){
    
    dge <- DGEList(counts = input)
    dge <- calcNormFactors(dge, method = "RLE")
    return(dge)
  
  } else {
    
    if(class(input) == "AffyBatch"){
      
      if(method == "gcrma"){
        
        invisible(capture.output(eset <- gcrma(input)))
      }
      
      if(method == "rma"){
        
        eset <- affy::rma(input)
      }
      
      if(method == "mas"){
        
        eset <- mas5(input)
      }
      
    } else {
      
      eset <- oligo::rma(input)
    }
  }
  
  eset_matrix <- exprs(eset)
  
  return(eset_matrix)
}