#################################################################################################################################################
# Function to read affymetrix CEL files using oligo and affy packages
# Author: Rubén Sánchez Fernández
# Input 1: Path to .zip file containing CEL files; Input 2: Path to .txt file with experimental information
#################################################################################################################################################


reading_files <- function(input1, input2,norm_method, data_type){
  
  if(data_type == "affymetrix"){
    
    # Checking extension
    ext <- grepl("*.txt$", input2)
    
    if(ext){
      # Reading .txt file
      sampleInfo <- read.AnnotatedDataFrame(input2, header = TRUE)
      
    } else {
      
      stop("Annotation file must be .txt")
    }
    
    # Getting filenames
    fileNames <- sampleInfo$FileName
    
    
    # Reading .CEL files
    tmp <- tempdir() #temporary directory
    
    # Creating temporary folder if it doesn't exists
    if (!dir.exists(tmp)) {
      dir.create(tmp)
    }
    
    # Descompressing zip
    files <- unzip(input1, exdir = tmp, overwrite = TRUE)
    
    # Checking extension
    ext1 <- grepl("*.cel$", files)[1]
    ext2 <- grepl("*.CEL$", files)[1]
    
    
    # In some cases, affy returns a warning
    if(ext1 || ext2){
      
      warn <- tryCatch(read.affybatch(filenames = file.path(tmp,fileNames), phenoData=sampleInfo), error=function(e) e, warning=function(w) w)
      # If read.affybatch returns error or warning, we should use oligo
      if(is(warn,"error") || is(warn, "warning")){
        
        rawData <- read.celfiles(filenames = file.path(tmp,fileNames), phenoData=sampleInfo)
        
      }else{
        
        rawData <- read.affybatch(filenames = file.path(tmp,fileNames), phenoData=sampleInfo)
      } 
      
    } else {
      # Extension not valid
      stop("Data files must be .CEL")
      
    }
    
    sampleInfo <- pData(sampleInfo)
    
    # Normalizing data if user requires to
    source("functions/normalization.R")
    data_norm <- normalization(rawData, norm_method)
    
    results <- list(info = sampleInfo, data_norm = data_norm, data_raw = rawData)
    
    return(results)
  }
  if(data_type == "rna-seq"){
    
    # Checking extension
    ext1 <- grepl("*.txt$", input1)
    ext2 <- grepl("*.txt$", input2)
    
    if(ext1){
      # Reading .txt file
      counts <- read.delim(file=input1)
      
    } else {
      
      stop("Counts file must be .txt")
    }
    
    if(ext2){
      # Reading .txt file
      targets <- read.delim(file=input2)
      
    } else {
      
      stop("Target file must be .txt")
    }
    
    # Algunos archivos count contienen el nombre del gen en la primera columna, es necesario eliminar esa columna
    # y nombrar los genes como nombre de fila
    if(class(counts[,1]) == "factor"){
      
      rownames(counts) <- counts[,1]
      counts[,1] <- NULL
    }
    
    # Filtrado de genes con baja expresión
    cpm <- cpm(counts) # cálculo de recuento por millón
    thresh <- cpm > 1 # filtro baja expresión 
    keep <- rowSums(thresh) >= 2 # filtrado de mínimo dos muestras sin baja expresión
    counts.keep <- counts[keep,] # filtrar genes que no superan las condiciones de expresión 
    
    results <- list(info = targets, data_raw = counts.keep)
    
    return(results)
  }
  
 
}

