#################################################################################################################################################
# Function to import affymetrix data from ArrayExpression database
# Author: Rubén Sánchez Fernández
# Input : ArrayExpress code
#################################################################################################################################################

reading_AE <- function(input, norm_method){
  
  # Temporary directory
  tmp <- tempdir()
  
  # Creating temporary folder if it doesn't exists
  if (!dir.exists(tmp)) {
    dir.create(tmp)
  }
  
  # Accepting only codes that meet the requirements
  warn <- tryCatch(getAE(input, path = tmp, type = "raw"), error=function(e) e, warning=function(w) w)
  # If function returns error, stop compiling
  if(is(warn,"error")){
    return(NULL)}
  
  # Downloading data from ArrayExpress
  a_AE <- getAE(input, path = tmp, type = "raw")
  
  # Extracting sample annotation
  annotation_path <- file.path(tmp, a_AE$sdrf)
  sampleInfo <- read.delim(annotation_path)
  
  # Getting filenames
  fileNames <- sampleInfo$Array.Data.File
  sampleInfo <- AnnotatedDataFrame(sampleInfo)
  
  # Reading CEL files
  # Using affy or oligo package depending on type of array
  warn <- tryCatch(read.affybatch(filenames = file.path(tmp,fileNames), phenoData=sampleInfo), error=function(e) e, warning=function(w) w)
  
  # If read.affybatch returns error or warning, we should use oligo
  if(is(warn,"error") || is(warn, "warning")){
    
    rawData <- read.celfiles(filenames = file.path(tmp,fileNames), phenoData=sampleInfo)
                             
  }else{
    
    rawData <- read.affybatch(filenames = file.path(tmp,fileNames), phenoData=sampleInfo)
  } 
    
  
  # Check if the object is valid
  stopifnot(validObject(rawData))
  
  # Changing features names for further analysis
  sampleInfo <- pData(sampleInfo)
  Name <- sampleInfo[, grep("Source.Name", colnames(sampleInfo))]
  sampleInfo$Name <- Name
  rownames(sampleInfo) <- Name
  
  # Getting target for differential analysis
  #Target <- sampleInfo[, grep("Factor", colnames(sampleInfo))]
  
  # For now we will pick the first target variable, in shiny app we will configurate this step so user can select the target
  #sampleInfo$Target <- as.data.frame(Target)[,1]
  
  # Normalizing data if user requires to
  source("functions/normalization.R")
  data_norm <- normalization(rawData, norm_method)
  
  results <- list(info = sampleInfo, data_norm = data_norm, data_raw = rawData)

  
  return(results)
}
