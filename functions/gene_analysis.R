#################################################################################################################################################
# Function to perform differential statistical analysis with LIMMA package and EMA (sam function)
# Author: Rubén Sánchez Fernández
# Inputs: eset_rma_matrix: data frame containing normalized probe intensities; sampleInfo: data frame containing experimental information
# method: "linear" for linear analysis or "sam" for significance analysis
#################################################################################################################################################

analysis_affy <- function(eset_rma_matrix, sampleInfo, method = "linear", fdr = 0.1, column_conditions, conditions){
  
  
  index <- apply(as.data.frame(conditions),1, function(x) which(sampleInfo[,column_conditions] == x))
  
  if(class(index) == "list"){
    index <- unlist(index, use.names=FALSE)
  }
  
  which(sampleInfo[,column_conditions] == "healthy control")
  # Levels
  lev <- factor(sampleInfo[,column_conditions][index], levels=unique(conditions))
  lev <- as.factor(make.names(lev))
  
  # If experiment only has 1 sample per group, is not possible to perform statistical analysis
  if(length(levels(lev)) == ncol(eset_rma_matrix)){
    stop("Impossible to perform statistical analysis because there is only one sample per group")
  }
  
  eset_contrast_matrix <- eset_rma_matrix[,index]
  
  # Creating design matrix with limma package
  design <- model.matrix(~ 0+lev)
  colnames(design)<-levels(lev)
  #rownames(design) <- sampleInfo$Name
  
  ## Defining contrast of interest iterativelly
  # Computing possible combinations
  combos <- combn(unique(colnames(design)), 2)
  
  # Computing possible contrasts
  cnt <- vector()
  for(i in 1:ncol(combos)){
    cnt[i] <- paste0(combos[1,i], " - ", combos[2,i])
  }
  
  # Computing contrast matrix
  cont.matrix <- makeContrasts(contrasts = cnt, levels = design)
  cont.matrix
  
  # SAM analysis
  if(method == "sam"){
    # Fitting the model
    invisible(capture.output(fit <- EMA::runSAM(eset_contrast_matrix, lev, nbpermut = 200, q = fdr))) #fdr =0.1
    # Extracting significant genes
    tables <- fit[which(fit$Significant == TRUE),1:3]
    
    plot <- recordPlot()
    
  }
  
  if(method == "linear"){
    
    # Model estimation
    fit<-lmFit(eset_contrast_matrix, design)
    fit.main<-contrasts.fit(fit, cont.matrix)
    fit.main<-eBayes(fit.main)
    
    # Results
    #result_test <- decideTests(fit.main)
    
    # Creating one table for each contrast to save diferentially expresed genes
    tables <- list()
    for( i in 1:ncol(cont.matrix)){
      
      tables[[i]] <- topTable(fit.main, number=nrow(fit.main), coef=i, adjust = "fdr", p.value = fdr)
      
    }
    # Creating volcano plot for each contrast
    plot <- list()
    for( i in 1:ncol(cont.matrix)){
      
      opt <- par(cex.lab = 0.7) 
      
      volcanoplot(fit.main, coef=i, highlight=10, names=rownames(fit.main), 
                               main=paste("Differentially expressed genes",colnames(cont.matrix)[i],sep="\n") , col="darkred")
      abline(v=c(-1,1))
      
      plot[[i]] <- recordPlot()
      
      par(opt)
      
    }
    
    
    # res<-decideTests(fit.main, method="separate", adjust.method="fdr", p.value=0.01)
    # 
    # # Extracting under and over expressed genes
    # sum.res.rows<-apply(abs(res),1,sum)
    # res.selected<-res[sum.res.rows!=0,]
    
  }
  
  results <- list(results = tables, contrasts = colnames(cont.matrix), plots = plot)
  
  return(results)


}