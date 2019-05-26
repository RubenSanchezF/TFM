#################################################################################################################################################
# Function to create plots for exploratory array analysis
# Author: Rubén Sánchez Fernández
# Inputs: rawData: AffyBatch dataset, oligo FeatureSet dataset, or count matrix (RNA-Seq); sampleInfo: data frame with experimental information
#################################################################################################################################################


exploratory_analysis <- function(rawData, sampleInfo, column=NULL, data_type){
  
  if(data_type == "affymetrix"){
    
    # Getting sample names
    sampleNames <- rownames(sampleInfo)
    
    ## Boxplot (oligo package)
    box_plot <- oligo::boxplot(rawData, target = "core", 
                               main = "Boxplot of log2-intensitites for the raw data")
    # Storing plot in a variable
    box_plot <- recordPlot()
    
    
    ## PCA
    PCA_raw <- pcaMethods::pca(t(exprs(rawData)), method="nipals")
    
    data_plot <- data.frame(PC1 = scores(PCA_raw)[,1], PC2 = scores(PCA_raw)[,2], Names = sampleNames)
    
    # Plot
    p <- ggplot(data_plot, aes(PC1, PC2)) + 
      geom_point(colour = "dodgerblue4", shape = 19) +
      ggtitle("PCA plot of the log-transformed raw expression data") +
      xlab(paste("PC1", PCA_raw@R2[1] * 100, "% of variance")) +
      ylab(paste("PC2", PCA_raw@R2[2] * 100, "% of variance")) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Labels
    pca_plot <- p + 
      geom_label_repel(aes(label = sampleNames),
                       box.padding   = 0.35, 
                       point.padding = 0.5,
                       segment.color = 'grey50') +
      theme_classic()
    
    
    # Dendogram
    clust.euclid.average <- hclust(dist(t(exprs(rawData))),method="average")
    dendo <- ggdendrogram(clust.euclid.average, rotate = TRUE) + labs(title = "Hierarchical clustering of samples")
    
    # Returning all plots
    results <- list(boxplot = box_plot, pca = pca_plot, dendogram = dendo)
    
    return(results)
    
  }
  if(data_type == "rna_seq"){
    
    #colors
    colors <- sample(colors(),length(unique(sampleInfo[,column])))
    group.col <- colors[sampleInfo[,column]]
    
    # Boxplot
    logcpm <- cpm(rawData,log=TRUE)
    # Check distributions of samples using boxplots
    boxplot(logcpm, xlab="", ylab="Log2 counts per million",las=2,outline=FALSE, col=group.col)
    # Let's add a blue horizontal line that corresponds to the median logCPM
    abline(h=median(logcpm),col="blue")
    legend("topleft",legend=levels(sampleInfo[,column]),fill=colors)
    title("Boxplots of logCPMs (unnormalised)")
    # Storing plot in a variable
    box_plot <- recordPlot()
    
    # MDS
    plotMDS(rawData,col=group.col)
    legend("topright",legend=levels(sampleInfo[,column]),fill=colors)
    mds <- recordPlot()
    
    # Heatmap
    var_genes <- apply(logcpm, 1, var)
    # Get top 500 genes most variable
    select_var <- names(sort(var_genes, decreasing=TRUE))[1:500]
    highly_variable_lcpm <- logcpm[select_var,]
    
    mypalette <- brewer.pal(11,"RdYlBu")
    morecols <- colorRampPalette(mypalette)
    
    # Plot the heatmap
    heatmap.2(highly_variable_lcpm,col=rev(morecols(50)),trace="none", main="Top 500 most variable genes across samples",
              ColSideColors=group.col,scale="row",margins=c(10,5))
    heatmap <- recordPlot()
    
    results <- list(boxplot = box_plot, pca = mds, dendogram = heatmap)
    
    
    return(results)
    
  }
  
}
            