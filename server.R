
library(shiny)


options(shiny.maxRequestSize=1000*1024^2) #1GB

server <- function(input, output, session) {

  ############ CONFIGURATION PAGE #################################
  
  # Hiding/showing inputs as a function of affymetrix/rna-seq selection
  choice1 <- reactive({ input$data })
  
  observe({
    if(choice1() == "Affymetrix"){
      shinyjs::show("file1")
      shinyjs::show("file2")
      shinyjs::show("arraycode")
      shinyjs::show("txt_file")
      shinyjs::show("note")
      shinyjs::show("txt_file_AE")
      shinyjs::show("data_affymetrix")
      shinyjs::show("normalization")
      shinyjs::show("analysis")
      shinyjs::show("column")
      shinyjs::show("conditions")
      shinyjs::show("chip")
      shinyjs::show("species")
      shinyjs::show("help_text")
      shinyjs::show("contrast_message")
      shinyjs::hide("file3")
      shinyjs::hide("file4")
      shinyjs::hide("note2")
      shinyjs::hide("txt_file2")
      shinyjs::hide("column_explo")
      shinyjs::hide("normalization2")
      shinyjs::hide("analysis2")
      shinyjs::hide("column2")
      shinyjs::hide("species_rnaseq")
      shinyjs::hide("contrast_selection")
      
    }
    
    if(choice1() == "RNA-seq"){
      shinyjs::hide("arraycode")
      shinyjs::hide("file1")
      shinyjs::hide("file2")
      shinyjs::hide("txt_file")
      shinyjs::hide("note")
      shinyjs::hide("txt_file_AE")
      shinyjs::hide("data_affymetrix")
      shinyjs::hide("normalization")
      shinyjs::hide("analysis")
      shinyjs::hide("column")
      shinyjs::hide("conditions")
      shinyjs::hide("chip")
      shinyjs::hide("help_text")
      shinyjs::hide("contrast_message")
      shinyjs::hide("contrast_selection")
      shinyjs::hide("species")
      shinyjs::show("file3")
      shinyjs::show("file4")
      shinyjs::show("note2")
      shinyjs::show("txt_file2")
      shinyjs::show("column_explo")
      shinyjs::show("normalization2")
      shinyjs::show("analysis2")
      shinyjs::show("column2")
      shinyjs::show("species_rnaseq")
    }
    
  })
  
  # Hiding/showing affymetrix inputs as a function of file loading or array express
  choice2 <- reactive({ input$data_affymetrix })
  
  observe({
    
    if(choice2() == "Upload your own file"){
      shinyjs::enable("file1")
      shinyjs::enable("file2")
      shinyjs::disable("arraycode")
      shinyjs::show("txt_file")
      shinyjs::show("note")
      shinyjs::hide("txt_file_AE")
      
    }
    
    if(choice2() == "Use ArrayExpress code"){
      shinyjs::enable("arraycode")
      shinyjs::disable("file1")
      shinyjs::disable("file2")
      shinyjs::hide("txt_file")
      shinyjs::hide("note")
      shinyjs::show("txt_file_AE")
      
    }
  })
  
 ##############################################################################################################################
  
  ## AFFYMETRIX/RNA-SEQ DATA PROCESSING CONFIGURATION
 
  ############################################################################################################################# 
  
  # Reading dataset depending on affymetrix (file loading or array express) and rna-seq selection
  dataset <- eventReactive(input$button,{
    
    if(choice1() == "Affymetrix"){
      
      if(choice2() == "Upload your own file"){
        
        if (!isTruthy(input$file1)) return(NULL)
        if (!isTruthy(input$file2)) return(NULL)
        
        inFile1 <- input$file1
        inFile2 <- input$file2
        
        data <- reading_files(inFile1$datapath, inFile2$datapath, "none",data_type = "affymetrix")
        
        return(data)
      }
      
      if(choice2() == "Use ArrayExpress code"){
        
        if(!isTruthy(input$arraycode)) return(NULL)
        
        data <- reading_AE(input$arraycode, "none")
        
        return(data)
      }
    }
    
    if(choice1() == "RNA-seq"){
      
      if (!isTruthy(input$file3)) return(NULL)
      if (!isTruthy(input$file4)) return(NULL)
      
      inFile3 <- input$file3
      inFile4 <- input$file4
      
      data <- reading_files(inFile3$datapath, inFile4$datapath, "none",data_type = "rna-seq")
      
      return(data)
    }
  })
  
  # Printing loading messages
  observeEvent(input$button,{
    
    shinyjs::show("loading")
    
    if(isTruthy(dataset())){
      
      shinyjs::hide("loading")
      
      output$message <- renderPrint({
        
        cat("Data loaded!")
      })
      
      if(choice2() == "Use ArrayExpress code"){
        
        output$txt_file_AE <- renderDataTable(dataset()$info)
      }
    }
  })
  
  # Affymetrix informative note 
  output$note <- renderPrint({
    
    cat("Upload a .txt file containing a table with the phenotypic information.", "\n",
        "The table MUST:","\n", 
        "The first column must be a column named 'Name' naming each experimental sample","\n",
        "The second column must be a column named 'FileName' naming the cel files (e.g. low10.cel)","\n", 
        "See example below")
  })
  
  
  # Rna-seq informative note
  output$note2 <- renderPrint({
    
    cat("Upload a .txt file containing a table with the phenotypic information.", "\n",
        "See example below")
  })
  
  # Loading and rendering target examples
  example <- read.table("./datasets/affymetrix/estrogen/targLimma.txt", header = TRUE)
  example2 <- read.table("./datasets/RNA-Seq/mouse/SampleInfo_corrected.txt", header = TRUE)
  output$txt_file <- renderDataTable(example)
  output$txt_file2 <- renderDataTable(example2)
  
  #############################################################################################################################
  
    ## EXPLORATORY PAGE 
  
  #############################################################################################################################
  
  # Rendering plot information message
  output$description <- renderPrint({
    
    cat("Create a boxplot, a PCA plot and","\n", "a dendogram plot for Affymetrix data","\n",
        "or a boxplot, MDS and a heatmap for RNA-Seq data")
  })
  
  # Updating inputs as a function of dataset loaded
  observe({
    
    column_names <- colnames(dataset()$info)
    
    # Updating selectInput from dataset 
    updateSelectInput(session, "column_explo",
                      choices = column_names, # update choices
                      selected = NULL)
    # Updating selectInput from dataset 
    updateSelectInput(session, "column2",
                      choices = column_names, # update choices
                      selected = NULL)
  })
  
  # Calling exploratory function
  plots <- eventReactive(input$exploratory,{
    
    if(isTruthy(dataset())){
      
      if(choice1() == "Affymetrix"){
        
        p <- exploratory_analysis(dataset()$data_raw, dataset()$info, data_type = "affymetrix")
        
        return(p)
      }
      if(choice1() == "RNA-seq"){
        
        if (!isTruthy(input$column_explo)) return(NULL)
        
        p <- exploratory_analysis(dataset()$data_raw, dataset()$info, input$column_explo, data_type = "rna_seq")
        
        return(p)
      }
      
    }
    
    # Rendering warning message
    if(!isTruthy(dataset())){
      
      output$warning <- renderPrint({
        
        cat("There is no data loaded.")
      })
    }
  })
  
  # Rendering plots
  output$boxplot <- renderPlot(plots()$boxplot)
  output$pca <- renderPlot(plots()$pca)
  output$dendogram <- renderPlot(plots()$dendogram)

  
  # Rendering processing message
  observeEvent(input$exploratory,{
    
    shinyjs::show("processing")
    
    if(isTruthy(plots()$dendogram)){
      shinyjs::hide("processing")
    }
    
  })
  
  
  ###########################################################################################################################
  
    ## DIFFERENTIAL ANALYSIS PAGE
  
  ###########################################################################################################################
  
  # Calling analysis function
  
  analysis <- eventReactive(input$analysis_button,{

    if (!isTruthy(input$analysis)) return(NULL)
    if (!isTruthy(input$fdr)) return(NULL)
    if (!isTruthy(input$column)) return(NULL)
    if (!isTruthy(dataset())) return(NULL)
    
    shinyjs::show("calculating")
    
    if(choice1() == "Affymetrix"){
      
      if(choice2() == "Upload your own file"){
        
        if(input$normalization == "none"){
          
          results_analysis <- analysis_affy(dataset()$data_raw, dataset()$info, input$analysis, input$fdr, input$column, input$conditions)
          
        } else {
          
          inFile1 <- input$file1
          inFile2 <- input$file2
          
          data <- reading_files(inFile1$datapath, inFile2$datapath, input$normalization, data_type = "affymetrix")
          
          results_analysis <- analysis_affy(data$data_norm, data$info, input$analysis, input$fdr, input$column, input$conditions)
        }
      }
      if(choice2() == "Use ArrayExpress code"){
        
        if(input$normalization == "none"){
          
          results_analysis <- analysis_affy(dataset()$data_raw, dataset()$info, input$analysis, input$fdr, input$column, input$conditions)
        } else {
          
          data <- reading_AE(input$arraycode, input$normalization)
          
          results_analysis <- analysis_affy(data$data_norm, data$info, input$analysis, input$fdr, input$column, input$conditions)
        }
        
      }
      
      return(results_analysis)
    }
    if(choice1() == "RNA-seq"){
      
      results_analysis <- analysis_rnaseq(dataset()$data_raw, dataset()$info, input$normalization2, fdr = input$fdr, 
                                               column_conditions=input$column2, organism = "none")
    }
  })
  
  # Hide calculating message when function ends
  observe({
    if(isTruthy(analysis())){
      shinyjs::hide("calculating")}
  })
  
  # Normalization information message
  output$normalization_message <- renderPrint({
    
    cat("GC-RMA and MAS 5.0 only possible with","\n", "Affymetrix data")
  })
  
  
  # Updating input as a function of data loaded
  observe({
    
    column_names <- colnames(dataset()$info)
    
    # Updating selectInput from dataset 
    updateSelectInput(session, "column",
                      choices = column_names, # update choices
                      selected = NULL)
  })
  
  # Updating input as a function of another input selected by user
  observeEvent(input$column,{


    lev <- factor(dataset()$info[,input$column], levels=unique(dataset()$info[,input$column]))

    # Updating selectInput from dataset
    updateSelectInput(session, "conditions",
                      choices = lev, # update choices
                      selected = NULL)
  })
  
  observe({
    
    contrasts <- analysis()$contrasts
    
    
    
    # Updating selectInput from calculated conditions
    updateSelectInput(session, "contrast_selection",
                      choices = contrasts, # update choices
                      selected = contrasts[1])
    
    # Updating selectInput from calculated conditions
    updateSelectInput(session, "contrast_selection2",
                      choices = contrasts, # update choices
                      selected = contrasts[1])
  
   
  })
  
  # Rendering plots and table and activating downloader when user selects contrast
  observeEvent(input$contrast_selection,{
    
    if(!isTruthy(analysis())){
      return(NULL)}
    
    position <- which(analysis()$contrasts == input$contrast_selection)
    
    # Download button
    output$download1 <- downloadHandler(
      filename = function(){
        "statistical_analysis.csv"},
      content = function(fname){
        write.csv(analysis()$results[[position]], fname)},
      contentType = "text/csv")
    
    
    if(input$analysis == "linear"){
      
      shinyjs::show("contrast_selection")
      
      output$analysis_table <- renderDataTable(analysis()$results[[position]],
                                               options = list(pageLength = 10, width="100%", scrollX = TRUE))
      output$plot_diff_analysis <- renderPlot(analysis()$plots[[position]])
    }
    
    if(input$analysis == "sam"){
      
      shinyjs::hide("contrast_selection")
      
      output$analysis_table <- renderDataTable(analysis()$results,
                                               options = list(pageLength = 10, width="100%", scrollX = TRUE))
      output$plot_diff_analysis <- renderPlot(analysis()$plots)
    }
    
    if(input$analysis == "VOOM+EBayes"){
      
      shinyjs::show("contrast_selection")
      
      output$analysis_table <- renderDataTable(analysis()$results[[position]],
                                               options = list(pageLength = 10, width="100%", scrollX = TRUE))
      output$plot_diff_analysis <- renderPlot(analysis()$plots[[position]])
    }

    })
  
  
  ##############################################################################################################################
  
    ## ANNOTATION AND ENRICHMENT
  
  ##############################################################################################################################
  
  position2 <- eventReactive(input$contrast_selection2,{
    
    if(!isTruthy(analysis())){
      return(NULL)}
    
    pos <- which(analysis()$contrasts == input$contrast_selection2)
    
    return(pos)
  })
  
  
  
  # Hiding and showing inputs as a function if user requires enrichment or not
  choice_enrichment <- reactive({ input$choice_enrichment })
  
  observe({
    
    if(choice_enrichment() == "Yes"){
      shinyjs::show("pvalue")
      shinyjs::enable("go")
      shinyjs::enable("kegg")
      
      if(choice1() == "Affymetrix"){
        shinyjs::show("species")
      }
      if(choice1() == "RNA-seq"){
        shinyjs::show("species_rnaseq")
      }
      
    }
    
    if(choice_enrichment() == "No"){
      shinyjs::hide("species")
      shinyjs::hide("species_rnaseq")
      shinyjs::hide("pvalue")
      shinyjs::disable("go")
      shinyjs::disable("kegg")
      
    }
  })
  
  # Calling annotation and enrichment functions
  annotation <- eventReactive(input$button_annotation,{
    
    if(choice1() == "Affymetrix"){
      
      if (!isTruthy(analysis())) return(NULL)
      if (!isTruthy(input$chip)) return(NULL)
      
      shinyjs::show("calculating2")
      
      if(choice_enrichment() == "Yes"){
        
        genes_annotated <- gene_annotation(analysis()$results[[position2()]], input$chip)
        
        enrich <- enrichment(genes_annotated$annotated_data, input$species, input$chip, input$pvalue)
        
        return(enrich)
      }
      
      if(choice_enrichment() == "No"){
        
        genes_annotated <- gene_annotation(analysis()$results[[position2()]], input$chip)
        
        return(genes_annotated)}
      
    }
    if(choice1() == "RNA-seq"){
      
      if (!isTruthy(analysis())) return(NULL)
      if (!isTruthy(input$species_rnaseq)) return(NULL)
      
      results_analysis <- analysis_rnaseq(dataset()$data_raw, dataset()$info, input$normalization2, fdr = input$fdr, 
                                               column_conditions=input$column2, organism = input$species_rnaseq)
      
      return(results_analysis)
    }
    })
  
  # Hiding calculating message when function is done
  observe({

    if(isTruthy(annotation())){
      shinyjs::hide("calculating2")
    }
  })
  
  
  # Rendering outputs
  observe(
    if(choice1() == "Affymetrix"){
      output$annot <- renderDataTable(annotation()$annotated_data,
                                      options = list(pageLength = 10, width="100%", scrollX = TRUE))
      output$download2 <- downloadHandler(
        filename = function(){
          "annotation_results.csv"},
        content = function(fname){
          write.csv(annotation()$annotated_data, fname)},
        contentType = "text/csv")
      
    }else{
      output$annot <- renderDataTable(annotation()$annotated_data[[position2()]],
                                      options = list(pageLength = 10, width="100%", scrollX = TRUE))
      output$download2 <- downloadHandler(
        filename = function(){
          "annotation_results.csv"},
        content = function(fname){
          write.csv(annotation()$annotated_data[[position2()]], fname)},
        contentType = "text/csv")
    })
    
  
  output$go <- renderDataTable(annotation()$GO_results,
                               options = list(pageLength = 10, width="100%", scrollX = TRUE))
  output$kegg <- renderDataTable(annotation()$KEGG_results,
                                 options = list(pageLength = 10, width="100%", scrollX = TRUE))
  
  # Download buttons
  output$download3 <- downloadHandler(
    filename = function(){
      "go_results.csv"},
    content = function(fname){
      write.csv(annotation()$GO_results, fname)},
    contentType = "text/csv")
  
  output$download4 <- downloadHandler(
    filename = function(){
      "kegg_results.csv"},
    content = function(fname){
      write.csv(annotation()$KEGG_results, fname)},
    contentType = "text/csv")
  
}










