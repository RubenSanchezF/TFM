# Shiny application for differential expresion analysis

This repository contains a web-based tool that allows the user to load his own experimental Affymetrix microarray data or RNA-Seq data (alternatively data can be downloaded from ArrayExpress database), and perform the usual pipeline of differential gene expression analysis:

1. Pre-analysis visual exploration of the data
2. Identification of DEG
3. Annotation and enrichment analysis

The tool is developed using shiny, a web application framework for R.

## To run the tool:

1. Download the repository and extract it.

2. Run [ui.R](https://github.com/RubenSanchezF/TFM/blob/master/ui.R) or [server.R](https://github.com/RubenSanchezF/TFM/blob/master/server.R)

## Requirements:

R version 3.5.3.

[Biocmanager()](https://cran.r-project.org/web/packages/BiocManager/index.html) package installed

