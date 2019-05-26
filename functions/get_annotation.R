get_annotation <- function(organism){
  
  # Selecting annotation package from organism selection
  if( organism == "Anopheles"){
    annotation <- "org.Ag.eg.db"
  }
  if( organism == "Arabidposis"){
    annotation_db <- "org.At.tair.db"
  }
  if (organism == "Bovine"){
    annotation_db <- "org.Bt.eg.db"
  }
  if (organism == "Worm"){
    annotation_db <- "org.Ce.eg.db"
  }
  if (organism == "Canine"){
    annotation_db <- "org.Cf.eg.db"
  }
  if (organism == "Fly"){
    annotation_db <- "org.Dm.eg.db"
  }
  if (organism == "Zebrafish"){
    annotation_db <- "org.Dr.eg.db"
  }
  if (organism == "E.coli strain K12"){
    annotation_db <- "org.EcK12.eg.db"
  }
  if (organism == "E.coli strain Sakai"){
    annotation_db <- "org.EcSakai.eg.db"
  }
  if (organism == "Chicken"){
    annotation_db <- "org.Gg.eg.db"
  }
  if (organism == "Human"){
    annotation_db <- "org.Hs.eg.db"
  }
  if (organism == "Mouse"){
    annotation_db <- "org.Mm.eg.db"
  }
  if (organism == "Rhesus"){
    annotation_db <- "org.Mmu.eg.db"
  }
  if (organism == "Malaria"){
    annotation_db <- "org.Pf.plasmo.db"
  }
  if (organism == "Chimp"){
    annotation_db <- "org.Pt.eg.db"
  }
  if (organism == "Rat"){
    annotation_db <- "org.Rn.eg.db"
  }
  if (organism == "Yeast"){
    annotation_db <- "org.Sc.sgd.db"
  }
  if (organism == "Pig"){
    annotation_db <- "org.Ss.eg.db"
  }
  if (organism == "Xenopus"){
    annotation_db <- "org.Xl.eg.db"
  }
  
  return(annotation_db)
}