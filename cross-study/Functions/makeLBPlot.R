#Output LB plot for Cross-Study Analysis for organSystem or all defined 
#in CrossStudyGraph_Combined.R or CrossStudyVisualizationApp.R

#Gender must be restricted for each plot to either M or F or Combined

#Dose must be specified; Vehicle/Control will be added for comparison

#LBSPEC Method must be specified: WHOLE BLOOD, URINE or URINE

#Current organSystem options: REPRODUCTIVE, LIVER, KIDNEY, HEMATOPOIETIC, and
#ENDOCRINE

makeLBplot <- function(LBresults, OrganSystem, sources, Dose, Gender){
  
  #Limit to Desired OrganSystem and Gender
  LBData <- LBresults[[OrganSystem]][[Gender]]
  #Convert Source to Test Group
  if (sources == 'WHOLE BLOOD'){
    testgroup <- "Hematology"
  } else if (sources == 'SERUM'){
    testgroup <- 'Clinical Chemistry'
  } else if (sources == 'URINE'){
    testgroup <- 'Urinalysis'
  }
  
  #Create LBplotData from LBData for source and aggregate
  LBplotData <- LBData[which(LBData$LBSPEC == sources),]
  LBplotData <- LBplotData[which(LBplotData$ARMCD == Dose),]
  LBplotData$Species <- word(LBplotData$StudyID,1)
  LBplotData$Compound <- word(LBplotData$StudyID, 2)
  LBplotData$LBTESTCD <- factor(LBplotData$LBTESTCD)
  LBplotData <- aggregate(zscore ~ StudyID + Compound + Species + LBTESTCD + ARMCD, FUN = mean, data = LBplotData)
  #Remove Values that Do NOT have Data for all studies
  completeData <- names(which(table(LBplotData$LBTESTCD) == length(unique(LBplotData$StudyID))))
  LBplotData <- LBplotData[which(LBplotData$LBTESTCD %in% completeData),]
  #Set Maximum/Minimum to realistic values
  LBplotData$zscore <- replace(LBplotData$zscore, LBplotData$zscore>4,4)
  LBplotData$zscore <- replace(LBplotData$zscore, LBplotData$zscore<(-4),-4)
  
  #Sort Y-axis by means
  meanData <- aggregate(zscore ~ LBTESTCD, FUN = mean, LBplotData)
  meanData <- meanData[order(as.vector(meanData$zscore), decreasing = F),]
  LBplotData$LBTESTCD <- factor(LBplotData$LBTESTCD, levels = meanData$LBTESTCD)
  
  #Replace Anonymized Compounds w/ 'Compound A' and 'Compound B' for paper
  LBplotData$Compound <-str_replace(LBplotData$Compound, "6576", "Compound A")
  LBplotData$Compound <-str_replace(LBplotData$Compound, "5492", "Compound B")

  qr <- ggplot(LBplotData,aes(x = zscore, y = LBTESTCD, group = LBTESTCD, color = Compound, shape = Species)) +
    geom_rect(aes(ymin= -Inf, ymax=Inf,xmin=-1,xmax=1), color = 'gray', fill = "gray", alpha = .05) +
    geom_point(size =5) +
    scale_color_manual(values=c(rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))) +
    ggtitle(paste0(OrganSystem, " ", testgroup, ' zScore', " - ", Dose, " ",  Gender)) +
    geom_point(stat = 'summary', color = 'black', shape = '|', size = 10) + scale_x_continuous(limits = c(-4,4))+
    theme_light()
  return(qr)

}