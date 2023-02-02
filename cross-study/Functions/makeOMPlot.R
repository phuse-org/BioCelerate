#Output OM plot for Cross-Study Analysis for organSystem or all defined 
#in CrossStudyGraph_Combined.R or CrossStudyVisualizationApp.R

#Gender must be restricted for each plot to either M or F or Combined

#Dose must be specified;OMMetric (I.e. Brain Ratio or BW Ratio) must be Specified

#Current organSystem options: REPRODUCTIVE, LIVER, KIDNEY, HEMATOPOIETIC, and
#ENDOCRINE

makeOMplot <- function(OMresults, OrganSystem, OMMetric, Dose, Gender){
  
  #Limit to Desired OrganSystem, Gender, and Dose
  OMGraphData <- OMresults[[OrganSystem]][[Gender]]
  OMGraphData <- OMGraphData[which(OMGraphData$ARMCD == Dose),]
  
  #Filter to Chosen Metric
  if (OMMetric == "BrainRatio"){
    plotDataOM <- aggregate(BrainRatiozScore ~ StudyID + Compound + Species + OMSPEC + ARMCD, FUN = mean, data = OMGraphData) 
  } else if (OMMetric == "BWRatio"){
    plotDataOM <- aggregate(BWRatiozScore ~ StudyID + Compound + Species + OMSPEC + ARMCD, FUN = mean, data = OMGraphData)    
  } else if (OMMetric == "Organ"){
    plotDataOM <- aggregate(OrganzScore ~ StudyID + Compound + Species + OMSPEC + ARMCD, FUN = mean, data = OMGraphData)    
  }
  names(plotDataOM)[6] <- "zScore"
  n <- unique(plotDataOM$OMSPEC) # Number of Unique Tissues for Graph Arrangement
  if (length(n)==1){
    tissue <- n
    plotDataOM2 <- plotDataOM[which(plotDataOM$OMSPEC == tissue),]
    #Make maximum/minimum values 4 and -4
    plotDataOM2$zScore <- replace(plotDataOM2$zScore , plotDataOM2$zScore >4,4)
    plotDataOM2$zScore  <- replace(plotDataOM2$zScore , plotDataOM2$zScore <(-4),-4)
    br2 <- ggplot(plotDataOM2, aes(x = StudyID, y=zScore, fill = Compound)) +
      geom_bar(stat="identity") + ylab(paste0(OMMetric, " Weight zScore")) + 
      ggtitle(paste0("OM ", tissue, " ",Dose," - ",Gender)) +
      ylim(-4,4) + geom_rect(aes(ymin= -1, ymax=1,xmin=0,xmax=Inf), fill = "gray", alpha = .1) +
      theme_light()
    return(br2) 
  } else {
    p <- list()
    for (j in 1:length(n)){ #Filter for multiple organs such as in Hematopoietic 
      tissue <- n[j]
      plotDataOM2 <- plotDataOM[which(plotDataOM$OMSPEC == tissue),]
      #Make maximum/minimum values 4 and -4
      plotDataOM2$zScore <- replace(plotDataOM2$zScore , plotDataOM2$zScore >4,4)
      plotDataOM2$zScore  <- replace(plotDataOM2$zScore , plotDataOM2$zScore <(-4),-4)
      p[[j]] <- ggplot(plotDataOM2, aes(x = StudyID, y=zScore, fill = Compound)) +
        geom_bar(stat="identity") + ylab(paste0(OMMetric, " Weight zScore")) + 
        ggtitle(paste0("OM ", tissue, " ",Dose," - ",Gender)) +
        ylim(-4,4) + geom_rect(aes(ymin= -1, ymax=1,xmin=0,xmax=Inf), fill = "gray", alpha = .1) +
        theme_light()
    }
    p <- do.call(grid.arrange,p)
    
    return(p)
  }
  
}