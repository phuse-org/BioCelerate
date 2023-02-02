
#Output Radar plot for Cross-Study Analysis for organSystem or all defined 
#in CrossStudyGraph_Combined.R or CrossStudyVisualizationApp.R

#Gender must be restricted for each plot to either M or F or Combined

#Current organSystem options: ALL, SUMMARY, REPRODUCTIVE, LIVER, KIDNEY, HEMATOPOIETIC,
#ENDOCRINE and BW

makeRadar <- function(summaryData, organSystem, Gender) {
  
  library(fmsb)
  organSystem <- toupper(organSystem)
  #Limit summaryData to desired Organ System
  if (organSystem %in% c('ALL', 'SUMMARY'))
  {
    #Takes Summary Values which do not have '.'
    Data <- summaryData[!grepl("\\.", rownames(summaryData)),]
    Title <- "Summary"
    a <- 0
  } else {
    #Limits summaryData to desired organSystem for individual radar plots
    Data <- summaryData[which(grepl(paste0(organSystem,"."),rownames(summaryData))),]
    Title <- organSystem
    a <- 1
  }
  
  #Limit to Gender Desired
  Gender <- toupper(Gender)
  Data <- Data[ ,which(grepl(Gender, colnames(Data)))]
  
  #Create better labeps
  if (a == 0){

  } else {
    rownames(Data) <- gsub(paste0(organSystem,"."),"",rownames(Data))
  }
  
  #Format Data for RadarChart by adding min and max to dataset
  Data <- t(Data)
  Data <- rbind(rep(3,ncol(Data)),rep(0,ncol(Data)), Data)
  Data <- as.data.frame(Data)
  
  #Create Group Names
  GroupNames <-str_replace(rownames(Data)[3:nrow(Data)],"\\.", " ")
  
  #Set Colors for Radar Plot >> Currently Hardcoded for BioCelerate Order
  colors_border <- c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9), rgb(0.2,0.7,0.5,0.9), 
                     rgb(0.4,0.2,0.5,0.9))
  
  #Generate Radar plot
  par(xpd= TRUE,mar = c(1,1,1,1), oma = c(2,2,2,2))
  radarplot <- radarchart(Data,
                          #Customize Background Grid
                          cglcol = "grey",cglwd = 0.8, 
                          #Customize Data Coloring
                          plty = 1, pcol = colors_border,plwd = 4, 
                          title = paste0(Title, " Radar Plot"))
  legend(x=.75, y=1.25, legend=GroupNames, bty = "n", pch = 20, 
         col = colors_border, text.col = "black", cex = 0.9, pt.cex = 1.6)
  p <- recordPlot(radarplot)
  return(p)
}