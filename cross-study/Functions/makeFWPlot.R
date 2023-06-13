#Output FW plot Day Plot for Cross-Study Analysis for 
#in CrossStudyGraph_Combined.R or CrossStudyVisualizationApp.R

#Includes Variable to display as a Week or by Day Visualizations as Time

#Requires Dose and Gender

makeFWplot <- function(FWData, Time, Dose, Gender){
  

  #FW Plots
  FWData <- FWData[which(FWData$ARMCD %in% Dose),]
  FWData <- FWData[complete.cases(FWData),]
  
  #Replace Anonymized Compounds w/ 'Compound A' and 'Compound B' for paper
  FWData$Compound <-str_replace(FWData$Compound, "6576", "Compound A")
  FWData$Compound <-str_replace(FWData$Compound, "5492", "Compound B")
  
  if (Time == 'Day'){
    #FW Mixed Species Graph
    p <- ggplot(data = FWData, aes(x=FWDY, y=Diff, color = Compound, shape = Species)) +
      ggtitle(paste0("Food Consumption - ", Dose, " ", Gender))+ 
      scale_color_manual(values=c(rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))) +
      scale_x_continuous(breaks = round(seq(min(FWData$FWDY), max(FWData$FWDY), by = 5)))+
      geom_point() +geom_line(linetype = "dashed")+
      ylab("Percent Food Consumption Relative to Control (g/animal/day)") +xlab("Day")
    return(p) 
  } else {
    #Group Data into 7 Day Ranges
    FWData$Week <- floor(round(FWData$FWDY/7))
    FWData2 <- FWData
    FWData2$Diff <- as.numeric(FWData2$Diff)
    FWData2$Diff[is.na(FWData2$Diff)] <- 0
    FWData2 <- FWData2 %>%
      group_by(Compound, Species, Week) %>% summarise(Diff = mean(Diff), na.rm = TRUE)
    
    pg <- ggplot(data = FWData2, aes(x=Week, y=Diff, color = Compound, shape = Species)) +
      ggtitle(paste0("Food Consumption - ", Dose, " ", Gender))+ 
      scale_color_manual(values=c(rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9))) +
      scale_x_continuous(breaks = round(seq(min(FWData$Week), max(FWData$Week), by = 1)))+
      geom_point() +geom_line(linetype = "dashed")+
      ylab("% Food Consumption Relative to Control (g/animal Averaged per Week)") +xlab("Week")
    return(pg)  
  }
  
    
}

