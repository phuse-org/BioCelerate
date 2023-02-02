#Output BW plots for Cross-Study Analysis for
#in CrossStudyGraph_Combined.R or CrossStudyVisualizationApp.R

#Requires BWMethod and BWMetric to be supplied from possibilities:
#BW Method: TERMBW or BW
#BW Metric: zScore or PercentChange

#Requires Dose and Gender

makeBWplot <- function(BodyWeight,CompileData,BWMethod, BWMetric, Dose, Gender){

  
  if (BWMethod == "BW"){
    if (BWMetric == 'zScore'){
      BWplotData <- merge(BodyWeight, CompileData[,c("USUBJID", "Species")], by = "USUBJID")  
      BWplotData$BWzScore <- NA
      CompileData$BWzScore <- NA
      for (study in unique(BWplotData$StudyID)){
        StudyWeight <- BWplotData[which(BWplotData$StudyID == study),]
        stdyidx <- which(BWplotData$StudyID %in% study)
        compidx <- which(CompileData$StudyID %in% study)
        for (day in unique(StudyWeight$VISITDY)){
          DayWeight <- StudyWeight[which(StudyWeight$VISITDY == day),]
          DYidx <- which(BWplotData$StudyID %in% study & BWplotData$VISITDY == day)
          ControlAnimals <- DayWeight[which(DayWeight$ARMCD == 'Vehicle'),]
          ControlMean <- mean(as.numeric(ControlAnimals$BWSub), na.rm = TRUE)
          ControlSD <- sd(as.numeric(ControlAnimals$BWSTRESN), na.rm = TRUE)
          BWplotData$BWzScore[DYidx] <- (BWplotData$BWSub[DYidx] - ControlMean)/ControlSD
        }
      }
      
    } else if (BWMetric == "PercentChange"){
      BWplotData <- merge(BodyWeight, CompileData[,c("USUBJID", "Species")], by = "USUBJID") 
      for (study in unique(BWplotData$StudyID)){
        StudyWeight <- BWplotData[which(BWplotData$StudyID == study),]
        stdyidx <- which(BWplotData$StudyID %in% study)
        compidx <- which(CompileData$StudyID %in% study)
        for (day in unique(StudyWeight$VISITDY)){
          DayWeight <- StudyWeight[which(StudyWeight$VISITDY == day),]
          DYidx <- which(BWplotData$StudyID %in% study & BWplotData$VISITDY == day)
          ControlAnimals <- DayWeight[which(DayWeight$ARMCD == 'Vehicle'),]
          BaselineControlMean <- mean(ControlAnimals$BaselinePercentChange, na.rm = TRUE)
          BWplotData$BaselinePercentChange[DYidx] <- BWplotData$BaselinePercentChange[DYidx] - BaselineControlMean
        }
      }
    }
  } 
  if (BWMethod == "TERMBW"){
    if (BWMetric == 'zScore'){
      BWplotData <- BodyWeight
      BWplotData$BWzScore <- NA
      for (study in unique(BWplotData$StudyID)){
        stdyidx <- which(BWplotData$StudyID %in% study)
        Cntrolanimals <- BWplotData[which(BWplotData$ARMCD == "Vehicle" & BWplotData$StudyID %in% study),]
        CntrolBaselinemean <- mean(Cntrolanimals$BWSub, na.rm = TRUE)
        CntrolBaseSD <- sd(Cntrolanimals$BWSTRESN, na.rm = TRUE)
        BWplotData$BWzScore[stdyidx] <- (BWplotData$BWSub[stdyidx] - CntrolBaselinemean )/CntrolBaseSD
      }
    }
    if (BWMetric == 'PercentChange'){
      BWplotData <- BodyWeight
      BWplotData$BaselinePercentChange <- NA
      for (study in unique(BWplotData$StudyID)){
        stdyidx <- which(BWplotData$StudyID %in% study)
        Cntrolanimals <- BWplotData[which(BWplotData$ARMCD == "Vehicle" & BWplotData$StudyID %in% study),]
        CntrolBaselinemean <- mean(Cntrolanimals$BWSTRESN, na.rm = TRUE)
        BWplotData$BaselinePercentChange[stdyidx] <- BWplotData$BWSub[stdyidx]/CntrolBaselinemean*100
      }
    }
  }
  #BW Plots
  BWplotData$Compound <- word(BWplotData$StudyID,2)
  BWplotData <- BWplotData[which(BWplotData$ARMCD == Dose),] 
  if (Gender == "M"){
    BWplotData <- BWplotData[-which(BWplotData$USUBJID %in% c("87497-4001", "87497-4002", "87497-4003")),] #manually excluding some animals not caught earlier
  }
  if (BWMethod == "BW"){
    if (BWMetric == 'zScore') {
      BWplotData <- aggregate(BWzScore ~ VISITDY+ Species + ARMCD+Compound, FUN = mean, data = BWplotData)
      q <- ggplot(BWplotData, aes(x = VISITDY, y = BWzScore, color = Compound, shape = Species))+
        geom_point() + geom_line() + ylab(paste0(BWMetric, " of Weight")) + 
        scale_x_continuous(breaks = round(seq(min(BWplotData$VISITDY), max(BWplotData$VISITDY), by = 5)))+
        ggtitle(paste0("Baseline Subtracted BW - ", Dose, " ", Gender)) 
      return (q)
    } else {
      BWplotData <- aggregate(BaselinePercentChange ~ VISITDY+ Species + ARMCD+Compound, FUN = mean, data = BWplotData)
      q <- ggplot(BWplotData, aes(x = VISITDY, y = BaselinePercentChange, color = Compound, shape = Species))+
        geom_point() + geom_line() + ylab(paste0(BWMetric, " of Weight")) + 
        scale_x_continuous(breaks = round(seq(min(BWplotData$VISITDY), max(BWplotData$VISITDY), by = 5)))+
        ggtitle(paste0("Baseline Subtracted BW - ", Dose, " ", Gender)) 
      return (q)
    }
  } else {
    #TermBW
    if (BWMetric == 'zScore'){
      BWplotData <- aggregate(BWzScore ~ Species + ARMCD+Compound, FUN = mean, data = BWplotData)
      q <- ggplot(BWplotData, aes(x = Species, y = BWzScore, fill = Compound, shape = Species)) +
        guides(colour = guide_legend(override.aes = list(shape = NA))) +
        geom_col(position = 'dodge2') + ylab(paste0(BWMetric, " of Weight")) +
        ggtitle(paste0("TermBW - ", Dose, " ", Gender)) +
        scale_fill_manual(values = c('black', 'blue', 'dark green', 'red', 'purple')) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      return (q)
      
    } else {
      BWplotData <- aggregate(BaselinePercentChange ~ Species + ARMCD + Compound, FUN = mean, data = BWplotData)
      q <- ggplot(BWplotData, aes(x = Species, y = BaselinePercentChange, fill = Compound, shape = Species)) +
        guides(colour = guide_legend(override.aes = list(shape = NA))) +
        geom_col(position = 'dodge2') + ylab(paste0(BWMetric, " of Weight")) +
        ggtitle(paste0("TermBW - ", Dose, " ", Gender)) +
        scale_fill_manual(values = c('black', 'blue', 'dark green', 'red', 'purple')) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      return (q)
      
    }
  }
  
}