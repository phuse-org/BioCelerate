rm(list=ls())

library(ggplot2)
library(Hmisc)
library(tools)
library(devtools)
library(httr)

localPath <- '~/PhUSE/Git/phuse-scripts/'

Local <- T
savePlots <- F
saveFolder <- 'C:/Users/Kevin.Snyder/Box Sync/Biocelerate/Cross Study Comparison Project/BW Analysis/'

# Define Test
TESTCD <- 'BW'
TESTCD <- 'TERMBW'

if (Local == T) {
  source(paste0(localPath,'contributed/Nonclinical/R/Functions/Functions.R'))
  source(paste0(localPath,'contributed/Nonclinical/R/Functions/groupSEND.R'))
} else {
  source_url('https://raw.githubusercontent.com/phuse-org/phuse-scripts/master/contributed/Nonclinical/R/Functions/Functions.R')
  source_url('https://raw.githubusercontent.com/phuse-org/phuse-scripts/master/contributed/Nonclinical/R/Functions/groupSEND.R')
}
  
paths <- c('data/send/FFU-Contribution-to-FDA',
           'data/send/PDS',
           'data/send/instem',
           'data/send/PointCross')
# paths <- c('data/send/FFU-Contribution-to-FDA')
# paths <- c('data/send/PDS')
# paths <- c('data/send/instem')
# paths <- c('data/send/PointCross')

# Define Names of Dose Ranks
doseRanks <- c('Vehicle', 'Treatment LD', 'Treatment MD', 'Treatment HD', 'Treatment Max')

for (path in paths) {
  print(path)
  
  if (Local == T) {
    Data <- load.xpt.files(paste0(localPath,path))
  } else {
    Data <- load.GitHub.xpt.files(studyDir = path)
  }
  
  # Get Species of Study
  Species <- Data$ts$TSVAL[which(Data$ts$TSPARMCD == 'SPECIES')]
  
  # Merge BW with other relevant domains
  BW <- groupSEND(Data,'bw')
  
  # Set Day 1 Index
  d1Index <- which(BW$VISITDY == 1)
  baselineData <- BW[d1Index,]
  
  # Rank Order Treatment Groups by TRTDOS
  BW$TRTDOSrank <- NA
  if (!is.null(BW$TRTDOS)) {
    TRTDOSvalues <- as.numeric(unique(BW$TRTDOS))
  } else {
    TRTDOSvalues <- as.numeric(unique(BW$EXDOSE))
  }
  TRTDOSranks <- rank(TRTDOSvalues)
  for (i in seq(nrow(BW))) {
    if (!is.null(BW$TRTDOS)) {
      index <- TRTDOSranks[which(TRTDOSvalues == as.numeric(BW$TRTDOS[i]))]
    } else {
      index <- TRTDOSranks[which(TRTDOSvalues == as.numeric(BW$EXDOSE[i]))]
    }
    BW$TRTDOSrank[i] <- doseRanks[index]
  }
  
  # Concatenate STUDYID and TreatmentDose and Species
  BW$StudySpecies <- paste0(BW$STUDYID, ' (', Species, ')')
  
  # Cap Study at 28 days
  if (TESTCD != 'TERMBW') {
    removeIndex <- which(BW$VISITDY > 29)
    if (length(removeIndex) > 0) {
      BW <- BW[-removeIndex,]
    }
  }
  
  # Remove TK Animals
  if (Species == 'RAT') {
    removeIndex <- which(BW$TKstatus == T)
    if (length(removeIndex) > 0) {
      BW <- BW[-removeIndex,]
    }
  }
  
  # Remove Recovery Animals
  removeIndex <- which(BW$RecoveryStatus == T)
  if (length(removeIndex) > 0) {
    BW <- BW[-removeIndex,]
  }
  
  # Select Sex
  sexIndex <- which(BW$SEX == 'F')
  BW <- BW[sexIndex,]
  
  # Select Test
  testIndex <- which(BW$BWTESTCD == TESTCD)
  BW <- BW[testIndex,]
  
  # Find Control Animals
  c1Index <- grep('control', BW$Treatment, ignore.case = T)
  c2Index <- grep('vehicle', BW$Treatment, ignore.case = T)
  cIndex <- union(c1Index,c2Index)
  
  Model <- lm(BWSTRESN ~ VISITDY, data = BW)
  BW.sd.c <- sd(Model$residuals[cIndex])
  
  BW$zScore <- NA
  BW$zScoreModel <- NA
  BW$percentControl <- NA
  BW$baselineChange <- NA
  BW$baselineChangePercent <- NA
  BW$zScoreBaselineChange <- NA
  BW$zScoreModelBaselineChange <- NA
  BW$percentChangeBaselineChange <- NA
  
  Days <- unique(BW$VISITDY)
  Subjects <- unique(BW$USUBJID)
  
  if (TESTCD == 'BW') {
    
    for (day in Days) {
      dIndex <- which(BW$VISITDY == day)
      
      BW.day.mean.c <- mean(BW$BWSTRESN[intersect(dIndex,cIndex)], na.rm = T)
      BW.day.sd.c <- sd(BW$BWSTRESN[intersect(dIndex,cIndex)], na.rm = T)
      
      for (subject in Subjects) {
        sIndex <- which(BW$USUBJID == subject)
        
        baselineIndex <- which(baselineData$USUBJID == subject)
        baseline <- baselineData$BWSTRESN[baselineIndex]
        
        index <- intersect(dIndex, sIndex)
        
        BW$zScore[index] <- (BW$BWSTRESN[index] - BW.day.mean.c)/BW.day.sd.c
        BW$zScoreModel[index] <- (BW$BWSTRESN[index] - BW.day.mean.c)/BW.sd.c
        BW$percentControl[index] <- (BW$BWSTRESN[index] - BW.day.mean.c)/BW.day.mean.c*100
        BW$baselineChange[index] <- BW$BWSTRESN[index] - baseline
        BW$baselineChangePercent[index] <- BW$baselineChange[index]/baseline*100
      }
      
      BW.day.baseline.mean.c <- mean(BW$baselineChange[intersect(dIndex, cIndex)], na.rm = T)
      BW.day.baseline.sd.c <- sd(BW$baselineChange[intersect(dIndex, cIndex)], na.rm = T)
      BW.day.baselinePercent.mean.c <- mean(BW$baselineChangePercent[intersect(dIndex, cIndex)], na.rm = T)
      
      for (subject in Subjects) {
        sIndex <- which(BW$USUBJID == subject)
        
        index <- intersect(dIndex, sIndex)
        
        BW$zScoreBaselineChange[index] <- (BW$baselineChange[index] - BW.day.baseline.mean.c)/BW.day.sd.c
        BW$zScoreModelBaselineChange[index] <- (BW$baselineChange[index] - BW.day.baseline.mean.c)/BW.sd.c
        BW$percentChangeBaselineChange[index] <- (BW$baselineChangePercent[index] - BW.day.baselinePercent.mean.c)  
      }
    }
    
    BWstudy <- BW[,c('STUDYID',
                     'StudySpecies',
                     'USUBJID',
                     'VISITDY',
                     'Treatment',
                     'TreatmentDose',
                     'TRTDOSrank',
                     'BWSTRESN',
                     'zScore',
                     'zScoreModel',
                     'baselineChange',
                     'zScoreBaselineChange',
                     'zScoreModelBaselineChange',
                     'percentControl',
                     'baselineChangePercent',
                     'percentChangeBaselineChange')]
  } else {
    
    BW.mean.c <- mean(BW$BWSTRESN[cIndex], na.rm = T)
    BW.sd.c <- sd(BW$BWSTRESN[cIndex], na.rm = T)
    
    for (subject in Subjects) {
      sIndex <- which(BW$USUBJID == subject)
      
      baseline <- baselineData$BWSTRESN[which(baselineData$USUBJID == subject)]
      
      index <- sIndex
      
      BW$zScore[index] <- (BW$BWSTRESN[index] - BW.mean.c)/BW.sd.c
      BW$percentControl[index] <- (BW$BWSTRESN[index] - BW.mean.c)/BW.mean.c*100
      BW$baselineChange[index] <- BW$BWSTRESN[index] - baseline
      BW$baselineChangePercent[index] <- BW$baselineChange[index]/baseline*100
    }
    
    BW.baseline.mean.c <- mean(BW$baselineChange[cIndex], na.rm = T)
    BW.baseline.sd.c <- sd(BW$baselineChange[cIndex], na.rm = T)
    BW.baselinePercent.mean.c <- mean(BW$baselineChangePercent[cIndex], na.rm = T)
    
    for (subject in Subjects) {
      sIndex <- which(BW$USUBJID == subject)
      
      index <- sIndex
      
      BW$zScoreBaselineChange[index] <- (BW$baselineChange[index] - BW.baseline.mean.c)/BW.sd.c
      BW$zScoreModelBaselineChange[index] <- (BW$baselineChange[index] - BW.baseline.mean.c)/BW.sd.c
      BW$percentChangeBaselineChange[index] <- (BW$baselineChangePercent[index] - BW.baselinePercent.mean.c)  
    }
    
    
    BWstudy <- BW[,c('STUDYID',
                     'StudySpecies',
                     'USUBJID',
                     'VISITDY',
                     'Treatment',
                     'TreatmentDose',
                     'TRTDOSrank',
                     'BWSTRESN',
                     'zScore',
                     'baselineChange',
                     'zScoreBaselineChange',
                     'percentControl',
                     'baselineChangePercent',
                     'percentChangeBaselineChange')]
  }
  
  if (exists('BWstudies')) {
    BWstudies <- rbind(BWstudies,BWstudy)
  } else {
    BWstudies <- BWstudy
  }
}

BWstudies$TRTDOSrank <- factor(BWstudies$TRTDOSrank, levels = doseRanks)

if (TESTCD == 'BW') {
  metrics <- c('BWSTRESN',
               'zScore',
               'zScoreModel',
               'percentControl',
               'baselineChange',
               'baselineChangePercent',
               'zScoreBaselineChange',
               'zScoreModelBaselineChange',
               'percentChangeBaselineChange')
  
  for (metric in metrics) {
    plotData <- aggregate(get(metric) ~ StudySpecies + VISITDY + TRTDOSrank, FUN = mean, data = BWstudies)
    colnames(plotData) <- c('Study', 'Day', 'Treatment', metric)
    p <- ggplot(plotData, aes(x = Day, y = get(metric), color = Treatment, shape = Study)) +
      geom_point() + geom_line() + ylab(metric) + ggtitle(metric) +
      scale_color_manual(values = c('black', 'blue', 'dark green', 'red', 'purple'))
    print(p)
    if ((savePlots == T)&(Local == T)) {
      ggsave(filename = paste0(metric,'.png'),
             plot = p,
             device = 'png',
             path = paste0(saveFolder,TESTCD,'plots'))
    }
  }
} else {
  metrics <- c('BWSTRESN',
               'zScore',
               'percentControl',
               'baselineChange',
               'baselineChangePercent',
               'zScoreBaselineChange',
               'percentChangeBaselineChange')
  for (metric in metrics) {
    plotData <- aggregate(get(metric) ~ StudySpecies + TRTDOSrank, FUN = mean, data = BWstudies)
    colnames(plotData) <- c('Study', 'Treatment', metric)
    p <- ggplot(plotData, aes(x = Study, y = get(metric), fill = Treatment, shape = Study)) +
      geom_col(position = 'dodge2') + ylab(metric) + ggtitle(metric) +
      scale_fill_manual(values = c('black', 'blue', 'dark green', 'red', 'purple'))
    print(p)
    if ((savePlots == T)&(Local == T)) {
      ggsave(filename = paste0(metric,'.png'),
             plot = p,
             device = 'png',
             path = paste0(saveFolder,TESTCD,'plots'))
    }
  }
}
