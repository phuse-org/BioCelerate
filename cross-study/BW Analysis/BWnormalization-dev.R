rm(list=ls())

library(ggplot2)
library(Hmisc)
library(tools)
library(devtools)
library(httr)
library(reshape2)
library(scales)

# localPath <- '~/PhUSE/Git/phuse-scripts/'
localPath <- 'C:/Users/Kevin.Snyder/OneDrive - FDA/Documents/PhUSE/BioCelerate/DataSharing/'

Local <- T
savePlots <- F
saveFolder <- 'C:/Users/Kevin.Snyder/Box Sync/Biocelerate/Cross Study Comparison Project/BW Analysis/BioCelerate/'

# Define Test
TESTCD <- 'BW'
# TESTCD <- 'TERMBW'

plotType <- 'line'
plotType <- 'heatmap'

heatmapMethod <- 'interpolate'
# heatmapMethod <- 'bin'

binSize <- 2

if (Local == T) {
  source(paste0(localPath,'contributed/Nonclinical/R/Functions/Functions.R'))
  source(paste0(localPath,'contributed/Nonclinical/R/Functions/groupSEND.R'))
} else {
  source_url('https://raw.githubusercontent.com/phuse-org/phuse-scripts/master/contributed/Nonclinical/R/Functions/Functions.R')
  source_url('https://raw.githubusercontent.com/phuse-org/phuse-scripts/master/contributed/Nonclinical/R/Functions/groupSEND.R')
}

paths <- c('data/send/1-Month Dog_IDO1_Company A',
           'data/send/1-Month Dog_IDO1_Company B',
# paths <- c('data/send/1-Month Rat_IDO1_Company A',
           'data/send/1-Month Rat_IDO1_Company A',
           'data/send/1-Month Rat_IDO1_Company B')
# paths <- c('data/send/FFU-Contribution-to-FDA',
#            'data/send/PDS',
#            'data/send/instem',
#            'data/send/PointCross')
# paths <- c('data/send/FFU-Contribution-to-FDA')
# paths <- c('data/send/PDS')
# paths <- c('data/send/instem')
# paths <- c('data/send/PointCross')

# Define Names of Dose Ranks
# doseRanks <- c('Vehicle', 'Treatment LD', 'Treatment MD', 'Treatment HD', 'Treatment Max')
doseRanks <- c('Vehicle', 'Treatment LD', 'Treatment MD', 'Treatment HD')

for (path in paths) {
  print(path)
  
  if (Local == T) {
    Data <- load.xpt.files(paste0(localPath,path))
  } else {
    Data <- load.GitHub.xpt.files(studyDir = path)
  }
  
  # fill in NA VISITDY values with BWDY values
  index <- which(is.na(Data$bw$VISITDY))
  if (length(index) > 0) {
    Data$bw$VISITDY[index] <- Data$bw$BWDY[index]
  }
  
  # Get Species of Study
  Species <- Data$ts$TSVAL[which(Data$ts$TSPARMCD == 'SPECIES')]
  
  # Merge BW with other relevant domains
  BW <- groupSEND(Data,'bw')
  
  # Set Day 1 Index
  dayTable <- table(BW$VISITDY)
  minIndex <- which(abs(as.numeric(names(dayTable))) == min(abs(as.numeric(names(dayTable)))))
  if (length(minIndex) > 1) {
    minIndex <- minIndex[which(dayTable[minIndex] == max(dayTable[minIndex]))]
  }
  d1 <- as.numeric(names(dayTable)[minIndex])
  d1Index <- which(BW$VISITDY == d1)
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
  
  treatmentTable <- table(BW$Treatment)
  maxIndex <- which(treatmentTable == max(treatmentTable))
  BW$studyTreatment <- names(treatmentTable)[maxIndex]
  
  # Concatenate STUDYID and TreatmentDose and Species
  BW$StudySpecies <- paste0(BW$studyTreatment, ' (', Species, ')')
  
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
  c3Index <- grep('reference', BW$Treatment, ignore.case = T)
  cIndex <- Reduce(union, list(c1Index, c2Index, c3Index))
  
  BW$BWSTRESN <- as.numeric(BW$BWSTRESN)
  baselineData$BWSTRESN <- as.numeric(baselineData$BWSTRESN)
  
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
    if (plotType == 'line') {
      colnames(plotData) <- c('Study', 'Day', 'Treatment', metric)
      p <- ggplot(plotData, aes(x = Day, y = get(metric), color = Treatment, shape = Study)) +
        geom_point() + geom_line() + ylab(metric) + ggtitle(metric) +
        scale_color_manual(values = c('black', 'blue', 'dark green', 'red', 'purple'))
    } else if (plotType == 'heatmap') {
      plotData$StudyTreatment <- paste(plotData$StudySpecies, plotData$TRTDOSrank, sep = ': ')
      colnames(plotData) <- c('Study', 'Day', 'Treatment', metric, 'StudyTreatment')
      
      
      allTime <- seq(min(plotData$Day), max(plotData$Day))
      negBreaks <- allTime[seq(which(allTime == 1), 1, by = -1*binSize)]
      posBreaks <- allTime[seq(which(allTime == 1), length(allTime), by = binSize)]
      allTimeBreaks <- sort(union(negBreaks, posBreaks), decreasing = F)
      removeIndex <- which(allTime == 0)
      allTime <- allTime[-removeIndex]
      if (exists('interpData')) {
        rm(interpData)
      }
      for (StudyTreatment in unique(plotData$StudyTreatment)) {
        index <- which(plotData$StudyTreatment == StudyTreatment)
        studyTreatmentData <- plotData[index,]
        if (heatmapMethod == 'interpolate') {
          
          interpDataTmp <- data.frame(approx(studyTreatmentData$Day, studyTreatmentData[[metric]],
                                             xout = allTime, rule = 2, method = 'linear', ties = mean))
          interpDataTmp <- as.data.frame(cbind(StudyTreatment,interpDataTmp))
        } else if (heatmapMethod == 'bin') {
          allTimeBinBreaks <- unique(c(min(allTime), allTimeBreaks, (max(allTime)+1)))
          if (exists('interpDataTmp')) {
            rm(interpDataTmp)
          }
          for (i in seq(2, length(allTimeBinBreaks))) {
            minTime <- allTimeBinBreaks[i-1]
            maxTime <- allTimeBinBreaks[i]
            avgTime <- mean(c(minTime,maxTime))
            minIndex <- which(studyTreatmentData$Day >= minTime)
            maxIndex <- which(studyTreatmentData$Day < maxTime)
            binIndex <- intersect(minIndex, maxIndex)
            binTmp <- as.data.frame(cbind(StudyTreatment, avgTime, mean(studyTreatmentData[[metric]][binIndex], na.rm = T)))
            if (!exists('interpDataTmp')) {
              interpDataTmp <- binTmp
            } else {
              interpDataTmp <- rbind(interpDataTmp, binTmp)
            }
          }
        }
        if (!exists('interpData')) {
          interpData <- interpDataTmp
        } else {
          interpData <- rbind(interpData, interpDataTmp)
        }
      }
      colnames(interpData) <- c('StudyTreatment', 'Day', metric)
      interpData$Day <- as.integer(interpData$Day)
      interpDataMapTmp <- dcast(interpData, StudyTreatment ~ Day)
      startIndex <- min(as.numeric(colnames(interpDataMapTmp)[which(colnames(interpDataMapTmp) >= 1)]), na.rm =T)
      interpDataMap <- as.matrix(interpDataMapTmp[,startIndex:ncol(interpDataMapTmp)])
      row.names(interpDataMap) <- interpDataMapTmp[,1]
      
      interpDataClust <- hclust(dist((interpDataMap)))
      heatmapOrder <- row.names(interpDataMap) <- row.names(interpDataMap)[interpDataClust$order]
      interpData$StudyTreatment <- factor(interpData$StudyTreatment, levels = heatmapOrder)
      interpData$Day <- factor(interpData$Day)
      interpData[[metric]] <- as.numeric(interpData[[metric]])
      if (heatmapMethod == 'interpolate') {
        heatmapBreaks <- allTimeBreaks
      } else if (heatmapMethod == 'bin') {
        heatmapBreaks <- as.numeric(levels(interpData$Day))
      }
      p <- ggplot(interpData, aes(x = Day, fill = get(metric), y = StudyTreatment)) +
        geom_tile() + ylab('Study (Species): Treatment') + ggtitle(metric) +
        scale_fill_gradient2(low = muted('blue'), high = muted('red'), name = metric) + theme_classic() +
        scale_x_discrete(breaks = heatmapBreaks)
    }
    print(p)
    if ((savePlots == T)&(Local == T)) {
      if (plotType == 'heatmap') {
        if (heatmapMethod == 'interpolate') {
          fileName <- paste0(metric,'-',plotType,'-',heatmapMethod,'.png')
        } else if (heatmapMethod == 'bin') {
          fileName <- paste0(metric,'-',plotType,'-',heatmapMethod,'-',binSize,'.png')
        }
      } else if (plotType == 'line') {
        fileName <- paste0(metric,'-',plotType,'.png')
      }
      ggsave(filename = fileName,
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

