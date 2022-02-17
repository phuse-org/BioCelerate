rm(list=ls())

library(ggplot2)
library(Hmisc)
library(tools)
library(devtools)
library(httr)
library(reshape2)
library(scales)
library(ini)

# Set working directory to location of script
homePath <- dirname(sys.calls()[[1]][[2]])
setwd(homePath)

dataPaths <- read.ini('dataPaths.ini')

###########################################################################################################
# Parameter Settings:

# Select Data Source (Public = phuse-scripts GitHub data; BioCelerate = TDS datasets)
dataSource <- 'Public'
# dataSource <- 'BioCelerate'

# Select TRUE to Save Plot Figures
savePlots <- T

# Define BWTESTCD to Analyze
TESTCD <- 'BW'
# TESTCD <- 'TERMBW'

# Select Plot Type (TERMBW defaults to bar graph)
plotType <- 'line'
# plotType <- 'heatmap'

# Select Method for Generating Heatmap
heatmapMethod <- 'interpolate'
heatmapMethod <- 'bin'

# Select Heatmap Bin Size
binSize <- 7

# Select Sex of Animals to Analyze
Sex <- 'M'

###########################################################################################################

# Populate parameters based on dataSource selection
if (dataSource == 'Public') {
  source_url('https://raw.githubusercontent.com/phuse-org/phuse-scripts/master/contributed/Nonclinical/R/Functions/Functions.R')
  source_url('https://raw.githubusercontent.com/phuse-org/phuse-scripts/master/contributed/Nonclinical/R/Functions/groupSEND.R')
  
  doseRanks <- c('Vehicle', 'Treatment LD', 'Treatment MD', 'Treatment HD', 'Treatment Max')
  
} else if (dataSource == 'BioCelerate') {
  source('Functions/Functions.R')
  source('Functions/groupSEND.R')

  doseRanks <- c('Vehicle', 'Treatment LD', 'Treatment MD', 'Treatment HD')
}
paths <- as.character(unlist(dataPaths[[dataSource]]))

# Define Metrics to Plot based on TESTCD Selection
metrics <- c('BWSTRESN',
             'percentControl',
             'zScore',
             'baselineChange',
             'baselineChangePercent',
             'percentChangeBaselineChange',
             'zScoreBaselineChange'
             )
if (TESTCD == 'BW') {
  metrics <- c(metrics, 'zScoreModelBaselineChange', 'zScoreModel')
}

# Define path to write files (and create directory if necessary)
writeDirectory <- paste0('results/', dataSource, '/', TESTCD, 'plots')
if (!dir.exists(writeDirectory)) {
  dir.create(writeDirectory, recursive = T)
}

# Load and Process Study Data
for (path in paths) {
  print(path)
  
  if (dataSource == 'BioCelerate') {
    Data <- load.xpt.files(paste(homePath, path, sep = '/'))
  } else if (dataSource == 'Public') {
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
  sexIndex <- which(BW$SEX == Sex)
  BW <- BW[sexIndex,]
  
  if (nrow(BW) == 0) {
    next
  }
  
  # Set Baseline Index
  baselineIndex <- NULL
  for (subject in unique(BW$USUBJID)) {
    subjectIndex <- which(BW$USUBJID == subject)
    subjectDays <- BW$VISITDY[subjectIndex]
    subjectPredoseDays <- subjectDays[which(subjectDays <= 1)]
    subjectBaselineDay <- max(subjectPredoseDays)
    subjectBaselineIndex <- subjectIndex[which(subjectDays == subjectBaselineDay)]
    baselineIndex <- c(baselineIndex, subjectBaselineIndex)
  }
  baselineData <- BW[baselineIndex,]
  
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

# Generate Figures

BWstudies$TRTDOSrank <- factor(BWstudies$TRTDOSrank, levels = doseRanks)


for (metric in metrics) {
  if (TESTCD == 'BW') {
    plotData <- aggregate(get(metric) ~ StudySpecies + VISITDY + TRTDOSrank, FUN = mean, data = BWstudies)
    if (plotType == 'line') {
      colnames(plotData) <- c('Study', 'Day', 'Treatment', metric)
      p <- ggplot(plotData, aes(x = Day, y = get(metric), color = Treatment, shape = Study)) +
        guides(colour = guide_legend(override.aes = list(shape = NA))) +
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
  } else {
    plotData <- aggregate(get(metric) ~ StudySpecies + TRTDOSrank, FUN = mean, data = BWstudies)
    colnames(plotData) <- c('Study', 'Treatment', metric)
    p <- ggplot(plotData, aes(x = Study, y = get(metric), fill = Treatment, shape = Study)) +
      guides(colour = guide_legend(override.aes = list(shape = NA))) +
      geom_col(position = 'dodge2') + ylab(metric) + ggtitle(metric) +
      scale_fill_manual(values = c('black', 'blue', 'dark green', 'red', 'purple')) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    print(p)
  }
  if (savePlots == T) {
    if (TESTCD == 'BW') {
      if (plotType == 'heatmap') {
        if (heatmapMethod == 'bin') {
          fileName <- paste0(metric,'-',plotType,'-',heatmapMethod,'-',binSize, '-', Sex, '.png')
        } else {
          fileName <- paste0(metric,'-',plotType,'-',heatmapMethod, '-', Sex, '.png')
        }
      } else {
        fileName <- paste0(metric,'-',plotType, '-', Sex, '.png')
      }
    } else {
      fileName <- paste0(metric, '-', Sex, '.png')
    }
    ggsave(filename = fileName,
           plot = p,
           device = 'png',
           path = writeDirectory)
  }
}


