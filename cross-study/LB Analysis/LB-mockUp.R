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
savePlots <- T
saveFolder <- 'C:/Users/Kevin.Snyder/Box Sync/Biocelerate/Cross Study Comparison Project/LB Analysis/BioCelerate/'

Sex <- 'F'

# Define Test
TESTCDs <- c('ALT','AST','ALP')
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
  # index <- which(is.na(Data$lb$VISITDY))
  # if (length(index) > 0) {
  #   Data$lb$VISITDY[index] <- Data$lb$LBDY[index]
  # }
  
  # remove negative VISITDY values
  removeIndex <- which(Data$lb$VISITDY < 0)
  if (length(removeIndex) > 0) {
    Data$lb <- Data$lb[-removeIndex,]
  }
  
  # Get Species of Study
  Species <- Data$ts$TSVAL[which(Data$ts$TSPARMCD == 'SPECIES')]
  
  # Merge BW with other relevant domains
  LB <- groupSEND(Data,'lb')
  # 
  # # Set Day 1 Index
  # dayTable <- table(BW$VISITDY)
  # minIndex <- which(abs(as.numeric(names(dayTable))) == min(abs(as.numeric(names(dayTable)))))
  # if (length(minIndex) > 1) {
  #   minIndex <- minIndex[which(dayTable[minIndex] == max(dayTable[minIndex]))]
  # }
  # d1 <- as.numeric(names(dayTable)[minIndex])
  # d1Index <- which(BW$VISITDY == d1)
  # baselineData <- BW[d1Index,]
  
  # Rank Order Treatment Groups by TRTDOS
  LB$TRTDOSrank <- NA
  if (!is.null(LB$TRTDOS)) {
    TRTDOSvalues <- as.numeric(unique(LB$TRTDOS))
  } else {
    TRTDOSvalues <- as.numeric(unique(LB$EXDOSE))
  }
  TRTDOSranks <- rank(TRTDOSvalues)
  for (i in seq(nrow(LB))) {
    if (!is.null(LB$TRTDOS)) {
      index <- TRTDOSranks[which(TRTDOSvalues == as.numeric(LB$TRTDOS[i]))]
    } else {
      index <- TRTDOSranks[which(TRTDOSvalues == as.numeric(LB$EXDOSE[i]))]
    }
    LB$TRTDOSrank[i] <- doseRanks[index]
  }
  
  treatmentTable <- table(LB$Treatment)
  maxIndex <- which(treatmentTable == max(treatmentTable))
  LB$studyTreatment <- names(treatmentTable)[maxIndex]
  
  # Concatenate STUDYID and TreatmentDose and Species
  LB$StudySpecies <- paste0(LB$studyTreatment, ' (', Species, ')')
  
  # Concatenate LBSPEC and LBTESTCD
  LB$LBTESTCD <- paste(LB$LBSPEC, LB$LBTESTCD, sep = ' | ')
  
  # # Cap Study at 28 days
  # if (TESTCD != 'TERMBW') {
  #   removeIndex <- which(BW$VISITDY > 29)
  #   if (length(removeIndex) > 0) {
  #     BW <- BW[-removeIndex,]
  #   }
  # }
  
  # # Remove TK Animals
  # if (Species == 'RAT') {
  #   removeIndex <- which(BW$TKstatus == T)
  #   if (length(removeIndex) > 0) {
  #     BW <- BW[-removeIndex,]
  #   }
  # }
  
  # Remove Recovery Animals
  removeIndex <- which(LB$RecoveryStatus == T)
  if (length(removeIndex) > 0) {
    LB <- LB[-removeIndex,]
  }
  
  # Select Sex
  sexIndex <- which(LB$SEX == Sex)
  LB <- LB[sexIndex,]
  
  # Select Test
  # testIndex <- which(LB$LBTESTCD %in% TESTCDs)
  # LB <- LB[testIndex,]
  
  # Find Control Animals
  c1Index <- grep('control', LB$Treatment, ignore.case = T)
  c2Index <- grep('vehicle', LB$Treatment, ignore.case = T)
  c3Index <- grep('reference', LB$Treatment, ignore.case = T)
  cIndex <- Reduce(union, list(c1Index, c2Index, c3Index))
  
  # LB$LBSTRESN <- as.numeric(LB$LBSTRESN)
  # baselineData$LBSTRESN <- as.numeric(baselineData$LBSTRESN)
  # 
  # Model <- lm(LBSTRESN ~ VISITDY, data = BW)
  # BW.sd.c <- sd(Model$residuals[cIndex])
  
  LB$zScore <- NA
  LB$zScoreModel <- NA
  LB$percentControl <- NA
  # BW$baselineChange <- NA
  # BW$baselineChangePercent <- NA
  # BW$zScoreBaselineChange <- NA
  # BW$zScoreModelBaselineChange <- NA
  # BW$percentChangeBaselineChange <- NA
  
  TESTCDs <- unique(LB$LBTESTCD)
  Subjects <- unique(LB$USUBJID)
  
  # Add for loop for LBTESTCD here!!!!!!!!!!!!!!!!!!!!!!!!
  for (TESTCD in TESTCDs) {
    testIndex <- which(LB$LBTESTCD == TESTCD)
    ctIndex <- intersect(cIndex, testIndex)
    LB.mean.c <- mean(LB$LBSTRESN[ctIndex], na.rm = T)
    LB.sd.c <- sd(LB$LBSTRESN[ctIndex], na.rm = T)
    
    for (subject in Subjects) {
      sIndex <- which(LB$USUBJID == subject)
      
      index <- intersect(sIndex, testIndex)
      
      LB$zScore[index] <- (LB$LBSTRESN[index] - LB.mean.c)/LB.sd.c
      LB$percentControl[index] <- (LB$LBSTRESN[index] - LB.mean.c)/LB.mean.c*100
    }
  }
  
  
  LBstudy <- LB[,c('STUDYID',
                   'StudySpecies',
                   'USUBJID',
                   'VISITDY',
                   'LBTESTCD',
                   'Treatment',
                   'TreatmentDose',
                   'TRTDOSrank',
                   'LBSTRESN',
                   'zScore',
                   'percentControl')]
  
  if (exists('LBstudies')) {
    LBstudies <- rbind(LBstudies,LBstudy)
  } else {
    LBstudies <- LBstudy
  }
}

# LBstudies$TRTDOSrank <- factor(LBstudies$TRTDOSrank, levels = doseRanks)

# FIX THIS!!!!!!!!!!!!!!!!!!!!!
Compound <- unlist(lapply(strsplit(LBstudies$StudySpecies, ' (', fixed = T), '[[', 1))
Species <- substr(unlist(lapply(strsplit(LBstudies$StudySpecies, ' (', fixed = T), '[[', 2)), 1, 3)

LBstudies$Compound <- Compound
LBstudies$Species <- Species

metrics <- c('LBSTRESN',
             'zScore',
             'percentControl')
for (metric in metrics) {
  plotData <- aggregate(get(metric) ~ STUDYID + Compound + Species + LBTESTCD + TRTDOSrank, FUN = mean, data = LBstudies)
  colnames(plotData)[6] <- 'Value'
  
  plotData <- plotData[which(plotData$TRTDOSrank == 'Treatment HD'),]
  
  lengthData <- aggregate(Value ~ LBTESTCD, FUN = length, plotData)
  includeTests <- lengthData$LBTESTCD[which(lengthData$Value == 4)]
  plotData <- plotData[which(plotData$LBTESTCD %in% includeTests),]
  
  meanData <- aggregate(Value ~ LBTESTCD, FUN = mean, plotData)
  meanData <- meanData[order(meanData$Value, decreasing = F),]
  
  plotData$LBTESTCD <- factor(plotData$LBTESTCD, levels = meanData$LBTESTCD)
  
  p <- ggplot(plotData, aes(x = LBTESTCD, y = Value, group = LBTESTCD, color = Compound, shape = Species)) +
    geom_point(size =5) + geom_bar(stat = 'summary', fun.x = 'mean', alpha = 0, color = 'black') +
    coord_flip() + geom_line(aes(group = STUDYID)) + ggtitle(metric)
  print(p)
  
  if ((savePlots == T)&(Local == T)) {
    ggsave(filename = paste0(metric,'.png'),
           plot = p,
           device = 'png',
           path = paste0(saveFolder,'LBplots'))
  }
}

