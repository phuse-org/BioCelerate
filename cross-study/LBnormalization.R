rm(list=ls())

library(ggplot2)
library(Hmisc)
library(tools)
library(devtools)
library(httr)
library(reshape2)
library(scales)
library(ini)
library(dplyr)

# Set working directory to location of script
homePath <- dirname(sys.calls()[[1]][[2]])
setwd(homePath)

dataPaths <- read.ini('dataPaths.ini')

###########################################################################################################
# Parameter Settings:

# Select Data Source (Public = phuse-scripts GitHub data; BioCelerate = TDS datasets)
dataSource <- 'Public'
dataSource <- 'BioCelerate'

# Select Organ
organ <- 'LIVER'
organ <- 'KIDNEY'
organ <- 'ALL'

# Select TRUE to Save Plot Figures
savePlots <- T

# Select Sex of Animals to Analyze
Sex <- 'M'

###########################################################################################################

organTESTCDlist <- list('LIVER' = c('SERUM | ALT',
                                    'SERUM | AST',
                                    'SERUM | ALP',
                                    'SERUM | GGT',
                                    'SERUM | BILI',
                                    'SERUM | ALB'),
                        'KIDNEY' = c('SERUM | CREAT',
                                     'SERUM | UREAN',
                                     'SERUM | ALB',
                                     'SERUM | CL',
                                     'SERUM | K',
                                     'SERUM | PHOS',
                                     'SERUM | SODIUM',
                                     'URINE | CL',
                                     'URINE | K',
                                     'URINE | SODIUM',
                                     'URINE | GLUC',
                                     'URINE | SPGRAV',
                                     'URINE | VOLUME',
                                     'URINE | PROT',
                                     'URINE | UROBIL',
                                     'WHOLE BLOOD | RBC',
                                     'WHOLE BLOOD | HCT',
                                     'WHOLE BLOOD | RETI')
                        )

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

# Define Metrics
metrics <- c('LBSTRESN',
             'percentControl',
             'zScore')

# Define path to write files (and create directory if necessary)
writeDirectory <- paste0('results/', dataSource, '/', 'LBplots')
if (!dir.exists(writeDirectory)) {
  dir.create(writeDirectory, recursive = T)
}

for (path in paths) {
  print(path)
  
  if (dataSource == 'BioCelerate') {
    Data <- load.xpt.files(paste(homePath, path, sep = '/'))
  } else if (dataSource == 'Public') {
    Data <- load.GitHub.xpt.files(studyDir = path)
  }
  
  # remove negative VISITDY/LBNOMDY (pretest) values
  removeIndex1 <- which(Data$lb$LBNOMDY < 0)
  removeIndex2 <- which(Data$lb$VISITDY < 0)
  removeIndex <- union(removeIndex1, removeIndex2)
   if (length(removeIndex) > 0) {
    Data$lb <- Data$lb[-removeIndex,]
  }
  
  # Get Species of Study
  Species <- Data$ts$TSVAL[which(Data$ts$TSPARMCD == 'SPECIES')]
  
  # Merge BW with other relevant domains
  LB <- groupSEND(Data,'lb')
  
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
  
  if (organ != 'ALL') {
    TESTCDlist <- organTESTCDlist[[organ]]
    organIndex <- which(LB$LBTESTCD %in% TESTCDlist)
    LB <- LB[organIndex,]
  }
  
  # Find the LB records for the last bleeding before the recovery
  # 1. remove all LB records after recovery start
  dIndex <- which(as.numeric(LB$LBDY) > as.numeric(LB$RecoveryStartDY))
  if (length(dIndex) > 0) {
    LB1 <- LB[-dIndex,]
  } else{
    LB1 <- LB
  }
  # 2. The LB last bleeding before recovery has the max LBDY
  LB <- LB1 %>%
    group_by(USUBJID, LBCAT, LBTESTCD) %>%
    slice(which.max(LBDY))
  #data check
  #Write.csv(LB,file="lb.csv",na="",row.names=FALSE) 
 
  # Select Sex
  sexIndex <- which(LB$SEX == Sex)
  LB <- LB[sexIndex,]
  
  if (nrow(LB) == 0) {
    next
  }
  
  # Find Control Animals
  c1Index <- grep('control', LB$Treatment, ignore.case = T)
  c2Index <- grep('vehicle', LB$Treatment, ignore.case = T)
  c3Index <- grep('reference', LB$Treatment, ignore.case = T)
  cIndex <- Reduce(union, list(c1Index, c2Index, c3Index))
  
  LB$zScore <- NA
  LB$percentControl <- NA
  
  TESTCDs <- unique(LB$LBTESTCD)
  Subjects <- unique(LB$USUBJID)
  
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
  
  for (col in colnames(LBstudy)) {
    if (class(LBstudy[[col]])[1] == 'labelled') {
      LBstudy[[col]] <- as.character(LBstudy[[col]])
    }
  }
  
  if (exists('LBstudies')) {
    LBstudies <- rbind(LBstudies,LBstudy)
  } else {
    LBstudies <- LBstudy
  }
}

Compound <- unlist(lapply(strsplit(LBstudies$StudySpecies, ' (', fixed = T), '[[', 1))
Species <- substr(unlist(lapply(strsplit(LBstudies$StudySpecies, ' (', fixed = T), '[[', 2)), 1, 3)

LBstudies$Compound <- Compound
LBstudies$Species <- Species

for (metric in metrics) {
  plotData <- aggregate(as.numeric(get(metric)) ~ STUDYID + Compound + Species + LBTESTCD + TRTDOSrank, FUN = mean, data = LBstudies)
  colnames(plotData)[6] <- 'Value'
  
  plotData <- plotData[which(plotData$TRTDOSrank == 'Treatment HD'),]
  
  lengthData <- aggregate(Value ~ LBTESTCD, FUN = length, plotData)
  includeTests <- lengthData$LBTESTCD[which(lengthData$Value == length(unique(plotData$STUDYID)))]
  plotData <- plotData[which(plotData$LBTESTCD %in% includeTests),]
  
  meanData <- aggregate(Value ~ LBTESTCD, FUN = mean, plotData)
  meanData <- meanData[order(meanData$Value, decreasing = F),]
  
  plotData$LBTESTCD <- factor(plotData$LBTESTCD, levels = meanData$LBTESTCD)
  
  p <- ggplot(plotData, aes(x = LBTESTCD, y = Value, group = LBTESTCD, color = Compound, shape = Species)) +
    geom_point(size =5) + geom_bar(stat = 'summary', fun.x = 'mean', alpha = 0, color = 'black') +
    coord_flip() + geom_line(aes(group = STUDYID)) + ggtitle(metric)
  print(p)
  
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
  
  if (savePlots == T) {
    ggsave(filename = paste0(metric, '-', organ, '-', Sex, '.png'),
           plot = p,
           device = 'png',
           path = paste0(writeDirectory))
  }
}

