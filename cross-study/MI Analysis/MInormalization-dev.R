rm(list=ls())

library(ggplot2)
library(Hmisc)
library(tools)
library(devtools)
library(httr)
library(reshape2)
library(scales)
library(forcats)

localPath <- 'C:/Users/Kevin.Snyder/OneDrive - FDA/Documents/PhUSE/BioCelerate/DataSharing/'

Local <- T
savePlots <- F
saveFolder <- 'C:/Users/Kevin.Snyder/Box Sync/Biocelerate/Cross Study Comparison Project/MI Analysis/BioCelerate/'

# Define Organ
MISPEC <- 'LIVER'

plotType <- 'table'
plotType <- 'heatmap'

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
  
  # Get Species of Study
  Species <- Data$ts$TSVAL[which(Data$ts$TSPARMCD == 'SPECIES')]
  
  # Merge BW with other relevant domains
  MI <- groupSEND(Data,'mi')
  
  MI$MISTRESC <- toupper(MI$MISTRESC)
  
  # Rank Order Treatment Groups by TRTDOS
  MI$TRTDOSrank <- NA
  if (!is.null(MI$TRTDOS)) {
    TRTDOSvalues <- as.numeric(unique(MI$TRTDOS))
  } else {
    TRTDOSvalues <- as.numeric(unique(MI$EXDOSE))
  }
  TRTDOSranks <- rank(TRTDOSvalues)
  for (i in seq(nrow(MI))) {
    if (!is.null(MI$TRTDOS)) {
      index <- TRTDOSranks[which(TRTDOSvalues == as.numeric(MI$TRTDOS[i]))]
    } else {
      index <- TRTDOSranks[which(TRTDOSvalues == as.numeric(MI$EXDOSE[i]))]
    }
    MI$TRTDOSrank[i] <- doseRanks[index]
  }
  
  treatmentTable <- table(MI$Treatment)
  maxIndex <- which(treatmentTable == max(treatmentTable))
  MI$studyTreatment <- names(treatmentTable)[maxIndex]
  
  # Concatenate STUDYID and TreatmentDose and Species
  MI$StudySpecies <- paste0(MI$studyTreatment, ' (', Species, ')')
  
  # Remove TK Animals
  # if (Species == 'RAT') {
  #   removeIndex <- which(MI$TKstatus == T)
  #   if (length(removeIndex) > 0) {
  #     MI <- MI[-removeIndex,]
  #   }
  # }
  
  # Remove Recovery Animals
  removeIndex <- which(MI$RecoveryStatus == T)
  if (length(removeIndex) > 0) {
    MI <- MI[-removeIndex,]
  }
  
  # Select Sex
  sexIndex <- which(MI$SEX == 'M')
  MI <- MI[sexIndex,]
  
  # Select Organ
  organIndex <- which(MI$MISPEC == MISPEC)
  MI <- MI[organIndex,]
  
  # Add Incidence Count
  MI$Count <- 1
  for (i in seq(nrow(MI))) {
    # treatmentIndex <- which(MI$TreatmentDose == MI$TreatmentDose[i])
    treatmentIndex <- which(MI$TRTDOSrank == MI$TRTDOSrank[i])
    studyIndex <- which(MI$StudySpecies == MI$StudySpecies[i])
    index <- intersect(treatmentIndex, studyIndex)
    MI$Count[i] <- 1/length(index)
  }
  
  MIstudy <- MI[,c('STUDYID',
                   'StudySpecies',
                   'USUBJID',
                   'Treatment',
                   'TreatmentDose',
                   'TRTDOSrank',
                   'MISTRESC',
                   'Count')]
  
  if (exists('MIstudies')) {
    MIstudies <- rbind(MIstudies, MIstudy)
  } else {
    MIstudies <- MIstudy
  }
}

MIstudies$TRTDOSrank <- factor(MIstudies$TRTDOSrank, levels = doseRanks)

plotData <- aggregate(Count ~ StudySpecies + MISTRESC + TRTDOSrank, FUN = sum, data = MIstudies)

plotData$StudyTreatment <- paste(plotData$StudySpecies, plotData$TRTDOSrank, sep = ': ')
colnames(plotData) <- c('Study', 'Finding', 'Treatment', 'Count', 'StudyTreatment')

# interpData$Day <- as.integer(interpData$Day)
plotDataMapTmp <- dcast(plotData, StudyTreatment ~ Finding, value.var = 'Count')
for (i in 2:ncol(plotDataMapTmp)) {
  index <- which(is.na(plotDataMapTmp[,i]))
  plotDataMapTmp[index,i] <- 0
}
plotDataMap <- as.matrix(plotDataMapTmp[,2:ncol(plotDataMapTmp)])
row.names(plotDataMap) <- plotDataMapTmp[,1]

plotDataClustY <- hclust(dist((plotDataMap)))
heatmapOrder <- row.names(plotDataMap) <- row.names(plotDataMap)[plotDataClustY$order]
plotDataClustX <- hclust(dist((t(plotDataMap))))
heatmapOrderFindings <- colnames(plotDataMap)[plotDataClustY$order]
plotData$StudyTreatment <- factor(plotData$StudyTreatment, levels = heatmapOrder)
# plotData$Finding <- factor(plotData$Finding)
plotData$Finding <- factor(plotData$Finding, levels = heatmapOrderFindings)
plotData[['Count']] <- as.numeric(plotData[['Count']])
# if (heatmapMethod == 'interpolate') {
#   heatmapBreaks <- allTimeBreaks
# } else if (heatmapMethod == 'bin') {
#   heatmapBreaks <- as.numeric(levels(interpData$Day))
# }

# Reconcile Finding Names
termFactors <- levels(plotData$Finding)
termNames <- termFactors
termList <- c('Necrosis', 'Aggregates', 'Normal','Vacuolization', 'Hypertrophy', 'Inflammation', 'Infiltration')
for (term in termList) {
  termIndex <- grep(term, termFactors, ignore.case = T)
  termNames[termIndex] <- term
}
names(termFactors) <- termNames
plotData$Finding <- fct_recode(plotData$Finding, !!!termFactors)

p <- ggplot(plotData, aes(x = Finding, fill = Count, y = StudyTreatment)) +
  geom_tile() + ylab('Study (Species): Treatment') + ggtitle(MISPEC) +
  scale_fill_gradient2(low = muted('blue'), high = muted('red'), name = MISPEC) + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(p)

# Modeling Analysis
MIstudies$MISTRESC <- factor(MIstudies$MISTRESC)
termFactors <- levels(MIstudies$MISTRESC)
termNames <- termFactors
termList <- c('Necrosis', 'Aggregates', 'Normal','Vacuolization', 'Hypertrophy', 'Inflammation', 'Infiltration')
for (term in termList) {
  termIndex <- grep(term, termFactors, ignore.case = T)
  termNames[termIndex] <- term
}
names(termFactors) <- termNames
MIstudies$MISTRESC <- fct_recode(MIstudies$MISTRESC, !!!termFactors)

MIstudies$SPECIES <- MIstudies$StudySpecies
for (i in seq(nrow(MIstudies))) {
  SPECIES <- unlist(strsplit(MIstudies$SPECIES[i], '(', fixed = T))[2]
  SPECIES <- gsub(')', '', SPECIES, fixed = T)
  MIstudies$SPECIES[i] <- SPECIES
}

allFindings <- levels(MIstudies$MISTRESC)
modelMIstudies <- MIstudies
# MIstudies$Present <- rep(1, nrow(MIstudies))
for (study in unique(MIstudies$StudySpecies)) {
  studyIndex <- which(MIstudies$StudySpecies == study)
  newData <- NULL
  for (subject in unique(MIstudies$USUBJID[studyIndex])) {
    subjectIndex <- studyIndex[which(MIstudies$USUBJID[studyIndex] == subject)]
    subjectMIstudies <- MIstudies[subjectIndex,]
    Findings <- subjectMIstudies$MISTRESC
    missingFindings <- allFindings[which(allFindings %ni% Findings)]
    nMissing <- length(missingFindings)
    newRows <- data.frame(STUDYID = rep(unique(subjectMIstudies$STUDYID), nMissing),
                          StudySpecies = rep(study, nMissing),
                          USUBJID = rep(subject, nMissing),
                          Treatment = rep(unique(subjectMIstudies$Treatment)),
                          TreatmentDose = rep(unique(subjectMIstudies$TreatmentDose), nMissing),
                          TRTDOSrank = rep(unique(subjectMIstudies$TRTDOSrank), nMissing),
                          MISTRESC = missingFindings,
                          Count = rep(0, nMissing),
                          SPECIES = rep(unique(subjectMIstudies$SPECIES), nMissing)
                          )
    if (nrow(newRows) > 0) {
      if (is.null(newData)) {
        newData <- newRows
      } else {
        newData <- rbind(newData, newRows)
      }
    }
  }
  if (nrow(newData) > 0) {
    modelMIstudies <- rbind(modelMIstudies, newData)
  }
}
modelMIstudies$Present <- ceiling(modelMIstudies$Count)

# mylogit <- glm(MISTRESC ~ StudySpecies*TRTDOSrank, data = MIstudies,family = 'binomial')
write.csv(modelMIstudies, paste0(saveFolder, 'LIVERdata.csv'), row.names = F)

