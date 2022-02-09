rm(list=ls())

library(ggplot2)
library(Hmisc)
library(tools)
library(devtools)
library(httr)
library(reshape2)
library(scales)
library(forcats)
library(DT)

localPath <- '~/PhUSE/Git/phuse-scripts/'

Local <- T
savePlots <- T
saveFolder <- 'C:/Users/Kevin.Snyder/Box Sync/Biocelerate/Cross Study Comparison Project/MI Analysis/'

# Define Organ
# MISPEC <- 'LIVER'

plotType <- 'table'
plotType <- 'heatmap'

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
# doseRanks <- c('Vehicle', 'Treatment LD', 'Treatment MD', 'Treatment HD', 'Treatment Max')
doseRanks <- c('Vehicle', 'Treatment', 'Treatment', 'Treatment', 'Treatment')

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
  
  # Concatenate STUDYID and TreatmentDose and Species
  MI$StudySpecies <- paste0(MI$STUDYID, ' (', Species, ')')
  
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
  sexIndex <- which(MI$SEX == 'F')
  MI <- MI[sexIndex,]
  
  # # Select Organ
  # organIndex <- which(MI$MISPEC == MISPEC)
  # MI <- MI[organIndex,]
  
  # Add Incidence Count
  MI$Count <- 1
  for (i in seq(nrow(MI))) {
    # treatmentIndex <- which(MI$TreatmentDose == MI$TreatmentDose[i])
    treatmentIndex <- which(MI$TRTDOSrank == MI$TRTDOSrank[i])
    studyIndex <- which(MI$StudySpecies == MI$StudySpecies[i])
    index <- intersect(treatmentIndex, studyIndex)
    subjects <- unique(MI$USUBJID[index])
    MI$Count[i] <- 1/length(subjects)
  }
  
  MIstudy <- MI[,c('STUDYID',
                   'StudySpecies',
                   'USUBJID',
                   'Treatment',
                   'TreatmentDose',
                   'TRTDOSrank',
                   'MISPEC',
                   'MISTRESC',
                   'Count')]
  
  if (exists('MIstudies')) {
    MIstudies <- rbind(MIstudies, MIstudy)
  } else {
    MIstudies <- MIstudy
  }
}

removeIndex <- which(MIstudies$MISTRESC == 'NORMAL')
MIstudies <- MIstudies[-removeIndex,]

studies <- unique(MIstudies$STUDYID)
targetOrgans <- rep(NA,50*length(studies))
dim(targetOrgans) <- c(50,length(studies))

bins <- seq(1, -1, by = -0.1)
nbins <- length(bins)
binResults <- rep(NA, length(studies)*nbins)
dim(binResults) <- c(nbins, length(studies))

vehicleIndex <- which(MIstudies$TRTDOSrank == 'Vehicle')
treatmentIndex <- which(MIstudies$TRTDOSrank == 'Treatment')
count <- 0

# for (study in studies) {
#   count <- count + 1
#   studyIndex <- which(MIstudies$STUDYID == study)
#   organs <- unique(MIstudies$MISPEC[studyIndex])
#   organValues <- NULL
#   for (organ in organs) {
#     findings <- NULL
#     organIndex <- which(MIstudies$MISPEC == organ)
#     vIndex <- Reduce(intersect, list(vehicleIndex, studyIndex, organIndex))
#     organValueVehicle <- sum(MIstudies$Count[vIndex])#/length(unique(MIstudies$MISTRESC[vIndex]))
#     tIndex <- Reduce(intersect, list(treatmentIndex, studyIndex, organIndex))
#     organValueTreatment <- sum(MIstudies$Count[tIndex])#/length(unique(MIstudies$MISTRESC[tIndex]))
#     organValueTmp <- organValueTreatment - organValueVehicle
#     # if (organValueVehicle > 0) {
#     #   organValueTmp <- (organValueTreatment + (1-organValueVehicle))/(organValueVehicle + (1 - organValueTreatment))
#     # } else {
#     #   organValueTmp <- NA
#     # }
#     findings <- unique(MIstudies$MISTRESC[tIndex])
#     findings <- paste(findings, collapse = ', ')
#     organValues <- c(organValues, organValueTmp)
#     names(organValues)[length(organValues)] <- paste0(organ, ': ', findings)
#     # stop()
#   }
#   # names(organValues) <- organs
#   organValues <- sort(organValues, decreasing = T)[1:10]
#   targetOrgans[,count] <- paste0(names(organValues), ' (', round(organValues, digits = 1), ')')
# }
# colnames(targetOrgans) <- studies

for (study in studies) {
  count <- count + 1
  studyIndex <- which(MIstudies$STUDYID == study)
  organs <- unique(paste0(MIstudies$MISPEC[studyIndex], ': ', MIstudies$MISTRESC[studyIndex]))
  organValues <- NULL
  for (organ in organs) {
    # findings <- NULL
    organIndex <- which(paste0(MIstudies$MISPEC, ': ', MIstudies$MISTRESC) == organ)
    vIndex <- Reduce(intersect, list(vehicleIndex, studyIndex, organIndex))
    organValueVehicle <- sum(MIstudies$Count[vIndex])#/length(unique(MIstudies$MISTRESC[vIndex]))
    tIndex <- Reduce(intersect, list(treatmentIndex, studyIndex, organIndex))
    organValueTreatment <- sum(MIstudies$Count[tIndex])#/length(unique(MIstudies$MISTRESC[tIndex]))
    organValueTmp <- organValueTreatment - organValueVehicle
    # if (organValueVehicle > 0) {
    #   organValueTmp <- (organValueTreatment + (1-organValueVehicle))/(organValueVehicle + (1 - organValueTreatment))
    # } else {
    #   organValueTmp <- NA
    # }
    findings <- unique(MIstudies$MISTRESC[tIndex])
    findings <- paste(findings, collapse = ', ')
    organValues <- c(organValues, organValueTmp)
    names(organValues)[length(organValues)] <- organ
    # stop()
  }
  # names(organValues) <- organs
  organValues <- sort(organValues, decreasing = T)[1:50]
  
  # assign(paste0('organValues_', study), organValues)
  for (bin in bins) {
    binIndex <- which(bins == bin)
    organValuesIndex <- which(round(organValues, digits = 1) == bin)
    binValue <- paste0(names(organValues)[organValuesIndex], collapse = '\n')
    binResults[binIndex, count] <- binValue
  }
  
  targetOrgans[,count] <- paste0(names(organValues), ' (', round(organValues, digits = 1), ')')
}
colnames(targetOrgans) <- studies
colnames(binResults) <- studies
row.names(binResults) <- bins


print(DT::datatable(targetOrgans))
print(DT::datatable(binResults))

# study <- 'Study ID'
# organ <- 'THYMUS'
# 
# studyIndex <- which(MIstudies$STUDYID == study)
# organIndex <- which(MIstudies$MISPEC == organ)
# index <- intersect(studyIndex, organIndex)
# print(MIstudies[index,c('USUBJID', 'TRTDOSrank', 'MISPEC', 'MISTRESC', 'Count')])
