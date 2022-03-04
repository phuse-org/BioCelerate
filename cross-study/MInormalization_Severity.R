rm(list=ls())

library(devtools)
library(forcats)
library(ggplot2)
library(ggpattern)
library(Hmisc)
library(httr)
library(ini)
library(magick)
library(reshape2)
library(scales)
library(tidyr)
library(tools)

# Set working directory to location of script
homePath <- dirname(sys.calls()[[1]][[2]])
setwd(homePath)

dataPaths <- read.ini('dataPaths - Template.ini')

###########################################################################################################
# Parameter Settings:

# Select Data Source (Public = phuse-scripts GitHub data; BioCelerate = TDS datasets)
dataSource <- 'Public'
dataSource <- 'BioCelerate'

# Select Organ
MISPEC <- 'BRAIN'

# Enter terms that will replace any string containing these terms (case-insensitive)
termList <- c('NECROSIS' = 'Necrosis',
              'AGGREGATES' = 'Aggregates', 
              'NORMAL' = 'Normal',
              'VACUOLATION' = 'Vacuol', 
              'HYPERTROPHY' = 'Hypertrophy',
              'INFLAMMATION' = 'Inflammation',
              'INFILTRATE' = 'Infiltrat')

# Select TRUE to Save Plot Figures
savePlots <- T

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


# Define path to write files (and create directory if necessary)
writeDirectory <- paste0('results/', dataSource, '/', 'MIplots')
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
  
  # Remove Recovery Animals
  removeIndex <- which(MI$RecoveryStatus == T)
  if (length(removeIndex) > 0) {
    MI <- MI[-removeIndex,]
  }
  
  # Select Sex
  sexIndex <- which(MI$SEX == Sex)
  MI <- MI[sexIndex,]
  
  # Select Organ
  organIndex <- which(MI$MISPEC == MISPEC)
  MI <- MI[organIndex,]
  
  if (nrow(MI) == 0) {
    next
  }
  
          
  # Add Incidence Count
  MI$Count <- 1
  for (i in seq(nrow(MI))) {
    # treatmentIndex <- which(MI$TreatmentDose == MI$TreatmentDose[i])
    treatmentIndex <- which(MI$TRTDOSrank == MI$TRTDOSrank[i])
    studyIndex <- which(MI$StudySpecies == MI$StudySpecies[i])
    index <- intersect(treatmentIndex, studyIndex)
    MI$Count[i] <- 1/length(index)
  }

  #Create Severity Ranking for Studies 
  if (isTRUE(grepl("1 OF 5",MI$MISEV))){
    MI$Severity <- ordered(MI$MISEV, levels = c("O OF 5","1 OF 5","2 OF 5","3 OF 5", "4 OF 5", "5 OF 5"))
    MI$Severity = MI$Severity %>% replace_na("O OF 5")
  } else {
    MI$Severity <- ordered(MI$MISEV, levels = c("NONE","MINIMAL","MILD","MODERATE", "MARKED"))
    MI$Severity = MI$Severity %>% replace_na("NONE")
  }
    
  MIstudy <- MI[,c('STUDYID',
                   'StudySpecies',
                   'USUBJID',
                   'Treatment',
                   'TreatmentDose',
                   'TRTDOSrank',
                   'MISTRESC',
                   'Count',
                   'Severity')] 
  
  if (exists('MIstudies')) {
    MIstudies <- rbind(MIstudies, MIstudy)
  } else {
    MIstudies <- MIstudy
  }
}

MIstudies$TRTDOSrank <- factor(MIstudies$TRTDOSrank, levels = doseRanks)


plotData <- aggregate(Count ~ StudySpecies + MISTRESC + TRTDOSrank + Severity, FUN = sum, data = MIstudies)
plotData$StudyTreatment <- paste(plotData$StudySpecies, plotData$TRTDOSrank, sep = ': ')
colnames(plotData) <- c('Study', 'Finding', 'Treatment','Severity', 'Count', 'StudyTreatment')

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
plotData$Finding <- factor(plotData$Finding, levels = heatmapOrderFindings)
plotData[['Count']] <- as.numeric(plotData[['Count']])

# Reconcile Finding Names
termFactors <- levels(plotData$Finding)
termNames <- termFactors
for (i in seq(length(termList))) {
  term <- as.character(termList[i])
  termName <- names(termList)[i]
  termIndex <- grep(term, termFactors, ignore.case = T)
  termNames[termIndex] <- termName
}
names(termFactors) <- termNames
plotData$Finding <- fct_recode(plotData$Finding, !!!termFactors)

p <- ggplot(plotData) +
  geom_tile_pattern(aes(x = Finding, fill = Count, y = StudyTreatment,pattern_fill = Severity, pattern_density = Severity), 
                    pattern= 'stripe', pattern_color="black") +
  scale_pattern_density_discrete(range=c(0,0.5))+
  scale_pattern_fill_brewer(palette = 3)+
  ylab('Study (Species): Treatment') + ggtitle(MISPEC) +
  scale_fill_gradient2(low = muted('blue'), high = muted('red'), name = 'Incidence') + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(p)

if (savePlots == T) {
  ggsave(filename = paste0(MISPEC, '-heatmap', '-', Sex, '.png'),
         plot = p,
         device = 'png',
         path = paste0(writeDirectory))
}

# 
# # Modeling Analysis (INCOMPLETE)
# MIstudies$MISTRESC <- factor(MIstudies$MISTRESC)
# termFactors <- levels(MIstudies$MISTRESC)
# termNames <- termFactors
# for (term in termList) {
#   termIndex <- grep(term, termFactors, ignore.case = T)
#   termNames[termIndex] <- term
# }
# names(termFactors) <- termNames
# MIstudies$MISTRESC <- fct_recode(MIstudies$MISTRESC, !!!termFactors)
# 
# MIstudies$SPECIES <- MIstudies$StudySpecies
# for (i in seq(nrow(MIstudies))) {
#   SPECIES <- unlist(strsplit(MIstudies$SPECIES[i], '(', fixed = T))[2]
#   SPECIES <- gsub(')', '', SPECIES, fixed = T)
#   MIstudies$SPECIES[i] <- SPECIES
# }
# 
# allFindings <- levels(MIstudies$MISTRESC)
# modelMIstudies <- MIstudies
# # MIstudies$Present <- rep(1, nrow(MIstudies))
# for (study in unique(MIstudies$StudySpecies)) {
#   studyIndex <- which(MIstudies$StudySpecies == study)
#   newData <- NULL
#   for (subject in unique(MIstudies$USUBJID[studyIndex])) {
#     subjectIndex <- studyIndex[which(MIstudies$USUBJID[studyIndex] == subject)]
#     subjectMIstudies <- MIstudies[subjectIndex,]
#     Findings <- subjectMIstudies$MISTRESC
#     missingFindings <- allFindings[which(allFindings %ni% Findings)]
#     nMissing <- length(missingFindings)
#     newRows <- data.frame(STUDYID = rep(unique(subjectMIstudies$STUDYID), nMissing),
#                           StudySpecies = rep(study, nMissing),
#                           USUBJID = rep(subject, nMissing),
#                           Treatment = rep(unique(subjectMIstudies$Treatment)),
#                           TreatmentDose = rep(unique(subjectMIstudies$TreatmentDose), nMissing),
#                           TRTDOSrank = rep(unique(subjectMIstudies$TRTDOSrank), nMissing),
#                           MISTRESC = missingFindings,
#                           Count = rep(0, nMissing),
#                           SPECIES = rep(unique(subjectMIstudies$SPECIES), nMissing)
#                           )
#     if (nrow(newRows) > 0) {
#       if (is.null(newData)) {
#         newData <- newRows
#       } else {
#         newData <- rbind(newData, newRows)
#       }
#     }
#   }
#   if (nrow(newData) > 0) {
#     modelMIstudies <- rbind(modelMIstudies, newData)
#   }
# }
# modelMIstudies$Present <- ceiling(modelMIstudies$Count)
# 
# # mylogit <- glm(MISTRESC ~ StudySpecies*TRTDOSrank, data = MIstudies,family = 'binomial')
# write.csv(modelMIstudies, paste0(saveFolder, 'LIVERdata.csv'), row.names = F)

