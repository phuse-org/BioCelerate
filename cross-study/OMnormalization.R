#Generates plots from organ weight normalized 3 different ways for Biocelerate
#studies.
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

#Set File Path to Biocelerate Data
homePath <- dirname(sys.calls()[[1]][[2]])
setwd(homePath)
dataSource <- "BioCelerate"

#Choose what is plotted
Organ <- c('KIDNEY', 'LIVER')
Dose <- ('HD') #Options are in doseRanks
Sex <- ('M') #Options are 'M', 'F', or 'ALL'

dataPaths <- read.ini('dataPaths - Template.ini')
paths <- as.character(unlist(dataPaths[[dataSource]]))
doseRanks <- c('Vehicle', 'LD', 'MD', 'HD')

#Load Relevant Functions
source('Functions/Functions.R')
source('Functions/groupSEND.R')

####Load XPT Files for Biocelerate Dataset ##################################
#Dog Datasets
Dog5492 <- load.xpt.files(path = paste(homePath, paths[1], sep = '/'),
                          showProgress = F)
Dog6756<- load.xpt.files(path = paste(homePath, paths[2], sep = '/'),
                         showProgress = F)
#Rat Datasets
Rat5492 <- load.xpt.files(path = paste(homePath, paths[3], sep = '/'),
                          showProgress = F)
Rat6756 <- load.xpt.files(path = paste(homePath, paths[4], sep = '/'),
                          showProgress = F)

#Create Shared Data Frame with StudyID, Species, USUBJID, SEX, and DOSES
Dog5492_dm <- data.frame(StudyID = rep("Dog 5492", length(unique(Dog5492$dm$USUBJID))),
                         Species = rep("Dog", length(unique(Dog5492$dm$USUBJID))),
                         USUBJID = Dog5492$dm$USUBJID, SEX = Dog5492$dm$SEX, ARMCD = Dog5492$dm$ARMCD)
Dog6756_dm <- data.frame(StudyID = rep("Dog 6756", length(unique(Dog6756$dm$USUBJID))),
                         Species = rep("Dog", length(unique(Dog6756$dm$USUBJID))),
                         USUBJID = Dog6756$dm$USUBJID, SEX = Dog6756$dm$SEX, ARMCD = Dog6756$dm$ARMCD)
Rat5492_dm <- data.frame(StudyID = rep("Rat 5492", length(unique(Rat5492$dm$USUBJID))),
                         Species = rep("Rat", length(unique(Rat5492$dm$USUBJID))),
                         USUBJID = Rat5492$dm$USUBJID, SEX = Rat5492$dm$SEX, ARMCD = Rat5492$dm$ARMCD)
Rat6756_dm <- data.frame(StudyID = rep("Rat 6756", length(unique(Rat6756$dm$USUBJID))),
                         Species = rep("Rat", length(unique(Rat6756$dm$USUBJID))),
                         USUBJID = Rat6756$dm$USUBJID, SEX = Rat6756$dm$SEX, ARMCD = Rat6756$dm$ARMCD)
CompileData <- rbind(Dog5492_dm, Dog6756_dm, Rat5492_dm, Rat6756_dm)
CompileData$SEX <- str_replace_all(CompileData$SEX,"Combined", "F")



#Compile % Change for BW 
#Find Terminal and Initial BW for all studies
TermBodyWeight <- Dog6756$bw[which(Dog6756$bw$BWTESTCD == "TERMBW"), c("USUBJID", "BWSTRESN")]
TermBodyWeight <- rbind(TermBodyWeight, Dog5492$bw[which(Dog5492$bw$BWTESTCD == "TERMBW"), c("USUBJID", "BWSTRESN")],
                        Rat5492$bw[which(Rat5492$bw$BWTESTCD == "TERMBW"), c("USUBJID", "BWSTRESN")],
                        Rat6756$bw[which(Rat5492$bw$BWTESTCD == "TERMBW"), c("USUBJID", "BWSTRESN")] )
TermBodyWeight$BWSTRESN <- as.numeric(TermBodyWeight$BWSTRESN)
#Convert Dog BW from kg to g by multiplying by 1000
Dogidx <- 1:(length(unique(Dog6756$bw$USUBJID))+length(unique(Dog5492$bw$USUBJID)))
TermBodyWeight$BWSTRESN[Dogidx] <- TermBodyWeight$BWSTRESN[Dogidx]*1000
CompileData <- merge(CompileData, TermBodyWeight, by = "USUBJID")


#Remove Gender 
if (Sex == 'M'){
  CompileData <- CompileData[which(CompileData$SEX == "M"),]
} else if (Sex == 'F'){
  CompileData <- CompileData[which(CompileData$SEX == "F"),]
} 
#Compile OM Data for Organlist
#Find Brain indexes
Dog6756idx <- str_which(Dog6756$om$OMSPEC, "BRAIN")
Dog5492idx <- str_which(Dog5492$om$OMSPEC, "BRAIN")
Rat6756idx <- str_which(Rat6756$om$OMSPEC, "BRAIN")
Rat5492idx <- str_which(Rat5492$om$OMSPEC, "BRAIN")

#Add Indxes that are Organs
for (i in 1:length(Organ)){
  Dog6756idx <- append(Dog6756idx, str_which(Dog6756$om$OMSPEC, Organ[i]))
  Dog5492idx <- append(Dog5492idx, str_which(Dog5492$om$OMSPEC, Organ[i]))
  Rat6756idx <- append(Rat6756idx, str_which(Rat6756$om$OMSPEC, Organ[i]))
  Rat5492idx <- append(Rat5492idx, str_which(Rat5492$om$OMSPEC, Organ[i]))
}

OrganWeights <- data.frame(USUBJID = Dog6756$om$USUBJID[Dog6756idx],
                           OMSPEC = Dog6756$om$OMSPEC[Dog6756idx],
                           OMSTRESN = Dog6756$om$OMSTRESN[Dog6756idx],
                           OMTEST = Dog6756$om$OMTEST[Dog6756idx])
OrganWeights <- rbind(OrganWeights,Dog5492$om[Dog5492idx,c("USUBJID", "OMSPEC", "OMSTRESN","OMTEST")],
                      Rat6756$om[Rat6756idx,c("USUBJID", "OMSPEC", "OMSTRESN","OMTEST")],
                      Rat5492$om[Rat5492idx, c("USUBJID", "OMSPEC", "OMSTRESN","OMTEST")])
OrganWeights <- OrganWeights[which(OrganWeights$OMTEST == "Weight"), c("USUBJID", "OMSPEC", "OMSTRESN")]
CompileData <- merge(CompileData, OrganWeights, by = "USUBJID")

#Remove Recovery Animals
CompileData <- CompileData[!str_detect(CompileData$ARMCD, "R"),]
CompileData$ARMCD <- factor(CompileData$ARMCD)
levels(CompileData$ARMCD) <- doseRanks

############## Calculate Ratios and Zscores of Organ Weights ##################

#Calculate Organ to Terminal BW Ratio
CompileData$OrgantoTermBW <- CompileData$OMSTRESN/CompileData$BWSTRESN
#Calculate Organ to Brain Weight Ratio
CompileData$OrgantoBrainOW <- NA
for (sub in unique(CompileData$USUBJID)){
  SubjectData <- CompileData[which(CompileData$USUBJID == sub),]
  Subidx <-which(CompileData$USUBJID == sub)
  SubBrainWeight <- SubjectData$OMSTRESN[which(SubjectData$OMSPEC == "BRAIN")]
  CompileData$OrgantoBrainOW[Subidx] <- CompileData$OMSTRESN[Subidx]/SubBrainWeight
}

#Calculate Accuracy of Ratios to OM Value >> BioCelerate Studies DO have them
#Pull out "Organ to Brain Weight Ratio" 
#OMBrainRatio <- Rat6756$om[Rat6756$om$OMTEST == "Organ to Brain Weight Ratio",c("USUBJID", "OMSTRESN", "OMSPEC")]
#OMBrainRatio <- rbind(OMBrainRatio, Rat5492$om[Rat5492$om$OMTEST == "Organ to Brain Weight Ratio",c("USUBJID", "OMSTRESN", "OMSPEC")],
#                      Dog6756$om[Dog6756$om$OMTEST == "Organ to Brain Weight Ratio",c("USUBJID", "OMSTRESN", "OMSPEC")],
#                      Dog5492$om[Dog5492$om$OMTEST == "Organ to Brain Weight Ratio",c("USUBJID", "OMSTRESN", "OMSPEC")])
#OMBrainRatio$OMSTRESN <- OMBrainRatio$OMSTRESN/100 #Convert from Percent to Ratio
#Pull out "Organ to Body Weight Ratio" 
#OMBodyRatio <- Rat6756$om[Rat6756$om$OMTEST == "Organ to Body Weight Ratio",c("USUBJID", "OMSTRESN", "OMSPEC")]
#OMBodyRatio <- rbind(OMBrainRatio, Rat5492$om[Rat5492$om$OMTEST == "Organ to Body Weight Ratio",c("USUBJID", "OMSTRESN", "OMSPEC")],
#                      Dog6756$om[Dog6756$om$OMTEST == "Organ to Body Weight Ratio",c("USUBJID", "OMSTRESN", "OMSPEC")],
#                      Dog5492$om[Dog5492$om$OMTEST == "Organ to Body Weight Ratio",c("USUBJID", "OMSTRESN", "OMSPEC")])
#OMBodyRatio$OMSTRESN <- OMBodyRatio$OMSTRESN/100 #Convert from Percent to Ratio
#Limit to Chosen Organs
#OMBrainRatio <- OMBrainRatio[OMBrainRatio$OMSPEC == Organ,]
#OrganBrain <- c(Organ, "BRAIN")
#OMBodyRatio <- OMBodyRatio[OMBodyRatio$OMSPEC == c(OrganBrain),]
#BrainCheck <- merge(OMBrainRatio, CompileData[,c("USUBJID","OMSPEC","OrgantoBrainOW")])
#BodyCheck <- merge(OMBodyRatio, CompileData[,c("USUBJID","OMSPEC","OrgantoBrainOW")])

#Calculate Z Score for straight Organ Weight, Terminal BW Ratio, and Brain Ratio 
CompileData$OrganzScore <- NA
CompileData$BWRatiozScore <- NA
CompileData$BrainRatiozScore <- NA
CompileData$BWzScore <- NA
for (study in unique(CompileData$StudyID)){
  StudyData <- CompileData[which(CompileData$StudyID == study),]
  StudyIdx <- which(CompileData$StudyID == study)
  #Find Control Animals Per Study and Calculate Mean and SD per Organ
  ControlAnimals <- StudyData[which(StudyData$ARMCD == "Vehicle"),]
  for (organ in unique(StudyData$OMSPEC)){
    organdata <- ControlAnimals[which(ControlAnimals$OMSPEC == organ),]
    MeansSd <- data.frame(ORGAN = organ,
                               ORGANMEAN = mean(organdata$OMSTRESN, na.rm = TRUE),
                               ORGANSd = sd(organdata$OMSTRESN, na.rm = TRUE),
                               OrgantoBWmean = mean(organdata$OrgantoTermBW, na.rm = TRUE),
                               OrgantoBWSd = sd(organdata$OrgantoTermBW, na.rm = TRUE),
                               OrgantoBrainmean = mean(organdata$OrgantoBrainOW, na.rm = TRUE),
                               OrgantoBrainSd = sd(organdata$OrgantoBrainOW, na.rm = TRUE),
                               BWmean = mean(organdata$BWSTRESN, na.rm = TRUE),
                               BWsd = sd(organdata$BWSTRESN, na.rm = TRUE))
    Idx<- which(StudyData$OMSPEC == organ)
    StudyData$OrganzScore[Idx] <-(StudyData$OMSTRESN[Idx] - MeansSd$ORGANMEAN)/MeansSd$ORGANSd 
    StudyData$BWRatiozScore[Idx] <-(StudyData$OrgantoTermBW[Idx] - MeansSd$OrgantoBWmean)/MeansSd$OrgantoBWSd
    StudyData$BrainRatiozScore[Idx] <-(StudyData$OrgantoBrainOW[Idx] -MeansSd$OrgantoBrainmean)/MeansSd$OrgantoBrainSd  
    StudyData$BWzScore[Idx] <-(StudyData$BWSTRESN[Idx] -MeansSd$BWmean)/MeansSd$BWsd  
    }
  #Reconcile Study into CompileData with Zscores
  CompileData$OrganzScore[StudyIdx] <- StudyData$OrganzScore
  CompileData$BWRatiozScore[StudyIdx] <- StudyData$BWRatiozScore
  CompileData$BrainRatiozScore[StudyIdx] <- StudyData$BrainRatiozScore
  CompileData$BWzScore[StudyIdx] <- StudyData$BWzScore
}
CompileData$Compound <- word(CompileData$StudyID,2)

#Plot the Organ Zscore Values 
plotData <- CompileData[which(CompileData$ARMCD == Dose),]
plotDatazScore <- aggregate(OrganzScore ~ StudyID + Compound + Species + OMSPEC + ARMCD, FUN = mean, data = plotData)
plotDataBWRatiozScore <- aggregate(BWRatiozScore ~ StudyID + Compound + Species + OMSPEC + ARMCD, FUN = mean, data = plotData)
plotDataBrainRatiozScore <- aggregate(BrainRatiozScore ~ StudyID + Compound + Species + OMSPEC + ARMCD, FUN = mean, data = plotData)
plotDataBWzScore <- aggregate(BWzScore ~ StudyID + Compound + Species + OMSPEC + ARMCD, FUN = mean, data = plotData)
#Organ zScore
p <- ggplot(plotDatazScore, aes(y = OMSPEC, x = OrganzScore, color = Compound, shape = Species)) +
  geom_point(size =5) + geom_point(stat = 'summary', fun.x = 'mean', color = 'black', shape = '|', size = 10) +
  guides(color = guide_legend(override.aes = list(shape = 'square'))) + xlab('Organ Weight Zscore') + ggtitle(paste0(Dose," - ",Sex))
print(p)
#BW Ratio zScore
q <-ggplot(plotDataBWRatiozScore, aes(y = OMSPEC, x = BWRatiozScore, color = Compound, shape = Species)) +
  geom_point(size =5) + geom_point(stat = 'summary', fun.x = 'mean', color = 'black', shape = '|', size = 10) +
  guides(color = guide_legend(override.aes = list(shape = 'square'))) + xlab('Organ Weight to Terminal BW Ratio Zscore') + ggtitle(paste0(Dose," - ",Sex))
print(q)
#Brain Ratio zScore
br <-ggplot(plotDataBrainRatiozScore, aes(y = OMSPEC, x = BrainRatiozScore, color = Compound, shape = Species)) +
  geom_point(size =5) + geom_point(stat = 'summary', fun.x = 'mean', color = 'black', shape = '|', size = 10) +
  guides(color = guide_legend(override.aes = list(shape = 'square'))) + xlab('Organ Weight to Brain Weight Ratio Zscore') + ggtitle(paste0(Dose," - ",Sex))
print(br)
#BW zScore
br <-ggplot(plotDataBWzScore, aes(y = Compound , x = BWzScore, color = Compound, shape = Species)) +
  geom_point(size =5) + geom_point(stat = 'summary', fun.x = 'mean', color = 'black', shape = '|', size = 10) +
  guides(color = guide_legend(override.aes = list(shape = 'square'))) + xlab('BW Zscore') + ggtitle(paste0(Dose," - ",Sex))
print(br)