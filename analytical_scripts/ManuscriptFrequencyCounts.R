# Note: working directory needs
# to be set one level above 
# the current directory i.e, 
# the parent directory 


# This script will generate all the
# for the SEND Manuscript 2.

source("importSENDDomains.R")
source("extractTSParam.R")
source("addFindingsAnimalAge.R")
source("subjDataExtract.R")

require(dplyr)


cont_terms <- readxl::read_excel('data/SEND Terminology 2019-09-27.xls', 
                                 sheet = "SEND Terminology 2019-09-27")

### REsults 3.1.1 Study Design ###
importSENDDomains('TS')

uniqueStudiesTS <- length(unique(TS$STUDYID))
tsDesign <- TS[TS$TSPARMCD == 'SDESIGN']

uniqueStudiesDesign <- length(unique(tsDesign$STUDYID))

tsDesign$TSVAL <- gsub("[\r\n]", "", toupper(tsDesign$TSVAL))

table(tsDesign$TSVAL, useNA = 'always')

length(unique(tsDesign[tsDesign$TSVAL == 'PARALLEL']$STUDYID))

### Results 3.1.2 Route of Administration ### 

importSENDDomains('TS')
importSENDDomains('EX')
uniqueStudiesTS <- length(unique(TS$STUDYID))
tsRoute <- TS[TS$TSPARMCD == 'ROUTE']

tsRoute <- tsRoute[,.(STUDYID, EXROUTE=toupper(TSVAL))]


tsRoute <- tsRoute %>%
  dplyr::group_by(STUDYID) %>%
  dplyr::mutate(NUM_ROUTE = n()) %>%
  dplyr::ungroup()

roaCodelist <- 'C66729'
roaControlledTerms <- cont_terms[cont_terms$`Codelist Code` == roaCodelist,]$`CDISC Submission Value`
roaControlledTerms <- roaControlledTerms[!is.na(roaControlledTerms)]

# controlled terms TS Route
sum(toupper(tsRoute$EXROUTE) %in% roaControlledTerms) / nrow(tsRoute)


# controlled terms EX Route

sum(toupper(EX$EXROUTE) %in% roaControlledTerms)
nrow(EX)


### Results 3.1.4 Vehicle ### 

tsVehicle <-  TS[TS$TSPARMCD == 'TRTV']
length(unique(tsVehicle$STUDYID))

sum((!is.na(EX$EXTRTV)) & (EX$EXTRTV != '')) / nrow(EX)


### Results 3.1.5 Study Start Date ### 

tsStartDate <-  TS[TS$TSPARMCD == 'STSTDTC']
tsStartDate <- tsStartDate[tsStartDate$TSVAL != '']
length(unique(tsStartDate$STUDYID))

# not in ISO format
sum(is.na(as.Date(tsStartDate$TSVAL)))



### Results 3.1.5  TEst faciility ### 

tsFacility <-  TS[TS$TSPARMCD == 'TSTFNAM']
# tsFacility <- tsFacility[tsFacility$TSVAL != '']
length(unique(tsFacility$STUDYID))


### Results 3.2.2 Sex ### 

importSENDDomains('DM')

table(DM$SEX, useNA='always')

### Results 3.2.4  Test Subject Suppler ### 

tsSupplier <-  TS[TS$TSPARMCD == 'SPLRNAM']
tsSupplier <- tsSupplier[tsSupplier$TSVAL != '']
length(unique(tsSupplier$STUDYID))


##### Table 2 ######

# will import the TS Domain
importSENDDomains('TS')
importSENDDomains('EX')

uniqueStudiesTS <- length(unique(TS$STUDYID))
tsRoute <- TS[TS$TSPARMCD == 'ROUTE']

# TS variable
uniqueStudiesTS <- length(unique(TS$STUDYID))
tsRoute <- TS[TS$TSPARMCD == 'ROUTE']

length(unique(tsRoute$STUDYID))
table(tsRoute$TSVAL, useNA = 'always')

sum(EX$EXROUTE == '' | is.na(EX$EXROUTE))

length(EX$EXROUTE)
table(EX$EXROUTE, useNA = 'always')

##### Table 3 ######

tsSpecies <- TS[TS$TSPARMCD == 'SPECIES']

length(unique(tsSpecies$STUDYID))
# 1764 species, 1763 unique studies

tsStrain <- TS[TS$TSPARMCD == 'STRAIN']

length(unique(tsStrain$STUDYID))
# 1764 species, 1763 unique studies



importSENDDomains('TX')

uniqueStudiesTX <- unique(TX$STUDYID)

length(uniqueStudiesTX)

txSpecies <- TX[TX$TXPARMCD == 'SPECIES']

length(unique(txSpecies$STUDYID))


txStrain <- TX[TX$TXPARMCD == 'STRAIN']

length(unique(txStrain$STUDYID))

importSENDDomains('DM')

uniqueStudiesDM <- unique(DM$STUDYID)

length(uniqueStudiesDM)


dmSpecies <- DM[,c('STUDYID', 'SPECIES')] 

# count number of unique studies where dm is nan
# and is also not an empty string
length(unique(dmSpecies$STUDYID[((!is.na(dmSpecies$SPECIES)) & (dmSpecies$SPECIES != ''))]))

dmStrain <- DM[,c('STUDYID', 'STRAIN')] 

# count number of unique studies where dm is nan
# and is also not an empty string
length(unique(dmStrain$STUDYID[((!is.na(dmStrain$STRAIN)) & (dmStrain$STRAIN != ''))]))


####




#### Table # ####

txControl <- TX[TX$TXPARMCD == 'TCNTRL']

length(unique(txControl$STUDYID))


freqCntrls <- txControl %>% dplyr::count(TXVAL) %>% dplyr::arrange(dplyr::desc(n))


#### Table # ####

tsSplrnam <- TS[TS$TSPARMCD == 'SPLRNAM']
length(unique(tsSplrnam$STUDYID))

# 1763/1763 manuscript >99% 

tsSplrloc <- TS[TS$TSPARMCD == 'SPLRLOC']
length(unique(tsSplrloc$STUDYID))

# 977/1763 55.04 % vs 53.04 % in manuscript

# animal age at time of Finding

importSENDDomains('MI')
miFindingsAge <- addFindingsAnimalAge('MI', MI)
nFindings <- nrow(miFindingsAge)

findingsWithAge <- sum(!(is.na(miFindingsAge$AGE)))
findingsWithAge / nFindings


##### 3.3.2 MI Findings #####

tsIG <- TS[TS$TSPARMCD == 'SNDIGVER']

studies_30 <-  tsIG$STUDYID[grepl("3.0", tsIG$TSVAL)]
studies_31 <-  tsIG$STUDYID[grepl("3.1", tsIG$TSVAL)]

findings_30 <- MI[MI$STUDYID %in% studies_30]
findings_31 <- MI[MI$STUDYID %in% studies_31]

# frequencies 

sum(findings_30$MISTRESC != "") / length(findings_30$MISTRESC)

sum(findings_31$MISTRESC != "") / length(findings_31$MISTRESC)

findings_30$MISTRESC <- toupper(findings_30$MISTRESC)
findings_31$MISTRESC <- toupper(findings_31$MISTRESC)

cont_terms <- readxl::read_excel('data/SEND Terminology 2019-09-27.xls', sheet = "SEND Terminology 2019-09-27")[c('Codelist Code', 'CDISC Submission Value')]
miCodelists <- c('C120531', 'C88025', 'C132321')
all_cont_terms <- cont_terms[cont_terms$`Codelist Code` %in% miCodelists,]$`CDISC Submission Value`
all_cont_terms <- all_cont_terms[!is.na(all_cont_terms)]

just_mi_terms <- cont_terms[cont_terms$`Codelist Code` %in% c('C120531', 'C88025'),]$`CDISC Submission Value`
just_mi_terms <- just_mi_terms[!is.na(just_mi_terms)]

# control counts
sum(toupper(findings_30$MISTRESC) %in% all_cont_terms) / length(findings_30$MISTRESC)
sum(toupper(findings_31$MISTRESC) %in% all_cont_terms) / length(findings_31$MISTRESC)

findings_31_not_normal <- findings_31[(findings_31$MISTRESC != 'UNREMARKABLE'),]
findings_31_not_normal <- findings_31_not_normal[(findings_31_not_normal$MISTRESC != 'NORMAL'),]

sum(toupper(findings_31_not_normal$MISTRESC) %in% just_mi_terms) / length(findings_31_not_normal$MISTRESC)



frequencyMI30 <- findings_30 %>%
                  count(MISTRESC) %>%
                  arrange(desc(n))


write.csv(frequencyMI30, 'data/freqMIFindings30.csv')


frequencyMI31 <- findings_31 %>%
                  count(MISTRESC) %>%
                  arrange(desc(n))


write.csv(frequencyMI31, 'data/freqMIFindings31.csv')

frequencyMI_not_in31 <- findings_31[!(findings_31$MISTRESC %in% just_mi_terms),] %>%
                          count(MISTRESC) %>%
                          arrange(desc(n))

write.csv(frequencyMI_not_in31, 'data/NotInfreqMIFindings31.csv')
          