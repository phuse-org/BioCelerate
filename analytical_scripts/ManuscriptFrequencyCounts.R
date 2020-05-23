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


##### Table 2 ######

# will import the TS Domain
importSENDDomains('TS')
importSENDDomains('EX')

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

# 98.12 vs vs 97.93 in manuscript

##### 3.3.2 MI Findings #####

tsIG <- TS[TS$TSPARMCD == 'SNDIGVER']

studies_30 <-  tsIG$STUDYID[grepl("3.0", tsIG$TSVAL)]
studies_31 <-  tsIG$STUDYID[grepl("3.1", tsIG$TSVAL)]

findings_30 <- MI[MI$STUDYID %in% studies_30]
findings_31 <- MI[MI$STUDYID %in% studies_31]

# frequencies 

sum(findings_30$MISTRESC != "") / length(findings_30$MISTRESC)
# 98.57 vs 98.5

sum(findings_31$MISTRESC != "") / length(findings_31$MISTRESC)
# 98.7 vs 99.4

cont_terms <- readxl::read_excel('data/SEND Terminology 2019-09-27.xls', sheet = "SEND Terminology 2019-09-27")[c('Codelist Code', 'CDISC Submission Value')]
miCodelists <- c('C120531', 'C88025', 'C132321')
cont_terms <- cont_terms[cont_terms$`Codelist Code` %in% miCodelists,]$`CDISC Submission Value`
cont_terms <- cont_terms[!is.na(cont_terms)]


sum(toupper(findings_30$MISTRESC) %in% cont_terms) / length(findings_30$MISTRESC)

sum(toupper(findings_31$MISTRESC) %in% cont_terms) / length(findings_31$MISTRESC)
# 94.30 vs 95.6

frequencyMI30 <- findings_30 %>%
                  count(MISTRESC) %>%
                  arrange(desc(n))


write.csv(frequencyMI30, 'data/freqMIFindings30.csv')


frequencyMI31 <- findings_31 %>%
                  count(MISTRESC) %>%
                  arrange(desc(n))


write.csv(frequencyMI31, 'data/freqMIFindings31.csv')


          