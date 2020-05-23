source("sysParameters.R")
require(dplyr)
require(readxl)
require(reshape2)

db <- dbConnect(RSQLite::SQLite(), dbFullName)

dm <- dbGetQuery(db, "SELECT STUDYID, USUBJID, ARMCD, SETCD, SPECIES, STRAIN, SEX FROM DM")

animals <- unique(dbGetQuery(db, 'SELECT STUDYID, TSPARMCD, TSVAL FROM TS WHERE TSPARMCD = "SPECIES" or TSPARMCD = "STRAIN"'))

# need to create a unique id in order to 
# cast the 
animals['ID'] <- rowidv(animals, cols=c("STUDYID", "TSPARMCD"))

animals <- dcast(animals, STUDYID+ID~TSPARMCD, value.var='TSVAL')

merged <- merge(dm, animals, by='STUDYID')

# replace emty string with NA values to get the 
# coalesce function to work properly
merged[merged == ""] <- NA


dt <- merged %>%
            mutate(SPECIES = coalesce(SPECIES.x, SPECIES.y)) %>%
            mutate(STRAIN = coalesce(STRAIN.x, STRAIN.y)) %>%
            mutate(SPECIES = ifelse(SPECIES == 'Rats', 'RAT', SPECIES)) %>%
            select(-SPECIES.x, -SPECIES.y, -STRAIN.x, -STRAIN.y) %>%
            unique()

dt <- dt %>% 
  group_by(STUDYID) %>% 
  tidyr::fill(SPECIES) %>% #default direction down
  tidyr::fill(SPECIES, .direction = "up") %>%
  ungroup()


dt$SPECIES <- toupper(dt$SPECIES)
dt$STRAIN <- toupper(dt$STRAIN)

animalCounts <- dt %>% count(SPECIES, STRAIN)
speciesCounts <- dt %>% count(SPECIES)

speciesCounts <- rename(speciesCounts, total=n)



animalFreq <- dt %>% count(SPECIES, STRAIN) %>% 
               group_by(SPECIES) %>% 
               transmute(STRAIN, Percentage=n/sum(n)*100)

counts <- merge(animalCounts, animalFreq, by=c('SPECIES', 'STRAIN'))
counts <- merge(counts, speciesCounts, by='SPECIES')

write.csv(counts, 'data/SpeciesStrainCounts.csv')

