###################################################################################
# Script name   : filterStudyAnimalSpeciesStrain.R
# Date Created  : 16-Jan-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extract studies and animals which fulfills a specified species
#                 value and optional strain value from a pooled SEND data store.
#
# Description   : Function FilterAnimalsSpeciesStrain:
# 
#                 Returns a data table with a set of animals extracted from the table of  
#                 animals given as input where the animals fits the species value(s) given 
#                 as input (a list of multiple species values may be given as input) 
#                 and also fits the strain value(s) if given as input (optional).
#                 Animals from the input set, which are included in studies where one of  
#                 of the input species values - and optionally also strain values - is 
#                 registered in TS (and TS have only this species or species/strain 
#                 value included) are included in the output set.
#                 Animals from the input set, belonging to studies 
#                   - with no species/strain value registered  or  
#                   - with more than one species/strain value registered
#                 in TS are included in the output set if they exists in DM with matching 
#                 DM.SPECIES or DM.SPECIES/STRAIN values or are included in TX in a trial set 
#                 with matching TX.SPECIES or TX.SPECIES/STRAIN (i.e. TXVAL where TXPARMCD is 
#                 SPECIES' or 'STRAIN')
#                 The comparisons of the species and strain values are done case 
#                 insensitive.
#                 
#                 If the input parameter inclUncertain flag is enabled, uncertain animals
#                 are included in the output set.
#                 These uncertain situations are identified and reported for SPECIES and STRAIN respectively 
#                 (in column UNCERTAIN_MSG):
#                  - TS parameter SPECIES/STRAIN is missing or invalid (not CT value - CDISC code list SPECIES/STRAIN) 
#                    and TX parameter SPECIES/STRAIN is missing or invalid (not CT value) and DM.SPECIES/STRAIN is 
#                    missing or invalid (not CT value)
#                  - Different values of SPECIES/STRAIN across TS, TX and DM for studies where no or only one 
#                    TS parameter SPECIES/STRAIN is registered
#                  - Multiple TS parameter SPECIES/STRAIN values are registered for study and TX parameter 
#                    SPECIES/STRAIN and/or DM.SPECIES/STRAIN do not match any of the TS values.
#                  - Multiple TS parameter SPECIES/STRAIN values are registered for study and TX parameter 
#                    SPECIES/STRAIN and DM.SPECIES/STRAIN are unequal.
#                  Non-empty UNCERTAIN_MSG values are merged with non-empty UNCERTAIN_MSG values 
#                  which may exist in the input set of animals (animalList).
#
# Input         : - TS, TX and DM
#                 - A data tables specified in the input parameters animalList:
#                   It contains the list of animals to filter for specified species and strain value(s)
#                   - must contain these character variables:
#                       STUDYID
#                       USUBJID
#                     other variables may be included 
#                 - CDISC CT code lists SPECIES and STRAIN.
#                   
# Output        : A data table with the character columns:
#                   STUDYID
#                   USUBJID
#                   SPECIES
#                   STRAIN (if strainFilter has been specified)
#                   UNCERTAIN_MSG - if input parameter inclUncertain flag is enabled
#                 plus any additional columns which may be included in the input data animalList
#
# Parameters    : animalList:     Mandatory, data table (see Input).
#                 speciesFilter:  Mandatory, character.
#                                   The species value(s) to use as criterion for filtering of the input data table.
#                                   It can be a single string, a vector or a list of multiple strings.
#                 strainFilter:   Optional, character.
#                                   The species value(s) to use as criterion for filtering of the input data table.
#                                   It can be a single string, a vector or a list of multiple strings.
#                                   Only allowed when a single value is specified for speciesFilter.
#                 inclUncertain:  Optional, Include uncertain rows or not
#                   
###################################################################################

library(data.table)

FilterAnimalsSpeciesStrain<-function(animalList, speciesFilter=NULL, strainFilter=NULL, inclUncertain=FALSE, exclusively=FALSE) {
  
  ##################################################################################################################
  #### Identify uncertain animals at species level
  ##################################################################################################################
  identifyUncertainSPECIES<-function(SPECIES, SPECIES_TS, SPECIES_TX, SPECIES_DM,  ALL_SPECIES_TS, NUM_SPECIES_TS, NUM_ANIMALS) {
    msgArr<-c()
    if (is.na(SPECIES))
      msgArr<-c(msgArr, 'TS and TX parameters SPECIES and DM.SPECIES are all missing')
    else {
      if (NUM_ANIMALS > NUM_SPECIES_TS)
        msgArr<-c(msgArr, 'TX parameter SPECIES included multiple times for the SET')
      else {
        if (! SPECIES %in% ctSPECIES) {
          if (!is.na(SPECIES_DM) & ! SPECIES_DM %in% ctSPECIES)
            msgArr<-c(msgArr, 'DM.SPECIES does not contain a valid CT value')
          else if (!is.na(SPECIES_TX) & ! SPECIES_TX %in% ctSPECIES)
            msgArr<-c(msgArr, 'TX parameter SPECIES does not contain a valid CT value')
          else if (!is.na(SPECIES_TS) & ! SPECIES_TS %in% ctSPECIES)
            msgArr<-c(msgArr, 'TS parameter SPECIES does not contain a valid CT value')
        }
        
        if (NUM_SPECIES_TS == 1 & length(unique(na.omit(c(SPECIES_TS, SPECIES_TX, SPECIES_DM)))) > 1)
          msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters SPECIES and/or DM.SPECIES')
        else
          if (NUM_SPECIES_TS > 1 & ((!SPECIES %in% ALL_SPECIES_TS) | (!is.na(SPECIES_TX) & !is.na(SPECIES_DM) & SPECIES_TX != SPECIES_DM))) 
            msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters SPECIES and/or DM.SPECIES')
      }
    }
    msg<-paste(msgArr, collapse = ' & ')
    return(ifelse(msg=="", as.character(NA), msg))
  }
  
  ##################################################################################################################    
  #### Identify uncertain animals at strain level
  ##################################################################################################################
  identifyUncertainSTRAIN<-function(STRAIN, STRAIN_TS, STRAIN_TX, STRAIN_DM,  ALL_STRAIN_TS, NUM_STRAIN_TS, NUM_ANIMALS) {
    msgArr<-c()
    if (is.na(STRAIN))
      msgArr<-c(msgArr, 'TS and TX parameters STRAIN and DM.STRAIN are all missing')
    else {
      if (NUM_ANIMALS > NUM_STRAIN_TS)
        msgArr<-c(msgArr, 'TX parameter STRAIN included multiple times for the SET')
      else {
        if (! STRAIN %in% ctSTRAIN) {
          if (!is.na(STRAIN_DM) & ! STRAIN_DM %in% ctSTRAIN)
            msgArr<-c(msgArr, 'DM.STRAIN does not contain a valid CT value')
          else if (!is.na(STRAIN_TX) & ! STRAIN_TX %in% ctSTRAIN)
            msgArr<-c(msgArr, 'TX parameter STRAIN does not contain a valid CT value')
          else if (!is.na(STRAIN_TS) & ! STRAIN_TS %in% ctSTRAIN)
            msgArr<-c(msgArr, 'TS parameter STRAIN does not contain a valid CT value')
        }
        
        if (NUM_STRAIN_TS == 1 & length(unique(na.omit(c(STRAIN_TS, STRAIN_TX, STRAIN_DM)))) > 1)
          msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters STRAIN and/or DM.STRAIN')
        else
          if (NUM_STRAIN_TS > 1 & ((!STRAIN %in% ALL_STRAIN_TS) | (!is.na(STRAIN_TX) & !is.na(STRAIN_DM) & STRAIN_TX != STRAIN_DM))) 
            msgArr<-c(msgArr, 'Mismatch in values of TS and/or TX parameters STRAIN and/or DM.STRAIN')
      }
    } 
    msg<-paste(msgArr, collapse = ' & ')
    return(ifelse(msg=="", as.character(NA), msg))
  }
  
  ##################################################################################################################
  #### Doing the filtering for one species and potential list of related strain(s) 
  ##################################################################################################################
  doFiltering <- function(speciesFilterX, strainFilterX) {
    
    if (!(is.null(strainFilterX) | isTRUE(is.na(strainFilterX)) | isTRUE(strainFilterX == "")))
      InclStrainFilter <- TRUE
    else InclStrainFilter <- FALSE
    
    # Extract animals matching the species filter - exclude uncertain rows
    foundAnimalSpecies <- animalSpecies[SPECIES %in% speciesFilterX & is.na(SPECIES_UNCERTAIN_MSG),
                                        .(STUDYID, USUBJID, SPECIES)]
    if (exclusively) {
      # Find and exclude animals for studies with animals having other SPECIES than the requested
      foundAnimalSpecies<-
        merge( foundAnimalSpecies,
               # Set of studies to keep:
               fsetdiff(unique( foundAnimalSpecies[,.(STUDYID)]),
                        # Set of studies (included in the found set of animals with matching SPECIES values) with possible 
                        # SPECIES values not included in the speciesFilterX:  
                        unique(fsetdiff(merge(# Set of possible SPECIES values per study in the input set of animals:
                          unique(animalSpecies[,.(STUDYID, SPECIES)]),
                          unique( foundAnimalSpecies[,.(STUDYID)]), 
                          by='STUDYID'),
                          unique( foundAnimalSpecies[,.(STUDYID, SPECIES)]))[,.(STUDYID)])),
               by='STUDYID')
      
    }
    
    ## Look into the STRAIN
    
    # Extract unique strain data per animal matching the set of animals filtered for species
    animalStrainAll <- 
      unique(merge(animalSpeciesStrainAll[SPECIES == as.character(SPECIES_TS) | 
                                            (is.na(SPECIES_TS) & SPECIES == as.character(SPECIES_TX)) |
                                            (is.na(SPECIES_TS) & is.na(SPECIES_TX) & SPECIES == as.character(SPECIES_DM)),
                                          .(STUDYID, USUBJID, 
                                            STRAIN_TS = as.character(STRAIN_TS), 
                                            STRAIN_TX = as.character(STRAIN_TX), 
                                            STRAIN_DM = as.character(STRAIN_DM), 
                                            STRAIN, SPECIES)],
                   foundAnimalSpecies,
                   by = c('STUDYID', 'USUBJID', 'SPECIES'),
                   all.y = TRUE))
    
    # Add variables with 
    #  - count of number of distinct STRAINS per study
    #  - concatenation of all species per study (for studies with one species, this is equal to SPECIES_TS)
    studyStrain <-
      unique(unique(animalStrainAll[, .(STUDYID, STRAIN_TS)])[
        , `:=` (NUM_STRAIN_TS = .N), by = STUDYID][
          , `:=` (ALL_STRAIN_TS = c(.SD)), by = STUDYID, .SDcols='STRAIN_TS'][
            , .(STUDYID,NUM_STRAIN_TS,ALL_STRAIN_TS )], by='STUDYID')
    # Add calculated columns to the list of animals
    animalStrainAll <- merge(animalStrainAll, 
                             studyStrain, 
                             by = 'STUDYID')
    
    # Add variable with count of unique USUBJID per study (is expected to be one usubjid per studyid per TSPARMCD 'STRAIN' )
    animalStrainAll[, `:=` (NUM_ANIMALS = .N), by = .(STUDYID, USUBJID)]
    
    
    # Identify uncertain animals - add variable STRAIN_UNCERTAIN_MSG
    # - remove temp columns used in the processing and remove duplicates for for multiple STRAIN at study level
    animalStrain <- 
      unique(
        animalStrainAll[,`:=` (STRAIN_UNCERTAIN_MSG=mapply(identifyUncertainSTRAIN, 
                                                           STRAIN, 
                                                           STRAIN_TS, 
                                                           STRAIN_TX,
                                                           STRAIN_DM,  
                                                           ALL_STRAIN_TS,
                                                           NUM_STRAIN_TS, 
                                                           NUM_ANIMALS ))][, `:=` (STRAIN_TS = NULL,
                                                                                   STRAIN_TX = NULL,
                                                                                   STRAIN_DM = NULL,
                                                                                   ALL_STRAIN_TS = NULL,
                                                                                   NUM_STRAIN_TS = NULL,
                                                                                   NUM_ANIMALS = NULL)], 
        by=c('STUDYID', 'USUBJID'))
    
    if (InclStrainFilter) {
      # Extract animals matching the strain filter - exclude uncertain rows
      foundAnimalSpeciesStrain <- animalStrain[STRAIN %in% strainFilterX & is.na(STRAIN_UNCERTAIN_MSG),
                                               .(STUDYID, USUBJID, SPECIES, STRAIN)]
      
      if (exclusively) {
        # Find studies with animals having other STRAIN than the requested
        foundAnimalSpeciesStrain <-
          merge(foundAnimalSpeciesStrain,
                # Set of studies to keep:
                fsetdiff(unique(foundAnimalSpeciesStrain[,.(STUDYID)]),
                         # Set of studies (included in the found set of animals with matching STRAIN values) with possible 
                         # STRAIN values not included in the strainFilterX:  
                         unique(fsetdiff(merge(# Set of possible STRAIN values per study in the input set of animals:
                           unique(animalStrain[,.(STUDYID, STRAIN)]),
                           unique(foundAnimalSpeciesStrain[,.(STUDYID)]), by='STUDYID'),
                           unique(foundAnimalSpeciesStrain[,.(STUDYID, STRAIN)]))[,.(STUDYID)])),
                by='STUDYID')
        
      }
    } else 
      # Save all found species/strain rows - drop the STRAIN_UNCERTAIN_MSG column
      foundAnimalSpeciesStrain <- animalStrain
    
    if (inclUncertain) {
      # Try to populate the uncertain animal species rows with a strain value
      # - if only a single strain value has been identified for an animal in the 
      #   joined data extracted from the database, this value is assigned 
      animalSpeciesUncertain <- 
        merge(animalSpecies[!is.na(SPECIES_UNCERTAIN_MSG)], 
              unique(animalSpeciesStrainAll[,.(STUDYID, USUBJID, STRAIN)])[
                , `:=` (NUM_STRAIN = .N), by = c('STUDYID','USUBJID')][
                  NUM_STRAIN == 1, .(STUDYID, USUBJID, STRAIN)],
              by = c('STUDYID','USUBJID'))
      
      # Add uncertain animal species rows to found set of animals
      foundAnimalSpeciesStrain <-
        rbindlist(list( foundAnimalSpeciesStrain, animalSpeciesUncertain),
                  use.names=TRUE, fill=TRUE )
      
      if (InclStrainFilter) { 
        # Add uncertain animal species rows to found set of animals
        foundAnimalSpeciesStrain <-
          rbindlist(list( foundAnimalSpeciesStrain, animalStrain[!is.na(STRAIN_UNCERTAIN_MSG)]),
                    use.names=TRUE, fill=TRUE )
        # merge content of SPECIES_UNCERTAIN_MSG and STRAIN_UNCERTAIN_MSG into UNCERTAIN_MSG
        #  - non-empty messages are separated by ' & '
        #  - A function prefix is included as first part of the non-empty combined texts
        #  - exclude the xx_UNCERTAIN_MSG columns after the merge  
        funcPrefix <- 'FilterSpeciesStrain: '
        foundAnimalSpeciesStrain <- 
          foundAnimalSpeciesStrain[,`:=` (UNCERTAIN_MSG = ifelse(!is.na(SPECIES_UNCERTAIN_MSG) & !is.na(STRAIN_UNCERTAIN_MSG), 
                                                                 paste0(funcPrefix, paste(STRAIN_UNCERTAIN_MSG, SPECIES_UNCERTAIN_MSG, sep=' & ')),
                                                                 ifelse(!is.na(SPECIES_UNCERTAIN_MSG),
                                                                        paste0(funcPrefix, SPECIES_UNCERTAIN_MSG),
                                                                        ifelse(!is.na(STRAIN_UNCERTAIN_MSG),
                                                                               paste0(funcPrefix, STRAIN_UNCERTAIN_MSG),
                                                                               as.character(NA)))))]
        
      } 
    } 
    return(foundAnimalSpeciesStrain)
  }
  
  ##################################################################################################################
  #### Extract potential list of strains from strainFilter for actual species and execute filtering
  ##################################################################################################################
  execOneSpeciesFilter <- function(species) {
    # Extract list of select strains for current species 
    # - remove prefixed species value
    strainList <- str_replace(strainFilter[str_detect(strainFilter, paste0(species,': '))], 
                          paste0(species,': '),'')
    if (length(strainList) == 0) strainList <- NULL
    
    # Execute species/strain filtering for current species/strain(s)
    return(doFiltering(species, strainList))
  }
  
  
  ##################################################################################################################
  
  ##  Evaluate input parameters
  if (!is.data.table(animalList)) {
    stop("animalList must be be specified with a data table")
  } 
  
  if (is.null(speciesFilter) | isTRUE(is.na(speciesFilter)) | isTRUE(speciesFilter=="")) {
    stop("speciesFilter must be specified")
  } 
  
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter inclUncertain must be either TRUE or FALSE")
  }
  
  if (!(exclusively %in% c(TRUE,FALSE))) {
    stop("Parameter Exclusively must be either TRUE or FALSE")
  }
  
  ## Inital preparation of data common for each execution of filtering per species included in speciesFilter
  
  # Get values of code lists SPECIES and STRAINS from CDISC CT
  ctSPECIES<-getCTCodListValues("SPECIES")
  ctSTRAIN<-getCTCodListValues("STRAIN")
  
  # Extract set of on all potential control animals for list of studyid values 
  # included in the input table of animals
  # Join species and strains values at trial level (TS), set level (TX) and 
  # animal level (DM) together for each animal
  #  - ensure all empty SPECIES_xx and STRAIN_xx values are NA
  animalSpeciesStrainAll <-
    genericQuery("select distinct 
                         dm.studyid  as STUDYID, 
                         dm.usubjid   as USUBJID,
                         case ts1.tsval 
                            when '' then null 
                            else ts1.tsval
                         end          as SPECIES_TS, 
                         case ts2.tsval 
                            when '' then null 
                            else ts2.tsval
                         end          as STRAIN_TS, 
                         case tx2.txval 
                            when '' then null 
                            else tx2.txval
                         end          as SPECIES_TX, 
                         case tx3.txval 
                            when '' then null 
                            else tx3.txval
                         end          as STRAIN_TX,
                         case dm.species 
                            when '' then null 
                            else dm.species
                         end          as SPECIES_DM,
                         case dm.strain 
                            when '' then null 
                            else dm.strain
                         end          as STRAIN_DM
                    from dm
                    join (select distinct studyid, setcd
                             from tx
                            where txparmcd = 'TCNTRL'
                              and studyid in (:1))  tx1
                      on dm.studyid = tx1.studyid
                     and dm.setcd = tx1.setcd
                    left join ts                    ts1
                      on ts1.studyid = dm.studyid
                     and ts1.tsparmcd = 'SPECIES'
                    left join ts                    ts2
                      on ts2.studyid = dm.studyid
                     and coalesce(ts2.tsgrpid, '<null>') = coalesce(ts1.tsgrpid, '<null>')
                     and ts1.tsparmcd = 'STRAIN'
                    left join tx                    tx2
                      on tx2.studyid = dm.studyid
                     and tx2.setcd = dm.setcd
                     and tx2.txparmcd = 'SPECIES'
                    left join tx                    tx3
                      on tx3.studyid = dm.studyid
                     and tx3.setcd = dm.setcd
                     and tx3.txparmcd = 'STRAIN'",
                 unique(animalList[,.(STUDYID)]))
  
  # Add variables SPECIES and STRAIN with the first non-empty species/strain 
  # value from (in this order) DM, TX or TS
  animalSpeciesStrainAll[, `:=` (SPECIES = fcoalesce(as.character(SPECIES_DM),
                                                     as.character(SPECIES_TX),
                                                     as.character(SPECIES_TS)),
                                 STRAIN  = fcoalesce(as.character(STRAIN_DM),
                                                     as.character(STRAIN_TX),
                                                     as.character(STRAIN_TS)))]
  
  ## Look into the SPECIES
  
  # Extract unique species data per animals
  animalSpeciesAll <- unique(animalSpeciesStrainAll[,.(STUDYID, USUBJID, 
                                                       SPECIES_TS = as.character(SPECIES_TS),
                                                       SPECIES_TX = as.character(SPECIES_TX), 
                                                       SPECIES_DM = as.character(SPECIES_DM),
                                                       SPECIES = as.character(SPECIES))])
  # Add variables with 
  #  - count of number of distinct SPECIES per study
  #  - concatenation of all species per study (for studies with one species, this is equal to SPECIES_TS)
  studySpecies <-
    unique(unique(animalSpeciesAll[, .(STUDYID, SPECIES_TS)])[
            , `:=` (NUM_SPECIES_TS = .N), by = STUDYID][
            , `:=` (ALL_SPECIES_TS = c(.SD)), by = STUDYID, .SDcols='SPECIES_TS'][
            , .(STUDYID,NUM_SPECIES_TS,ALL_SPECIES_TS )], by='STUDYID')
  # Add calculated columns to the list of animals
  animalSpeciesAll <- merge(animalSpeciesAll, 
                            studySpecies, 
                            by = 'STUDYID')
  
  # Add variable with count of unique USUBJID per study (is expected to be one usubjid per studyid per TSPARMCD 'SPECIES' )
  animalSpeciesAll[, `:=` (NUM_ANIMALS = .N), by = .(STUDYID, USUBJID)]
  
  
  # Identify uncertain animals - add variable SPECIES_UNCERTAIN_MSG
  # - remove temp columns used in the processing and remove duplicates for for multiple SPECIES at study level
  animalSpecies <- 
    unique(
      animalSpeciesAll[,`:=` (SPECIES_UNCERTAIN_MSG=mapply(identifyUncertainSPECIES, 
                                                   SPECIES, 
                                                   SPECIES_TS, 
                                                   SPECIES_TX,
                                                   SPECIES_DM,  
                                                   ALL_SPECIES_TS,
                                                   NUM_SPECIES_TS, 
                                                   NUM_ANIMALS ))][, `:=` (SPECIES_TS = NULL,
                                                                           SPECIES_TX = NULL,
                                                                           SPECIES_DM = NULL,
                                                                           ALL_SPECIES_TS = NULL,
                                                                           NUM_SPECIES_TS = NULL,
                                                                           NUM_ANIMALS = NULL)], 
      by=c('STUDYID', 'USUBJID'))
  
  
  if (length(speciesFilter) == 1) 
    # One species selected - just execute  the filtering of the species/strain
    # and return result
    foundAnimalSpeciesStrain <- doFiltering(speciesFilter, strainFilter)
  else 
    # Multiple species selected - execute filtering for species/strain per species
    # - combine all outputs into one table
    foundAnimalSpeciesStrain <- 
      rbindlist(lapply(speciesFilter, function(species) {execOneSpeciesFilter(species)}), 
                use.names=TRUE, fill=TRUE)
  
  
  ##################################################################################################################
  
  # Handling of the final set of animals to return
  
  # Merge the set of extracted animals with the input set of animals to keep
  # any additional columns from the input table 
  foundAnimals<-merge(foundAnimalSpeciesStrain, animalList, by=c('STUDYID', 'USUBJID'))
  if (inclUncertain)
    if ("UNCERTAIN_MSG.y" %in% names(foundAnimals)) {
      # An UNCERTAIN_MSG column is included in both input and found list of animals
      #  - merge the UNCERTAIN_MSG from each of the merged tables into one column
      #  - non-empty messages are separated by '|'
      #  - exclude the original UNCERTAIN_MSG columns after the merge  
      foundAnimals<-foundAnimals[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & ! is.na(UNCERTAIN_MSG.y), 
                                                              paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep='|'),
                                                              fcoalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][
                                 , `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
  
      # remove xx_UNCERTAIN_MSG columns
      foundAnimals[, `:=` (SPECIES_UNCERTAIN_MSG=NULL, STRAIN_UNCERTAIN_MSG=NULL)]
    }
  
  # Return list of found animals  
  return(foundAnimals)
  
}
