###################################################################################
# Script name   : studyListStudyStartDate.R
# Date Created  : 25-Mar-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Extract a list of SEND study ids the study start date a specified 
#                 study design.
#
# Description   : Contains function GetStudyListSTSTDTC:
#                 Returns a data table with the list of study ids from TS 
#                 where the value of TSVAL for the TSPARMCD 'STSTDTC' falls within 
#                 a specified start/end date interval in IS8601 format (input 
#                 parameters 'fromDTC'/'toDTC').
#                 Both complete and incomplete input start/end dates can be handled. 
#                 If only a year is specified - the date set to the first of 
#                 January that year.
#                 If only a year and month is specified - the date set to the 
#                 first day in that month.
#                 If a time part is included in a specified input starte/end date, 
#                 it is ignored.
#                 If both a start and end input date are specified - all the STUDYID
#                 values from TS where TSVAL for TSPARMCD 'STSTDTC' is with the 
#                 interval of the specifed start/end date interval are extracted and 
#                 returned - including the values equal to the start/end dates. 
#                 are included.
#                 If only a start input date is specified - all the STUDYID values 
#                 from TS where TSVAL for TSPARMCD 'STSTDTC' is equal to or later than 
#                 the input date are extracted and returned.
#                 If only an end date is specified - all the STUDYID values from TS 
#                 where TSVAL for TSPARMCD 'STSTDTC' is equal to or earlier than the  
#                 are date are extracted and returned.
#                 If specified (imput parameter 'inclUncertain') - studyid values
#                 included in TS 
#                   a) without any row for TSPARMCD='STSTDTC' or
#                   b) with an invalid ISO8601 date format included in TSVAL for 
#                      TSPARMCD='STSTDTC'
#                 are extracted and returned too - including af message tellinge 
#                 whether reason is a) or b).
#
# Input         : The TS domain - is imported from the pooled SEND data store if 
#                 it doesn't exist in workspace
#
# Output        : A data table with columns:
#                     STUDYID       (character)
#                     STSTDTC       (character - ISO8601 format) 
#                     UNCERTAIN_MSG (character)
#                       Only included when parameter inclUncertain=TRUE 
#                       Contains indication of whether STSTDTC is missing of has wrong 
#                       format - is NA for rows where STSTDTC is valid.
#                     Additional columns contained in the studyList table (if specified) 
#
# Parameters    : The function GetStudyListSTSTDTC are defined with these input parameters:
#                   fromDTC:  Optional (either or both of fromDTC and toDTC must be filled).
#                             The start of interval for date interval to extract.
#                             Must be in ISO8601 interval.
#                   fromDTC:  Optional (either or both of fromDTC and toDTC must be filled).
#                             The end of interval for date interval to extract.
#                             Must be in ISO8601 interval.
#                   studyList:
#                             Optional, a data table with a list of studies to 
#                             limit the output to be within this set of studies
#                   inclUncertain:
#                             Optional, boolean (TRUE or FALSE), default: FALSE 
#                             Indicates whether study ids STSTDTC is missing or wrong 
#                             shall be included or not in the output data table
#
#######################################################################################################################################################################

library(data.table)
library(parsedate)
library(DescTools)

GetStudyListSTSTDTC<-function(fromDTC=NULL, toDTC=NULL, studyList=NULL, inclUncertain=FALSE) {
  # Evaluate input parameters
  if ((is.null(fromDTC) | isTRUE(is.na(fromDTC)) | isTRUE(fromDTC=="")) & (is.null(toDTC) | isTRUE(is.na(toDTC)) | isTRUE(toDTC==""))) {
    stop("A start and/or an end date must be specified")
  } 
  
  if ((!(is.null(fromDTC) | isTRUE(is.na(fromDTC)) | isTRUE(fromDTC=="")) & isTRUE(is.na(parse_iso_8601(fromDTC)))) | (!(is.null(toDTC) | isTRUE(is.na(toDTC)) | isTRUE(toDTC=="")) & isTRUE(is.na(parse_iso_8601(toDTC))))) {
    stop("The value(s) specified for fromDTC and/or toDTC is not a valid ISO8601 date")
  }
  
  if (!(inclUncertain %in% c(TRUE,FALSE))) {
    stop("Parameter Exclusively must be either TRUE or FALSE")
  }
  
  studyListIncl<-FALSE
  if (!(is.null(studyList) | isTRUE(is.na(studyList)) | isTRUE(studyList==''))) {
    # An initial list of studies is included
    studyListIncl<-TRUE
  }
  
  if (!exists("TS")) {
    # import TS if it's not already exists
    importSENDDomains(c("TS"))
  }
  
  # Extract all TS rows for parameter STSTDTC 
  #  - rename TSVAL to STSTDTC
  tsSTSTDTC<-TS[TSPARMCD == 'STSTDTC', .(STUDYID, STSTDTC = TSVAL)]
  if (inclUncertain) {
    #Include all studies with no STSTDTC parameter included
    tsSTSTDTCMiss<-fsetdiff(unique(TS[,.(STUDYID)]), tsSTSTDTC[,.(STUDYID)])
    if (nrow(tsSTSTDTCMiss) > 0) {
      tsSTSTDTC<-rbindlist(list(tsSTSTDTC, tsSTSTDTCMiss[,.(STUDYID, STSTDTC = NA)]))
    }
  }
  if (studyListIncl) {
    # Limit to the set of studies given as input
    #tsSTSTDTC<-merge(tsSTSTDTC, studyList, by='STUDYID') 
    tsSTSTDTC<-merge(tsSTSTDTC, studyList[,.(STUDYID)], by='STUDYID')
  } 
  
  # Construct the statement to apply the specified date interval
  dtcFilter<-NA
  if (!(is.null(fromDTC) | isTRUE(is.na(parse_iso_8601(fromDTC))))) {
    # The filter condition for the fromDTC
    dtcFilter<-"as.Date(parse_iso_8601(STSTDTC)) >= as.Date(parse_iso_8601(fromDTC))"
  }
  if (!(is.null(toDTC) | isTRUE(is.na(parse_iso_8601(toDTC))))) {
    # Check the granularity if the specified toDTC and a 1 year/month/day to end of the interval to extract
    if (nchar(toDTC)==4) {
      # Only year has been specified + add 1 year to the date
      toDTCdate<-AddMonths(as.Date(parse_iso_8601(toDTC)),12)
    }
    else if (nchar(toDTC)==7) {
      # Only year and month has been specified -  add 1 month to the date
      toDTCdate<-AddMonths(as.Date(parse_iso_8601(toDTC)),1)
    }
    else {
      # A full date has been specified - add one day
      toDTCdate<-as.Date(parse_iso_8601(toDTC))+1
    }
    if (is.na(dtcFilter)) {
      # only toDTC filter part
      dtcFilter<-"as.Date(parse_iso_8601(STSTDTC)) < toDTCdate"
    }
    else {
      # Add this filter part to the frodmDTC filter part
      dtcFilter<-paste(dtcFilter," & as.Date(parse_iso_8601(STSTDTC)) < toDTCdate",sep="")
    }
  }
  if (inclUncertain) {
    ## Include uncertain studies
    
    # Include condition for rows with empty or wrong value of STSTDTC 
    dtcFilter<-paste(paste("(", dtcFilter), ") | is.na(parse_iso_8601(STSTDTC))")
    
    # Build the statement to execute - include column indication of missing or wrong value
    stmt=paste(paste("tsSTSTDTC[", dtcFilter, sep=""),
               ",.(STUDYID, STSTDTC, UNCERTAIN_MSG=ifelse(is.na(parse_iso_8601(STSTDTC)),
                                                          ifelse(is.na(STSTDTC), 
                                                                 'GetStudyListSTSTDTC: TS parameter STSTDTC is missing', 
                                                                 'GetStudyListSTSTDTC: TS parameter STSTDTC has wrong format'),
                                                          NA))]", 
               sep="")
    # Execute statement to extract studies fulfilling the condition(s) plus uncertain studies
    foundStudies<-eval(parse(text=stmt))
    
    if (studyListIncl) {
      # Merge the list of extracted studies with the input set of studies to keep
      # any additional columns from the input table 
      foundStudies<-merge(foundStudies, studyList, by='STUDYID')
      if ("UNCERTAIN_MSG.y" %in% names(foundStudies)) {
        # The studyList table contains alread an UNCERTAIN_MSG column
        #  - merge the UNCERTAIN_MSG from each of the merged tables into one column
        #  - non-empty messages are separated by '|'
        #  - exclude the original studyList.UNCERTAIN_MSG after the merge  
        foundStudies<-foundStudies[,`:=` (UNCERTAIN_MSG=ifelse(!is.na(UNCERTAIN_MSG.x) & !is.na(UNCERTAIN_MSG.y), 
                                                               paste(UNCERTAIN_MSG.y, UNCERTAIN_MSG.x, sep='|'),
                                                               Coalesce(UNCERTAIN_MSG.x, UNCERTAIN_MSG.y)))][, `:=` (UNCERTAIN_MSG.x=NULL,UNCERTAIN_MSG.y=NULL)]
      }
    }
    # Return the list of extracted studies
    return(foundStudies)
  }
  else {
    ## Do not include uncertain studies
    
    # Build the statement to extract studies fulfilling the condition(s) and execute
    foundStudies<-eval(parse(text=paste(paste("tsSTSTDTC[", 
                                              dtcFilter, 
                                              sep=""),
                                        ",.(STUDYID, STSTDTC)]", 
                                        sep="")))
    
    if (studyListIncl) {
      # Return the list of extracted studies merged with the input set of studies to keep
      # any additional columns from the input table 
      return(merge(foundStudies, studyList, by='STUDYID'))
    }
    else {
      # Return the list of extracted studies
      return(foundStudies)
    }
  }
}




