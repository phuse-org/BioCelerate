###################################################################################
# Script name   : initSENDFunctions.R
# Date Created  : 23-Jul-2020
# Programmer    : Bo Larsen
# --------------------------------------------------------------------------------
# Change log: 
# Programmer/date     Description
# -----------------   ------------------------------------------------------------
# <init/dd-Mon-yyyy>  <description>
#
# -------------------------------------------------------------------------------
# Purpose       : Initiate the set of SEND functions 
#
# Description   : - Defines an initiation function to be called for setup the 
#                   database environment and import of CDISC CT
#                 - Multiple types of databases are supported - for each of the
#                    supported db types:
#                     - A metadata record is included describing the relevant
#                       properties for a type of database
#                     - A function to connect to the specific database
#                     - A function to import data for a SEND domain from the 
#                       specific database
#                 - Compiles functions in all modules
#                   
###################################################################################

library(tools)
library(data.table)
library(readxl)
library(magrittr)

###################################################################################
# Function name : initEnvironment
#
# Purpose       : Initialize the environment 
#
# Description   : Defines relevant global variables for
#                   - type of SEND database
#                   - pointer to database (filename, connect string....)
#                   - username/password to database (if relevant)
#                 Import CDISC CT data from specified CT file in Excel format
#                 
# Input         :  n/a
#
# Output        : Global variables:
#                   - GdbHandle - handle to opened db connection
#                   - GdbSchenma - schema name for SEND db tables
#                 Global data tables with imported CDISC CT code lists and values
#                   - CDISCctCodeLists
#                   - CDISCctCodeValues
#
# Parameters    : - dbType
#                   Mandatory - type of database
#                 - dbPath
#                   Mandatory - the pointre to the database (path to file or db name)
#                 - dbUser
#                 - dbPwd
#                   Mandatory if login credendtiales are required for the specific 
#                   db type - The user name and password respectively
#                 - dbSchema
#                   Optional - The table owner of the SEND table in the specific 
#                   database
#                 - ctFile
#                   Mandatory - name (full path) of CDISC CT file in Excel xls 
#                   format to be imported
#
###################################################################################
initEnvironment<-function(dbType=NULL, dbPath=NULL,  dbUser=NULL, dbPwd=NULL, dbSchema=NULL, ctFile=NULL) {
  
  ## Import specified CT file
  importCtFile<-function() {
    # Check file is XLS and exists
    if (! tolower(file_ext(ctFile)) == 'xls')
      stop(paste0('The ctFile ' , ctFile, ' is not an XLS file'))
    if (!file.exists(ctFile)) {
      stop(paste0('The ctFile ' , ctFile, 'could not be found'))
    }
    
    # Import content from worksheet named SEND<sep>Terminology<something> - include relevant columns and all rows
    ctSheets<-excel_sheets(ctFile)
    ctAll<-as.data.table(read_xls(ctFile, sheet=ctSheets[grepl('send[_ ]terminology', 
                                                               tolower(ctSheets) )]))[,c("Code", "Codelist Code", "CDISC Submission Value")]
    setnames(ctAll, c("Codelist Code","CDISC Submission Value"),c("CodelistCode","CDISCSubmissionValue"))
    
    # Extract all CDISC CT code list names
    CDISCctCodeLists<<-ctAll[is.na(CodelistCode), .(CodelistCode=Code, CodeList=CDISCSubmissionValue)]
    
    # Extract all CDISC CT code list values
    CDISCctCodeValues<<-ctAll[!is.na(CodelistCode), .(CodelistCode,CDISCSubmissionValue)]
  }
  
  ## Verify database specification parameters
  
  # dbType
  errMsg<-paste0('Parameter dbType must be one of: ', paste(sapply(GvalidDbTypes[,.(db_type)], paste0), collapse=','))
  if (is.null(dbType) | isTRUE(is.na(dbType)) | isTRUE(dbType==''))
    stop(errMsg)
  else if (nrow(dbProperties<-GvalidDbTypes[db_type==tolower(dbType)]) == 0)
    stop(errMsg)
  GdbType<<-tolower(dbType)
  
  errMsg<-'Parameter %s must be a non-empty string'
  # dbPath
  if (is.null(dbPath) | isTRUE(is.na(dbPath)) | isTRUE(dbPath==''))
    stop(sprintf(errMsg, 'dbPath')) 
  
  # dbUSer and dbPwd
  if (as.logical(dbProperties[,.(req_credentials)])) {
    if (is.null(dbUser) | isTRUE(is.na(dbUser)) | isTRUE(dbUser==''))
      stop(sprintf(errMsg, 'dbUser')) 
    if (is.null(dbPwd) | isTRUE(is.na(dbPwd)) | isTRUE(dbPwd==''))
      stop(sprintf(errMsg,'dbPwd')) 
  }
  
  # dbSchema
  if (!is.null(dbSchema) & isFALSE(is.na(dbSchema)) & isFALSE(dbSchema==''))
    GdbSchema<<-dbSchema
  else
    GdbSchema<<-""
  
  ## Verifying specified CT file - and import
  if (is.null(ctFile) | isTRUE(is.na(ctFile)) | isTRUE(ctFile==''))
    stop(sprintf(errMsg,'ctFile')) 
  else 
    importCtFile()
  
  ## Import package for the specified db type
  library(as.character(dbProperties[,.(package_name)]), character.only=TRUE)
  
  ## Connect to the database 
  ## - execute the function specific for the db type to create the connections
  if (as.logical(dbProperties[,.(req_credentials)]))
    dbInputParams<-paste0('(dbPath, dbUser, dbPwd)')
  else
    dbInputParams<-paste0('(dbPath)')
  GdbHandle<<-eval(parse(text=paste0('connectDB_',dbProperties[,.(db_type)],dbInputParams)))
  
  
  ## Verify existence of function specific for db type to execute a generic query
  if (!exists(paste0('genericQuery_', dbProperties[,.(db_type)])))
    stop(sprintf('A function named %s is missing', disconnectDBName))
  
  ## Assign function specific  for db type to disconnect from db
  disconnectDBName<-paste0('disconnectDB_', dbProperties[,.(db_type)])
  if (exists(disconnectDBName))
    disconnectDB<<-get(disconnectDBName)
  else
    stop(sprintf('A function named %s is missing', doImportDomainName))
}

##############################################################################################
## Valid db types
GvalidDbTypes<-
    data.table(db_type         = c('sqlite',  'oracle'),
               req_credentials = c( FALSE,     TRUE), 
               package_name    = c('RSQLite', 'ROracle'))
# data.table(db_type         = c('sqlite',  'oracle', 'odbc_login', 'odbc_nologin'),
#            req_credentials = c( FALSE,     TRUE,     TRUE,         FALSE), 
#            package_name    = c('RSQLite', 'ROracle','RODBCDBI',   'RODBCDBI'))

##############################################################################################
## Connect function specific for each db type
##############################################################################################

# SQLite
connectDB_sqlite<-function(dbPath) {
  return(RSQLite::dbConnect(RSQLite::SQLite(), dbPath))
}

# Oracle
connectDB_oracle<-function(dbPath, dbUser, dbPwd) {
  return(ROracle::dbConnect(dbDriver("Oracle"), username=dbUser, password=dbPwd, dbname=dbPath))
}

# ODBC with login credentials
# connectDB_odbc_login<-function(dbPath, dbUser, dbPwd) {
#   return(dbConnect(RODBCDBI::ODBC(), dsn=dbPath, user=dbUser, password=dbPwd))
# }

# ODBC without login credentials
# connectDB_odbc_nologin<-function(dbPath) {
#   return(dbConnect(RODBCDBI::ODBC(), dsn=dbPath))
# }

##############################################################################################
## Disconnect function specific for each db type
##############################################################################################

# SQLite
disconnectDB_sqlite<-function() {
  return(RSQLite::dbDisconnect(GdbHandle))
}

# Oracle
disconnectDB_oracle<-function() {
  return(ROracle::dbDisconnect(GdbHandle))
}



##############################################################################################
## Functions to execute a generic query specific for each db 
## Result data set always returned as data table
## NOTE: Does not currently support 'in' functionality in the where clause !
##############################################################################################

## Helper function 
# Take a SQL statement as input 
#   return the statement modified in this way:
#   - substitute lineshifts (\n) with space
#   - substitute multiple consecutive spaces with one space
#   - substitute '==' with '='
#   - if the global defined db schame names is not empty -
#     - add the schema plus '.' in front of all table names , 
#       i.e. names following a 'from ' or 'join ' except 
#       subqueries (starting with '(')
selectStmtAddSchema <- function(stmt) {
  vSchema <- ifelse(GdbSchema=='','',paste0(GdbSchema,'.'))
  return(
    stmt %>%
      str_replace_all('\n', ' ') %>%
      str_replace_all(' +', ' ') %>%
      str_replace_all('=+', '=') %>%
      str_replace_all(regex('(from |join )([^(])', ignore_case = TRUE), 
                      paste0('\\1', vSchema, '\\2'))
  )
}

##
# The function to be called by programs
#  It prepares the given select statement by adding potential schema name 
#  to all tables and execute the genericQuery funtcion specific for the 
#  actual db type
#  ## ADD POSIBILITY FOR MULTIPLE QUERY PARAMS
genericQuery <- function(queryString, queryParams=NULL) {
  get(paste0('genericQuery_', GdbType))(selectStmtAddSchema(queryString),
                                        queryParams)
}

## DB specific functions:
# SQLite
# 
#  ## ADD POSIBILITY FOR MULTIPLE QUERY PARAMS
genericQuery_sqlite<-function(queryString, queryParams) {
 
  if (is.null(queryParams)){
    return(setDT(RSQLite::dbGetQuery(GdbHandle, queryString)))
  } else {
    # Input query parameters are converted to a unnamed list used as bind variable 
    # regardless of the type of input
    return(setDT(RSQLite::dbGetQuery(GdbHandle, queryString, 
                                              list(unname(unlist(list(list(queryParams))))))))
  } 
}

# Oracle
# 
#  ## ADD 
#  # POSIBILITY FOR MULTIPLE QUERY PARAMS
genericQuery_oracle<-function(queryString, queryParams=NULL) {
  # SQL clause to replace 'in (:nn)' clause to make Oracle possible to execute an 'in' statement 
  # using a bind variable with values for the in-list
  inStrReplacement <- "in (select regexp_substr(colval\\1, '[^,]+', 1, column_value) colval\\1
                            from (select :\\1 as colval\\1 from dual) t,
                            table(cast(multiset(select level from dual
                                                connect by level <= regexp_count(colval\\1, ',') + 1
                            ) as sys.odcinumberlist)))"
  # TO BE CHANGED TO JOIN INSTEAD (if it's not an 'not in' or not part of a 'or' construction....): 
  #join ( select regexp_substr(colval1, '[^,]+', 1, column_value) colval\\1 
  #                          from (select :\\1 as colval\\1 
  #                                  from dual) t, 
  #                                             table(cast(multiset(select level 
  #                                                                   from dual 
  #                                                                   connect by level <= regexp_count(colval\\1, ',') + 1 ) as sys.odcinumberlist ) )) bindtab\\1
  #    on bindtab\\1.colval\\1 = <column referenced in clause>;
  
  if (is.null(queryParams)){
    cur <- ROracle::dbSendQuery(GdbHandle, queryString)
  } else {
    # Check if statements contains in statements
    if (grepl('in \\(:\\d+\\)', queryString, ignore.case = TRUE)) {
      # Replace the in statement with correct Oracle syntax
      stmt <- str_replace_all(queryString, 
                              regex('in \\(:(\\d+)\\)', ignore_case = TRUE), 
                              inStrReplacement)
      print(stmt)
      # Ensure in the in-list is a comma separated string
      bindVarVal <- paste0(unlist(list(queryParams)), collapse=',')
    }
    else {
      stmt <- queryString
      bindVarVal <- unname(unlist(list(queryParams)))
    }
    
    # Create a data frame with the bind var value included for each 
    # instance of bind var reference in statement
    # TO BE USED WHEN SUPPORT FOR MULTIPLE QUERY PARAMS IS ADDED:
    #      sort(unique(str_extract_all(stmt, ':\\d*')[[1]]))
    n <- 0
    for (v in str_extract_all(stmt, ':\\d+')[[1]]) {
      n <- n + 1
      if (n==1) 
        bindVarDF <- data.frame(bindVarVal)
      else 
        bindVarDF <- cbind(bindVarDF, bindVarVal)
    }
    cur <- ROracle::dbSendQuery(GdbHandle, stmt, bindVarDF)
  }
  
  # Fetch all rows, clear buffer and return data
  queryResult<-setDT(ROracle::fetch(cur))
  ROracle::dbClearResult(cur)
  
  return(queryResult) 
}

###################################################################################

# define the path to R scripts to actual script location
dummyuseCaseQuestionMiFindings<-function() {
  # dummy function only to be used to get this script's location
}
setwd(getSrcDirectory(dummyuseCaseQuestionMiFindings))


# Compiling functions included in all modules
source("miscFunctions.R")
source("importSENDDomains.R")
source("studyListStudyDesign.R")
source("filterStudyAnimalSpeciesStrain.R")
source("filterStudyAnimalRoute.R")
source("studyListStudyStartDate.R")
source("animalListControl.R")
source("filterAnimalsSex.R")
source("subjDataExtract.R")
source("filterFindingsPhase.R")
source("addFindingsAnimalAge.R")
source("filterFindingsAnimalAge.R")
