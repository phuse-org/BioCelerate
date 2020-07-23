#
# Just a simple CLI to create the sysParameters.R file for a user
#
#
# Should be run from the command line like 
# RScript config.R

library(tools)
require(stringi)

askPrompt <- "Please enter the folder containing the SEND datasets: "
studyRoot <- file.path(readline(prompt=askPrompt))

while (!(file.exists(studyRoot))) {
  
  checkPrompt <- paste(sprintf("Cannot find folder: %s\n", studyRoot), askPrompt)
  studyRoot <- file.path(readline(prompt=checkPrompt))
}

askPrompt <- "Please enter the folder where you would like to store resulting SEND SQLite database: "
dbFolder <- file.path(readline(prompt=askPrompt))

while (!(file.exists(dbFolder))) {
  
  checkPrompt <- paste(sprintf("Cannot find folder: %s\n", dbFolder), askPrompt)
  dbFolder <- file.path(readline(prompt=checkPrompt))
}

scriptContents <- sprintf("# sysParameters.R created by config.R

library(tools)


studyRoot <- file.path('%s')
metadataRoot <- file.path(file_path_as_absolute('.'), 'metadata')

# SEND data base name and location
dbFullName <- file.path('%s', 'send.db')

", stri_escape_unicode(studyRoot), stri_escape_unicode(dbFolder))



fileConn <- file(file.path(file_path_as_absolute("."), 'sysParameters.R'))
writeLines(c(scriptContents), fileConn)
close(fileConn)
