#This R File Makes an App that allows for visualization of cross-study analysis
#of SEND Data

#Need to do:
#* Add Detailed Scoring Controls (i.e. user can determine scoring ranges)
#* Ability to load different studies
#* 
#Packages
library(cowplot)
library(dplyr)
library(devtools)
library(fmsb)
library(farver)
library(forcats)
library(gridExtra)
library(gridGraphics)
library(ggplot2)
library(ggpattern)
library(Hmisc)
library(httr)
library(ini)
library(magrittr)
library(openxlsx)
library(readxl)
library(reshape2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyTree)
library(scales)
library(tidyverse)
library(tools)
library(this.path)
library(textshaping)


#Set File Path to Biocelerate Data
homePath <- dirname(this.path())
setwd(homePath)

##### Values Set for Data Load ####
dataSource <- "BioCelerate"
dataPaths <- read.ini('dataPaths - Template.ini')
paths <- as.character(unlist(dataPaths[[dataSource]]))

#Loading Initial Datasets
summaryResults <- list()
CompileDataSummary <- list()
BodyWeightSummary <- list()
FWDataSummary <- list()
defaultVal <- 350
SEX <- 'M'

#Load Relevant Functions
source('Functions/Functions.R')
source('Functions/groupSEND.R')
source('Functions/makeRadar.R')
source('Functions/makeMIPlot.R')
source('Functions/makeLBPlot.R')
source('Functions/makeOMPlot.R')
source('Functions/makeBWPlot.R')
source('Functions/makeFWPlot.R')

#Standardizing Terminology
MITESTCDlist <- list('LIVER' = c('LIVER'),
                     'KIDNEY' = c('KIDNEY'),
                     'HEMATOPOIETIC' = c('BONE MARROW', 'SPLEEN', 'THYMUS', 'LYMPH NODE, MESENTERIC', 'LYMPH NODE, MANDIBULAR'),
                     'ENDOCRINE' = c('GLAND, THYROID', 'GLAND, ADRENAL', 'GLAND, PITUITARY',
                                                        'GLAND, PARATHYROID', 'PANCREAS'),
                     'REPRODUCTIVE' = c('CERVIX','EPIDIDYMIS','GLAND, PROSTATE','GLAND, MAMMARY',
                                                      'OVARY','PROSTATE','TESTIS','UTERUS','VAGINA'))
OMTESTCDlist <- MITESTCDlist
organTESTCDlist <- list('LIVER' = c('SERUM | ALT',
                                    'SERUM | AST',
                                    'SERUM | ALP',
                                    'SERUM | GGT',
                                    'SERUM | BILI',
                                    'SERUM | ALB'),
                        'KIDNEY' = c('SERUM | CREAT',
                                     'SERUM | UREAN',
                                     'SERUM | ALB',
                                     'SERUM | CL',
                                     'SERUM | K',
                                     'SERUM | PHOS',
                                     'SERUM | SODIUM',
                                     'URINE | CL',
                                     'URINE | K',
                                     'URINE | SODIUM',
                                     'URINE | GLUC',
                                     'URINE | SPGRAV',
                                     'URINE | VOLUME',
                                     'URINE | PROT',
                                     'URINE | UROBIL'),
                        'HEMATOPOIETIC' = c( 'WHOLE BLOOD | RBC',
                                             'WHOLE BLOOD | HCT',
                                             'WHOLE BLOOD | MCHC',
                                             'WHOLE BLOOD | MCH',
                                             'WHOLE BLOOD | MCV',
                                             'WHOLE BLOOD | RDW',
                                             'WHOLE BLOOD | WBC',
                                             'WHOLE BLOOD | MONO',
                                             'WHOLE BLOOD | BASO',
                                             'WHOLE BLOOD | EOS',
                                             'WHOLE BLOOD | LYM',
                                             'WHOLE BLOOD | PLAT',
                                             'WHOLE BLOOD | MPV'),
                        'ENDOCRINE' = c('URINE | CL',
                                        'URINE | K',
                                        'URINE | SODIUM',
                                        'URINE | GLUC',
                                        'URINE | SPGRAV',
                                        'URINE | VOLUME',
                                        'URINE | PROT'),
                        'REPRODUCTIVE' = c('SERUM | GNRH',
                                           'SERUM | LH',
                                           'SERUM | FSH',
                                           'SERUM | DHT',
                                           'SERUM | DOXMTST',
                                           'SERUM | FAI',
                                           'SERUM | MTESTOS',
                                           'SERUM | NANDRLN',
                                           'SERUM | TESTOS',
                                           'SERUM | TESTOSBA',
                                           'SERUM | TESTOSFR',
                                           'SERUM | TESTOSWB',
                                           'SERUM | TSTFTSTT',
                                           'SERUM | TSTFWTST',
                                           'SERUM | TST4OH',
                                           'SERUM | ESTROGEN'))
doseRanks <- c('Vehicle', 'LD', 'MD', 'HD')

############################## UI #############################################
ui <- dashboardPage (
  dashboardHeader(title="BioCelerate: Cross-Study Analysis", titleWidth = 350),
  dashboardSidebar(
    tags$head(tags$style(type = "text/css", "
              #loadmessage {
                position: relative;
                top: 0px;
                left: 0px;
                width: 100%;
                padding: 5px 0px 5px 0px;
                text-align: center;
                font-weight: bold;
                font-size: 100%;
                color: #000000;
                background-color: #eef66c;
                z-index: 1000;
              }
                ")),
    
    actionBttn("PLOT", label = "Generate Visuals", color = "primary",
               style = "jelly"),
    sidebarMenu(id='sidebar', 
                checkboxGroupInput('studies', 'Study Selection:',
                                   c('Dog Compound A' = "Dog 6576", 
                                     'Dog Compound B' = "Dog 5492",
                                     'Rat Compound A' = "Rat 6576",
                                     'Rat Compound B' = "Rat 5492"), 
                                   selected = c("Dog 5492","Dog 6576", "Rat 5492", "Rat 6576")),
                radioButtons("sex", "Sex Selection:",
                             c("Male" = "M",
                               "Female" = "F",
                               "Male/Female" = "M/F"), selected = 'M'),
                
                radioButtons(inputId = "dose", label = "Dose Selection:",
                             c("HD" = "HD", 
                               "MD" = "MD",
                               "LD" = "LD"), selected = 'HD'),
                menuItem('Detailed Control', startExpanded = TRUE,
                         h5('Detailed Test Selection:')
                         ,shinyTree("tree",checkbox = TRUE),
                         pickerInput(inputId = "AGGMethod",
                                     label = "Radar Aggregation Method",
                                     c('mean', 'animalMax','endpointMax'),
                                     selected = 'animalMax'),
                         pickerInput(inputId = 'bwMethod',
                                     label = "BW Method",
                                     c("BW","TERMBW"), selected = 'BW'),
                         pickerInput(inputId = 'omMethod',
                                     label = "OM Ratio Method", 
                                     c("BrainRatio", "BWRatio", "Organ"), selected = 'BrainRatio'))
         )
    ),

  dashboardBody(
    conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                     tags$div("Loading...", id="loadmessage")
    ),
    tabsetPanel(
      tabPanel('Overall',
               uiOutput('ReactSummaryRadar')),
      tabPanel('Body Weight',
               column(width = 5,pickerInput(inputId = 'bwMetric',
                           label = "BW Metric", 
                           c("zScore","PercentChange"), selected = 'PercentChange')),
               column(width = 5,pickerInput(inputId = "FWTime",
                           label = "FW Time Duration",
                           c("Week","Day"), selected = "Week")),
               column(width = 10,plotOutput('FWplot')),
               column(width = 10,plotOutput('BWplot'))
      ),
      tabPanel('Kidney',
               tabsetPanel(
                 tabPanel('Overall',
                          uiOutput('ReactKidneyRadar')),
                 tabPanel('Clinical Chemistry',
                          plotOutput('KSERLBplot')),
                 tabPanel('Urinalysis',
                          plotOutput('KURILBplot')),
                 tabPanel('Organ Weights',
                          plotOutput('OMplot')),
                 tabPanel('Histopathology',
                          column(width = 5,pickerInput(inputId = 'KMIClustY',
                                      label = "Cluster the Y Axis?", 
                                      c("NO","YES"), selected = 'NO')),
                          column(width = 5,pickerInput(inputId = 'KMIClustX',
                                      label = "Cluster the X Axis?", 
                                      c("NO","YES"), selected = 'NO')),
                          column(width = 10,uiOutput('KMIplotreactive')))
               )
      ),
      tabPanel('Liver',
               tabsetPanel(
                 tabPanel('Overall',
                          uiOutput('ReactLiverRadar')),
                 tabPanel('Clincal Chemistry',
                          plotOutput('LSERLBplot')),
                 tabPanel('Organ Weights',
                          plotOutput('LOMplot')),
                 tabPanel('Histopathology',
                          column(width = 5,pickerInput(inputId = 'LMIClustY',
                                                       label = "Cluster the Y Axis?", 
                                                       c("NO","YES"), selected = 'NO')),
                          column(width = 5,pickerInput(inputId = 'LMIClustX',
                                                       label = "Cluster the X Axis?", 
                                                       c("NO","YES"), selected = 'NO')),
                          column(width = 10, uiOutput('LMIplotreactive')))),
      ),
      tabPanel('Hematopoietic',
               tabsetPanel(
                 tabPanel('Overall',
                          uiOutput('ReactHemaRadar')),
                 tabPanel('Hematology',
                          plotOutput('HHEMELBplot')),
                 tabPanel('Organ Weights',
                          plotOutput('HOMplot')),
                 tabPanel('Histopathology',
                          column(width = 5,pickerInput(inputId = 'HMIClustY',
                                                       label = "Cluster the Y Axis?", 
                                                       c("NO","YES"), selected = 'NO')),
                          column(width = 5,pickerInput(inputId = 'HMIClustX',
                                                       label = "Cluster the X Axis?", 
                                                       c("NO","YES"), selected = 'NO')),
                          column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                               uiOutput('HMIplotreactive'), 
                                   uiOutput('HMIplotreactive2')))),
                          column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                               uiOutput('HMIplotreactive4'), 
                                                               uiOutput('HMIplotreactive5')))),
                          column(width=10,fluidRow(uiOutput('HMIplotreactive3'))))
               )),
      tabPanel('Endocrine',
               tabsetPanel(
                  tabPanel('Overall',
                           uiOutput('ReactEndoRadar')),
                  tabPanel('Clinical Chemistry',
                           plotOutput('ESERLBplot')),
                  tabPanel('Organ Weights',
                           plotOutput('EOMplot',height = 600)),
                  tabPanel('Histopathology',
                           column(width = 5,pickerInput(inputId = 'EMIClustY',
                                                        label = "Cluster the Y Axis?", 
                                                        c("NO","YES"), selected = 'NO')),
                           column(width = 5,pickerInput(inputId = 'EMIClustX',
                                                        label = "Cluster the X Axis?", 
                                                        c("NO","YES"), selected = 'NO')),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('EMIplotreactive'),
                                                uiOutput('EMIplotreactive2')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('EMIplotreactive3'),
                                                uiOutput('EMIplotreactive4')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('EMIplotreactive5'))))
                  ))),
      tabPanel('Reproductive',
               tabsetPanel(
                  tabPanel('Overall',
                           uiOutput('ReactReproRadar')),
                  tabPanel('Organ Weights',
                           plotOutput('ROMplot',height = 600)),
                  tabPanel('Histopathology',
                           column(width = 5,pickerInput(inputId = 'RMIClustY',
                                                        label = "Cluster the Y Axis?", 
                                                        c("NO","YES"), selected = 'NO')),
                           column(width = 5,pickerInput(inputId = 'RMIClustX',
                                                        label = "Cluster the X Axis?", 
                                                        c("NO","YES"), selected = 'NO')),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('RMIplotreactive'),
                                                uiOutput('RMIplotreactive2')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('RMIplotreactive3'),
                                                uiOutput('RMIplotreactive4')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('RMIplotreactive5'),
                                                uiOutput('RMIplotreactive6')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('RMIplotreactive7'),
                                                uiOutput('RMIplotreactive8')))))
               ))
    )
  ),
)

############################### Server #######################################
server <- shinyServer(function(input, output, session) {
   
   plotHeight <- reactiveValues(X=defaultVal)
   numSEX <- reactiveValues(X = 1)
   
   observeEvent(input$PLOT,{
      #Remake the plot values based on User Selection
      ####Controllable Variables###############
      #Studies Chosen 
      if (length(input$studies)<2){
         showNotification("Two Datasets Must be selected", type = "error")
      } else {
         chosenstudies <- input$studies
         #organSystems for grouping of tests
         organSystems <- c('LIVER', 'KIDNEY', 'HEMATOPOIETIC','ENDOCRINE','REPRODUCTIVE')
         
         #Find Selected Tests from shinyTree ouput
         TreeSelect <- input$tree
         SelectedDomainTests <- names(unlist(get_selected(input$tree, format = "slices")))
         #Check Which Domains are Selected and update organSystems
         systemsselected <- toupper(unlist(strsplit(SelectedDomainTests,"[.]"))) 
         organSystems <- toupper(unique(systemsselected[which(systemsselected %in% organSystems)]))
         #Convert Selected Tests to usable format
         SelectedLBTests <- SelectedDomainTests[which(grepl("Laboratory",SelectedDomainTests) == TRUE)]
         SelectedOMTests <- SelectedDomainTests[which(grepl("Weight",SelectedDomainTests) == TRUE)]
         SelectedMITests <- SelectedDomainTests[which(grepl("Histopathology",SelectedDomainTests) == TRUE)]
         #Remove all but Lowest Levels of Tree (Values which have 3 '.')
         SelectedLBTests <- SelectedLBTests[which(str_count(SelectedLBTests, "[.]") == 3)]
         SelectedMITests <- SelectedMITests[which(str_count(SelectedMITests, "[.]") == 2)]
         SelectedOMTests <- SelectedOMTests[which(str_count(SelectedOMTests, "[.]") == 2)]
         SelectedLBTests <- as.data.frame(strsplit(SelectedLBTests,"[.]"))
         SelectedMITests <- as.data.frame(strsplit(SelectedMITests,"[.]"))
         SelectedOMTests <- as.data.frame(strsplit(SelectedOMTests,"[.]"))
         SelectedLBTests <- SelectedLBTests[c(1,4),]
         SelectedMITests <- SelectedMITests[c(1,3),]
         SelectedOMTests <- SelectedOMTests[c(1,3),]
         SelectedLBTests <- t(SelectedLBTests)
         SelectedOMTests <- t(SelectedOMTests)
         SelectedMITests <- t(SelectedMITests)
         rownames(SelectedLBTests) <- NULL
         rownames(SelectedOMTests) <- NULL
         rownames(SelectedMITests) <- NULL
         #Update organTESTCDlist by separating by organsystem 
         for (organSystem in organSystems){
            #Limit to organ System
            Tests <- SelectedLBTests[which(toupper(SelectedLBTests[,1]) %in% organSystem),2]
            MITESTS <- SelectedMITests[which(toupper(SelectedMITests[,1]) %in% organSystem),2]
            OMTESTS <- SelectedOMTests[which(toupper(SelectedOMTests[,1]) %in% organSystem),2]
            #update relavant organTESTCDlist
            organTESTCDlist[[organSystem]] <- Tests
            ## Update MITESTCDlist FOR OM AND MI using dataframes made above
            MITESTCDlist[[organSystem]] <- as.character(MITESTS)
            OMTESTCDlist[[organSystem]] <- as.character(OMTESTS)
         }
         #Specific Error Catch for BioCelerate Studies
         if (length(MITESTCDlist$HEMATOPOIETIC) == 1 & any(MITESTCDlist$HEMATOPOIETIC == "BONE MARROW")){
            showNotification("Dog 5492 Does not Have Bone Marrow MI. Need to add another MI Selection to HEMATOPOIETIC", type = "error")
            return(NULL)
            stop()
         }
         
         #End point aggregation Method (for Radar): 'mean', 'animalMax', or 'endpointMax'
         aggregationMethod <- input$AGGMethod
         
         #BW Control Variables
         BWMethod <- input$bwMethod
         
         #OM Metric: BrainRatio, BWRatio, or Organ
         OMMetric <- input$omMethod
         
         SEX <- input$sex
         if (SEX == "M/F"){
            SEX <- c("M","F")
            plotHeight$X <- 700
            numSEX$X <- 2
         } else {
            plotHeight$X <- 350
            numSEX$X <- 1
         }
         #Check that Gender and Detailed Test Work
         if ('M' %in% SEX & 'REPRODUCTIVE' %in% organSystems){
            if (any(c('EPIDIDYMIS','GLAND, PROSTATE','PROSTATE','TESTIS') %in% c(MITESTCDlist$REPRODUCTIVE,OMTESTCDlist$REPRODUCTIVE)) == FALSE){
               showNotification("REPRODUCTIVE must include MI/OM from Sex Selected", type = "error")
               return(NULL)
               stop()
      } 
   } 
   if ('F' %in% SEX & 'REPRODUCTIVE' %in% organSystems) {
      if (any(c('CERVIX','GLAND, MAMMARY','OVARY','UTERUS','VAGINA') %in% c(MITESTCDlist$REPRODUCTIVE,OMTESTCDlist$REPRODUCTIVE)== FALSE)){
         showNotification("REPRODUCTIVE must include MI/OM from Sex Selected", type = "error")
         return(NULL)
         stop()
      } 
   } 
   #Select Dose Option for Visualization: 'Vehicle', 'LD','MD','HD
   Dose <- input$dose
  
   #Clear RDS Data
   summaryResults <- list()
   MIresults <- list()
   LBresults <- list()
   OMresults <- list()
   summaryData <- data.frame()
  ##### Load in Data ##########
   #Load Data Pertaining to all organSystems
   #Dog Datasets
   Dog5492 <- load.xpt.files(path = paste(homePath, paths[1], sep = '/'),
                             showProgress = F)
   Dog6576<- load.xpt.files(path = paste(homePath, paths[2], sep = '/'),
                            showProgress = F)
   #Rat Datasets
   Rat5492 <- load.xpt.files(path = paste(homePath, paths[3], sep = '/'),
                             showProgress = F) 
   Rat6576 <- load.xpt.files(path = paste(homePath, paths[4], sep = '/'),
                             showProgress = F) 
   for (Gender in SEX){
     #Create Shared Data Frame with StudyID, Species, USUBJID, SEX, and DOSES     
     Dog5492_dm <- data.frame(StudyID = rep("Dog 5492", length(unique(Dog5492$dm$USUBJID))),
                              Species = rep("Dog", length(unique(Dog5492$dm$USUBJID))),
                              USUBJID = Dog5492$dm$USUBJID, SEX = Dog5492$dm$SEX, ARMCD = Dog5492$dm$ARMCD)
     Dog6576_dm <- data.frame(StudyID = rep("Dog 6576", length(unique(Dog6576$dm$USUBJID))),
                              Species = rep("Dog", length(unique(Dog6576$dm$USUBJID))),
                              USUBJID = Dog6576$dm$USUBJID, SEX = Dog6576$dm$SEX, ARMCD = Dog6576$dm$ARMCD) 
     Rat5492_dm <- data.frame(StudyID = rep("Rat 5492", length(unique(Rat5492$dm$USUBJID))),
                              Species = rep("Rat", length(unique(Rat5492$dm$USUBJID))),
                              USUBJID = Rat5492$dm$USUBJID, SEX = Rat5492$dm$SEX, ARMCD = Rat5492$dm$ARMCD) 
     Rat6576_dm <- data.frame(StudyID = rep("Rat 6576", length(unique(Rat6576$dm$USUBJID))),
                              Species = rep("Rat", length(unique(Rat6576$dm$USUBJID))),
                              USUBJID = Rat6576$dm$USUBJID, SEX = Rat6576$dm$SEX, ARMCD = Rat6576$dm$ARMCD) 
     CompileData <- rbind(Dog5492_dm, Dog6576_dm, Rat6576_dm, Rat5492_dm)

     
     #Pull out PP -TK Animals From Analysis
     #Find Pool ID and Correlate to USUBJID
       Rat6576TKPools <- unique(Rat6576$pp$POOLID)
       Rat6576TKIndv <- Rat6576$pooldef$USUBJID[which(Rat6576$pooldef$POOLID %in% Rat6576TKPools)]
       Rat5492TKPools <- unique(Rat5492$pp$POOLID)
       Rat5492TKIndv <- Rat5492$pooldef$USUBJID[which(Rat5492$pooldef$POOLID %in% Rat5492TKPools)] 
       CompileData <- CompileData[!(CompileData$USUBJID %in% c(Rat5492TKIndv,Rat6576TKIndv)),]
     
     #Remove Recovery Animals and Recode Treatment Ranks
     AllData <- CompileData
     CompileData <- CompileData[!str_detect(CompileData$ARMCD, "R"),]
     CompileData$ARMCD <- factor(CompileData$ARMCD)
     levels(CompileData$ARMCD) <- doseRanks
     if (Gender == 'M') {
       CompileData <- CompileData[which(CompileData$SEX == Gender),]
     } else if (Gender == 'F') {
       CompileData <- CompileData[which(CompileData$SEX == Gender),]
     }
     #BW Load
     #Find Terminal and Initial BW for all studies
     #Make InitialWeight Formatting Data Frame
     InitialWeight <- Dog6576$bw[which((Dog6576$bw$VISITDY <= 1)), c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")]
     InitialWeight <- rbind(InitialWeight, Dog5492$bw[which((Dog5492$bw$VISITDY <= 1)), c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")],
                            Rat5492$bw[which(Rat5492$bw$VISITDY <=1),c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")],
                            Rat6576$bw[which(Rat6576$bw$VISITDY <=1),c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")])
     InitialWeight <- InitialWeight[!(InitialWeight$USUBJID %in% c(Rat5492TKIndv,Rat6576TKIndv)),]
     
     BodyWeight <- Dog6576$bw[which(Dog6576$bw$BWTESTCD == "BW"), c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")]
     BodyWeight <- rbind(BodyWeight, Dog5492$bw[which(Dog5492$bw$BWTESTCD == "BW") , c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")],
                         Rat5492$bw[which(Rat5492$bw$BWTESTCD == "BW"),c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")],
                         Rat6576$bw[(Rat6576$bw$BWTESTCD == "BW"),c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")])
     if (BWMethod == "TERMBW"){
       TermBodyWeight <- Dog6576$bw[which(Dog6576$bw$BWTESTCD == "TERMBW"), c("STUDYID", "USUBJID", "BWSTRESN")]
       TermBodyWeight <- rbind(TermBodyWeight, Dog5492$bw[which(Dog5492$bw$BWTESTCD == "TERMBW"), c("STUDYID", "USUBJID", "BWSTRESN")],
                               Rat5492$bw[which(Rat5492$bw$BWTESTCD == "TERMBW"), c("STUDYID", "USUBJID", "BWSTRESN")],
                               Rat6576$bw[which(Rat6576$bw$BWTESTCD == "TERMBW"), c("STUDYID", "USUBJID", "BWSTRESN")] )
       TermBodyWeight <- TermBodyWeight[!(TermBodyWeight$USUBJID %in% c(Rat5492TKIndv,Rat6576TKIndv)),]
     } else if (BWMethod == "BW"){
       TermBodyWeight <- data.frame(USUBJID = unique(BodyWeight$USUBJID),
                                    BWSTRESN = NA)
       for (Indv in unique(BodyWeight$USUBJID)){
         IndvTerm <- BodyWeight[which(BodyWeight$USUBJID == Indv),]
         InitialW <- IndvTerm[which(IndvTerm$VISITDY <= 0),c("BWSTRESN","VISITDY")]
         InitialW <- as.numeric(InitialW$BWSTRESN[which(InitialW$VISITDY == max(InitialW$VISITDY),)])
         IndvTerm$Diff <- abs(as.numeric(IndvTerm$BWSTRESN) - InitialW)
         maxdiff <- max(as.numeric(IndvTerm$Diff), na.rm = TRUE)  ## Find largest difference from initial
         index <- which(TermBodyWeight$USUBJID == Indv)
         TermBodyWeight$BWSTRESN[index] <- IndvTerm$BWSTRESN[which(as.numeric(IndvTerm$Diff) == maxdiff)]
       }
       TermBodyWeight$BWSTRESN <- as.numeric(TermBodyWeight$BWSTRESN)
       if ('3' %in% chosenstudies){
       TermBodyWeight <- TermBodyWeight[!(TermBodyWeight$USUBJID %in% c(Rat5492TKIndv)),]
       }
       if ('4' %in% chosenstudies){
         TermBodyWeight <- TermBodyWeight[!(TermBodyWeight$USUBJID %in% c(Rat6576TKIndv)),]
       }
     }
     if (is.factor(TermBodyWeight$BWSTRESN)) {
       TermBodyWeight$BWSTRESN <- as.numeric(levels(TermBodyWeight$BWSTRESN)[TermBodyWeight$BWSTRESN])
       InitialWeight$BWSTRESN <- as.numeric(levels(InitialWeight$BWSTRESN)[InitialWeight$BWSTRESN])
     } else {
       TermBodyWeight$BWSTRESN <- as.numeric(TermBodyWeight$BWSTRESN)
       InitialWeight$BWSTRESN <- as.numeric(InitialWeight$BWSTRESN)
     }
     CompileData <- merge(CompileData, TermBodyWeight, by = "USUBJID")
     #Subtract Starting Weight
     CompileData$BWSub <- NA
     CompileData$BWBaseline <- NA
     BodyWeight$BWSub <- NA
     BodyWeight$BaselinePercentChange <- NA
     BodyWeight$Baseline <- NA
     for (indv in unique(InitialWeight$USUBJID)){
       Relvdata <- CompileData[which(CompileData$USUBJID == indv),]
       inx <- which(CompileData$USUBJID == indv)
       BWinx <- which(BodyWeight$USUBJID == indv)
       InitialW <- InitialWeight[which(InitialWeight$USUBJID == indv),c("BWSTRESN","VISITDY")]
       InitialW <- as.numeric(InitialW$BWSTRESN[which(InitialW$VISITDY == max(InitialW$VISITDY),)])
       CompileData$BWSub[inx] <- CompileData$BWSTRESN[inx]-InitialW
       CompileData$BWBaseline[inx] <- InitialW
       BodyWeight$Baseline[BWinx] <- InitialW
       BodyWeight$BWSub[BWinx] <- as.numeric(BodyWeight$BWSTRESN[BWinx]) - InitialW
       BodyWeight$BaselinePercentChange[BWinx] <- BodyWeight$BWSub[BWinx]/InitialW*100
     }

     #Make Compile Data BWzScore for Scoring
     CompileData$BWzScore <- NA
     for (study in unique(CompileData$StudyID)){
       stdyidx <- which(CompileData$StudyID %in% study)
       Controlanimals <- CompileData[which(CompileData$ARMCD == "Vehicle" & CompileData$StudyID %in% study),]
       ControlBaselinemean <- mean(Controlanimals$BWSub)
       ControlBaseSD <- sd(Controlanimals$BWSTRESN)
       CompileData$BWzScore[stdyidx] <- (CompileData$BWSub[stdyidx] - ControlBaselinemean )/ControlBaseSD
     }
     if (BWMethod == "BW"){
       BodyWeight <- merge(BodyWeight, CompileData[,c('USUBJID','SEX','StudyID','ARMCD')], by='USUBJID')
       BodyWeightSummary[[Gender]] <- BodyWeight
     }
     if (BWMethod == "TERMBW"){
       BodyWeight <- NA
       BodyWeight <- merge(TermBodyWeight[,c("USUBJID","BWSTRESN")], CompileData[,c("USUBJID","Species","ARMCD","SEX", "StudyID", "BWSub","BWBaseline")],
                            by='USUBJID')
       BodyWeightSummary[[Gender]] <- BodyWeight
     }

     #FW Load
     #Standardize Rats by Cage: Combine Pooldef and Cage Name
     #Make Combindation Dataframes for Graphs
     RatDailyFood <- data.frame(ARMCD = "0", FWDY = 0, FWSTRESN = 0, Compound = '0')
     DogDailyFood <- data.frame(ARMCD = "0", FWDY = 0, FWSTRESN = 0, Compound = '0')
       Rat5492Pool <- merge(Rat5492$pooldef,
                            Rat5492$fw[,c("POOLID","FWDY","FWSTRESN")], by = "POOLID")
       Rat5492FC <- merge(CompileData[which(CompileData$StudyID == "Rat 5492"),c("USUBJID","StudyID","Species","SEX","ARMCD")],
                          Rat5492Pool[,c("USUBJID", "FWDY","FWSTRESN")], by = "USUBJID")
       Rat5492DailyFood <- Rat5492FC %>%  #Average by Treatment Group per Day
         group_by(ARMCD,FWDY) %>% summarise_at(vars(FWSTRESN),mean)
       Rat5492DailyFood$Compound <- rep("5492", nrow(Rat5492DailyFood))
       RatDailyFood <- rbind(RatDailyFood,Rat5492DailyFood)
       Rat6576Pool <- merge(Rat6576$pooldef,
                            Rat6576$fw[,c("POOLID", "FWDY","FWSTRESN")], by = "POOLID")
       Rat6576FC <- merge(CompileData[which(CompileData$StudyID == "Rat 6576"),c("USUBJID","StudyID","Species","SEX","ARMCD")],
                          Rat6576Pool[,c("USUBJID", "FWDY","FWSTRESN")], by = "USUBJID") 
       Rat6576DailyFood <- Rat6576FC %>%  #Average by Treatment Group per Day
         group_by(ARMCD,FWDY) %>% summarise_at(vars(FWSTRESN),mean)
       Rat6576DailyFood$Compound <- rep("6576", nrow(Rat6576DailyFood))
       RatDailyFood <-rbind(RatDailyFood, Rat6576DailyFood)
     #Look at Food Consumption per Dog >> Non-numeric for Dog5492FC
       Dog5492FC <- Dog5492$cl[which(Dog5492$cl$CLTESTCD == "FC"),] #Taking Non-Numeric Variables
       Dog5492FC <- merge(CompileData[which(CompileData$StudyID == "Dog 5492"),c("USUBJID","StudyID","Species","SEX","ARMCD")],
                          Dog5492FC[,c("USUBJID", "CLDY","CLSTRESC")])
       #Convert Categorical to Numeric %
       Dog5492DailyFood <- Dog5492FC %>%
         dplyr::mutate(CLSTRESC = case_when(CLSTRESC == "NORMAL" ~ 1.0
                                            ,CLSTRESC == "Food Consumption, reduced" ~ 0.5
                                            ,CLSTRESC == "Food Consumption, minimal" ~ 0.25
                                            ,TRUE ~ 0
         ))
       #Rename Column names to match Dog6576
       names(Dog5492DailyFood)[6] <- "FWDY"
       names(Dog5492DailyFood)[7] <- "FWSTRESN"
       Dog5492DailyFood <- Dog5492DailyFood %>% #Average by Treatment Group per Day
         group_by(ARMCD,FWDY) %>% summarise_at(vars(FWSTRESN),mean)
       Dog5492DailyFood$FWDY <- as.integer(Dog5492DailyFood$FWDY)
       Dog5492DailyFood$Compound <- rep("5492", nrow(Dog5492DailyFood))
       DogDailyFood <- rbind(DogDailyFood, Dog5492DailyFood)
       Dog6576FC <- merge(CompileData[which(CompileData$StudyID == "Dog 6576"), c("USUBJID","StudyID","Species","SEX","ARMCD")],
                          Dog6576$fw[,c("USUBJID", "FWDY","FWSTRESN")], by = "USUBJID")
       Dog6576DailyFood <- Dog6576FC %>% #Average by Treatment Group per Day
         group_by(ARMCD,FWDY) %>% summarise_at(vars(FWSTRESN),mean)
       Dog6576PreStudy <- Dog6576DailyFood[which(Dog6576DailyFood$FWDY <=0),]
       DogMeanFC <- mean(Dog6576PreStudy$FWSTRESN, na.rm = TRUE)
       Dog6576DailyFood$FWSTRESN <- (Dog6576DailyFood$FWSTRESN/DogMeanFC)
       Dog6576DailyFood$Compound <- rep("6576", nrow(Dog6576DailyFood))
       DogDailyFood <- rbind(DogDailyFood, Dog6576DailyFood)
     #Remove Formatting Line
     RatDailyFood <- RatDailyFood[2:nrow(RatDailyFood),]
     DogDailyFood <- DogDailyFood[2:nrow(DogDailyFood),]
     
     #Combine Rat Information and Get Pretreatment Data
     RatPreStudy <- RatDailyFood[which(RatDailyFood$FWDY <=0),]
     RatMeanFC <- mean(RatPreStudy$FWSTRESN, na.rm = TRUE)
     RatDailyFood$FWSTRESN <- (RatDailyFood$FWSTRESN/RatMeanFC)
     
    #Normalize to Control
     DogDaily <- DogDailyFood %>%
       group_by(FWDY, Compound) %>%
       arrange(FWDY) %>%
       mutate(Diff = FWSTRESN - lag(FWSTRESN, default = first(FWSTRESN)))
     RatDaily <- RatDailyFood %>%
       group_by(FWDY, Compound) %>%
       arrange(FWDY) %>%
       mutate(Diff = FWSTRESN - lag(FWSTRESN, default = first(FWSTRESN)))
     DogDaily$Species <- rep("Dog", nrow(DogDaily))
     RatDaily$Species <- rep("Rat", nrow(RatDaily))
     FWData <- rbind(DogDaily,RatDaily)
     FWDataSummary[[Gender]] <- FWData
  
     ##################### Organ System Specific Graphs ######################
     #Will Run through all of the possible organSystems to generate graphs and fill CompileData
     for (organSystem in organSystems) {
       MIDOMAIN <- organSystem
       Organ <- MIDOMAIN #For OM, will automatically include brain for brain ratio
       #Make LB Data Data Frame to Hold Information
       LBData <- Dog5492$lb[which((Dog5492$lb$VISITDY >= 1)),c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "VISITDY")]
       LBData<- rbind(LBData, Dog6576$lb[which((Dog6576$lb$VISITDY >= 1)),c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN","VISITDY")],
                      Rat5492$lb[which((Rat5492$lb$VISITDY >= 1)),c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "VISITDY")],
                      Rat6576$lb[which((Rat6576$lb$VISITDY >= 1)),c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "VISITDY")])
       # Concatenate LBSPEC and LBTESTCD
       LBData$LBTESTCD <- paste(LBData$LBSPEC, LBData$LBTESTCD, sep = ' | ')
       #Remove Not Included Tests
       organIndex <- which(LBData$LBTESTCD %in% organTESTCDlist[[organSystem]])
       LBData <- LBData[organIndex,]
       #Check if LBData has become empty
       if (nrow(LBData) == 0){
         #Removes empty LBData from FINALDAYS
         #print(paste0("No LB TESTS SELECTED in ", organSystem))
       } else {
       #Make list of Recovery Animals
       RecoveryAnimals<-unique(subset(LBData$USUBJID, !(LBData$USUBJID %in% CompileData$USUBJID)))
       #Find Final Day for Before Recovery for Recovery Animals
       RecovData <- LBData[which(LBData$USUBJID %in% RecoveryAnimals),]
       RecovData <- RecovData[which(RecovData$VISITDY < 40),] #set manually for Biocelerate Studies
       FinalDays <- NA
       for (indv in unique(LBData$USUBJID)){
         indvtests <- LBData[which(LBData$USUBJID == indv), "LBTESTCD"]
         for (TEST in unique(indvtests)){
           if (indv %in% RecoveryAnimals){
             Indv <- which(RecovData$USUBJID == indv)
             IndvData <- RecovData[Indv,]
             IndvData <- IndvData[which(IndvData$LBTESTCD == TEST),]
             maxday <- suppressWarnings(max(IndvData$VISITDY))
           } else {
             Indv <- which(LBData$USUBJID == indv)
             IndvData <- LBData[Indv,]
             IndvData <- IndvData[which(IndvData$LBTESTCD == TEST),]
             maxday <- suppressWarnings(max(IndvData$VISITDY))
           }
           LastTest <- which(LBData$USUBJID == indv & LBData$VISITDY == maxday)
           FinalDays <- append(FinalDays, LastTest)
         }
       }
       FinalDays <- unique(FinalDays) #Removes accidentally created duplicates in FINAL DAYS
       LBData <- LBData[FinalDays, c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN")]
       }
  
       #MI Values included in grouped systems replace MIDOMAIN and Organ for analysis
       Group <- MIDOMAIN
       MIDOMAIN <-MITESTCDlist[[organSystem]]
       Organ <- OMTESTCDlist[[organSystem]]
       #Make DataFrame to hold MI Information 
       MIData <-Dog5492$mi[which(Dog5492$mi$MISPEC %in% MIDOMAIN), c("USUBJID", "MISTRESC","MISEV","MISPEC")]
       MIData <- rbind(MIData, Dog6576$mi[which(Dog6576$mi$MISPEC %in% MIDOMAIN), c("USUBJID", "MISTRESC","MISEV","MISPEC")],
                       Rat5492$mi[which(Rat5492$mi$MISPEC %in% MIDOMAIN), c("USUBJID", "MISTRESC","MISEV","MISPEC")],
                       Rat6576$mi[which(Rat6576$mi$MISPEC %in% MIDOMAIN), c("USUBJID", "MISTRESC","MISEV","MISPEC")])
       gsub(pattern = 'Tension lipidosis', replacement = 'Tension Lipidosis', MIData$MISTRESC)
       MIData$MISTRESC <- toupper(MIData$MISTRESC)
       #Convert Severity
       MIData$MISEV <- str_replace_all(MIData$MISEV, "1 OF 5", "1")
       MIData$MISEV <- str_replace_all(MIData$MISEV, "MINIMAL", "1")
       MIData$MISEV <-  str_replace_all(MIData$MISEV, "2 OF 5", "2")
       MIData$MISEV <-  str_replace_all(MIData$MISEV, "MILD", "2")
       MIData$MISEV <-  str_replace_all(MIData$MISEV, "3 OF 5", "3")
       MIData$MISEV <-  str_replace_all(MIData$MISEV, "MODERATE", "3")
       MIData$MISEV <-  str_replace_all(MIData$MISEV, "4 OF 5", "4")
       MIData$MISEV <-  str_replace_all(MIData$MISEV, "MARKED", "4")
       MIData$MISEV <-  str_replace_all(MIData$MISEV, "5 OF 5", "5")
       MIData$MISEV <-  str_replace_all(MIData$MISEV, "SEVERE", "5")
       MIData$MISEV <- ordered(MIData$MISEV, levels= c("0","1", "2", "3", "4","5"))
       MIData$MISEV = MIData$MISEV %>% replace_na("0")
       
       #Compile OM Data for Organlist
       #Find Brain indexes
       Dog6576idx <- str_which(Dog6576$om$OMSPEC, "BRAIN")
       Dog5492idx <- str_which(Dog5492$om$OMSPEC, "BRAIN")
       Rat6576idx <- str_which(Rat6576$om$OMSPEC, "BRAIN")
       Rat5492idx <- str_which(Rat5492$om$OMSPEC, "BRAIN")
       
       #Add Indxes that are Organs
       for (i in 1:length(Organ)){
         Dog6576idx <- append(Dog6576idx, str_which(Dog6576$om$OMSPEC, Organ[i]))
         Dog5492idx <- append(Dog5492idx, str_which(Dog5492$om$OMSPEC, Organ[i]))
         Rat6576idx <- append(Rat6576idx, str_which(Rat6576$om$OMSPEC, Organ[i]))
         Rat5492idx <- append(Rat5492idx, str_which(Rat5492$om$OMSPEC, Organ[i]))
       }
       
       OrganWeights <- data.frame(USUBJID = Dog6576$om$USUBJID[Dog6576idx],
                                  OMSPEC = Dog6576$om$OMSPEC[Dog6576idx],
                                  OMSTRESN = Dog6576$om$OMSTRESN[Dog6576idx],
                                  OMTEST = Dog6576$om$OMTEST[Dog6576idx])
       OrganWeights <- rbind(OrganWeights,Dog5492$om[Dog5492idx,c("USUBJID", "OMSPEC", "OMSTRESN","OMTEST")],
                             Rat6576$om[Rat6576idx,c("USUBJID", "OMSPEC", "OMSTRESN","OMTEST")],
                             Rat5492$om[Rat5492idx, c("USUBJID", "OMSPEC", "OMSTRESN","OMTEST")])
       OrganWeights <- OrganWeights[which(OrganWeights$OMTEST == "Weight"), c("USUBJID", "OMSPEC", "OMSTRESN")]
       CompileData <- merge(CompileData, OrganWeights, by = "USUBJID") 
       
      #Combine Levels on Findings
      MIData$MISTRESC <- as.factor(MIData$MISTRESC)
      levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "CELL DEBRIS"] <- "CELLULAR DEBRIS"
      levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "Infiltration, mixed cell"] <- "Infiltrate"
      levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "Infiltration, mononuclear cell"] <- "Infiltrate"
      levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "Fibrosis"] <- "Fibroplasia/Fibrosis"
  ######### OM Graph ###############

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

      #Calculate zScore
      CompileData$OrganzScore <- NA
      CompileData$BWRatiozScore <- NA
      CompileData$BrainRatiozScore <- NA
      for (sex in c('M','F')) {
        ## Restrict Gender of compile data based on sex
        GenderData <- CompileData[which(CompileData$SEX == sex),]
        #Calculate Z Score for straight Organ Weight, Terminal BW Ratio, and Brain Ratio
        for (study in unique(CompileData$StudyID)){
          StudyData <- GenderData[which(GenderData$StudyID == study),]
          StudyIdx <- which(CompileData$StudyID == study)
          StudyIdx <- intersect(StudyIdx, which(CompileData$SEX == sex))
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
                                  OrgantoBrainSd = sd(organdata$OrgantoBrainOW, na.rm = TRUE))
            Idx<- which(StudyData$OMSPEC == organ)
            StudyData$OrganzScore[Idx] <-(StudyData$OMSTRESN[Idx] - MeansSd$ORGANMEAN)/MeansSd$ORGANSd
            StudyData$BWRatiozScore[Idx] <-(StudyData$OrgantoTermBW[Idx] - MeansSd$OrgantoBWmean)/MeansSd$OrgantoBWSd
            StudyData$BrainRatiozScore[Idx] <-(StudyData$OrgantoBrainOW[Idx] -MeansSd$OrgantoBrainmean)/MeansSd$OrgantoBrainSd
          }
          #Reconcile Study into CompileData with Zscores
          CompileData$OrganzScore[StudyIdx] <- StudyData$OrganzScore
          CompileData$BWRatiozScore[StudyIdx] <- StudyData$BWRatiozScore
          CompileData$BrainRatiozScore[StudyIdx] <- StudyData$BrainRatiozScore
        }
      }

      #GRAPH OM ZScore
      OMGraphData <- CompileData[,c("USUBJID","StudyID","ARMCD","Species","SEX","OMSPEC","BrainRatiozScore",
                                    "BWRatiozScore", "OrganzScore")]
      OMGraphData$Compound <- word(OMGraphData$StudyID, 2)

      #Remove Brain and limit to Dose Chosen
      OMGraphData <- OMGraphData[which(OMGraphData$OMSPEC %in% Organ),]
      OMresults[[Group]][[Gender]] <-OMGraphData

      ###### LB GRAPHS #######################################################
      
      if (nrow(LBData) == 0){
        #Removes empty LBData from zScore Calcuation as it cannot calcluate
      } else {
      #Calculate Z Score per LBTESTCD
      LBData <- merge(LBData, unique(AllData[,c("USUBJID", "ARMCD","StudyID","SEX")]), by = "USUBJID")
      #Recode Arms for LBData
      LBData$ARMCD <- as.factor(LBData$ARMCD)
      levels(LBData$ARMCD) <- c("1", "1", "2", "3", "3", "4", "4") #Done Manually for BioCelerate Data
      if (Gender %in% c('M','Combined')){ #Removing miscoded animals from RAT 5492 study
          a <- which(LBData$USUBJID %in% c("96298-3111", "96298-3112", "96298-3113", "96298-3114","96298-3115",
                                           "96298-4111", "96298-4112", "96298-4113", "96298-4114", "96298-4115"))
          if (length(a)>0){
            LBData <- LBData[-a,] 
          }
        }
        if (Gender %in% c('F','Combined')){
          a <-which(LBData$USUBJID %in% c("96298-3211", "96298-3212",
                                          "96298-3213", "96298-3214", "96298-3215",
                                          "96298-4211","96298-4212","96298-4213",
                                          "96298-4214", "96298-4215"))
          if (length(a)>0){
            LBData <- LBData[-a,] 
          }
        } 
      levels(LBData$ARMCD) <- doseRanks
      if (length(LBData$LBTESTCD) == 0){
      } else {
        LBData$zscore <- NA
        for (sex in c('M','F')) {
          ## Restrict Gender of compile data based on sex
          GenderData <- LBData[which(LBData$SEX == sex),]
          for (study in unique(LBData$StudyID)){
            StudyData <- GenderData[which(GenderData$StudyID == study),]
            for (TEST in unique(StudyData$LBTESTCD)){
              TESTData <- StudyData[which(StudyData$LBTESTCD == TEST),]
              ControlTESTData <- TESTData[which(TESTData$ARMCD == "Vehicle"),]
              SIdx <- which(LBData$StudyID == study)
              SIdx <- intersect(SIdx, which(LBData$SEX == sex))
              index <- intersect(which(LBData$LBTESTCD == TEST),SIdx)
              LB.mean.C <- mean(ControlTESTData$LBSTRESN, na.rm = TRUE)
              LB.sd.C <- sd(ControlTESTData$LBSTRESN, na.rm = TRUE)
              if (is.na(LB.sd.C) == TRUE){
                #print(paste0(TEST, "For ", study, sex))
              }
              LBData$zscore[index] <- (LBData$LBSTRESN[index]- LB.mean.C)/LB.sd.C
            }
          }
        }
      }
      #Remove Recovery Animals from LBData
      LBData2 <- LBData[which(LBData$USUBJID %in% CompileData$USUBJID),]
      if (length(LBData2$LBTESTCD) == 0){
        
      } else{
        CompileData <- merge(CompileData, LBData2[,c("USUBJID","zscore","LBTESTCD")], by = "USUBJID")
        CompileData <- dcast(CompileData, USUBJID+StudyID+Species+SEX+ARMCD+OMSPEC+BrainRatiozScore+BWSTRESN+BWzScore~LBTESTCD, value.var = "zscore", fun.aggregate = mean)
      }
      }

      #Save Respective LBData per OrganSystem and Gender
      LBresults[[Group]][[Gender]] <- LBData

  ########## MI Data ###############
      
      #Merge Severity MI Data into Compile Data
      MIIncidencePRIME <-MIData[,c(1,2,4)] ##Keeps MISPEC
      Severity <- merge(MIData, CompileData[,c("USUBJID", "StudyID", "Species", "ARMCD")])
      MIData <- dcast(MIData, USUBJID ~ MISTRESC, value.var = "MISEV")
      MIData[is.na(MIData)] <- "0" #Fill NAs with Zero
      CompileData<- merge(CompileData, MIData, by = "USUBJID")

      # Remove Normal MI Results
      normalIndex <- which(colnames(CompileData) == 'NORMAL')
      if (length(normalIndex) > 0) {
        CompileData <- CompileData[, -normalIndex]
      }

      #Calculate Incidence per group for MI Data
      MIIncidencePRIME <- merge(MIIncidencePRIME, unique(CompileData[,c("USUBJID", "StudyID","ARMCD")]), by = "USUBJID")
      groups <- paste0(MIIncidencePRIME$StudyID," ", MIIncidencePRIME$ARMCD)
      #HAVE IT THROUGH ORGANS HERE
      for (MISPEC in unique(MIIncidencePRIME$MISPEC) ){
      MIIncidence <- MIIncidencePRIME[which(MIIncidencePRIME$MISPEC %in% MISPEC),c("USUBJID","MISTRESC", "StudyID","ARMCD")]
      
      GroupIncid <- data.frame(Treatment = NA,
                               Sex = NA,
                               Finding = NA,
                               Count = NA)
      GroupIncid2 <- GroupIncid
      for (Study in unique(MIIncidence$StudyID)){
        for (sex in c('M','F')) {
          StudyMI <- MIIncidence[which(MIIncidence$StudyID==Study),]
          StudyGroupIncid <- data.frame(Treatment = NA,
                                        Sex = NA,
                                        Finding = NA,
                                        Count = NA)
          StudyNonBaselineIncid <- StudyGroupIncid
          sexSubjects <- CompileData$USUBJID[which(CompileData$SEX == sex)]
          sexIndex <- which(StudyMI$USUBJID %in% sexSubjects)
          StudyMI <- StudyMI[sexIndex,]
          for(dose in unique(StudyMI$ARMCD)){
            doseMI <- StudyMI[which(StudyMI$ARMCD == dose),]

            Incid <- data.frame(table(toupper(doseMI$MISTRESC))/length(unique(doseMI$USUBJID)))
            names(Incid)[2] <- "Count"
            names(Incid)[1] <- "Finding"
            Incid$Treatment <- paste0(Study, " ",dose)
            Incid$Sex <- sex

            StudyGroupIncid <- rbind(StudyGroupIncid,Incid)
            StudyNonBaselineIncid <- rbind(StudyNonBaselineIncid, Incid)
          }
          #Removing of Vehicle Baseline
          for (finding in unique(StudyGroupIncid$Finding)) {
            findingIndex <- which(StudyGroupIncid$Finding == finding)
            vehicleIndex <- grep('Vehicle', StudyGroupIncid$Treatment[findingIndex])
            if (length(vehicleIndex) > 0) {
              baseline <- StudyGroupIncid$Count[findingIndex][vehicleIndex]
              StudyGroupIncid$Count[findingIndex] <- StudyGroupIncid$Count[findingIndex] - baseline
            }
          }
          negativeIndex <- which(StudyGroupIncid$Count < 0)
          if (length(negativeIndex) > 0) {
            StudyGroupIncid$Count[negativeIndex] <- 0
          }

          GroupIncid <- rbind(GroupIncid, StudyGroupIncid)
          GroupIncid2 <- rbind(GroupIncid2, StudyNonBaselineIncid) #Non-Baseline Removed for graph
        }
      }
      removeIndex <- which(is.na(GroupIncid$Treatment))
      removeIndex2 <- which(is.na(GroupIncid2$Treatment))
      if (length(removeIndex) > 0) {
        GroupIncid <- GroupIncid[-removeIndex,]
        GroupIncid2 <- GroupIncid2[-removeIndex2,]
      }
      MIIncidence <- GroupIncid
      MIIncidence2 <- GroupIncid2
      #Make MIplotData by Merging MI Incidence with Severity
      Severity$Treatment <- paste0(Severity$StudyID," ", Severity$ARMCD)
      names(Severity)[2] <- "Finding"
      MIplotData <- merge(MIIncidence2,Severity[,c("Treatment","MISEV","Finding")], by = c("Treatment","Finding"))
      MIplotData <- aggregate(MISEV ~Treatment+Finding+Sex+Count,FUN = max, data = MIplotData)
      #Reconcile names for Heatmap
      rep_str <- c("NORMAL" = "UNREMARKABLE", "INFILTRATION, MIXED CELL"= "INFILTRATE",
                   "INFILTRATION, MONONUCLEAR CELL" = "INFILTRATE")
      MIplotData$Finding <- str_replace_all(MIplotData$Finding, rep_str)
      #Filter for Parameters
      MIplotData$Dose <- word(MIplotData$Treatment,3)
      
      #SAVE MIplotData Per Gender and Dose AND ORGAN
      MIresults[[Group]][[Gender]][[MISPEC]]<- MIplotData
      }
      CompileDataSummary[[Gender]] <- CompileData
  ####################################### Scoring Portion #########################

      #Score the BW Domain based on breaks
      ScoredData <- CompileData[,1:6]
      ScoredData$BWzScore <- abs(CompileData$BWzScore)
      for (Study in unique(ScoredData$StudyID)){
        ScoredData[ScoredData$StudyID == Study,] %<>%
          dplyr::mutate(BWzScore = case_when(BWzScore > 3 ~ 3
                                             ,BWzScore >= 2 ~ 2
                                             ,BWzScore >= 1 ~ 1
                                             ,TRUE ~ 0
          ))

      }

      #Score the OM Domain Brain Ratios
      ScoredData$OMBrainRatio <- abs(CompileData$BrainRatiozScore)
      for (Study in unique(ScoredData$StudyID)){
        ScoredData[ScoredData$StudyID == Study,] %<>%
          dplyr::mutate(OMBrainRatio = case_when(OMBrainRatio > 3 ~ 3
                                                 ,OMBrainRatio  >= 2 ~ 2
                                                 ,OMBrainRatio  >= 1~ 1
                                                 ,TRUE ~ 0
          ))
      }

      #Score LB Data
      colIndex <- which(colnames(CompileData) %in% organTESTCDlist[[organSystem]])
      if (identical(integer(0),colIndex) == TRUE){
        ScoredData$OMSPEC <- CompileData$OMSPEC
        #Removes Scoring if there are no LB Tests
        colIndex <- which(colnames(CompileData) == "BWRatiozScore")
        print(paste0("No LB TESTS SELECTED in ", organSystem))
      } else {
        for (i in colIndex) {
          colName <- colnames(CompileData)[i]
          ScoredData[[colName]] <- NA
          for (Study in unique(ScoredData$StudyID)){
            j <- which(CompileData$StudyID == Study)
            StudyData <- CompileData[which(CompileData$StudyID == Study),]
            
            x <- ifelse(StudyData[,i]>3 | StudyData[,i]<(-3),3,
                        ifelse(StudyData[,i]>2 | StudyData[,i]<(-2),2,
                               ifelse((StudyData[,i]> 1 | StudyData[,i]< (-1)),1,0)))
            ScoredData[j,colName] <-x
            # colnames(ScoredData)[i] <- colnames(CompileData)[i+1]
          }
        }
      }
      IncidenceOverideCount <- 0
      #Score MI Data
      colIndex <- seq((colIndex[length(colIndex)]+1), ncol(CompileData))
      for (i in colIndex){
        colName <- colnames(CompileData)[i]
        ScoredData[[colName]] <- NA
        #Score Severity
        x <- ifelse(CompileData[,i]>3,3,
                    ifelse(CompileData[,i]==3,2,
                           ifelse(CompileData[,i]>0,1,0)))
        ScoredData[,colName] <-x
        #Check the Incidence percentage for each group
        for (Study in unique(ScoredData$StudyID)){
          for (sex in c('M','F')) {
            studyDataStudyIndex <- which(CompileData$StudyID == Study)
            studyDataSexIndex <- which(CompileData$SEX == sex)
            studyDataIndex <- intersect(studyDataStudyIndex, studyDataSexIndex)
            StudyData <- CompileData[studyDataIndex,]

            MIIncidStudyIndex <- grep(Study, MIIncidence$Treatment)
            MIIncidSexIndex <- which(MIIncidence$Sex == sex)
            MIIncidIndex <- intersect(MIIncidStudyIndex, MIIncidSexIndex)
            MIIncidStudy <- MIIncidence[MIIncidIndex,]

            for (Dose2 in unique(StudyData$ARMCD)){
              DoseSevIndex <- which(StudyData$ARMCD == Dose2)
              DoseSev <- StudyData[DoseSevIndex,]
              DoseIncid <- MIIncidStudy[which(word(MIIncidStudy$Treatment, -1) == Dose2),]
              if (colName %in% DoseIncid$Finding) {
                findingIndex <- which(DoseIncid$Finding == colName)
                Incid <- DoseIncid$Count[findingIndex]
                Incid <- ifelse(Incid>=0.5,3,
                                ifelse(Incid>=0.25,2,
                                       ifelse(Incid>=0.1,1,0)))
                swapIndex <- which(DoseSev[[colName]] < Incid & DoseSev[[colName]] > 0)
                if (length(swapIndex) > 0) {
                  DoseSev[swapIndex, colName] <- Incid
                  ScoredData[studyDataIndex[DoseSevIndex], colName] <- DoseSev[, colName]
                  IncidenceOverideCount <- IncidenceOverideCount + 1
                }

              }
            }
          }
        }
      }

      # #Remove Extra Lines for Brain OM with added catch for Reproductive F to ensure that it Dog 6576 data remains
      if (Gender == 'F' & organSystem == "REPRODUCTIVE"){
        ScoredData$OMSPEC[which(ScoredData$StudyID == 'Dog 6576')] <- "OVARY"
        CompileData$OMSPEC[which(CompileData$StudyID == 'Dog 6576')] <- "OVARY"
      }
      #Check if there is more than just OMSPEC of Brain per Study ID
      Rmxidx <- list()
      for (Study in unique(ScoredData$StudyID)){
        idx <- which(ScoredData$StudyID %in% Study)
       Data <- ScoredData[idx,]
       Val <- unique(Data$OMSPEC)
       if (length(Val) == 1){
  
       } else {
         #Add Rows with "BRAIN" to the remove list
         idx2 <- which(ScoredData$StudyID %in% Study & ScoredData$OMSPEC == "BRAIN")
         Rmxidx <-append(Rmxidx, idx2)
        }
      }
      Rmxidx <- unlist(Rmxidx)
      ScoredData <- ScoredData[-Rmxidx,]
      CompileData <- CompileData[-Rmxidx,]

      #   #Return MI DOMAIN to Initial Terminology if needed to be changed for groupings
      if (Group == 'HEMATOPOIETIC'){
        MIDOMAIN <- 'HEMATOPOIETIC'
      } else if (Group == 'ENDOCRINE'){
        MIDOMAIN <- 'ENDOCRINE'
      } else if (Group == 'REPRODUCTIVE'){
        MIDOMAIN <- 'REPRODUCTIVE'
      }

      summaryResults[[organSystem]]$BW <- NULL
      summaryResults[[organSystem]]$OM <- NULL
      summaryResults[[organSystem]]$LB$SERUM <- NULL
      summaryResults[[organSystem]][['LB']][['WHOLE BLOOD']] <- NULL
      summaryResults[[organSystem]]$LB$URINE <- NULL
      summaryResults[[organSystem]]$MI <- NULL

      organEndPoints <- c('Organ Weights', 'Clinical Pathology', 'Histopathology', 'Body Weights')
      for (sex in Gender) {
        for (Study in unique(ScoredData$StudyID)) {
          ScoredDataTmp <- ScoredData[which(ScoredData$StudyID == Study & ScoredData$SEX == sex & ScoredData$ARMCD == Dose),]
          for (organEndPoint in organEndPoints) {
            if (organEndPoint == 'Body Weights') {
              BWtmp <- mean(ScoredDataTmp$BWzScore, na.rm = T)
              summaryResults[[organSystem]]$BW <- c(summaryResults[[organSystem]]$BW, BWtmp)
            } else if (organEndPoint == 'Organ Weights') {
              OMtmp <- mean(ScoredDataTmp$OMBrainRatio, na.rm = T)
              summaryResults[[organSystem]]$OM <- c(summaryResults[[organSystem]]$OM, OMtmp)
            } else if (organEndPoint == 'Clinical Pathology') {
              for (endpoint in c('SERUM', 'WHOLE BLOOD', 'URINE')) {
                endpointIndex <- grep(endpoint, colnames(ScoredDataTmp))
                if (length(endpointIndex) > 0) {
                  if (aggregationMethod == 'mean') {
                    maxEndpointData <- apply(ScoredDataTmp[,endpointIndex], MARGIN=1, FUN=mean, na.rm = T)
                  } else if (aggregationMethod == 'animalMax') {
                    maxEndpointData <- apply(ScoredDataTmp[,endpointIndex], MARGIN=1, FUN=max, na.rm = T)
                  } else if (aggregationMethod == 'endpointMax') {
                    maxEndpointData <- apply(ScoredDataTmp[,endpointIndex], MARGIN=2, FUN=max, na.rm = T)
                  }
                  infIndex <- which(is.infinite(maxEndpointData))
                  if (length(infIndex) > 0) {
                    maxEndpointData[infIndex] <- NA
                  }
                  meanEndpointValue <- mean(maxEndpointData, na.rm = T)
                  summaryResults[[organSystem]][['LB']][[endpoint]] <- c(summaryResults[[organSystem]][['LB']][[endpoint]],
                                                                         meanEndpointValue)
                }
              }
            } else if (organEndPoint == 'Histopathology') {
              preMIindex <- grep('|', colnames(ScoredDataTmp), fixed = T)
              if (nrow(LBData) == 0) {
                preMIindex <- which(colnames(ScoredDataTmp) == "OMSPEC") #account for loss of LB Variables
              }
              ScoredDataTmp2 <- ScoredDataTmp[which(ScoredDataTmp$OMSPEC %in% MITESTCDlist[[organSystem]]),]
              MIindex <- seq((max(preMIindex)+1), ncol(ScoredDataTmp))
              if (aggregationMethod == 'mean') {
                 if (length(MIindex) ==1){
                  maxEndpointData <- mean(ScoredDataTmp2[,MIindex], na.rm = T)  
                 } else {
                  maxEndpointData <- apply(ScoredDataTmp2[,MIindex], MARGIN=1, FUN=mean, na.rm = T)  
                 }
              } else if (aggregationMethod == 'animalMax') {
                 if (length(MIindex) ==1){
                  maxEndpointData <- max(ScoredDataTmp2[,MIindex], na.rm = T)     
                 } else {
                  maxEndpointData <- apply(ScoredDataTmp2[,MIindex], MARGIN=1, FUN=max, na.rm = T)  
                 }
              } else if (aggregationMethod == 'endpointMax') {
                 if (length(MIindex) == 1){
                    maxEndpointData <- max(ScoredDataTmp2[,MIindex], na.rm = T) 
                 } else {
                    maxEndpointData <- apply(ScoredDataTmp2[,MIindex], MARGIN=2, FUN=max, na.rm = T)  
                 }
              }
              if (identical(maxEndpointData, integer(0)) == TRUE){ #Error Catch for missing values in studies
                 maxEndpointData <- 0
              }
              meanEndpointValue <- mean(as.numeric(maxEndpointData))
              summaryResults[[organSystem]]$MI <- c(summaryResults[[organSystem]]$MI, meanEndpointValue)
            }
          }
        }
      }
      #Clear for Reset
      CompileData <- CompileData[,c("USUBJID","StudyID", "Species", "SEX", "ARMCD", "BrainRatiozScore", "BWSTRESN", "BWzScore")]
    } 
    Results <- as.data.frame(summaryResults)
      rownames(Results) <- c(paste0("Dog 6576 ", Gender), paste0("Dog 5492 ", Gender),
                             paste0("Rat 6576 ", Gender), paste0("Rat 5492 ", Gender))
    summaryData <- rbind(summaryData,Results)
  }
  summaryData <- cbind(summaryData, setNames( lapply(c(organSystems, 'BW'), function(x) x= NA),c(organSystems, 'BW')  ))

  for (organSystem in c(organSystems, 'BW')) {
    Tmp <- rowMeans(summaryData[,grep(organSystem, colnames(summaryData))], na.rm = TRUE)
    # assign(organSystem, Tmp)
    summaryData[,organSystem] <- Tmp
  }
  summaryData <- t(summaryData)
  #Rename 'BW' as 'Body Weight' and add 'LB' to single LB source names to include LB
  rownames(summaryData)[which(rownames(summaryData) =="BW")] <- "BODY WEIGHT"
  rownames(summaryData)[which(rownames(summaryData) =="LIVER.SERUM")] <- "LIVER.LB.SERUM"
  rownames(summaryData)[which(rownames(summaryData) =="ENDOCRINE.SERUM")] <- "ENDOCRINE.LB.SERUM"
  rownames(summaryData)[which(rownames(summaryData) =="HEMATOPOIETIC.WHOLE.BLOOD")] <- "HEMATOPOIETIC.LB.WHOLE.BLOOD"

  #Edit Data to Account for chosen studies
  #Summary Data
  colschosen <- which(word(colnames(summaryData),1,2) %in% chosenstudies)
  summaryData <- summaryData[,colschosen]
  #BW and FW 
  BodyWeight <- BodyWeight[which(BodyWeight$StudyID %in% chosenstudies),]
  FWData$StudyID <- paste0(FWData$Species," ",FWData$Compound)
  FWData <- FWData[which(FWData$StudyID %in% chosenstudies),]
  CompileData <- CompileData[which(CompileData$StudyID %in% chosenstudies),]
  #OM, LB, and MI
  for (sex in SEX){
    BodyWeightSummary[[sex]] <- BodyWeightSummary[[sex]][which(BodyWeightSummary[[sex]]$StudyID %in% chosenstudies),]
    FWDataSummary[[sex]]$StudyID <- paste0(FWDataSummary[[sex]]$Species," ",FWDataSummary[[sex]]$Compound)
    FWDataSummary[[sex]] <- FWDataSummary[[sex]][which(FWDataSummary[[sex]]$StudyID %in% chosenstudies),]
    CompileDataSummary[[sex]] <- CompileDataSummary[[sex]][which(CompileDataSummary[[sex]]$StudyID %in% chosenstudies),]
    for (ORGAN in organSystems){
      OMresults[[ORGAN]][[sex]] <- OMresults[[ORGAN]][[sex]][which(OMresults[[ORGAN]][[sex]]$StudyID %in% chosenstudies),]
      LBresults[[ORGAN]][[sex]] <- LBresults[[ORGAN]][[sex]][which(LBresults[[ORGAN]][[sex]]$StudyID %in% chosenstudies),]
      #Fix MI to loop through all possible organs for MIResults to preoperly remove studies
      for ( MI in names(MIresults[[ORGAN]][[sex]])){
      MIresults[[ORGAN]][[sex]][[MI]] <- MIresults[[ORGAN]][[sex]][[MI]][which(word(MIresults[[ORGAN]][[sex]][[MI]]$Treatment,1,2) %in% chosenstudies),]
      }
    } 
  }
  print('DONE')
  
  #Re-Render Plots that Need Data Changes
  output$FWplot <- renderPlot({
    if (length(SEX) == 2){
      FWPM <-makeFWplot(FWDataSummary$M, input$FWTime, input$dose, 'M')
      FWPF <- makeFWplot(FWDataSummary$F, input$FWTime, input$dose,'F')
      print(plot(plot_grid(FWPM,FWPF, nrow = 1,ncol = 2))) 
    } else {
      FWP <- makeFWplot(FWData, input$FWTime,input$dose,SEX)
      print(FWP) 
    }
  })
  
  output$BWplot <- renderPlot({
    if (length(SEX) == 2){
      q <- makeBWplot(BodyWeightSummary$M,input$bwMethod,input$bwMetric,input$dose,'M')
      t <- makeBWplot(BodyWeightSummary$F,input$bwMethod,input$bwMetric,input$dose,'F')
      print(ggdraw(plot_grid(q,t))) 
    } else {
      q <- makeBWplot(BodyWeight,input$bwMethod,input$bwMetric,input$dose,SEX)
      print(q) 
    }
  })
  ## OM Plots ##
  
  output$OMplot <- renderPlot({ #Kidney Organ Weights Graph
    if (length(SEX) == 2){
      KOM <- makeOMplot(OMresults,'KIDNEY',input$omMethod,input$dose,'M')
      KOM2 <- makeOMplot(OMresults,'KIDNEY',input$omMethod,input$dose,'F')
      print(ggdraw(plot_grid(KOM,KOM2))) 
    } else{
      KOM <- makeOMplot(OMresults,'KIDNEY',input$omMethod,input$dose,SEX)
      print(KOM) 
    }
  })
  
  output$LOMplot <- renderPlot({ #Liver Organ Weights Graph
    if (length(SEX) == 2){
      LOM <- makeOMplot(OMresults,'LIVER',input$omMethod,input$dose,'M')
      LOM2 <- makeOMplot(OMresults,'LIVER',input$omMethod,input$dose,'F')
      print(ggdraw(plot_grid(LOM,LOM2))) 
    } else{
      LOM <- makeOMplot(OMresults,'LIVER',input$omMethod,input$dose,SEX)
      print(LOM) 
    }
  })
  
  output$HOMplot <- renderPlot({ #Hematopoietic Organ Weights Graph
    if (length(SEX) == 2){
      HOM <- makeOMplot(OMresults,'HEMATOPOIETIC',input$omMethod,input$dose,'M')
      HOM2 <- makeOMplot(OMresults,'HEMATOPOIETIC',input$omMethod,input$dose,'F')
      print(ggdraw(plot_grid(HOM,HOM2))) 
    } else{
      HOM <- makeOMplot(OMresults,'HEMATOPOIETIC',input$omMethod,input$dose,SEX)
      print(HOM) 
    }
  })
  
  output$EOMplot <-renderPlot({ #Endocrine Organ Weights Graph
    if (length(SEX) == 2){
      EOM <- makeOMplot(OMresults,'ENDOCRINE',input$omMethod,input$dose,'M')
      EOM2 <- makeOMplot(OMresults,'ENDOCRINE',input$omMethod,input$dose,'F')
      print(ggdraw(plot_grid(EOM,EOM2))) 
    } else{
      EOM <- makeOMplot(OMresults,'ENDOCRINE',input$omMethod,input$dose,SEX)
      print(EOM) 
    }
  })
  
  output$ROMplot <-renderPlot({ #Reproductive Organ Weights Graph
    if (length(SEX) == 2){
      ROM <- makeOMplot(OMresults,'REPRODUCTIVE',input$omMethod,input$dose,'M')
      ROM2 <- makeOMplot(OMresults,'REPRODUCTIVE',input$omMethod,input$dose,'F')
      print(ggdraw(plot_grid(ROM,ROM2))) 
    } else{
      ROM <- makeOMplot(OMresults,'REPRODUCTIVE',input$omMethod,input$dose,SEX)
      print(ROM) 
    }
  })
  
  ## LB Plots##
  output$KSERLBplot <- renderPlot({ #Kidney Clinical Chemistry LB Graph
    if (length(SEX) ==2){
      KERB <- makeLBplot(LBresults, 'KIDNEY','SERUM',input$dose,'M')
      KERB2 <- makeLBplot(LBresults, 'KIDNEY','SERUM',input$dose,'F')
      print(ggdraw(plot_grid(KERB,KERB2))) 
    } else {
      KERB <- makeLBplot(LBresults, 'KIDNEY','SERUM',input$dose,SEX)
      print(KERB) 
    }
  })
  
  output$KURILBplot <- renderPlot({ #Kidney Urinalysis LB Graph
    if (length(SEX) ==2){
      KURILB <- makeLBplot(LBresults, 'KIDNEY','URINE',input$dose,'M')
      KURILB2 <- makeLBplot(LBresults, 'KIDNEY','URINE',input$dose,'F')
      print(ggdraw(plot_grid(KURILB,KURILB2))) 
    } else {
      KURILB <- makeLBplot(LBresults, 'KIDNEY','URINE',input$dose,SEX)
      print(KURILB) 
    }
  })
  
  
  output$LSERLBplot <- renderPlot({ #Liver Clinical Chemistry LB Graph
    if (length(SEX) ==2){
      LSERLB <- makeLBplot(LBresults, 'LIVER','SERUM',input$dose,'M')
      LSERLB2 <- makeLBplot(LBresults, 'LIVER','SERUM',input$dose,'F')
      print(ggdraw(plot_grid(LSERLB,LSERLB2))) 
    } else {
      LSERLB <- makeLBplot(LBresults, 'LIVER','SERUM',input$dose,SEX)
      print(LSERLB) 
    }
  })
  
  output$HHEMELBplot <- renderPlot({ #Hematopoietic Hematology LB Graph
    if (length(SEX) ==2){
      HHEMELB <- makeLBplot(LBresults, 'HEMATOPOIETIC','WHOLE BLOOD',input$dose,'M')
      HHEMELB2 <- makeLBplot(LBresults, 'HEMATOPOIETIC','WHOLE BLOOD',input$dose,'F')
      print(ggdraw(plot_grid(HHEMELB,HHEMELB2))) 
    } else {
      HHEMELB <- makeLBplot(LBresults, 'HEMATOPOIETIC','WHOLE BLOOD',input$dose,SEX)
      print(HHEMELB) 
    }
  })
  
  output$ESERLBplot <- renderPlot({ #Endocrine Clinical Chemistry LB Graph
    if (length(SEX) ==2){
      ESERLB <- makeLBplot(LBresults, 'ENDOCRINE','SERUM',input$dose,'M')
      ESERLB2 <- makeLBplot(LBresults, 'ENDOCRINE','SERUM',input$dose,'F')
      print(ggdraw(plot_grid(ESERLB,ESERLB2))) 
    } else {
      ESERLB <- makeLBplot(LBresults, 'ENDOCRINE','SERUM',input$dose,SEX)
      print(ESERLB) 
    }
  })

  
  ##Radar Plots ##
     for (i in 1:length(SEX)) { #Overall Summary Radar
        local({
        genders <- SEX[i]
        output[[paste('SummaryRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'ALL',genders)
         return(plotData)
        })
        output[[paste('LRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'Liver',genders)
           return(plotData)
        })
        output[[paste('KRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'Kidney',genders)
           return(plotData)
        })
        output[[paste('HRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'HEMATOPOIETIC',genders)
           return(plotData)
        })
        output[[paste('ERadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'Endocrine',genders)
           return(plotData)
        })
        output[[paste('RRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'Reproductive',genders)
           return(plotData)
        })
        
     }) 
  }
  
  
  ## MI Plots ##
  output$KMIplot <- renderPlot({ #KIDNEY MI PLOT
    if (length(SEX) == 2){
      KM <- makeMIplot(MIresults,'KIDNEY','KIDNEY',input$dose,'M',
                       input$KMIClustY, input$KMIClustX)
      KM2 <- makeMIplot(MIresults,'KIDNEY','KIDNEY',input$dose,'F',
                        input$KMIClustY, input$KMIClustX)
      print(grid.arrange(KM,KM2))
    } else {
      KM <- makeMIplot(MIresults,'KIDNEY','KIDNEY',input$dose,SEX,
                       input$KMIClustY, input$KMIClustX)
      print(KM)
    }
  }, height = plotHeight$X)
  
  output$LMIplot <- renderPlot({ #LIVER MI PLOT
    if (length(SEX) == 2){
      LM <- makeMIplot(MIresults,'LIVER','LIVER',input$dose,'M',
                       input$LMIClustY, input$LMIClustX)
      LM2 <- makeMIplot(MIresults,'LIVER','LIVER',input$dose,'F',
                        input$LMIClustY, input$LMIClustX)
      print(grid.arrange(LM,LM2))
    } else {
      LM <- makeMIplot(MIresults,'LIVER','LIVER',input$dose,SEX,
                       input$LMIClustY, input$LMIClustX)
      print(LM)
    }
  }, height = plotHeight$X)
  
  output$HMIplotSpleen <- renderPlot({ #HEMATOPOIETIC SPLEEN MI PLOT
    if (length(SEX) == 2){
       if ("SPLEEN" %in% MITESTCDlist$HEMATOPOIETIC){
      HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"SPLEEN",input$dose,'M',
                       input$HMIClustY, input$HMIClustX)
      HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"SPLEEN",input$dose,'F',
                        input$HMIClustY, input$HMIClustX)
      print(grid.arrange(HM,HM2))
       }
    } else {
      if ("SPLEEN" %in% MITESTCDlist$HEMATOPOIETIC){
         HMS <- makeMIplot(MIresults,'HEMATOPOIETIC',"SPLEEN",input$dose,SEX,
                           input$HMIClustY, input$HMIClustX)  
         print(HMS)
      }
    }
  },height = plotHeight$X)
  
  output$HMIplotBM <- renderPlot({ #HEMATOPOIETIC BONE MARROW MI PLOT
     if (length(SEX) == 2){
        if ("BONE MARROW" %in% MITESTCDlist$HEMATOPOIETIC){
        HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"BONE MARROW",input$dose,'M',
                         input$HMIClustY, input$HMIClustX)
        HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"BONE MARROW",input$dose,'F',
                          input$HMIClustY, input$HMIClustX)
        print(grid.arrange(HM,HM2))
        }
     } else {
        if ("BONE MARROW" %in% MITESTCDlist$HEMATOPOIETIC){
           HMB <- makeMIplot(MIresults,'HEMATOPOIETIC',"BONE MARROW",input$dose,SEX,
                             input$HMIClustY, input$HMIClustX) 
           print(HMB)
        }
     }
  },height = plotHeight$X)
  
  output$HMIplotThymus <- renderPlot({ #HEMATOPOIETIC THYMUS MI PLOT
     if (length(SEX) == 2){
        if ("THYMUS" %in% MITESTCDlist$HEMATOPOIETIC){
           HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"THYMUS",input$dose,'M',
                            input$HMIClustY, input$HMIClustX)
           HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"THYMUS",input$dose,'F',
                             input$HMIClustY, input$HMIClustX)
           print(grid.arrange(HM,HM2))
        }
     } else {
        if ("THYMUS" %in%  MITESTCDlist$HEMATOPOIETIC){
           HMT <- makeMIplot(MIresults,'HEMATOPOIETIC',"THYMUS",input$dose,SEX,
                             input$HMIClustY, input$HMIClustX)
           print(HMT)
        }
     }
  },height = plotHeight$X)
  
  output$HMIplotLympMand <- renderPlot({ #HEMATOPOIETIC LYMPH NODE, MANDIBULAR MI PLOT
    if (length(SEX) == 2){
      if ("LYMPH NODE, MANDIBULAR" %in% MITESTCDlist$HEMATOPOIETIC){
        HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MANDIBULAR",input$dose,'M',
                         input$HMIClustY, input$HMIClustX)
        HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MANDIBULAR",input$dose,'F',
                          input$HMIClustY, input$HMIClustX)
        print(grid.arrange(HM,HM2))
      }
    } else {
      if ("LYMPH NODE, MANDIBULAR" %in%  MITESTCDlist$HEMATOPOIETIC){
        HMT <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MANDIBULAR",input$dose,SEX,
                          input$HMIClustY, input$HMIClustX)
        print(HMT)
      }
    }
  },height = plotHeight$X)
  output$HMIplotLympMESEN <- renderPlot({ #HEMATOPOIETIC LYMPH NODE, MESENTERIC MI PLOT
    if (length(SEX) == 2){
      if ("LYMPH NODE, MESENTERIC" %in% MITESTCDlist$HEMATOPOIETIC){
        HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MESENTERIC",input$dose,'M',
                         input$HMIClustY, input$HMIClustX)
        HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MESENTERIC",input$dose,'F',
                          input$HMIClustY, input$HMIClustX)
        print(grid.arrange(HM,HM2))
      }
    } else {
      if ("LYMPH NODE, MESENTERIC" %in%  MITESTCDlist$HEMATOPOIETIC){
        HMT <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MESENTERIC",input$dose,SEX,
                          input$HMIClustY, input$HMIClustX)
        print(HMT)
      }
    }
  },height = plotHeight$X)
  
  output$EMIplot <- renderPlot({ #ENDOCRINE GLAND, ADRENAL MI PLOT
     if (length(SEX) == 2){
        if ("GLAND, ADRENAL" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, ADRENAL",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, ADRENAL",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(grid.arrange(EM,EM2))
        }
     } else {
        if ("GLAND, ADRENAL" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, ADRENAL",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
  
  output$EMIplot2 <- renderPlot({ #ENDOCRINE GLAND, PITUITARY MI PLOT
     if (length(SEX) == 2){
        if ("GLAND, PITUITARY" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PITUITARY",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PITUITARY",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(grid.arrange(EM,EM2))
        }
     } else {
        if ("GLAND, PITUITARY" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PITUITARY",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
  
  output$EMIplot3 <- renderPlot({ #ENDOCRINE  GLAND, PARATHYROID MI PLOT
     if (length(SEX) == 2){
        if ("GLAND, PARATHYROID" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PARATHYROID",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PARATHYROID",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(grid.arrange(EM,EM2))
        }
     } else {
        if ("GLAND, PARATHYROID" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PARATHYROID",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
  
  output$EMIplot4 <- renderPlot({ #ENDOCRINE  GLAND, THYROID MI PLOT
     if (length(SEX) == 2){
        if ("GLAND, THYROID" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, THYROID",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, THYROID",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(grid.arrange(EM,EM2))
        }
     } else {
        if ("GLAND, THYROID" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, THYROID",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
  
  output$EMIplot5 <- renderPlot({ #ENDOCRINE  PANCREAS MI PLOT
     if (length(SEX) == 2){
        if ("PANCREAS" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "PANCREAS",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "PANCREAS",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(grid.arrange(EM,EM2))
        }
     } else {
        if ("PANCREAS" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "PANCREAS",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
 
  #MI Plots for Reproductive set by SEX they occur in
  output$RMIplot <- renderPlot({ #REPRODUCTIVE GLAND, PROSTATE MI PLOT
      if ("GLAND, PROSTATE" %in% MITESTCDlist$REPRODUCTIVE){
         RM <- makeMIplot(MIresults,'REPRODUCTIVE',"GLAND, PROSTATE",input$dose,'M',
                          input$RMIClustY, input$RMIClustX)
         print(RM)   
      }
  },height = 350)
  
  output$RMIplot2 <- renderPlot({ #REPRODUCTIVE EPIDIDYMIS MI PLOT
     if ("EPIDIDYMIS" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"EPIDIDYMIS",input$dose,'M',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot3 <- renderPlot({ #REPRODUCTIVE TESTIS MI PLOT
     if ("TESTIS" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"TESTIS",input$dose,'M',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot4 <- renderPlot({ #REPRODUCTIVE UTERUS MI PLOT
     if ("UTERUS" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"UTERUS",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot5 <- renderPlot({ #REPRODUCTIVE CERVIX MI PLOT
     if ("CERVIX" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"CERVIX",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot6 <- renderPlot({ #REPRODUCTIVE GLAND, MAMMARY MI PLOT
     if ("GLAND, MAMMARY" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"GLAND, MAMMARY",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot7 <- renderPlot({ #REPRODUCTIVE VAGINA MI PLOT
     if ("VAGINA" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"VAGINA",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot8 <- renderPlot({ #REPRODUCTIVE OVARY MI PLOT
     if ("OVARY" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"OVARY",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$tree <- renderTree(TreeSelect)
  
  }})
  
  
  #Dynamic Sizing of MI and Radar Plots
  output$ReactSummaryRadar <- renderUI({ #Generate one plot per Gender
    lapply(paste('SummaryRadar',1:numSEX$X),plotOutput)
  })
  output$ReactLiverRadar <- renderUI({
     lapply(paste('LRadar',1:numSEX$X),plotOutput)
  })
  output$ReactKidneyRadar <- renderUI({
     lapply(paste('KRadar',1:numSEX$X),plotOutput)
  })
  output$ReactHemaRadar <- renderUI({
     lapply(paste('HRadar',1:numSEX$X),plotOutput)
  })
  output$ReactEndoRadar <- renderUI({
     lapply(paste('ERadar',1:numSEX$X),plotOutput)
  })
  output$ReactReproRadar <- renderUI({
     lapply(paste('RRadar',1:numSEX$X),plotOutput)
  })
  
  output$KMIplotreactive <- renderUI({
    plotOutput('KMIplot',height = plotHeight$X)
  })
  
  output$LMIplotreactive <- renderUI({
    plotOutput('LMIplot', height = plotHeight$X)
  })
  
  output$HMIplotreactive <- renderUI({
    plotOutput('HMIplotSpleen', height = plotHeight$X)
  })
  output$HMIplotreactive2 <- renderUI({
     plotOutput('HMIplotBM', height = plotHeight$X)
  })
  output$HMIplotreactive3 <- renderUI({
     plotOutput('HMIplotThymus', height = plotHeight$X)
  })
  output$HMIplotreactive4<- renderUI({
    plotOutput('HMIplotLympMand', height = plotHeight$X)
  })
  output$HMIplotreactive5<- renderUI({
    plotOutput('HMIplotLympMESEN', height = plotHeight$X)
  })
  
  output$EMIplotreactive <- renderUI({
    plotOutput('EMIplot', height = plotHeight$X)
  })
  
  output$EMIplotreactive2 <- renderUI({
     plotOutput('EMIplot2', height = plotHeight$X)
  })
  
  output$EMIplotreactive3 <- renderUI({
     plotOutput('EMIplot3', height = plotHeight$X)
  })
  
  output$EMIplotreactive4 <- renderUI({
     plotOutput('EMIplot4', height = plotHeight$X)
  })
  
  output$EMIplotreactive5 <- renderUI({
     plotOutput('EMIplot5', height = plotHeight$X)
  })
  
  output$RMIplotreactive <- renderUI({
    plotOutput('RMIplot', height = plotHeight$X)
  })
  
  output$RMIplotreactive2 <- renderUI({
     plotOutput('RMIplot2', height = plotHeight$X)
  })
  
  output$RMIplotreactive3 <- renderUI({
     plotOutput('RMIplot3', height = plotHeight$X)
  })
  
  output$RMIplotreactive4 <- renderUI({
     plotOutput('RMIplot4', height = plotHeight$X)
  })
  
  output$RMIplotreactive5 <- renderUI({
     plotOutput('RMIplot5', height = plotHeight$X)
  })
  
  output$RMIplotreactive6 <- renderUI({
     plotOutput('RMIplot6', height = plotHeight$X)
  })
  
  output$RMIplotreactive7 <- renderUI({
     plotOutput('RMIplot7', height = plotHeight$X)
  })
  
  output$RMIplotreactive8 <- renderUI({
     plotOutput('RMIplot8', height = plotHeight$X)
  })
  
  output$tree <- renderTree ({
     organSystemList <- list( 
        'Kidney' = list(
           'Laboratory Values(LB)' = list("Clinical Chemistry" = structure(list('SERUM | CREAT'= 'SERUM | CREAT',
                                                                                'SERUM | UREAN' = 'SERUM | UREAN',
                                                                                'SERUM | ALB' = 'SERUM | ALB',
                                                                                'SERUM | CL' = 'SERUM | CL',
                                                                                'SERUM | K' = 'SERUM | K',
                                                                                'SERUM | PHOS' = 'SERUM | PHOS',
                                                                                'SERUM | SODIUM' = 'SERUM | SODIUM'), stselected = TRUE),
                                          'Urinanlysis' = structure(list('URINE | K'='URINE | K',
                                                                         'URINE | SODIUM' = 'URINE | SODIUM',
                                                                         'URINE | GLUC' = 'URINE | GLUC',
                                                                         'URINE | SPGRAV' = 'URINE | SPGRAV',
                                                                         'URINE | VOLUME' = 'URINE | VOLUME',
                                                                         'URINE | PROT' = 'URINE | PROT'),stselected = TRUE)),
           'Histopathology(MI)' = structure(list('KIDNEY' = structure('KIDNEY',stselected = TRUE),
                                                 'URETER' = structure('URETER'),
                                                 'URINARY BLADDER' = structure('URINARY BLADDER'),
                                                 'URETHRA' = structure('URETHRA'))),
           'Organ Weight(OM)' = structure(list('KIDNEY' = structure('KIDNEY',stselected = TRUE)))),
        'Liver' = list(
           'Laboratory Values(LB)' = list("Clinical Chemistry" = structure(list('SERUM | ALT'= 'SERUM | ALT',
                                                                                'SERUM | AST' = 'SERUM | AST',
                                                                                'SERUM | ALP' = 'SERUM | ALP',
                                                                                'SERUM | GGT' = 'SERUM | GGT',
                                                                                'SERUM | BILI' = 'SERUM | BILI',
                                                                                'SERUM | ALB' = 'SERUM | ALB'), stselected = TRUE)),
           'Histopathology(MI)' = structure(list('LIVER' = structure('LIVER',stselected = TRUE),
                                                 'GALLBLADDER' = structure('GALLBLADDER'))),
           'Organ Weight(OM)' = structure(list('LIVER' = structure('LIVER',stselected = TRUE),
                                               'GALLBLADDER' = structure('GALLBLADDER')))
        ),
        'Hematopoietic' = list(
           'Laboratory Values(LB)' = list("Hematology" = structure(list('WHOLE BLOOD | RBC'= 'WHOLE BLOOD | RBC',
                                                                        'WHOLE BLOOD | HCT' = 'WHOLE BLOOD | HCT',
                                                                        'WHOLE BLOOD | MCHC' = 'WHOLE BLOOD | MCHC',
                                                                        'WHOLE BLOOD | MCH' = 'WHOLE BLOOD | MCH',
                                                                        'WHOLE BLOOD | MCV' = 'WHOLE BLOOD | MCV',
                                                                        'WHOLE BLOOD | RDW' = 'WHOLE BLOOD | RDW',
                                                                        'WHOLE BLOOD | WBC' = 'WHOLE BLOOD | WBC',
                                                                        'WHOLE BLOOD | MONO' = 'WHOLE BLOOD | MONO',
                                                                        'WHOLE BLOOD | BASO' = 'WHOLE BLOOD | BASO',
                                                                        'WHOLE BLOOD | EOS' = 'WHOLE BLOOD | EOS',
                                                                        'WHOLE BLOOD | LYM' = 'WHOLE BLOOD | LYM',
                                                                        'WHOLE BLOOD | PLAT' = 'WHOLE BLOOD | PLAT',
                                                                        'WHOLE BLOOD | MPV' = 'WHOLE BLOOD | MPV'), stselected = TRUE)),
           'Histopathology(MI)' = structure(list('BONE MARROW' = "BONE MARROW",
                                                 'SPLEEN' = 'SPLEEN',
                                                 'THYMUS' = 'THYMUS',
                                                 "LYMPH NODE, MANDIBULAR" = "LYMPH NODE, MANDIBULAR",
                                                 "LYMPH NODE, MESENTERIC" = "LYMPH NODE, MESENTERIC"), stselected = TRUE),
           'Organ Weight(OM)' = structure(list('SPLEEN' = 'SPLEEN',
                                               'THYMUS' = 'THYMUS'), stselected = TRUE)
        ),
        'Endocrine' = list(
           # 'Laboratory Values(LB)' = list("Clinical Chemistry" = structure(list('SERUM | ALB'= 'SERUM | ALB',
           #                                                                      'SERUM | CL' = 'SERUM | CL',
           #                                                                      'SERUM | PHOS' = 'SERUM | PHOS',
           #                                                                      'SERUM | SODIUM' = 'SERUM | SODIUM',
           #                                                                      'SERUM | GLUC' = 'SERUM | GLUC',
           #                                                                      'SERUM | CA' = 'SERUM | CA'), stselected = TRUE)),
           'Histopathology(MI)' = structure(list('GLAND, THYROID' = 'GLAND, THYROID',
                                                 'GLAND, ADRENAL' = 'GLAND, ADRENAL',
                                                 'GLAND, PITUITARY'='GLAND, PITUITARY',
                                                 'GLAND, PARATHYROID'='GLAND, PARATHYROID',
                                                 'PANCREAS'='PANCREAS'), stselected = TRUE),
           'Organ Weight(OM)' = structure(list('GLAND, THYROID' = 'GLAND, THYROID',
                                               'GLAND, ADRENAL' = 'GLAND, ADRENAL',
                                               'GLAND, PITUITARY'='GLAND, PITUITARY',
                                               'GLAND, PARATHYROID'='GLAND, PARATHYROID',
                                               'PANCREAS'='PANCREAS'), stselected = TRUE)
        ),
        'Reproductive' = list(
           #Remove LB Values
           # 'Laboratory Values(LB)' = list('Hematology' = structure(list('WHOLE BLOOD | RBC'='WHOLE BLOOD | RBC',
           #                                                              'WHOLE BLOOD | HGB' = 'WHOLE BLOOD | HGB'),stselected = TRUE)),
           'Histopathology(MI)' = structure(list('GLAND, PROSTATE' = structure('GLAND, PROSTATE',stselected = TRUE),
                                                 'EPIDIDYMIS' = structure('EPIDIDYMIS',stselected = TRUE),
                                                 'TESTIS' = structure('TESTIS',stselected = TRUE),
                                                 'CERVIX' = structure('CERVIX'),
                                                 'GLAND, MAMMARY' = structure('GLAND, MAMMARY'),
                                                 'OVARY' = structure("OVARY"),
                                                 'UTERUS' = structure('UTERUS'),
                                                 'VAGINA' = structure('VAGINA'))),
           'Organ Weight(OM)' = structure(list('GLAND, PROSTATE' = structure('GLAND, PROSTATE',stselected = TRUE),
                                               'TESTIS' = structure('TESTIS',stselected = TRUE),
                                               'OVARY' = structure("OVARY")))
        )
     )
  })
  outputOptions(output,"tree", suspendWhenHidden = FALSE)

})
shinyApp(ui = ui, server = server)
