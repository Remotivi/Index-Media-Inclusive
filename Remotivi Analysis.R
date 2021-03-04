

rm(list=ls())

library("xlsx")
library("lubridate")
library("xtable")
library("stringr")
library("irr")
library("car")
library("rtf")

####################################################################
## INITIALIZE GLOBAL PARAMETERS
####################################################################
num.coders = 6


####################################################################
## READ AND PREPARE DATA
####################################################################

### Read Data
dat.coder1 = read.xlsx2("Coded Data from Coders.xlsx", "Coder 1", header=FALSE)[,1:25]
Sys.sleep(2)
dat.coder2 = read.xlsx2("Coded Data from Coders.xlsx", "Coder 2", header=FALSE)[,1:25]
Sys.sleep(2)
dat.coder3 = read.xlsx2("Coded Data from Coders.xlsx", "Coder 3", header=FALSE)[,1:25]
Sys.sleep(2)
dat.coder4 = read.xlsx2("Coded Data from Coders.xlsx", "Coder 4", header=FALSE)[,1:25]
Sys.sleep(2)
dat.coder5 = read.xlsx2("Coded Data from Coders.xlsx", "Coder 5", header=FALSE)[,1:25]
Sys.sleep(2)
dat.coder6 = read.xlsx2("Coded Data from Coders.xlsx", "Coder 6", header=FALSE)[,1:25]
Sys.sleep(2)
dat.pengganti = read.xlsx2("Coded Data from Coders.xlsx", "Data Pengganti", header=FALSE)



### Turn into List
dat = list(coder1=dat.coder1, 
           coder2=dat.coder2, 
           coder3=dat.coder3, 
           coder4=dat.coder4, 
           coder5=dat.coder5, 
           coder6=dat.coder6)




### PREPARE AND CLEAN-UP
source("Remotivi Prepare Data.R")
source("Remotivi Data Clean-Up.R")

### PRE-PROCESS DATA AND RECODE VARIABLES
source("Remotivi Recode Variables.R")

### CALCULATE RELIABILITY
source("Remotivi Reliability.R")

### GET STATS ABOUT NUMBER OF ORIGINAL ARTICLES, BACKUP ARTICLES, AND CODED ARTICLES
source("Remotivi Create List of Articles.R")

### SAMPLE FOR EXTERNAL CODERS
source("Remotivi Sample External.R")

### ANSWERING RESEARCH QUESTIONS
source("Remotivi Research Questions.R")



####################################################################
## ANALYSIS OF EXTERNAL CODERS
####################################################################

source("Remotivi Analysis External.R")


####################################################################
## DELETE MIDDLE-PROCESS FILES
####################################################################

to.del = c("Out - Article Information.xlsx", 
          "Out - External Cluster Agama.xlsx",
          "Out - External Cluster Disabilitas.xlsx",
          "Out - External Cluster Gender.xlsx",
          "Out - External Cluster Perempuan.xlsx",
          "Out - External Data with Multiple Entries.xlsx",
          "Out - External Responses to Double Coded Entries.xlsx",
          "Out - List of Double Coded Articles.xlsx",
          "Out - Response Tabulation.rtf",
          "Out - Responses to Double Coded Articles.xlsx")

file.remove(to.del)

