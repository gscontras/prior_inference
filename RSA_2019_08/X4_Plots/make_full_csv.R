setwd("~/Documents/GitHub/prior_inference/RSA_2019_08")

source("CommonCode/RSA_StratUtt.R")
source("CommonCode/getConstCodeStratUtt.R")

### Creating a data frame with all model predictions and human data for experiment 4 ###

## Before you do it make sure that all the files with data are commented out in X4_AvScatterPlot.R ##
## Also comment out source files for the X4_AvScatterPlot.R. That reduces running time (or not...)

########################################################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_SRSAindOpt_fixed00_and_fixed.20.csv") ###
source("X4_Code/X4_AvScatterPlot.R")

########################################################################################

# Simple rsa, Individually optimized, softness at 0, obedience at 0, no alpha

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m1 <-data
m1$Nr <- 1
remove(data)

# Individually optimized, softness at 0.2, obedience at 0, no alpha

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- 0.2
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m2 <-data
m2$Nr <- 2
remove(data)

###########################################################################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_SRSAindOpt_PrefStrengthOpt_obed0_and_obed.2.csv") ###
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

##  Simple rsa, Individually optimized, softness individually optimized, obedience at 0, no alpha

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m3 <-data
m3$Nr <- 3
remove(data)

##  Simple rsa, Individually optimized, softness individually optimized, obedience at 0, no alpha

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- 0.2
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m4 <-data
m4$Nr <- 4
remove(data)

###########################################################################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_SRSAindOpt_PrefandObedOpt_and_fixed.2.2.csv") ###
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

## Simple rsa, Individually optimized, softness individually optimized, obedience at 0, no alpha ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m5 <-data
m5$Nr <- 5
remove(data)

## Simple rsa, Individually optimized, softness fixed at 0.2, obedience at 0.2, no alpha ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- 0.2
data$obedience <- 0.2
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m6 <-data
m6$Nr <- 6
remove(data)


###########################################################################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_SRSAcrossVal_Opt1_and_Opt2.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

## Cross-validated
## Simple rsa, Individually optimized softness, obedience at 0, no alpha ##

################################################################################################
data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "yes"
m7 <-data
m7$Nr <- 7
remove(data)

## Cross-validated
## Simple rsa, Individually optimized softness, individually optimized obedience, no alpha ##


data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "yes"
m8 <-data
m8$Nr <- 8
remove(data)

###########################################################################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_SRSAcrossVal_Opt1obed.1_and_fixed.1.1.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

## Cross-validated
## Simple rsa, Individually optimized softness, obedience at 0.1, no alpha ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- 0.1
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "yes"
m9 <-data
m9$Nr <- 9
remove(data)

## Cross-validated
## Simple rsa, Individually optimized softness, individually optimized obedience, no alpha ##


data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- 0.1
data$obedience <- 0.1
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "yes"
m10 <-data
m10$Nr <- 10
remove(data)

###########################################################################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_SRSAglobaOpt_fixed.1.1_and_OptPrefobedFixed.1.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

## Global optimization
## Simple rsa, softness at 0.1, obedience at 0.1, no alpha ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- 0.1
data$obedience <- 0.1
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m11 <-data
m11$Nr <- 11
remove(data)

## Global optimization
## Simple rsa, softness globally optimized, obedience at 0.1, no alpha ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "globally_opt"
data$obedience <- 0.1
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m12 <-data
m12$Nr <- 12
remove(data)

###########################################################################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_SRSAglobalOpt_OptPrefObedFixed0_and_Opt12.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################


## Global optimization
## Simple rsa, softness globally optimized, obedience at 0, no alpha ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "globally_opt"
data$obedience <- 0
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m13 <-data
m13$Nr <- 13
remove(data)

## Global optimization
## Simple rsa, softness globally optimized, obedience at 0.1, no alpha ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "simpleRSA"
data$softness <- "globally_opt"
data$obedience <- "globally_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m14 <-data
m14$Nr <- 14
remove(data)



########################################## Full RSA ######################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_RSAindOpt_fixed.1.11_and_OptPrefandAlphaObed0.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

## Individual optimization
## Full rsa, softness 0.1, obedience at 0.1, alpha 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- 0.1
data$obedience <- 0.1
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m15 <-data
m15$Nr <- 15
remove(data)

##  Individual optimization
##  Full rsa, softness individually optimized, obedience at 0, alpha individually optimized ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "individually_opt"
data$obedience <- 0
data$alpha <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m16 <-data
m16$Nr <- 16
remove(data)

###########################################################################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_RSAindOpt_OptPrefAndObedAlpha1_and_OptAll3.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

## Individual optimization
## Full rsa, softness individually optimized, obedience individually optimized, alpha at 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m17 <-data
m17$Nr <- 17
remove(data)

##  Individual optimization
##  Full rsa, softness individually optimized, obedience individually optimized, alpha individually optimized ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$alpha <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m18 <-data
m18$Nr <- 18
remove(data)

###########################################################################################################

x4pilotData <- read.csv("X4_Data/x4pDataAugm_RSAcrossVal_fixed001_and_Opt1fixed01.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

## Cross-validated
## Individual optimization
## Full rsa, softness 0, obedience 0, alpha at 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- 0
data$obedience <- 0
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "yes"
m19 <-data
m19$Nr <- 19
remove(data)

##  Cross-validated
##  Individual optimization
##  Full rsa, softness individually optimized, obedience 0, alpha 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "individually_opt"
data$obedience <- 0
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "yes"
m20 <-data
m20$Nr <- 20
remove(data)

###########################################################################################################
# file missing. Generated on October 28, 2019
x4pilotData <- read.csv("X4_Data/x4pDataAugm_RSAcrossVal_Opt12fixed1_and_Opt123.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

## Cross-validated
## Individual optimization
## Full rsa, softness individually optimized, obedience individually optimized, alpha at 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "yes"
m21 <-data
m21$Nr <- 21
remove(data)

##  Cross-validated
##  Individual optimization
##  Full rsa, softness individually optimized, obedience individually optimized, alpha individually optimized ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$alpha <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "yes"
m22 <-data
m22$Nr <- 22
remove(data)

###########################################################################################################
# 
x4pilotData <- read.csv("X4_Data/x4pDataAugm_RSAglobalOpt_Opt1_and__Opt1obed.1.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

##
## Global optimization
## Full rsa, softness globally optimized, obedience 0, alpha at 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "globally_opt"
data$obedience <- 0
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m23 <-data
m23$Nr <- 23
remove(data)

##  Cross-validated
##  Individual optimization
##  Full rsa, softness globally optimized, obedience 0.1, alpha 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "globally_opt"
data$obedience <- 0.1
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m24 <-data
m24$Nr <- 24
remove(data)

###########################################################################################################
# 
x4pilotData <- read.csv("X4_Data/x4pDataAugm_RSAglobalOpt_Opt2_and__Opt12.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

##
## Global optimization
## Full rsa, softness 0, obedience globally optimized, alpha at 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- 0
data$obedience <- "globally_opt" 
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m25 <-data
m25$Nr <- 25
remove(data)

##  Cross-validated
##  Individual optimization
##  Full rsa, softness globally optimized, obedience globally optimized, alpha 1 ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "globally_opt"
data$obedience <- "globally_opt"
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m26 <-data
m26$Nr <- 26
remove(data)

###########################################################################################################
# 
x4pilotData <- read.csv("X4_Data/x4pDataAugm_RSAglobalOpt_Opt13_and__Opt123.csv")
source("X4_Code/X4_AvScatterPlot.R")

###########################################################################################################

##
## Global optimization
## Full rsa, softness globally optimized, obedience 0, alpha globally optimized ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "globally_opt"
data$obedience <- 0 
data$alpha <- "globally_opt"
data$workerData <- workerData
data$model <- rsaModel
data$cross_validated <- "no"
m27 <-data
m27$Nr <- 27
remove(data)

##  Cross-validated
##  Individual optimization
##  Full rsa, softness globally optimized, obedience globally optimized, alpha globally optimized ##

data <- matrix(NA,length(workerData),6)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","type")
data$type <- "fullRSA"
data$softness <- "globally_opt"
data$obedience <- "globally_opt"
data$alpha <- "globally_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m28 <-data
m28$Nr <- 28
remove(data)



## Big data set ####


full <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28)
write.csv(full,"X4_Plots/for_scatterplots_updated.csv")
