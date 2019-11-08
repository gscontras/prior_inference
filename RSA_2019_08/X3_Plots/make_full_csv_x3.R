setwd("~/Documents/GitHub/prior_inference/RSA_2019_08")

### Creating a data frame with all model predictions and human data for experiment 3 ###

## Before you do it make sure that all the files with data are commented out in X3_AvScatterPlot.R ##

########################################################################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_RSAindOpt_fixed_and_PrefOnly.csv") ###
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Full rsa, softness at 0, obedience at 0, alpha at 1, kl-factor at 1

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "fullRSA"
data$softness <- 0
data$obedience <- 0
data$kl_factor <- 1
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "no"
m1 <-data
m1$Nr <- 1
remove(data)

# Full rsa, softness individually optimized, obedience at 0, alpha at 1, kl-factor at 1

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "fullRSA"
data$softness <- "individually_opt"
data$obedience <- 0
data$kl_factor <- 1
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m2 <-data
m2$Nr <- 2
remove(data)


########################################################################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_RSAindOpt_KappaOnly_and_ObedAndKappa.csv") ###
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Full rsa, softness at 0, obedience at 0, alpha at 1, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "fullRSA"
data$softness <- 0
data$obedience <- 0
data$kl_factor <- "individually_opt"
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "no"
m3 <-data
m3$Nr <- 3
remove(data)

# Full rsa, softness at 0, obedience individually optimized, alpha at 1, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "fullRSA"
data$softness <- 0
data$obedience <- "individually_opt"
data$kl_factor <- "individually_opt"
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m4 <-data
m4$Nr <- 4
remove(data)

########################################################################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_RSAindOpt_PrefAndKappa_and_PrefObedAndKappa.csv") 
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Full rsa, softness individually optimized, obedience at 0, alpha at 1, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "fullRSA"
data$softness <- "individually_opt"
data$obedience <- 0
data$kl_factor <- "individually_opt"
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "no"
m5 <-data
m5$Nr <- 5
remove(data)

# Full rsa, softness individually optimized, obedience individually optimized, alpha at 1, 
# kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "fullRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$kl_factor <- "individually_opt"
data$alpha <- 1
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m6 <-data
m6$Nr <- 6
remove(data)

###############################   Simple RSA    ########################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_fixed001_and_fixed000.csv") 
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Simple rsa, softness at 0, obedience at 0, kl-factor at 1

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- 0
data$kl_factor <- 1
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "no"
m7 <-data
m7$Nr <- 7
remove(data)

# Simple rsa, softness at 0, obedience at 0, kl-factor at 0

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- 0
data$kl_factor <- 0
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m8 <-data
m8$Nr <- 8
remove(data)

########################################################################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_prefOnly_and_obedOnly.csv")
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Simple rsa, softness individually optimized, obedience at 0, kl-factor at 1

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- 0
data$kl_factor <- 1
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "no"
m9 <-data
m9$Nr <- 9
remove(data)

# Simple rsa, softness at 0, obedience individually optimized, kl-factor at 1

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- "individually_opt"
data$kl_factor <- 1
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m10 <-data
m10$Nr <- 10
remove(data)

########################################################################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_kappaOnly_and_obedAndKappa.csv") 
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Simple rsa, softness at 0, obedience at 0, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- 0
data$kl_factor <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "no"
m11 <-data
m11$Nr <- 11
remove(data)

# Simple rsa, softness at 0, obedience individually optimized, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- "individually_opt"
data$kl_factor <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m12 <-data
m12$Nr <- 12
remove(data)

########################################################################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_prefAndKappa_and_prefObedAndKappa.csv")
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Simple rsa, softness at 0, obedience at 0, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- 0
data$kl_factor <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "no"
m13 <-data
m13$Nr <- 13
remove(data)

# Simple rsa, softness at 0, obedience individually optimized, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$kl_factor <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m14 <-data
m14$Nr <- 14
remove(data)

 #################################### Cross-validated   ################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_SRSAcrossVal_KappaOnly_and_PrefAndKappa.csv")
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Simple rsa, softness at 0, obedience at 0, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- 0
data$kl_factor <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "yes"
m15 <-data
m15$Nr <- 15
remove(data)

# Simple rsa, softness individually optimized, obedience at 0, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- 0
data$kl_factor <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "yes"
m16 <-data
m16$Nr <- 16
remove(data)

#######################################################################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_SRSAcrossVal_ObedAndKappa_and_PrefObedAndKappa.csv")
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Simple rsa, softness at 0, obedience at 0, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- "individually_opt"
data$kl_factor <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "yes"
m17 <-data
m17$Nr <- 17
remove(data)

# Simple rsa, softness individually optimized, obedience at 0, kl-factor individually optimized

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- "individually_opt"
data$obedience <- "individually_opt"
data$kl_factor <- "individually_opt"
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "yes"
m18 <-data
m18$Nr <- 18
remove(data)

#######################################################################################

x3pilotData <- read.csv("X3_Data/x3pDataAugm_simpleRSAindOpt_fixed00-1_and_uniform.csv")
source("X3_Code/X3_AvScatterPlot.R")

########################################################################################

# Simple rsa, softness at 0, obedience at 0, kl-factor -1

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "simpleRSA"
data$softness <- 0
data$obedience <- 0
data$kl_factor <- - 1
data$workerData <- workerData
data$model <- rsaModel1
data$cross_validated <- "yes"
m19 <-data
m19$Nr <- 19
remove(data)

# Uniform model

data <- matrix(NA,length(workerData),7)
data <- as.data.frame(data)
colnames(data) <- c("workerData","model","softness","obedience","alpha","kl_factor","type")
data$type <- "uniform"
data$softness <- 0
data$obedience <- 0
data$kl_factor <- 0
data$workerData <- workerData
data$model <- rsaModel2
data$cross_validated <- "no"
m20 <-data
m20$Nr <- 20
remove(data)


## Big data set ####

full <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20)
write.csv(full,"X3_Plots/for_scatterplots_x3_updated.csv")
