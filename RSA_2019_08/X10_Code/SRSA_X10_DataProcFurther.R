############################################################################################
model <- "iterative"   ###########################################################################
############################################################################################
#  iterative 
#  non-iterative

# parSetting <- 1

source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/SRSA_StratUttOptimization_iterative.R") 

# Data file from Ella
x10data = read.csv(
  "X10_Data/x10data_annotated.csv",
  header = TRUE,
  na.strings = c("", " ", "NA")
)

# adding feature property codes (which feature was uttereed, which features were questioned)

uttFeat <- x10data$uttFeat
targetFeat <- x10data$targetFeatureNum
targetObject <- x10data$targetObject
object2 <- x10data$object2
object3 <- x10data$object3

workerIDs <- x10data$workerid
idMax <- max(workerIDs)

postListMat1Opt <- matrix(0,length(x10data$workerid),9)
######### September 30, 2020 ###### Teamprojekt 2019 data
workerID <- -1
for(i in c(1:length(x10data$workerid))) {
  objectConstellation <- c(targetObject[i],object2[i],object3[i])
  featChoice <- uttFeat[i]
#  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
#  uc <- 0
#  for(j in c(1:6)) {
#    uc <- (uc * 10) + constellationCode[i,j]
#  }
#  uniqueCCode[i] <- uc
  if(workerID != x10data$workerid[i]) {
    workerID <- x10data$workerid[i]
  }
  validUtterances <- determineValidUtterances(objectConstellation)
  
  if( i == 1) {
    priorPrefAll_1 <- getPreferencesPrior(x10data[i,"targetFeat"])
#    priorPrefAll_2 <- getPreferencesPrior(x10data[i,"targetFeatureNum"])
  } # uniform focussing on the feature type in question.
  
  if(model == "iterative") {
      postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                                 0, 0, priorPrefAll_1)
#      postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
#                                                                                 abs(params12[1]), abs(params12[2]), priorPrefAll_2)
  }else if(model == "non-iterative") {
    if(parSetting == 1) {
      postListMat1Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
                                                                                 0, 0, priorPrefAll_1)
 #     postListMat2Opt[i,] <- determineSpeakerPostListPrefsSimpleRSAWithPriorPref(objectConstellation, featChoice,
#                                                                                 abs(params12[1]), abs(params12[2]), priorPrefAll_2)
    }
  }
}


###########
## adding all those values to the x10data table.

postListMat1Opt <- round(postListMat1Opt, digits=5)
colnames(postListMat1Opt) <- colnames(postListMat1Opt, do.NULL = FALSE, prefix = "Post1_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat1Opt))
x10data <- data.frame(x10data, consCodeAndPosteriors) 

postListMat2Opt <- round(postListMat2Opt, digits=5)
colnames(postListMat2Opt) <- colnames(postListMat2Opt, do.NULL = FALSE, prefix = "Post2_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMat2Opt))
x10data <- data.frame(x10data, consCodeAndPosteriorsNO) 

x10data$CCode <- uniqueCCode

if(iterative12 == 1) {
  if(parSetting == 1) {
    write.csv(x10data, "x10_Data/x10dataAugm_SRSAindOpt_fixed00_andOpt12_iterative.csv")
  }else if(parSetting == 2) {
    write.csv(x10data, "x10_Data/x10dataAugm_SRSAglobalOpt_fixed10_andOpt12_iterative.csv")
  }
}else if(iterative12 == 2) {
  if(parSetting == 1) {
    write.csv(x10data, "x10_Data/x10dataAugm_SRSAindOpt_fixed00_andOpt12_nonIterative.csv")
  }else if(parSetting == 2) {
    write.csv(x10data, "x10_Data/x10dataAugm_SRSAglobalOpt_fixed10_Opt12_nonIterative.csv")
  }
}