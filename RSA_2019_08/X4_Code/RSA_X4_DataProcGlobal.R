source("CommonCode/RSA_StratUtt.R")
source("CommonCode/RSA_StratUttOptimization.R")

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x4pilotData <- read.csv("X4_Data/4-pilot-training.csv")

# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x4pilotData$utterance=="green" | x4pilotData$utterance=="red" | x4pilotData$utterance=="blue", 3,
                  ifelse(x4pilotData$utterance=="solid" | x4pilotData$utterance=="striped" | x4pilotData$utterance=="polka-dotted", 2, 1))
x4pilotData$uttFeat <- uttFeat

q1Feat <- ifelse(x4pilotData$pref1=="green things" | x4pilotData$pref1=="red things" | x4pilotData$pref1=="blue things", 3,
                 ifelse(x4pilotData$pref1=="solid things" | x4pilotData$pref1=="striped things" | x4pilotData$pref1=="polka-dotted things", 2, 
                        ifelse(x4pilotData$pref1=="clouds" | x4pilotData$pref1=="circles" | x4pilotData$pref1=="squares", 1,
                               -1 ) ))
x4pilotData$q1Feat <- q1Feat

q2Feat <- ifelse(x4pilotData$pref4=="green things" | x4pilotData$pref4=="red things" | x4pilotData$pref4=="blue things", 3,
                 ifelse(x4pilotData$pref4=="solid things" | x4pilotData$pref4=="striped things" | x4pilotData$pref4=="polka-dotted things", 2, 
                        ifelse(x4pilotData$pref4=="clouds" | x4pilotData$pref4=="circles" | x4pilotData$pref4=="squares", 1,
                               -1 ) ))
x4pilotData$q2Feat <- q2Feat

## adding the 1-27 target and object2 & object3 code.
temp <- x4pilotData$target
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x4pilotData$targetOC27 <- targetOC27

temp <- x4pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x4pilotData$obj2OC27 <- obj2OC27

temp <- x4pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x4pilotData$obj3OC27 <- obj3OC27

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x4pilotData$X),6)
# postListMat <- matrix(0,length(pilotData$X),9)
# logLik <- rep(0,length(pilotData$X))
for(i in c(1:length(x4pilotData$X))) {
  # objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  # featChoice <- uttFeat[i]
  # postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, .1, 0)
  # print(c(objectConstellation, featChoice))
  #
  subjectResponses[i,1] <- x4pilotData$response1[i] + 1e-100
  subjectResponses[i,2] <- x4pilotData$response2[i] + 1e-100
  subjectResponses[i,3] <- x4pilotData$response3[i] + 1e-100
  subjectResponses[i,4] <- x4pilotData$response4[i] + 1e-100
  subjectResponses[i,5] <- x4pilotData$response5[i] + 1e-100
  subjectResponses[i,6] <- x4pilotData$response6[i] + 1e-100
  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3])
  subjectResponses[i,4:6] <- subjectResponses[i,4:6] / sum(subjectResponses[i,4:6])
}

## ordering the recorded subject responses such that they can be compared directly 
#   to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x4pilotData$X),9)
for(i in c(1:length(x4pilotData$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q1Feat[i]-1)*3)] <- subjectResponses[i,j]
  }
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q2Feat[i]-1)*3)] <- subjectResponses[i,3+j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)


## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x4pilotData$workerid
idMax <- max(workerIDs)
klDivWorkers12 <- matrix(0,length(unique(workerIDs)), 10)
paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 14)
##########
## Starting with simple base model determination:
##
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    klDivWorkers12[workerIndex,1] <- workerID
    paramsWorkers12[workerIndex,1] <- workerID
    ## based model -> no change in preferences!
    klDivWorkers12[workerIndex,2] <- 0 # -2 * length(idICases) * log(1/3)
    for(i in c(1:length(idICases))) {
      for(j in c(1:6)) {
        klDivWorkers12[workerIndex, 2] <- klDivWorkers12[workerIndex, 2] + 
          subjectResponses[idICases[i],j] * 
          (log(subjectResponses[idICases[i],j]) - log(1/3))
      }
    }
    ## done with this worker -> proceed
    workerIndex <- workerIndex + 1
  }
}

#######################
## Optimizing the log likelihoods (maximum log likelihoods for each worker...)
# 1 parameter RSA model optimizations... 

allIndices <- c(1:length(workerIDs))
## generating data matrix for the purpose of optimization
dataWorker <- matrix(0, length(allIndices), 12)
dataWorker[,1] <- targetOC27[allIndices]
dataWorker[,2] <- obj2OC27[allIndices]
dataWorker[,3] <- obj3OC27[allIndices]
dataWorker[,4] <- uttFeat[allIndices]
dataWorker[,5] <- q1Feat[allIndices]
dataWorker[,6] <- q2Feat[allIndices]
dataWorker[,7:12] <- subjectResponses[allIndices,1:6]

# before optimization:         klDivWorkers12[1,3] <- RSAModelLL1(c(.2), dataWorker)
optRes1 <- optimize(RSAModelLL1_1, c(0,1e+10), dataWorker)
optRes1notObey.1 <- optimize(RSAModelLL1_1_notObey.1, c(0,1e+10), dataWorker)
optRes2 <- optimize(RSAModelLL1_2, c(0,1e+10), dataWorker)   
#optRes3 <- optimize(RSAModelLL1_3, c(.01,100), dataWorker)   
#print(optRes)
## 1 param RSA model
klDivWorkers12[1,3] <- optRes1$objective
klDivWorkers12[1,4] <- optRes1notObey.1$objective
klDivWorkers12[1,5] <- optRes2$objective
#klDivWorkers12[1,6] <- optRes3$objective
## resulting parameter choice
paramsWorkers12[1,2] <- optRes1$minimum
paramsWorkers12[1,3] <- optRes1notObey.1$minimum
paramsWorkers12[1,4] <- optRes2$minimum
#paramsWorkers12[1,5] <- optRes3$minimum
####
print(klDivWorkers12[1,])
print(paramsWorkers12[1,])
####

##########
## Optimizing the log likelihoods (maximum log likelihoods for each worker...)
# 2 & 3 parameters 
print("Starting optimization with two free parameters RSA model... ")

## generating data matrix for the purpose of optimization
dataWorker <- matrix(0, length(allIndices), 12)
dataWorker[,1] <- targetOC27[allIndices]
dataWorker[,2] <- obj2OC27[allIndices]
dataWorker[,3] <- obj3OC27[allIndices]
dataWorker[,4] <- uttFeat[allIndices]
dataWorker[,5] <- q1Feat[allIndices]
dataWorker[,6] <- q2Feat[allIndices]
dataWorker[,7:12] <- subjectResponses[allIndices,1:6]

# before optimization:     klDivWorkers12[workerIndex,7] <- RSAModelLL2(c(.2,.2), dataWorker)
#optRes2n1 <- optim(c(.2, .2), RSAModelLL2_n1, method="L-BFGS-B", gr=NULL, dataWorker,
#                   lower = c(0,0.01), upper = c(1e+10,100))
optRes2n2 <- optim(c(.2, .2), RSAModelLL2_n2, method="L-BFGS-B", gr=NULL, dataWorker,
                   lower = c(0,0.01), upper = c(1e+10,100))
optRes2n3 <- optim(c(.2, .2), RSAModelLL2_n3, method="L-BFGS-B", gr=NULL, dataWorker,
                   lower = c(0,0), upper = c(1e+10,1e+10))
optRes3 <- optim(c(.2, .2, .2), RSAModelLL3, method="L-BFGS-B", gr=NULL, dataWorker,
                 lower = c(0,0,0.01), upper = c(1e+10,1e+10,100))
# print(optRes)
## 2 and 3 param RSA model2
## max likelihood parameter choice
#klDivWorkers12[1,7] <- optRes2n1$value
klDivWorkers12[1,8] <- optRes2n2$value
klDivWorkers12[1,9] <- optRes2n3$value
klDivWorkers12[1,10] <- optRes3$value
## max likelihood parameter choice
#paramsWorkers12[1,6] <- optRes2n1$par[1]
#paramsWorkers12[1,7] <- optRes2n1$par[2]
paramsWorkers12[1,8] <- optRes2n2$par[1]
paramsWorkers12[1,9] <- optRes2n2$par[2]
paramsWorkers12[1,10] <- optRes2n3$par[1]
paramsWorkers12[1,11] <- optRes2n3$par[2]
paramsWorkers12[1,12] <- optRes3$par[1]
paramsWorkers12[1,13] <- optRes3$par[2]
paramsWorkers12[1,14] <- optRes3$par[3]
##
print(klDivWorkers12[1,])
print(paramsWorkers12[1,])

## sorting based on 2-parameter RSA optimized version. 
#klDivWorkers12 <- klDivWorkers12[order(klDivWorkers12[,4]),]
#klDivWorkers12[,2:7] <- klDivWorkers12[,2:7]*2
## writing out sorted table
write.csv(klDivWorkers12, "X4_Data/x4KLDivs_fullRSA_globalOpt_2019_1006.csv")
write.csv(paramsWorkers12, "X4_Data/x4Params_fullRSA_globalOpt_2019_1006.csv")

# 
# ### 
# # determining the model predictions after worker-specific model parameter optimization!
# constellationCode <- matrix(0,length(x4pilotData$X),6)
# uniqueCCode <- rep(0, length(x4pilotData$X))
# postListMat <- matrix(0,length(x4pilotData$X),9)
# postListMatNotOpt <- matrix(0,length(x4pilotData$X),9)
# logLik <- rep(0,length(x4pilotData$X))
# workerID <- -1
# for(i in c(1:length(x4pilotData$X))) {
#   objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
#   featChoice <- uttFeat[i]
#   constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
#   uc <- 0
#   for(j in c(1:6)) {
#     uc <- (uc * 10) + constellationCode[i,j]
#   }
#   uniqueCCode[i] <- uc
#   if(workerID != x4pilotData$workerid[i]) {
#     workerID <- x4pilotData$workerid[i]
#     params <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(11:13)]
#     # print(params)
#   }
#   postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
#                                                    abs(params[1]), abs(params[2]), abs(params[3]))
#   postListMatNotOpt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
#                                                    0, 0, 1)
# }
# # now determine expected log likelihoods given the subject responses and the optimized model values.
# logLik <- rep(0,length(x4pilotData$X))
# for(i in c(1:length(x4pilotData$X))) {
#   for(j in 1:3) {  
#     logLik[i] <- logLik[i] - subjectResponses[i,j] * 
#       log(postListMat[i, j+(q1Feat[i]-1)*3])
#   }
#   for(j in 1:3) {  
#     logLik[i] <- logLik[i] - subjectResponses[i,3+j] * 
#       log(postListMat[i, j+(q2Feat[i]-1)*3])
#   }
# }
# 
# ###########
# ## adding all those values to the x4pilotData table.
# subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
# colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
# x4pilotData <- data.frame(x4pilotData, as.data.frame(subjectResponsesOrdered)) 
# 
# postListMat <- round(postListMat, digits=5)
# colnames(postListMat) <- colnames(postListMat, do.NULL = FALSE, prefix = "Post_")
# consCodeAndPosteriors <- data.frame(as.data.frame(postListMat))
# x4pilotData <- data.frame(x4pilotData, consCodeAndPosteriors) 
# 
# postListMatNotOpt <- round(postListMatNotOpt, digits=5)
# colnames(postListMatNotOpt) <- colnames(postListMatNotOpt, do.NULL = FALSE, prefix = "PostNotOpt_")
# consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMatNotOpt))
# x4pilotData <- data.frame(x4pilotData, consCodeAndPosteriorsNO) 
# 
# x4pilotData$CCode <- uniqueCCode
# x4pilotData$logLik <- logLik
# 
# write.csv(x4pilotData, "X4_Data/x4pilotDataAugmented_2019_04_30.csv")
# 
# 
# 
# 
# 
