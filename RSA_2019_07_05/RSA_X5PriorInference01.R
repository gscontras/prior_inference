source("RSA_StratUttOptimization_2019_0114.R")
source("RSA_StratUttModel_2019_0114.R")

# loading the raw pilot data (as Greg sent it on 2019/01/16)
x5priorInfData <- read.csv("5-combined-unique.csv")
x5priorInfData <- x5priorInfData[which(x5priorInfData$trial_type=="prior_inference"),]

# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x5priorInfData$utterance=="green" | x5priorInfData$utterance=="red" | x5priorInfData$utterance=="blue", 3,
                            ifelse(x5priorInfData$utterance=="solid" | x5priorInfData$utterance=="striped" | x5priorInfData$utterance=="polka-dotted", 2, 1))
x5priorInfData$uttFeat <- uttFeat

q1Feat <- ifelse(x5priorInfData$pref1=="green things" | x5priorInfData$pref1=="red things" | x5priorInfData$pref1=="blue things", 3,
                  ifelse(x5priorInfData$pref1=="solid things" | x5priorInfData$pref1=="striped things" | x5priorInfData$pref1=="polka-dotted things", 2, 
                  ifelse(x5priorInfData$pref1=="clouds" | x5priorInfData$pref1=="circles" | x5priorInfData$pref1=="squares", 1,
                  -1 ) ))
x5priorInfData$q1Feat <- q1Feat

q2Feat <- ifelse(x5priorInfData$pref4=="green things" | x5priorInfData$pref4=="red things" | x5priorInfData$pref4=="blue things", 3,
                 ifelse(x5priorInfData$pref4=="solid things" | x5priorInfData$pref4=="striped things" | x5priorInfData$pref4=="polka-dotted things", 2, 
                        ifelse(x5priorInfData$pref4=="clouds" | x5priorInfData$pref4=="circles" | x5priorInfData$pref4=="squares", 1,
                               -1 ) ))
x5priorInfData$q2Feat <- q2Feat

## adding the 1-27 target and object2 & object3 code.
temp <- x5priorInfData$target
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5priorInfData$targetOC27 <- targetOC27

temp <- x5priorInfData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5priorInfData$obj2OC27 <- obj2OC27

temp <- x5priorInfData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5priorInfData$obj3OC27 <- obj3OC27

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x5priorInfData$X),6)
# postListMat <- matrix(0,length(pilotData$X),9)
# logLik <- rep(0,length(pilotData$X))
for(i in c(1:length(x5priorInfData$X))) {
  # objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  # featChoice <- uttFeat[i]
  # postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, .1, 0)
  # print(c(objectConstellation, featChoice))
  #
  subjectResponses[i,1] <- x5priorInfData$response1[i] + 1e-100
  subjectResponses[i,2] <- x5priorInfData$response2[i] + 1e-100
  subjectResponses[i,3] <- x5priorInfData$response3[i] + 1e-100
  subjectResponses[i,4] <- x5priorInfData$response4[i] + 1e-100
  subjectResponses[i,5] <- x5priorInfData$response5[i] + 1e-100
  subjectResponses[i,6] <- x5priorInfData$response6[i] + 1e-100
  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3])
  subjectResponses[i,4:6] <- subjectResponses[i,4:6] / sum(subjectResponses[i,4:6])
}

## ordering the recorded subject responses such that they can be compared directly 
#   to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x5priorInfData$X),9)
for(i in c(1:length(x5priorInfData$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q1Feat[i]-1)*3)] <- subjectResponses[i,j]
  }
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q2Feat[i]-1)*3)] <- subjectResponses[i,3+j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)


## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x5priorInfData$workerid
idMax <- max(workerIDs)
llWorkers12 <- matrix(0,length(unique(workerIDs)), 9)
paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 13)
##########
## Starting with simple base model determination:
##
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    llWorkers12[workerIndex,1] <- workerID
    paramsWorkers12[workerIndex,1] <- workerID
    ## based model -> no change in preferences!
    llWorkers12[workerIndex,2] <- 0 # -2 * length(idICases) * log(1/3)
    for(i in c(1:length(idICases))) {
      for(j in c(1:6)) {
        llWorkers12[workerIndex, 2] <- llWorkers12[workerIndex, 2] + 
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
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    ## generating data matrix for the purpose of optimization
    dataWorker <- matrix(0, length(idICases), 12)
    dataWorker[,1] <- targetOC27[idICases]
    dataWorker[,2] <- obj2OC27[idICases]
    dataWorker[,3] <- obj3OC27[idICases]
    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- q1Feat[idICases]
    dataWorker[,6] <- q2Feat[idICases]
    dataWorker[,7:12] <- subjectResponses[idICases,1:6]
    
    # before optimization:         llWorkers12[workerIndex,3] <- RSAModelLL1(c(.2), dataWorker)
    optRes1 <- optimize(RSAModelLL1_1, c(0,1e+10), dataWorker)   
    optRes2 <- optimize(RSAModelLL1_2, c(0,1e+10), dataWorker)   
    optRes3 <- optimize(RSAModelLL1_3, c(.01,100), dataWorker)   
    #print(optRes)
    llWorkers12[workerIndex,1] <- workerID
    paramsWorkers12[workerIndex,1] <- workerID
    ## 1 param RSA model
    llWorkers12[workerIndex,2] <- UniformModelKLDiv(dataWorker)
    llWorkers12[workerIndex,3] <- optRes1$objective
    llWorkers12[workerIndex,4] <- optRes2$objective
    llWorkers12[workerIndex,5] <- optRes3$objective
    ## resulting parameter choice
    paramsWorkers12[workerIndex,2] <- optRes1$minimum
    paramsWorkers12[workerIndex,3] <- optRes2$minimum
    paramsWorkers12[workerIndex,4] <- optRes3$minimum
    ####
    print(llWorkers12[workerIndex,])
    print(paramsWorkers12[workerIndex,])
    ####
    workerIndex <- workerIndex + 1
  }
}

##########
## Optimizing the log likelihoods (maximum log likelihoods for each worker...)
# 2 & 3 parameters 
print("Starting optimization with two free parameters RSA model... ")
workerIDs <- x5priorInfData$workerid
idMax <- max(workerIDs)
# llWorkers12 <- matrix(0,length(unique(workerIDs)), 2)
workerIndex <- 1
for(workerID in c(0:idMax)) {
  idICases <- which(workerIDs == workerID)
  if(length(idICases)>0) {
    ## generating data matrix for the purpose of optimization
    dataWorker <- matrix(0, length(idICases), 12)
    dataWorker[,1] <- targetOC27[idICases]
    dataWorker[,2] <- obj2OC27[idICases]
    dataWorker[,3] <- obj3OC27[idICases]
    dataWorker[,4] <- uttFeat[idICases]
    dataWorker[,5] <- q1Feat[idICases]
    dataWorker[,6] <- q2Feat[idICases]
    dataWorker[,7:12] <- subjectResponses[idICases,1:6]
    
# before optimization:     llWorkers12[workerIndex,7] <- RSAModelLL2(c(.2,.2), dataWorker)
    optRes2n1 <- optim(c(.2, .2), RSAModelLL2_n1, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,0.01), upper = c(1e+10,100))
    optRes2n2 <- optim(c(.2, .2), RSAModelLL2_n2, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,0.01), upper = c(1e+10,100))
    optRes2n3 <- optim(c(.2, .2), RSAModelLL2_n3, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,0), upper = c(1e+10,1e+10))
    optRes3 <- optim(c(.2, .2, .2), RSAModelLL3, method="L-BFGS-B", gr=NULL, dataWorker,
                     lower = c(0,0,0.01), upper = c(1e+10,1e+10,100))
    # print(optRes)
    ## 2 and 3 param RSA model2
    ## max likelihood parameter choice
    llWorkers12[workerIndex,6] <- optRes2n1$value
    llWorkers12[workerIndex,7] <- optRes2n2$value
    llWorkers12[workerIndex,8] <- optRes2n3$value
    llWorkers12[workerIndex,9] <- optRes3$value
    ## max likelihood parameter choice
    paramsWorkers12[workerIndex,5] <- optRes2n1$par[1]
    paramsWorkers12[workerIndex,6] <- optRes2n1$par[2]
    paramsWorkers12[workerIndex,7] <- optRes2n2$par[1]
    paramsWorkers12[workerIndex,8] <- optRes2n2$par[2]
    paramsWorkers12[workerIndex,9] <- optRes2n3$par[1]
    paramsWorkers12[workerIndex,10] <- optRes2n3$par[2]
    paramsWorkers12[workerIndex,11] <- optRes3$par[1]
    paramsWorkers12[workerIndex,12] <- optRes3$par[2]
    paramsWorkers12[workerIndex,13] <- optRes3$par[3]
    ##
    print(llWorkers12[workerIndex,])
    print(paramsWorkers12[workerIndex,])
    workerIndex <- workerIndex + 1
  }
}


## writing out sorted table
write.csv(llWorkers12, "x5PriorInfModelsKLDivs_2019_0114.csv")
write.csv(paramsWorkers12, "x5PriorInfModelsOptParams_2019_0114.csv")


### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x5priorInfData$X),6)
uniqueCCode <- rep(0, length(x5priorInfData$X))
postListMat <- matrix(0,length(x5priorInfData$X),9)
postListMatNotOpt <- matrix(0,length(x5priorInfData$X),9)
logLik <- rep(0,length(x5priorInfData$X))
workerID <- -1
for(i in c(1:length(x5priorInfData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
  if(workerID != x5priorInfData$workerid[i]) {
    workerID <- x5priorInfData$workerid[i]
    params <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(11:13)]
    # print(params)
  }
  postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
                                                   abs(params[1]), abs(params[2]), abs(params[3]))
  postListMatNotOpt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
                                                   0, 0, 1)
}
# now determine expected log likelihoods given the subject responses and the optimized model values.
logLik <- rep(0,length(x5priorInfData$X))
for(i in c(1:length(x5priorInfData$X))) {
  for(j in 1:3) {  
    logLik[i] <- logLik[i] - subjectResponses[i,j] * 
      log(postListMat[i, j+(q1Feat[i]-1)*3])
  }
  for(j in 1:3) {  
    logLik[i] <- logLik[i] - subjectResponses[i,3+j] * 
      log(postListMat[i, j+(q2Feat[i]-1)*3])
  }
}

###########
## adding all those values to the x5priorInfData table.
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
x5priorInfData <- data.frame(x5priorInfData, as.data.frame(subjectResponsesOrdered)) 

postListMat <- round(postListMat, digits=5)
colnames(postListMat) <- colnames(postListMat, do.NULL = FALSE, prefix = "Post_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat))
x5priorInfData <- data.frame(x5priorInfData, consCodeAndPosteriors) 

postListMatNotOpt <- round(postListMatNotOpt, digits=5)
colnames(postListMatNotOpt) <- colnames(postListMatNotOpt, do.NULL = FALSE, prefix = "PostNotOpt_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMatNotOpt))
x5priorInfData <- data.frame(x5priorInfData, consCodeAndPosteriorsNO) 

x5priorInfData$CCode <- uniqueCCode
x5priorInfData$logLik <- logLik

write.csv(x5priorInfData, "x5priorInfDataAugmented.csv")





