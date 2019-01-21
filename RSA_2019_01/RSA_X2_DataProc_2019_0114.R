source("RSA_StratUttOptimization_2019_0114.R")
source("RSA_StratUttModel_2019_0114.R")

# loading the raw pilot data (as Greg sent it on 2018/11)
x2pilotData<- read.csv("2-pilot.csv")

# adding feature property codes (which feature was uttereed, which features were questioned)
uttFeat <- ifelse(x2pilotData$utterance=="green" | x2pilotData$utterance=="red" | x2pilotData$utterance=="blue", 3,
                            ifelse(x2pilotData$utterance=="solid" | x2pilotData$utterance=="striped" | x2pilotData$utterance=="polka-dotted", 2, 1))
x2pilotData$uttFeat <- uttFeat

q1Feat <- ifelse(x2pilotData$pref1=="green things" | x2pilotData$pref1=="red things" | x2pilotData$pref1=="blue things", 3,
                  ifelse(x2pilotData$pref1=="solid things" | x2pilotData$pref1=="striped things" | x2pilotData$pref1=="polka-dotted things", 2, 
                  ifelse(x2pilotData$pref1=="clouds" | x2pilotData$pref1=="circles" | x2pilotData$pref1=="squares", 1,
                  -1 ) ))
x2pilotData$q1Feat <- q1Feat

q2Feat <- ifelse(x2pilotData$pref4=="green things" | x2pilotData$pref4=="red things" | x2pilotData$pref4=="blue things", 3,
                 ifelse(x2pilotData$pref4=="solid things" | x2pilotData$pref4=="striped things" | x2pilotData$pref4=="polka-dotted things", 2, 
                        ifelse(x2pilotData$pref4=="clouds" | x2pilotData$pref4=="circles" | x2pilotData$pref4=="squares", 1,
                               -1 ) ))
x2pilotData$q2Feat <- q2Feat

## adding the 1-27 target and object2 & object3 code.
temp <- x2pilotData$target
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x2pilotData$targetOC27 <- targetOC27

temp <- x2pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x2pilotData$obj2OC27 <- obj2OC27

temp <- x2pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x2pilotData$obj3OC27 <- obj3OC27

## now determining the recorded subject responses 
subjectResponses <- matrix(0,length(x2pilotData$X),6)
# postListMat <- matrix(0,length(pilotData$X),9)
# logLik <- rep(0,length(pilotData$X))
for(i in c(1:length(x2pilotData$X))) {
  # objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  # featChoice <- uttFeat[i]
  # postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, .1, 0)
  # print(c(objectConstellation, featChoice))
  #
  subjectResponses[i,1] <- x2pilotData$response1[i] + 1e-100
  subjectResponses[i,2] <- x2pilotData$response2[i] + 1e-100
  subjectResponses[i,3] <- x2pilotData$response3[i] + 1e-100
  subjectResponses[i,4] <- x2pilotData$response4[i] + 1e-100
  subjectResponses[i,5] <- x2pilotData$response5[i] + 1e-100
  subjectResponses[i,6] <- x2pilotData$response6[i] + 1e-100
  subjectResponses[i,1:3] <- subjectResponses[i,1:3] / sum(subjectResponses[i,1:3])
  subjectResponses[i,4:6] <- subjectResponses[i,4:6] / sum(subjectResponses[i,4:6])
}

## ordering the recorded subject responses such that they can be compared directly 
#   compared to the model predictions 
##             (particularly for visual comparison in the table) 
subjectResponsesOrdered <- matrix(NA ,length(x2pilotData$X),9)
for(i in c(1:length(x2pilotData$X))) {
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q1Feat[i]-1)*3)] <- subjectResponses[i,j]
  }
  for(j in 1:3) {  
    subjectResponsesOrdered[i, (j+(q2Feat[i]-1)*3)] <- subjectResponses[i,3+j]
  }
}
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)


## recording KL divergence and parameters (base model, 1 param, 2 params)
workerIDs <- x2pilotData$workerid
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
    ## 1 param RSA model
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
workerIDs <- x2pilotData$workerid
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


## sorting based on 2-parameter RSA optimized version. 
#llWorkers12 <- llWorkers12[order(llWorkers12[,4]),]
#llWorkers12[,2:7] <- llWorkers12[,2:7]*2
## writing out sorted table
write.csv(llWorkers12, "x2ModelsKLDivs_2019_0114.csv")
write.csv(paramsWorkers12, "x2ModelsOptParams_2019_0114.csv")


### 
# determining the model predictions after worker-specific model parameter optimization!
constellationCode <- matrix(0,length(x2pilotData$X),6)
uniqueCCode <- rep(0, length(x2pilotData$X))
postListMat <- matrix(0,length(x2pilotData$X),9)
postListMatNotOpt <- matrix(0,length(x2pilotData$X),9)
logLik <- rep(0,length(x2pilotData$X))
workerID <- -1
for(i in c(1:length(x2pilotData$X))) {
  objectConstellation <- c(targetOC27[i],obj2OC27[i],obj3OC27[i])
  featChoice <- uttFeat[i]
  constellationCode[i,] <- getConstellationCode(objectConstellation, featChoice)[[1]]
  uc <- 0
  for(j in c(1:6)) {
    uc <- (uc * 10) + constellationCode[i,j]
  }
  uniqueCCode[i] <- uc
  if(workerID != x2pilotData$workerid[i]) {
    workerID <- x2pilotData$workerid[i]
    params <- paramsWorkers12[which(paramsWorkers12[,1]==workerID)[1],c(11:13)]
    # print(params)
  }
  postListMat[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
                                                   abs(params[1]), abs(params[2]), abs(params[3]))
  postListMatNotOpt[i,] <- determineSpeakerPostListPrefs(objectConstellation, featChoice, 
                                                   0, 0, 1)
}
# now determine expected log likelihoods given the subject responses and the optimized model values.
logLik <- rep(0,length(x2pilotData$X))
for(i in c(1:length(x2pilotData$X))) {
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
## adding all those values to the x4pilotData table.
subjectResponsesOrdered <- round(subjectResponsesOrdered, digits=5)
colnames(subjectResponsesOrdered) <- colnames(subjectResponsesOrdered, do.NULL = FALSE, prefix = "DataPost_")
x2pilotData <- data.frame(x2pilotData, as.data.frame(subjectResponsesOrdered)) 

postListMat <- round(postListMat, digits=5)
colnames(postListMat) <- colnames(postListMat, do.NULL = FALSE, prefix = "Post_")
consCodeAndPosteriors <- data.frame(as.data.frame(postListMat))
x2pilotData <- data.frame(x2pilotData, consCodeAndPosteriors) 

postListMatNotOpt <- round(postListMatNotOpt, digits=5)
colnames(postListMatNotOpt) <- colnames(postListMatNotOpt, do.NULL = FALSE, prefix = "PostNotOpt_")
consCodeAndPosteriorsNO <- data.frame(as.data.frame(postListMatNotOpt))
x2pilotData <- data.frame(x2pilotData, consCodeAndPosteriorsNO) 

x2pilotData$CCode <- uniqueCCode
x2pilotData$logLik <- logLik

write.csv(x2pilotData, "x2pilotDataAugmented_2019_0114.csv")

