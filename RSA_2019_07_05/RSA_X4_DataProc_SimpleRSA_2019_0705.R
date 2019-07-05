source("RSA_StratUttOptimization_SimpleRSA_2019_0507.R")
source("RSA_StratUttModelSimple_2019_0505.R")

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x4pilotData <- read.csv("4-pilot-training.csv")

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
llWorkers12 <- matrix(0,length(unique(workerIDs)), 8)
paramsWorkers12 <- matrix(0,length(unique(workerIDs)), 8)

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
##    1 parameter RSA model optimizations... 
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
    optRes1 <- optimize(RSAModelLL1_1simpleRSA, c(0,1e+10), dataWorker)   
    optRes2 <- optimize(RSAModelLL1_2simpleRSA, c(0,1e+10), dataWorker)   
    optRes3 <- optimize(RSAModelLL1_1simpleRSA_notObey.2, c(0,1e+10), dataWorker)   
    optRes4 <- optimize(RSAModelLL1_2simpleRSA_pref2, c(0,1e+10), dataWorker)   
    #print(optRes)
    ## 1 param RSA model
    llWorkers12[workerIndex,3] <- optRes1$objective
    llWorkers12[workerIndex,4] <- optRes2$objective
    llWorkers12[workerIndex,5] <- optRes3$objective
    llWorkers12[workerIndex,6] <- optRes4$objective
    ## resulting parameter choice
    paramsWorkers12[workerIndex,2] <- optRes1$minimum
    paramsWorkers12[workerIndex,3] <- optRes2$minimum
    paramsWorkers12[workerIndex,4] <- optRes3$minimum
    paramsWorkers12[workerIndex,5] <- optRes4$minimum
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
print("Starting optimization with two free parameters Simple RSA model... ")
workerIDs <- x4pilotData$workerid
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
    optRes2n1 <- optim(c(.2, .2), RSAModelLL2_simpleRSA, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,0), upper = c(1e+10,1e+10))
    # print(optRes)
    ## 2 and 3 param RSA model2
    ## max likelihood parameter choice
    llWorkers12[workerIndex,7] <- optRes2n1$value
    ## max likelihood parameter choice
    paramsWorkers12[workerIndex,6] <- optRes2n1$par[1]
    paramsWorkers12[workerIndex,7] <- optRes2n1$par[2]
    ##
    print(llWorkers12[workerIndex,])
    print(paramsWorkers12[workerIndex,])
    workerIndex <- workerIndex + 1
  }
}



## writing out result tables
write.csv(llWorkers12, "x4SimpleRSAModelsKLDivs_2019_0705.csv")
write.csv(paramsWorkers12, "x4SimpleRSAModelsOptParams_2019_0705.csv")
