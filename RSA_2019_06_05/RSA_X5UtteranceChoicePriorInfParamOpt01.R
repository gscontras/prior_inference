source("RSA_StratUttModel_2019_0114.R")

doReRunOptimization <- TRUE

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x5UttChoiceData <- read.csv("5-combined-unique.csv")
x5UttChoiceData <- x5UttChoiceData[which(x5UttChoiceData$trial_type=="utterance_choice"),]
paramsWorkers12 <- as.matrix(read.csv("x5PriorInfModelsOptParams.csv"))
paramsWorkers12 <- paramsWorkers12[,c(2:ncol(paramsWorkers12))]

## adding the 1-27 target and object2 & object3 code.
temp <- x5UttChoiceData$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5UttChoiceData$obj1OC27 <- obj1OC27

temp <- x5UttChoiceData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5UttChoiceData$obj2OC27 <- obj2OC27

temp <- x5UttChoiceData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x5UttChoiceData$obj3OC27 <- obj3OC27

## now identify the first column number of the turker sliders and response pairs
sliderIndex <- grep("^pref1", colnames(x5UttChoiceData))
## and use that index to determine all slider identities and corresponding slider values.
sliderUtteranceTypes <- matrix(NA, nrow(x5UttChoiceData), 9)
sliderSetValues <- matrix(NA,  nrow(x5UttChoiceData), 9)
for(i in c(1:9)) {
  colIndex <- sliderIndex + (i-1) * 2
  relRows <- which(!is.na(x5UttChoiceData[[colIndex]]))
  for(j in c(1:length(relRows) ) ) { 
    sliderUtteranceTypes[relRows[j], i] <- which(allUtterancesNew1==x5UttChoiceData[[colIndex]][relRows[j]])
    sliderSetValues[relRows[j], i] <- x5UttChoiceData[[colIndex+1]][relRows[j]]
  }
}
### normalizing the turker estimates and setting them into the corresponding matrix.
bInfGainUttTurkers <- matrix(NA, nrow(x5UttChoiceData), 9)
for(i in c(1:nrow(x5UttChoiceData)) ) {
  s <- sum(sliderSetValues[i,c(1:x5UttChoiceData$numFeatures[i])])
  if(s > 0) {
    sliderSetValues[i,c(1:x5UttChoiceData$numFeatures[i])] <- sliderSetValues[i,c(1:x5UttChoiceData$numFeatures[i])] / s
  }else{
    sliderSetValues[i,c(1:x5UttChoiceData$numFeatures[i])] <- 1 / (x5UttChoiceData$numFeatures[i])
  }
  bInfGainUttTurkers[i, sliderUtteranceTypes[i,c(1:(x5UttChoiceData$numFeatures[i]) )] ] <- sliderSetValues[i,c(1:(x5UttChoiceData$numFeatures[i]) )]
  for(j in c(1:x5UttChoiceData$numFeatures[i])) {
    if(is.na(sliderSetValues[i,j])) {
      print("ERRor")
    }
  }
}


#pragmaticSpeaker <- function(utterance, obj, preferencesPrior, 
#                             relevantUtterances, currentObjects, mapUttToObjProbs,
#                             objectPreferenceSoftPriors, alpha) {
#bestInfGainUtterance <- function(preferencesPrior, relevantUtterances, currentObjects, 
#                                 mapUttToObjProbs, objectPreferenceSoftPriors, alpha) {


## determines the best information gain utterances based on the valid utterances determined from the currentObjectConstellation
getBestInfGainUttPreferences <- function(currentObjectConstellation, softPrefValue, notObeyInst, alpha, klValueFactor) {
  validUtterances <- determineValidUtterances(currentObjectConstellation)
  mapObjToUtt <- determineObjectToUtterancesMapping(currentObjectConstellation)
  mapUttToObjProbs <- determineUtteranceToObjectProbabilities(validUtterances, 
                                                              currentObjectConstellation, 
                                                              mapObjToUtt, notObeyInst)
  objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjectConstellation, 
                                                          softPrefValue, mapUttToObjProbs)
  preferencesPrior <- rep(1/length(validUtterances+1), length(validUtterances)+1)
  return( bestInfGainUtterance(preferencesPrior, validUtterances, currentObjectConstellation, 
                                                               mapUttToObjProbs, objectPreferenceSoftPriors, 
                                                                alpha, klValueFactor) )
}


## setting particular parameters
#notObeyInst <- 0
#softPrefValue <- 0.1
#alpha <- 1
## determining all model answer guesses and matching them to the turker data. 
if(FALSE) {
  bInfGainUttModel <- matrix(NA, nrow(x5UttChoiceData), 9)
  for(i in c(1:nrow(x5UttChoiceData))) {
    currentObjectConstellation <- c(obj1OC27[i], obj2OC27[i], obj3OC27[i])
    validUtterances <- determineValidUtterances(currentObjectConstellation)
    bInfGainUttModel[i, validUtterances] <- getBestInfGainUttPreferences(currentObjectConstellation, 
                                                                         softPrefValue, notObeyInst, alpha)
  }
}
############################################################
####### non-optimized version done #########################
####### proceeding to method with model optimization #######
####### individual data optimization #######################
############################################################

#### actual RSA model Kullback leibler divergence determination for utterance choice experiments.
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]
RSAModelUttKLDiv_3params <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getBestInfGainUttPreferences(currentObjects, par1, par2, par3, 1)
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterances))) {
      llRes <- llRes + data[i, 4+validUtterances[j]] * 
        (log(data[i, 4+validUtterances[j]] + 1e-100) - log(bInfGainUttModel[validUtterances[j]] + 1e-100) )
    }
    #    print(c(data[i, 4+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(llRes)
}

RSAModelUttKLDivParamA <- function(par, data) {
  return(RSAModelUttKLDiv_3params(data, abs(par[1]), 0, 1))
}
RSAModelUttKLDivParamB <- function(par, data) {
  return(RSAModelUttKLDiv_3params(data, 0, abs(par[1]), 1))
}
RSAModelUttKLDivParamC <- function(par, data) {
  return(RSAModelUttKLDiv_3params(data, 0, 0, abs(par[1])))
}
RSAModelUttKLDivParamBC <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, 0, abs(params[1]), abs(params[2])))
}
RSAModelUttKLDivParamAC <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, abs(params[1]), 0, abs(params[2])))
}
RSAModelUttKLDivParamAB <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), 1))
}
RSAModelUttKLDivParamABC <- function(params, data) {
  return(RSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), abs(params[3])))
}

#### actual RSA model Kullback leibler divergence determination for utterance choice experiments.
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]
RSAModelUttKLDiv_4params <- function(data, par1, par2, par3, par4) {
  #   print(c(par1, par2, par3, data))
  llRes <- 0
  for(i in c(1:nrow(data))) {
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
    numUtterances <- data[i,4]
    validUtterances <- determineValidUtterances(currentObjects)
    ## determining the model predictions
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[validUtterances] <- getBestInfGainUttPreferences(currentObjects, par1, par2, par3, par4)
    ## adding the negative log likelihoods
    for(j in c(1:length(validUtterances))) {
      llRes <- llRes + data[i, 4+validUtterances[j]] * 
        (log(data[i, 4+validUtterances[j]] + 1e-100) - log(bInfGainUttModel[validUtterances[j]] + 1e-100) )
    }
    #    print(c(data[i, 4+validUtterances],9999,bInfGainUttModel[validUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(llRes)
}
RSAModelUttKLDivParamD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, 0, 1, params[1]))
}
RSAModelUttKLDivParamAD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), 0, 1, params[2]))
}
RSAModelUttKLDivParamBD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, abs(params[1]), 1, params[2]))
}
RSAModelUttKLDivParamCD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, 0, abs(params[1]), params[2]))
}
RSAModelUttKLDivParamBCD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, 0, abs(params[1]), abs(params[2]), params[3]))
}
RSAModelUttKLDivParamACD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), 0, abs(params[2]), params[3]))
}
RSAModelUttKLDivParamABD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), abs(params[2]), 1, params[3]))
}
RSAModelUttKLDivParamABCD4 <- function(params, data) {
  return(RSAModelUttKLDiv_4params(data, abs(params[1]), abs(params[2]), abs(params[3]), params[4]))
}


# in the case, the worker ID is the first parameter
RSAModelUttKLDivParamD4AlphaBetaGamma <- function(params, data) {
#  print(c(wAlpha, wBeta, wGamma, params[1]))
  return(RSAModelUttKLDiv_4params(data, wBeta, wGamma, wAlpha, params[1]))
}


if(doReRunOptimization) {
  ## recording KL divergence and parameters (base model, 1 param, 2 params)
  workerIDs <- x5UttChoiceData$workerid
  idMax <- max(workerIDs)
  klDivUttWorkers <- matrix(0,length(unique(workerIDs)), 5)
  paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 4)
  
  #######################
  ## Starting with simple base model determination:
  #######################
  workerIndex <- 1
  for(workerID in c(0:idMax)) {
    idICases <- which(workerIDs == workerID)
    if(length(idICases)>0) {
      klDivUttWorkers[workerIndex,1] <- workerID
      paramsUttWorkers[workerIndex,1] <- workerID
      ## based model -> no change in preferences!
      klDivUttWorkers[workerIndex,2] <- 0
      for(i in c(1:length(idICases))) {
        len <- x5UttChoiceData$numFeatures[idICases[i]]
        for(j in c(1:len) ) {
          klDivUttWorkers[workerIndex, 2] <- klDivUttWorkers[workerIndex, 2] + 
            sliderSetValues[idICases[i],j] * 
            (log(sliderSetValues[idICases[i],j] + 1e-100) - log(1/len))
          if(is.na(klDivUttWorkers[workerIndex, 2])) {
            print("Is NA!???")
            print(c(sliderSetValues[idICases[i],j], log(1/len), i, j, len))
            j <- 10
            i <- length(idICases) +1 
          }
        }
      }
      ## done with this worker -> proceed
      workerIndex <- workerIndex + 1
    }
  }
  
  wAlpha <- 1
  wBeta <- 0
  wGamma <- 0
  #######################
  ## Optimizing (i.e. minimzing) the KL Divergence values for each worker...
  ## doing only the last parameter and taking the other parameters from the optimized prior inf trials.
  # data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X(max 15):TurkerSliderValues]
  workerIndex <- 1
  for(workerID in c(0:idMax)) {
    idICases <- which(workerIDs == workerID)
    if(length(idICases)>0) {
      ## generating data matrix for the purpose of optimization
      dataWorker <- matrix(0, length(idICases), 13)
      dataWorker[,1] <- obj1OC27[idICases]
      dataWorker[,2] <- obj2OC27[idICases]
      dataWorker[,3] <- obj3OC27[idICases]
      dataWorker[,4] <- x5UttChoiceData$numFeatures[idICases]
      dataWorker[,5:13] <- bInfGainUttTurkers[idICases,]
      # print(dataWorker)
      # now optimize for one parameter... 
      wAlpha <- 1
      wBeta <- 0
      wGamma <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 3]]
      optRes1 <- optimize(RSAModelUttKLDivParamD4AlphaBetaGamma, c(-10,10), dataWorker)
      print(optRes1$objective)
      #
      wAlpha <- 1
      wBeta <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 9]]
      wGamma <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 10]]
      optRes2 <- optimize(RSAModelUttKLDivParamD4AlphaBetaGamma, c(-10,10), dataWorker)   
      print(optRes2$objective)
      # 
      wAlpha <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 13]]
      wBeta <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 11]]
      wGamma <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 12]]
      optRes3 <- optimize(RSAModelUttKLDivParamD4AlphaBetaGamma, c(-10,10), dataWorker)   
      print(optRes3$objective)
      
      ## 1 param RSA Utt model
      klDivUttWorkers[workerIndex,3] <- optRes1$objective
      klDivUttWorkers[workerIndex,4] <- optRes2$objective
      klDivUttWorkers[workerIndex,5] <- optRes3$objective
      ## resulting parameter choice
      paramsUttWorkers[workerIndex,2] <- optRes1$minimum
      paramsUttWorkers[workerIndex,3] <- optRes2$minimum
      paramsUttWorkers[workerIndex,4] <- optRes3$minimum
      ####
      ##    
      print(c("Done with worker (parsams paratially from prior inf) ",workerIndex," with worder ID ", workerID))
      print(c(klDivUttWorkers[workerIndex,], paramsUttWorkers[workerIndex,]))
      ####
      workerIndex <- workerIndex + 1
    }
  }
  
  write.csv(klDivUttWorkers, "x5UttChoicePriorInfOptModelsKLDivs.csv")
  write.csv(paramsUttWorkers, "x5UttChoicePriorInfOptModelsParams.csv")
} ## done with optimization routines.

#####################################################################################################
##############  TIME To determine and record the actual (optimized) Model Predictions ###############
#####################################################################################################


if(!doReRunOptimization) {
  ## reloading optimization when NOT just optimized 
  klDivUttWorkers <- as.matrix(read.csv("KLDivUttWorkers.csv"))
  klDivUttWorkers <- klDivUttWorkers[ , 2:ncol(klDivUttWorkers)]
  paramsUttWorkers <- as.matrix(read.csv("KLDivUttParamsWorkers.csv"))
  paramsUttWorkers <- paramsUttWorkers[ , 2:ncol(paramsUttWorkers)]
}


### 
# determining the model predictions after worker-specific model parameter optimization!
postListMatNotOpt <- matrix(0,length(x5UttChoiceData$X),9)
postListMat1 <- matrix(0,length(x5UttChoiceData$X),9)
postListMat2 <- matrix(0,length(x5UttChoiceData$X),9)
postListMat3 <- matrix(0,length(x5UttChoiceData$X),9)
klDivValues <- matrix(NA,length(x5UttChoiceData$X),5)
workerID <- -1
for(i in c(1:length(x5UttChoiceData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  if(workerID != x5UttChoiceData$workerid[i]) {
    workerID <- x5UttChoiceData$workerid[i]
    allParams <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],]
#     params <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(4:4)]
    wAlpha1 <- 1
    wBeta1 <- 0
    wGamma1 <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 3]]
    wAlpha2 <- 1
    wBeta2 <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 9]]
    wGamma2 <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 10]]
    wAlpha3 <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 13]]
    wBeta3 <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 11]]
    wGamma3 <- paramsWorkers12[[which(paramsWorkers12[,1]==workerID)[1], 12]]
    # print(params)
  }
  ##
  validUtterances <- determineValidUtterances(objectConstellation)
  ## determining the model predictions
  postListMatNotOpt[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, 0, 0, 1, 1)
  postListMat1[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, wBeta1, wGamma1, wAlpha1, allParams[2])
  postListMat2[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, wBeta2, wGamma2, wAlpha2, allParams[3])
  postListMat3[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, wBeta3, wGamma3, wAlpha3, allParams[4])
  ## KL divergence values.... 
  klDivValues[i,1] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], rep(1/length(validUtterances), length(validUtterances)))
  klDivValues[i,2] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMatNotOpt[i, validUtterances])
  klDivValues[i,3] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMat1[i, validUtterances])
  klDivValues[i,4] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMat2[i, validUtterances])
  klDivValues[i,5] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMat3[i, validUtterances])
}

###########
## adding all those values to the x5 data table.
subjectResponses <- round(bInfGainUttTurkers, digits=3)
colnames(subjectResponses) <- colnames(subjectResponses, do.NULL = FALSE, prefix = "DPost_")
x5UttChoiceData <- data.frame(x5UttChoiceData, as.data.frame(subjectResponses)) 

postListMatNotOpt <- round(postListMatNotOpt, digits=3)
colnames(postListMatNotOpt) <- colnames(postListMatNotOpt, do.NULL = FALSE, prefix = "MPostNO_")
x5UttChoiceData <- data.frame(x5UttChoiceData, as.data.frame(postListMatNotOpt)) 

postListMat1 <- round(postListMat1, digits=3)
colnames(postListMat1) <- colnames(postListMat1, do.NULL = FALSE, prefix = "MPost1_")
x5UttChoiceData <- data.frame(x5UttChoiceData, as.data.frame(postListMat1)) 

postListMat2 <- round(postListMat2, digits=3)
colnames(postListMat2) <- colnames(postListMat2, do.NULL = FALSE, prefix = "MPost2_")
x5UttChoiceData <- data.frame(x5UttChoiceData, as.data.frame(postListMat2)) 

postListMat3 <- round(postListMat3, digits=3)
colnames(postListMat3) <- colnames(postListMat3, do.NULL = FALSE, prefix = "MPost3_")
x5UttChoiceData <- data.frame(x5UttChoiceData, as.data.frame(postListMat3)) 

klDivValues <- round(klDivValues, digits=3)
colnames(klDivValues) <- colnames(klDivValues, do.NULL = FALSE, prefix = "KLDiv_")
x5UttChoiceData <- data.frame(x5UttChoiceData, as.data.frame(klDivValues)) 

write.csv(x5UttChoiceData, "x5DataAugmentedUttChoicePriorInfPO.csv")






















