source("RSA_StratUttModel_2019_0114.R")

doReRunOptimization <- FALSE

# loading the raw pilot data (as Greg sent it on 2018/12/21)
x3pilotData <- read.csv("3-pilot-utterance-choice.csv")

## adding the 1-27 target and object2 & object3 code.
temp <- x3pilotData$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x3pilotData$obj1OC27 <- obj1OC27

temp <- x3pilotData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x3pilotData$obj2OC27 <- obj2OC27

temp <- x3pilotData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
x3pilotData$obj3OC27 <- obj3OC27

## now identify the first column number of the turker sliders and response pairs
sliderIndex <- grep("^pref1", colnames(x3pilotData))
## and use that index to determine all slider identities and corresponding slider values.
sliderUtteranceTypes <- matrix(NA, nrow(x3pilotData), 9)
sliderSetValues <- matrix(NA,  nrow(x3pilotData), 9)
for(i in c(1:9)) {
  colIndex <- sliderIndex + (i-1) * 2
  relRows <- which(!is.na(x3pilotData[[colIndex]]))
  for(j in c(1:length(relRows) ) ) { 
    sliderUtteranceTypes[relRows[j], i] <- which(allUtterancesNew1==x3pilotData[[colIndex]][relRows[j]])
    sliderSetValues[relRows[j], i] <- x3pilotData[[colIndex+1]][relRows[j]]
  }
}
### normalizing the turker estimates and setting them into the corresponding matrix.
bInfGainUttTurkers <- matrix(NA, nrow(x3pilotData), 9)
for(i in c(1:nrow(x3pilotData)) ) {
  s <- sum(sliderSetValues[i,c(1:x3pilotData$numFeatures[i])])
  if(s > 0) {
    sliderSetValues[i,c(1:x3pilotData$numFeatures[i])] <- sliderSetValues[i,c(1:x3pilotData$numFeatures[i])] / s
  }else{
    sliderSetValues[i,c(1:x3pilotData$numFeatures[i])] <- 1 / (x3pilotData$numFeatures[i])
  }
  bInfGainUttTurkers[i, sliderUtteranceTypes[i,c(1:(x3pilotData$numFeatures[i]) )] ] <- sliderSetValues[i,c(1:(x3pilotData$numFeatures[i]) )]
  for(j in c(1:x3pilotData$numFeatures[i])) {
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
  bInfGainUttModel <- matrix(NA, nrow(x3pilotData), 9)
  for(i in c(1:nrow(x3pilotData))) {
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


if(doReRunOptimization) {
  ## recording KL divergence and parameters (base model, 1 param, 2 params)
  workerIDs <- x3pilotData$workerid
  idMax <- max(workerIDs)
  klDivUttWorkers <- matrix(0,length(unique(workerIDs)), 9)
  paramsUttWorkers <- matrix(0,length(unique(workerIDs)), 13)
  
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
        len <- x3pilotData$numFeatures[idICases[i]]
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
  
  #######################
  ## Optimizing (i.e. minimzing) the KL Divergence values for each worker...
  ## starting with 1 parameter RSA model optimizations... 
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
      dataWorker[,4] <- x3pilotData$numFeatures[idICases]
      dataWorker[,5:13] <- bInfGainUttTurkers[idICases,]
      # print(dataWorker)
      # now optimize for one parameter... 
      optRes1 <- optimize(RSAModelUttKLDivParamA, c(0,1e+10), dataWorker)
      print(optRes1$objective)
      optRes2 <- optimize(RSAModelUttKLDivParamB, c(0,1e+10), dataWorker)   
      print(optRes2$objective)
      optRes3 <- optimize(RSAModelUttKLDivParamD4, c(-10,10), dataWorker)   
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
      optRes2n1 <- optim(c(.2, .2), RSAModelUttKLDivParamBD4, method="L-BFGS-B", gr=NULL, dataWorker,
                         lower = c(0,-10), upper = c(1e+10,10))
      print(optRes2n1$value)
      optRes2n2 <- optim(c(.2, .2), RSAModelUttKLDivParamAD4, method="L-BFGS-B", gr=NULL, dataWorker,
                         lower = c(0,-10), upper = c(1e+10,10))
      print(optRes2n2$value)
      optRes2n3 <- optim(c(.2, .2), RSAModelUttKLDivParamAB, method="L-BFGS-B", gr=NULL, dataWorker,
                         lower = c(0,0), upper = c(1e+10,1e+10))
      print(optRes2n3$value)
      optRes3 <- optim(c(.2, .2, 1), RSAModelUttKLDivParamABD4, method="L-BFGS-B", gr=NULL, dataWorker,
                       lower = c(0,0,-10), upper = c(1e+10,1e+10,10))
      print(optRes3$value)
      ## 2 and 3 param RSA model2
      ## max likelihood parameter choice
      klDivUttWorkers[workerIndex,6] <- optRes2n1$value
      klDivUttWorkers[workerIndex,7] <- optRes2n2$value
      klDivUttWorkers[workerIndex,8] <- optRes2n3$value
      klDivUttWorkers[workerIndex,9] <- optRes3$value
      ## max likelihood parameter choice
      paramsUttWorkers[workerIndex,5] <- optRes2n1$par[1]
      paramsUttWorkers[workerIndex,6] <- optRes2n1$par[2]
      paramsUttWorkers[workerIndex,7] <- optRes2n2$par[1]
      paramsUttWorkers[workerIndex,8] <- optRes2n2$par[2]
      paramsUttWorkers[workerIndex,9] <- optRes2n3$par[1]
      paramsUttWorkers[workerIndex,10] <- optRes2n3$par[2]
      paramsUttWorkers[workerIndex,11] <- optRes3$par[1]
      paramsUttWorkers[workerIndex,12] <- optRes3$par[2]
      paramsUttWorkers[workerIndex,13] <- optRes3$par[3]
      ##    
      print(c("Done with worker ",workerIndex," with worder ID ", workerID))
      print(c(klDivUttWorkers[workerIndex,], paramsUttWorkers[workerIndex,]))
      ####
      workerIndex <- workerIndex + 1
    }
  }
  
  write.csv(klDivUttWorkers, "KLDivUttWorkers.csv")
  write.csv(paramsUttWorkers, "KLDivUttParamsWorkers.csv")
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
postListMat <- matrix(0,length(x3pilotData$X),9)
postListMatNotOpt <- matrix(0,length(x3pilotData$X),9)
klDivValues <- matrix(NA,length(x3pilotData$X),3)
workerID <- -1
for(i in c(1:length(x3pilotData$X))) {
  objectConstellation <- c(obj1OC27[i],obj2OC27[i],obj3OC27[i])
  if(workerID != x3pilotData$workerid[i]) {
    workerID <- x3pilotData$workerid[i]
    params <- paramsUttWorkers[which(paramsUttWorkers[,1]==workerID)[1],c(4:4)]
    # print(params)
  }
  ##
  validUtterances <- determineValidUtterances(objectConstellation)
  ## determining the model predictions
  postListMat[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, 0, 0, 1, params[1])
  postListMatNotOpt[i,validUtterances] <- getBestInfGainUttPreferences(objectConstellation, 0, 0, 1, 1)
  ## KL divergence values.... 
  klDivValues[i,1] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], rep(1/length(validUtterances), length(validUtterances)))
  klDivValues[i,2] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMat[i, validUtterances])
  klDivValues[i,3] <- KLdivergence(bInfGainUttTurkers[i, validUtterances], postListMatNotOpt[i, validUtterances])
}

###########
## adding all those values to the x4pilotData table.
subjectResponses <- round(bInfGainUttTurkers, digits=3)
colnames(subjectResponses) <- colnames(subjectResponses, do.NULL = FALSE, prefix = "DPost_")
x3pilotData <- data.frame(x3pilotData, as.data.frame(subjectResponses)) 

postListMat <- round(postListMat, digits=3)
colnames(postListMat) <- colnames(postListMat, do.NULL = FALSE, prefix = "MPost_")
x3pilotData <- data.frame(x3pilotData, as.data.frame(postListMat)) 

klDivValues <- round(klDivValues, digits=3)
colnames(klDivValues) <- colnames(klDivValues, do.NULL = FALSE, prefix = "KLDiv_")
x3pilotData <- data.frame(x3pilotData, as.data.frame(klDivValues)) 

postListMatNotOpt <- round(postListMatNotOpt, digits=3)
colnames(postListMatNotOpt) <- colnames(postListMatNotOpt, do.NULL = FALSE, prefix = "MPostNO_")
x3pilotData <- data.frame(x3pilotData, as.data.frame(postListMatNotOpt)) 

write.csv(x3pilotData, "x3pilotDataAugmented012019.csv")






















