##########################################
source("CommonCode/AllUtterancesAndObjects.R")
source("CommonCode/getConstCodeStratUtt.R")


## determines the best information gain utterances based on the valid utterances determined from the currentObjects
getSimpleBestInfGainUttPreferencesIterative <- function(preferencesPriorAll,
                                                        currentObjects, softPrefValue, 
                                                        notObeyInst, klValueFactor, targetFeature) {
  relevantUtterances <- determineValidUtterances(currentObjects)
  mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
  mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances, 
                                                              currentObjects, 
                                                              mapObjToUtt, notObeyInst)
  mapUttToObjDeterministic <- determineUtteranceToObjectProbabilities(relevantUtterances, 
                                                                      currentObjects, 
                                                                      mapObjToUtt, 0)
  objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects, 
                                                          softPrefValue, mapUttToObjDeterministic)
  
  preferencesPrior <- preferencesPriorAll[relevantUtterances]
  
  # Define utterance prior excluding utterances of target feature
  utterancePrior <- rep(0,length(relevantUtterances))
  irrelevantIndices <- which(relevantUtterances>(3*(targetFeature-1)) & relevantUtterances<(3*targetFeature + 1))
  validUtterances <- relevantUtterances[-irrelevantIndices]
  utterancePriorShort <- rep (1/length(validUtterances),length(validUtterances)) 
  utterancePrior[-irrelevantIndices] <- utterancePriorShort
  
#  preferencesPrior <- rep(1/(length(relevantUtterances)), length(relevantUtterances)+1)
#  preferencesPrior[length(relevantUtterances)+1] = 0
  return( simpleBestInfGainUtteranceWithPrefPriorAll(preferencesPriorAll, relevantUtterances, currentObjects, 
                               mapUttToObjProbs, objectPreferenceSoftPriors, 
                               klValueFactor, targetFeature, utterancePrior) )
}


#### actual RSA model Kullback leibler divergence determination for utterance choice experiments.
# data is a matrix with data rows. column structure: [1:OC1,OC2,OC3,4:numUttOptions,7-X:TurkerSliderValues]
SimpleRSAModelUttKLDiv_3params_iterative <- function(data, par1, par2, par3) {
  #   print(c(par1, par2, par3, data))
  klRes <- 0
  for(i in c(1:nrow(data))) {
    if( (i-1)%%4 == 0) {
      preferencesPriorAll <- getPreferencesPrior(data[i,5]) # focussing on the feature type in question.
    }
    ## determining the object and utterance
    currentObjects <- c(data[i,1],data[i,2],data[i,3])
#    numUtterances <- data[i,4]
    uttFeat <- data[i,4]
    targetFeat <- data[i, 5]
    utterance <- data[i, 6]
    relevantUtterances <- determinerelevantUtterances(currentObjects)
    ## determining the model predictions
    #################      Code below not edited ############
    bInfGainUttModel <- rep(NA, 9)
    bInfGainUttModel[relevantUtterances] <- 
      getSimpleBestInfGainUttPreferencesIterative(preferencesPriorAll,currentObjects, abs(param1), 
                                                  abs(param2), abs(param3), targetFeature)
     
      getSimpleBestInfGainUttPreferencesIterative(preferencesPriorAll,currentObjects, abs(param1), 
                                                  abs(param2), abs(param3), targetFeature)
    ## adding the negative log likelihoods
    for(j in c(1:length(relevantUtterances))) {
      klRes <- klRes + data[i, 4+relevantUtterances[j]] * 
        (log(data[i, 4+relevantUtterances[j]] + 1e-100) - log(bInfGainUttModel[relevantUtterances[j]] + 1e-100) )
    }
    #    print(c(data[i, 4+relevantUtterances],9999,bInfGainUttModel[relevantUtterances],8888))
  }
  #  print(c("Result: ", llRes, par1, par2, par3) )
  return(klRes)
}

SimpleRSAModelUttKLDivParamA <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(par[1]), 0, 1))
}
SimpleRSAModelUttKLDivParamB <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivParamK <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0, 0, par[1]))
}
SimpleRSAModelUttKLDivParamBK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivParamAK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), 0, params[2]))
}

SimpleRSAModelUttKLDivParamAK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivParamA.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(par[1]), 0.2, 1))
}
SimpleRSAModelUttKLDivParamB.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, abs(par[1]), 1))
}
SimpleRSAModelUttKLDivParamK.2.2 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, 0.2, par[1]))
}
SimpleRSAModelUttKLDivParamK.2.0 <- function(par, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, 0, par[1]))
}
SimpleRSAModelUttKLDivParamBK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, 0.2, abs(params[1]), params[2]))
}
SimpleRSAModelUttKLDivParamAK.2 <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), 0.2, params[2]))
}
SimpleRSAModelUttKLDivParamAB <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), 1))
}
SimpleRSAModelUttKLDivParamABK <- function(params, data) {
  return(SimpleRSAModelUttKLDiv_3params(data, abs(params[1]), abs(params[2]), params[3]))
}

# Testing optimization function
#currentObjects <- c(1,2,6)
currentObjects <- c(27,5,17)
notObeyInst <- 0
klValueFactor <- 1
softPrefValue <- 0
targetFeature <- 1
trial <- 1
utt <- 4
obj <- 1
if (trial-1%%4 == 0){
  preferencesPriorAll <- getPreferencesPrior(targetFeature)
}

output <-  getSimpleBestInfGainUttPreferencesIterative(
  preferencesPriorAll, currentObjects, 
  softPrefValue, notObeyInst, klValueFactor, targetFeature)
posteriorUtterances <- round(output[[1]],3)
preferencesPriorAll <- round(output[[2]][utt,,obj],3)
