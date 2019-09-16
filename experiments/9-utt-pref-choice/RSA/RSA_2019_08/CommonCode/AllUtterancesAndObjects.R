## All Utterances
# All possible utterances (i.e. object features) that can be handled.
# Here, we assume a 3x3 matrix (three feature types with three expressions each)
allUtterances <- c('cloud', 'circle', 'square', 'solid', 'striped', 'dotted', 'blue', 'red', 'green')
allUtterancesNew1 <- c('cloud', 'circle', 'square', 'solid', 'striped', 'polka-dotted', 'blue', 'red', 'green')
allFeatureTypesNew1 <- c('shape','pattern','color')
allUttMatrix <- matrix(allUtterances, ncol=3, byrow=TRUE)
##
## All Objects
# all object matrix contains 3^3 types of objects.
# the matrix essentially specifies the 3 feature expressions for each object
# thus, the matrix maps objects to matching utterances
# all Objects implements the strings, 
# allObjectsToUtterancesMappings encodes the index mappings
allObjects <- matrix('',27,3)
allObjectsToUtterancesMappings <- matrix(0,27,3)
for(index in c(1:27)) {
  #  print(c(1+((index-1)%%3), 1+floor(((index-1)%%9)/3), 1+floor((index-1)/9)))
  allObjects[index,1] <- allUttMatrix[1,1+((index-1)%%3)]
  allObjects[index,2] <- allUttMatrix[2,1+floor(((index-1)%%9)/3)]
  allObjects[index,3] <- allUttMatrix[3,1+floor((index-1)/9)]
  allObjectsToUtterancesMappings[index,1] <- 1+((index-1)%%3)
  allObjectsToUtterancesMappings[index,2] <- 4+floor(((index-1)%%9)/3)
  allObjectsToUtterancesMappings[index,3] <- 7+floor((index-1)/9)
}

## 
## The relevant utterances are determined given currentObjects
# valid utterances correspond to all features present in the current objects!
determineValidUtterances <- function(currentObjects) {
  validUtterances <- c()
  for(i in c(1:length(currentObjects))) {
    validUtterances <- c(validUtterances, allObjectsToUtterancesMappings[currentObjects[i],])
  }
  validUtterances <- sort(unique(validUtterances))
  return(validUtterances)
}

###
## No preference is encoded with 4, whereas a specific feature expression preference is encode 
# by the respective index value
# get feature-respective priors returns general feature respective priors for all 3 features
# @deprecated (not used currently!)
getFeatureRespectivePriors <- function(softAddProb) {
  featureRespectivePriors <- list()
  for(i in c(1:3)) { ## for all three features generate a preference matrix
    m <- matrix(0,4,3)
    for(fPref in c(1:3)) {
      m[fPref,fPref] <- 1
      m[fPref,] <- m[fPref,] + softAddProb
      m[fPref,] <- m[fPref,] / sum(m[fPref,])
    }
    m[4,] <- 1/3
    featureRespectivePriors[[i]] <- m
  }
  return(featureRespectivePriors)
}

##
## Determining the specifc mapping of objects to utterances that applies given currentObjects
# mapping current objects to utterances
determineObjectToUtterancesMapping <- function(currentObjects) {
  mapObjToUtt <- matrix(0, length(currentObjects), 3)
  for(i in c(1:length(currentObjects))) {
    mapObjToUtt[i,] <- allObjectsToUtterancesMappings[currentObjects[i],]
  }
  return(mapObjToUtt)
}

##
# Determining the corresponding mappings from all relevant utterances to objects
# parameter notObeyInst determines if the instruction does not need to be obeyed (0=full obedience: -> infty  =full instruction ignorance) 
determineUtteranceToObjectProbabilities <- function(consideredUtterances, currentObjects, 
                                                    mapObjToUtt, notObeyInst) {
  mapUttToObj <- list()
  mapUttToObjProbs <- matrix(notObeyInst, length(consideredUtterances), length(currentObjects))
  for(utt in rep(1:length(consideredUtterances)) ) {
    # determine array of all objects that match the utterance
    mapUttToObj[[utt]] = ((which(mapObjToUtt[,] == consideredUtterances[utt])-1)%%nrow(mapObjToUtt))+1
    for(i in rep(1:length(mapUttToObj[[utt]]))) {
      mapUttToObjProbs[utt,mapUttToObj[[utt]][i]] <- mapUttToObjProbs[utt,mapUttToObj[[utt]][i]] + 1;
    }
    mapUttToObjProbs[utt,] <- mapUttToObjProbs[utt,] / sum(mapUttToObjProbs[utt,])# length(mapUttToObj[[utt]])
  }
  return(mapUttToObjProbs)
}

##
## Priors on object preferences - automatically derived from considered utterances
#    (i.e. derived from all relevant features)
# type == 0: hard priors; type > 0: soft prior with specified softness
# returns a list of preference priors for all considered features, i.e. utterances, 
# as well as for "no preference" whatsoever, i.e., uniform prior over all three objects
getObjectPreferencePriors <- function(consideredUtterances, currentObjects, type, mapUttToObjProbs) {
  objectPreferenceHardPriors <- list()
  for(utt in rep(1:length(consideredUtterances)) ) {
    objectPreferenceHardPriors[[utt]] <- mapUttToObjProbs[utt,]
  }
  objectPreferenceHardPriors[[length(consideredUtterances)+1]] = 
    rep(1/length(currentObjects), length(currentObjects) )
  # soft preferences with uniform choice fusion. 
  softAddProb <- type
  objectPreferenceSoftPriors <- list()
  for(utt in rep(1:(length(consideredUtterances)+1)) ) {
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceHardPriors[[utt]] + softAddProb
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceSoftPriors[[utt]] / sum(objectPreferenceSoftPriors[[utt]])
  }
  return(objectPreferenceSoftPriors)
}