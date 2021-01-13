
############# Utterance to object probabilities for negative feedback ######################

getObjectPreferencePriorsNegative <- function(consideredUtterances, currentObjects, type, mapUttToObjProbs) {
  objectPreferenceHardPriors <- list()
  for(utt in rep(1:length(consideredUtterances)) ) {
    objectPreferenceHardPriors[[utt]] <- mapUttToObjProbs[utt,]
  }
  objectPreferenceHardPriors[[length(consideredUtterances)+1]] = # Adding an extra row with flat prior over objects
    rep(1/length(currentObjects), length(currentObjects) )
  # soft preferences with uniform choice fusion. 
  softAddProb <- type
  objectPreferenceSoftPriors <- list()
  for(utt in rep(1:(length(consideredUtterances)+1)) ) {
    objectPreferenceSoftPriors[[utt]] <- objectPreferenceHardPriors[[utt]] + softAddProb
#    objectPreferenceSoftPriors[[utt]] <- objectPreferenceSoftPriors[[utt]] / sum(objectPreferenceSoftPriors[[utt]])
  }
  return(objectPreferenceHardPriors)
}

##########################################################################################

## Testing
currentObjects <- c(1,2,6)
validUtterances <- determineValidUtterances(currentObjects)
mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
mapUttToObjProbs <- determineUtteranceToObjectProbabilities(validUtterances, currentObjects, mapObjToUtt, 0)
negative <- determineUtteranceToObjectProbabilitiesNegative(validUtterances, currentObjects, mapObjToUtt, 0)
allObjects[currentObjects,]
allUtterances[validUtterances]
utterance <- 4
objectPreferenceSoftPriors <- getObjectPreferencePriors(validUtterances, currentObjects, 0, mapUttToObjProbs)
pref <- 1
listener <- simpleListener(utterance, mapUttToObjProbs, objectPreferenceSoftPriors[[pref]])
round(listener, 2)
determineUtteranceToObjectProbabilities(validUtterances, currentObjects, mapObjToUtt, 0)