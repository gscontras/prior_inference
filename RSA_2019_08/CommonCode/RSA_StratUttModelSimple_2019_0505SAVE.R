source("AllUtterancesAndObjects.R")

# simple KL divergence function 
KLdivergence <- function(p, q) {
  toleranceOffset <- 1e-20
  return( max(0, sum(p * (log( (toleranceOffset + p) / (toleranceOffset + q) ) ) ) ) )
}

# The simple listener function 
# determines the listener's object choice given the 
# objects to choose from and its preferences, determining
# P(obj | utt, listener's object preferences)
simpleListener <- function(utterance, mapUttToObjProbs, listenerObjectPreferences) {
  objPosterior <- mapUttToObjProbs[utterance,] * (listenerObjectPreferences + 1e-100)
  #objPosterior <- mapUttToObjProbs[utterance,] * (as.numeric(listenerObjectPreferences[utterance,]) + 1e-100)
  if(sum(objPosterior)==0) {
    return(objPosterior)
  }
  return(objPosterior)# / sum(objPosterior))
}

# With a setted target feature only some utterances are giving the information needed.
# targetFeature can either be "shape", "texture" or "color".
getTargetFeaturePriors <- function(targetFeature, relevantUtterances){
  targetFeaturePriors <- rep(0, length(relevantUtterances))
  targetFeatureUtts <- c()
  if (targetFeature == "shape"){
    targetFeatureUtts <- c(1:3)
  } else if (targetFeature == "texture"){
    targetFeatureUtts <- c(4:6)
  } else if (targetFeature == "color"){
    targetFeatureUtts <- c(7:9)
  }
  tarFeaRelevantUtts <- intersect(relevantUtterances, targetFeatureUtts)
  numTarFeaRelevantUtts <- length(tarFeaRelevantUtts)
  for(utt in c(1:numTarFeaRelevantUtts)){
    targetFeaturePriors[[which(relevantUtterances == tarFeaRelevantUtts[utt])]] <- 1/numTarFeaRelevantUtts
  }
  return(targetFeaturePriors)
}

# The simple pragmatic speaker considers all "imaginable" (i.e. implemented) 
# preference distributions over objects of the listener.
# Starting with a prior assumption over the possible listener's preferences. 
# It then infers the posterior over these preferences given the listener
# makes a particular object choice. 
# utterance is an index refering to one of the relevantUtterances 
# i.e. P(listener's feature value preferences | utterance, object choice by the listener, prior over preferences)
simplePragmaticSpeaker <- function(utterance, obj, targetFeaturePriors, 
                                   relevantUtterances, currentObjects, mapUttToObjProbs,
                                   objectPreferenceSoftPriors) {
  prefPost <- rep(0, length(relevantUtterances)+1)
  for(pref in c(1:length(targetFeaturePriors))) { # prior over the preferences the speaker is interested in
    if(targetFeaturePriors[pref] > 0) {
      pp <- simpleListener(utterance, mapUttToObjProbs, objectPreferenceSoftPriors[[pref]])
      prefPost[pref] <- pp[obj] * targetFeaturePriors[pref]
    }
  }
  if(sum(prefPost) == 0) { # no evidence for any preferences... -> no inference
    #cat(prefPost)
    return(prefPost)
  }
  #cat(prefPost / sum(prefPost))
  return(prefPost / sum(prefPost))
}


# Speaker utterance prior function (i.e. prior utterance preferences of the speaker)
getSpeakerUtteranceUniformPrior <- function(relevantUtterances) {
  return(rep(1./length(relevantUtterances), length(relevantUtterances) ) )
}



# The ultimate function that determines the utterance preferences of a
# speaker, who wants to learn about the listener's preferences. 
# That is, the speaker considers all relevant utterances given the currentObjects, 
#   considers all prior feature value preferences (of the listener)
#   NOTE: this can be manipulated to make the speaker for example focus on one particular feature type preference
#         by setting the other feature value preferences to zero!
#   and all possible object choices
#   infers the resulting posterior feature value preferences of the listener in the particular scenario via simple RSA
#   computes the KL diverenve between the expected prior and inferred posterior feature value preferences
#   and finally determines the utility value for the considered utterance in the imagined scenario,
#    adding this utility to all scearios for each considered utterance. 
# Essentially, U(utterances | listener's object preference priors) is computed. 
# The utility is determined as the expected information gain between prior and posterior of the 
#    determined listener's object preferences.
# @param targetFeaturePriors = probability mass over all feature values present in the scenario plus a "no preference" case
simpleBestInfGainUtterance <- function(targetFeaturePriors, relevantUtterances, currentObjects, 
                                 mapUttToObjProbs, objectPreferenceSoftPriors, klValueFactor=1) {
  postExpectedUtteranceGain <- rep(0, length(relevantUtterances))
  utterancePrior <- getSpeakerUtteranceUniformPrior(relevantUtterances) # prior over speaker utterances
  #
  for(utt in c(1:length(relevantUtterances))) { # evaluating the expected utility of uttering a particular utterance utt
    prefPostAll <- rep(0, length(targetFeaturePriors))
    for(listenerPref in c(1:length(targetFeaturePriors))) { # prior over the preferences the speaker is interested in
      ### What is the likelihood that this particular preference prior is the correct one?
      prefPost <- 0
      summedCaseProbabilities <- 0 
      for(objChoice in c(1:length(currentObjects)) ) {
        if(mapUttToObjProbs[utt,objChoice] > 0) {
          if(targetFeaturePriors[listenerPref] > 0) { # only pay attention to preferences with non-zero probability
            featurePrefsPosterior <- simplePragmaticSpeaker(utt, objChoice, targetFeaturePriors, 
                                                 relevantUtterances, currentObjects, 
                                                 mapUttToObjProbs, objectPreferenceSoftPriors)
            # print(objPrefPosterior)
            KLvalue <- KLdivergence(targetFeaturePriors, featurePrefsPosterior)
              # log-likelihood interpretation of KLvalue:
            probabilityOfSituation <- mapUttToObjProbs[utt,objChoice] * utterancePrior[utt] * 
              targetFeaturePriors[listenerPref] * objectPreferenceSoftPriors[[listenerPref]][objChoice]
            summedCaseProbabilities <- summedCaseProbabilities + probabilityOfSituation
            prefPost <- prefPost + probabilityOfSituation * exp(klValueFactor * KLvalue)
          }
        }
      }
      if(summedCaseProbabilities > 0) {
        prefPostAll[listenerPref] <- prefPost / summedCaseProbabilities
      }
    }
    postExpectedUtteranceGain[utt] <- sum(prefPostAll)
  }
  if(sum(postExpectedUtteranceGain) == 0) # no gain from any utterance... 
    return( rep(1/length(relevantUtterances), length(relevantUtterances)) )
  return(postExpectedUtteranceGain / sum(postExpectedUtteranceGain))
}


# ## Tests 1:
# notObeyInst <- 1e-10
# softPrefValue <- 0.1
# currentObjects <- c(1,2,3)
# relevantUtterances <- determineValidUtterances(currentObjects)
# mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
# mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances,
#                                                             currentObjects,
#                                                             mapObjToUtt, notObeyInst)
# objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects,
#                                                         softPrefValue, mapUttToObjProbs)
# #pragmaticSpeaker <- function(utterance, obj, targetFeaturePriors,
# #                             relevantUtterances, currentObjects, mapUttToObjProbs,
# #                             objectPreferenceSoftPriors) {
# simplePragmaticSpeaker(4, 1, c(0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
#                  mapUttToObjProbs, objectPreferenceSoftPriors) # sanity check - definite prior, no inf. gain possible
# simplePragmaticSpeaker(4, 1, c(.2, .2, .2, .2, .2, 0), relevantUtterances, currentObjects,
#                  mapUttToObjProbs, objectPreferenceSoftPriors) # NON compliant listener...
# 
# # Tests 2:
 notObeyInst <- 0.0
 softPrefValue <- .01
 currentObjects <- c(1,2,6)
 relevantUtterances <- determineValidUtterances(currentObjects)
 mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
 allUtterancePref <- getAllUtterancePref(c(0, 1/3, 2/3))
 #allUtterancePref <- getAllUtterancePref(c(1/3, 1/3, 1/3))
 targetFeature <- "color"
 targetFeaturePriors <- getTargetFeaturePriors(targetFeature, relevantUtterances)
 mapUttToPref <- getMapUttToPref(relevantUtterances, allObjects, allUtterancePref)
 mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances,
                                                             currentObjects,
                                                             mapObjToUtt, notObeyInst)
 objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects,
                                                         softPrefValue, mapUttToObjProbs, mapUttToPref)
#  simpleBestInfGainUtterance <- function(targetFeaturePriors, relevantUtterances, currentObjects,
#                                  mapUttToObjProbs, objectPreferenceSoftPriors)
# utteranceProbs <-  simpleBestInfGainUtterance(c(0, 0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
#                     mapUttToObjProbs, objectPreferenceSoftPriors) # sanity check - definite prior, no inf. gain possible
#utteranceProbs <- round(simpleBestInfGainUtterance(c(0, 0, 0, 1/2, 1/2, 0, 0), relevantUtterances, currentObjects,
#                     mapUttToObjProbs, objectPreferenceSoftPriors, .75), 3) # sanity check - definite prior, no inf. gain possible
utteranceProbs <- round(simpleBestInfGainUtterance(targetFeaturePriors, relevantUtterances, currentObjects,
                                                   mapUttToObjProbs, objectPreferenceSoftPriors, .75), 3)
maxProb <- max(utteranceProbs)
chosenUtterance <- which(utteranceProbs==maxProb)
listen <- simpleListener(1, mapUttToObjProbs, objectPreferenceSoftPriors[[1]])
# # kldFact <- (c(0:200)-100)/2
# # kldRes <- matrix(0,length(kldFact),6)
# # for(i in c(1:length(kldFact))) {
# #   kldRes[i,] <- round(bestInfGainUtterance(c(.1666, .1666, .1666, .1666, .1666, .1666, 0), relevantUtterances, currentObjects,
# #                              mapUttToObjProbs, objectPreferenceSoftPriors, alpha, kldFact[i]), 3) # sanity check - definite prior, no inf. gain possible
# # }
# # plot(kldFact, kldRes[,1], ylim = c(0:1))
# # lines(kldFact, kldRes[,2], col="black")
# # lines(kldFact, kldRes[,3], col="grey")
# # lines(kldFact, kldRes[,4], col="yellow")
# # lines(kldFact, kldRes[,5], col="orange")
# # lines(kldFact, kldRes[,6], col="blue")
# # 
# # bestInfGainUtterance(c(.1666, .1666, .1666, .1666, .1666, .1666, 0), relevantUtterances, currentObjects,
# #                      mapUttToObjProbs, objectPreferenceSoftPriors, alpha, kldFact[i])
# # 
# # round(pragmaticSpeaker(4, 1, c(.1666, .1666, .1666, .1666, .1666, .1666, 0),
# #                              relevantUtterances, currentObjects, mapUttToObjProbs,
# #                              objectPreferenceSoftPriors, alpha), 3)
# 
# 
