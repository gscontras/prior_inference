source("CommonCode/AllUtterancesAndObjects.R")

# simple KL divergence function 
KLdivergence <- function(p, q) {
  toleranceOffset <- 1e-20
  return(max(0, sum(p * (log( (toleranceOffset + p) / (toleranceOffset + q) ) ) ) ) )
}

# The simple listener function 
# determines the listener's object choice given the 
# objects to choose from and its preferences, determining
# P(obj | utt, listener's object preferences)
simpleListener <- function(utterance, mapUttToObjProbs, listenerObjectPreferences) {
  objPosterior <- mapUttToObjProbs[utterance,] * (listenerObjectPreferences + 1e-100)
  if(sum(objPosterior)==0) {
    return(objPosterior)
  }
  return(objPosterior / sum(objPosterior))
}

# The simple pragmatic speaker considers all "imaginable" (i.e. implemented) 
# preference distributions over objects of the listener.
# Starting with a prior assumption over the possible listener's preferences. 
# It then infers the posterior over these preferences given the listener
# makes a particular object choice. 
# utterance is an index refering to one of the relevantUtterances 
# i.e. P(listener's feature value preferences | utterance, object choice by the listener, prior over preferences)
simplePragmaticSpeaker <- function(utterance, obj, preferencesPrior, 
                                   relevantUtterances, currentObjects, mapUttToObjProbs,
                                   objectPreferenceSoftPriors) {
  prefPost <- rep(0, length(relevantUtterances)+1)
  for(pref in c(1:length(preferencesPrior))) { # prior over the preferences the speaker is interested in
    if(preferencesPrior[pref] > 0) {
      pp <- simpleListener(utterance, mapUttToObjProbs, objectPreferenceSoftPriors[[pref]])
      prefPost[pref] <- pp[obj] * preferencesPrior[pref]
    }
  }
  if(sum(prefPost) == 0) { # no evidence for any preferences... -> no inference
    return(prefPost)
  }
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
# @param preferencesPrior = probability mass over all feature values present in the scenario plus a "no preference" case
simpleBestInfGainUtterance <- function(preferencesPrior, relevantUtterances, currentObjects, 
                                       mapUttToObjProbs, objectPreferenceSoftPriors, klValueFactor=1) {
  postUttGPrefPrior <- rep(0, length(relevantUtterances))
  utterancePrior <- getSpeakerUtteranceUniformPrior(relevantUtterances) # prior over speaker utterances
  #
  for(utt in c(1:length(relevantUtterances))) { # evaluating the usage of a particular utterance utt
    prefPostAll <- rep(0, length(preferencesPrior))
    for(pref in c(1:length(preferencesPrior))) { # prior over the preferences the speaker is interested in
      ### What is the likelihood that this particular preference prior is the correct one?
      prefPost <- 0
      for(obj in c(1:length(currentObjects)) ) {
        if(mapUttToObjProbs[utt,obj] > 0) {
          if(preferencesPrior[pref] > 0) { # only pay attention to preferences with non-zero probability
            featurePrefsPosterior <- simplePragmaticSpeaker(utt, obj, preferencesPrior, 
                                                            relevantUtterances, currentObjects, 
                                                            mapUttToObjProbs, objectPreferenceSoftPriors)
            # print(objPrefPosterior)
            KLvalue <- KLdivergence(preferencesPrior, featurePrefsPosterior)
            
            # log-likelihood interpretation of KLvalue:
            prefPost <- prefPost + mapUttToObjProbs[utt,obj] * utterancePrior[utt] * 
              preferencesPrior[pref] * objectPreferenceSoftPriors[[pref]][obj] *
              exp(klValueFactor * KLvalue)
            
          }
        }
      }
      if(prefPost > 0) {
        prefPostAll[pref] <- prefPost
      }
    }
    postUttGPrefPrior[utt] <- sum(prefPostAll)
  }
  if(sum(postUttGPrefPrior) == 0) # no gain from any utterance... 
    return( rep(1/length(relevantUtterances), length(relevantUtterances)) )
  return(postUttGPrefPrior / sum(postUttGPrefPrior))
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
# #pragmaticSpeaker <- function(utterance, obj, preferencesPrior,
# #                             relevantUtterances, currentObjects, mapUttToObjProbs,
# #                             objectPreferenceSoftPriors) {
# # simplePragmaticSpeaker(4, 1, c(0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
# #                  mapUttToObjProbs, objectPreferenceSoftPriors) # sanity check - definite prior, no inf. gain possible
# simplePragmaticSpeaker(4, 1, c(.2, .2, .2, .2, .2, 0), relevantUtterances, currentObjects,
#                  mapUttToObjProbs, objectPreferenceSoftPriors) # NON compliant listener...
# 
# # # Tests 2:
# # notObeyInst <- 0.1
# # softPrefValue <- .01
# # currentObjects <- c(1,2,6)
# # relevantUtterances <- determineValidUtterances(currentObjects)
# # mapObjToUtt <- determineObjectToUtterancesMapping(currentObjects)
# mapUttToObjProbs <- determineUtteranceToObjectProbabilities(relevantUtterances,
#                                                             currentObjects,
#                                                             mapObjToUtt, notObeyInst)
# objectPreferenceSoftPriors <- getObjectPreferencePriors(relevantUtterances, currentObjects,
#                                                         softPrefValue, mapUttToObjProbs)
# # simpleBestInfGainUtterance <- function(preferencesPrior, relevantUtterances, currentObjects,
# #                                 mapUttToObjProbs, objectPreferenceSoftPriors)
# simpleBestInfGainUtterance(c(0, 0, 0, 0, 0, 0, 1), relevantUtterances, currentObjects,
#                     mapUttToObjProbs, objectPreferenceSoftPriors) # sanity check - definite prior, no inf. gain possible
# round(simpleBestInfGainUtterance(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6, 0), relevantUtterances, currentObjects,
#                     mapUttToObjProbs, objectPreferenceSoftPriors), 3) # sanity check - definite prior, no inf. gain possible
# 
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
