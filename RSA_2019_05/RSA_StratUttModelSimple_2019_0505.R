source("RSA_StratUtt_AllUtterancesAndObjects.R")

# simple KL divergence function 
KLdivergence <- function(p, q) {
  toleranceOffset <- 1e-20
  return(sum(p * (log( (toleranceOffset + p) / (toleranceOffset + q) ) ) ) )
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
# i.e. P(listenersObjectPreferences | utterance, object choice by the listener, prior over preferences)
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
getSpeakerUtterancePriors <- function(relevantUtterances) {
  return(rep(1./length(relevantUtterances), length(relevantUtterances) ) )
}
