simpleBestInfGainUtteranceWithPrefPriorAll <-
  function(preferencesPriorAll,
           relevantUtterances,
           currentObjects,
           mapUttToObjProbs,
           objectPreferenceSoftPriors,
           klValueFactor = 1,
           targetFeature) {
    postUttGPrefPrior <- rep(0, length(relevantUtterances))
    
    # TODO Need to modify utterancePrior to exclude target feature utterances
    irrelevantIndices <- which(relevantUtterances>(3*(targetFeature-1)) & relevantUtterances<(3*targetFeature + 1))
    validUtterances <- relevantUtterances[-irrelevantIndices]
    utterancePriorFull <- rep(0,length(relevantUtterances))
    utterancePrior <- rep (1/length(validUtterances),length(validUtterances)) # prior over speaker utterances excluding utterances of target feature
    utterancePriorFull[-irrelevantIndices] <- utterancePrior
    
    for (utt in c(1:length(utterancePriorFull))) {
      preferencesPrior <- preferencesPriorAll[relevantUtterances]
      # evaluating the usage of a particular utterance utt
      prefPostAll <- rep(0, length(relevantUtterances))
      for (pref in c(1:length(preferencesPrior))) {
        # prior over the preferences the speaker is interested in
        ### What is the likelihood that this particular preference prior is the correct one?
        prefPost <- 0
        for (obj in c(1:length(currentObjects))) {
          if (utterancePriorFull[utt] > 0){ 
            if (mapUttToObjProbs[utt, obj] > 0) {
              if (preferencesPrior[pref] > 0) {
                # only pay attention to preferences with non-zero probability
                featurePrefsPosterior <-
                  simplePragmaticSpeakerWithPrefPriorAll(
                    utt,
                    obj,
                    preferencesPriorAll,
                    relevantUtterances,
                    currentObjects,
                    mapUttToObjProbs,
                    objectPreferenceSoftPriors
                  )
                # Record featurePrefsPosterior to use as prior over feature values for the next trial
                
                #            print(preferencesPrior)
                #            print(featirePrefsPosterior)
                KLvalue <-
                  KLdivergence(preferencesPriorAll, featurePrefsPosterior)
                
                # log-likelihood interpretation of KLvalue:
                prefPost <- prefPost +  mapUttToObjProbs[utt, obj] *
                  objectPreferenceSoftPriors[[pref]][obj] *
                  utterancePriorFull[utt] *  preferencesPrior[pref] *
                  exp(klValueFactor * KLvalue)
                preferencesPriorAll <- featurePrefsPosterior
              }
            }
          }
        }
        if (prefPost > 0) {
          prefPostAll[pref] <- prefPost
        }
      }
      postUttGPrefPrior[utt] <- sum(prefPostAll)
    }
    if (sum(postUttGPrefPrior) == 0)
      # no gain from any utterance...
      return(rep(1 / length(relevantUtterances), length(relevantUtterances)))
    return(postUttGPrefPrior / sum(postUttGPrefPrior))
    # now use max to determine the utterance that gives the best information gain
    # pickedUtterance <- which(postUttGPrefPrior == max(postUttGPrefPrior))[[1]]
  }