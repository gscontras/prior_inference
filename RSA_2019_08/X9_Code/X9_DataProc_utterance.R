currentObjects <- c(1,2,6)
notObeyInst <- 0
klValueFactor <- 1
softPrefValue <- 0.01
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
posteriorUtterances <- round(output[[1]],4)
preferencesPriorAll <- round(output[[2]][utt,,obj],3)



