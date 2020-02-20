#library(knitr)
rm(list = ls())
source("CommonCode/SRSA_StratUtt.R")
source("CommonCode/AllUtterancesAndObjects.R")

library(gridExtra)
library(magrittr)
library(tidyverse)
library(rmarkdown)
#if (!require(devtools))
#  install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")

whichDataSet <- 0

if (whichDataSet == 0) {
  # pure data
  inputData = read.csv(
    "X9_Data/ella_total_allDataCleaned.csv",
    header = TRUE,
    na.strings = c("", " ", "NA")
  )
  totalWorker <-
    length(unique(inputData$workerid)) - 1 # total worker is the highest workerid
} else if (whichDataSet == 1) {
  inputData = read.csv("ella_total_trials.csv",
                       header = TRUE,
                       na.strings = c("", " ", "NA"))
  totalWorker <- 94 # total worker is the highest workerid
} else if (whichDataSet == 2) {
  # ambiguous data with first block
  inputData = read.csv(
    "ella_total_ambiguous.csv",
    header = TRUE,
    na.strings = c("", " ", "NA")
  )
  totalWorker <- 52
} else if (whichDataSet == 3) {
  # ambiguous data without first block
  inputData = read.csv(
    "ella_total_ambiguous_wo_first_block.csv",
    header = TRUE,
    na.strings = c("", " ", "NA")
  )
  totalWorker <- 52
}

# inputDataWO = read.csv(
#   "ella_total_ambiguous_wo_first_block.csv",
#   header = TRUE,
#   na.strings = c("", " ", "NA")
# )
# totalWorkerWO <- 52


temp <- inputData$simulatedAnswer
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
targetOC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
inputData$targetOC27 <- targetOC27

temp <- inputData$obj1
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj1OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
inputData$obj1OC27 <- obj1OC27

temp <- inputData$obj2
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj2OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
inputData$obj2OC27 <- obj2OC27

temp <- inputData$obj3
temp2 <- (temp - temp %% 10) / 10
temp3 <- (temp2 - temp2 %% 10) / 10
obj3OC27 <- temp3 + 3 * ((temp2 %% 10) - 1) + 9 * ((temp %% 10) - 1)
inputData$obj3OC27 <- obj3OC27


## Reordering objects in input data

targetObject <- rep(NA, length(inputData$X))
object2 <- rep(NA, length(inputData$X))
object3 <- rep(NA, length(inputData$X))

for (i in 1:length(inputData$X)){
  if(targetOC27[i] == obj1OC27[i]){
    targetObject[i] <- targetOC27[i]
    object2[i] <- obj2OC27[i]
    object3[i] <- obj3OC27[i]
  } else if (targetOC27[i] == obj2OC27[i]) 
  {targetObject[i] <- obj2OC27[i]
  object2[i] <- obj1OC27[i]
  object3[i] <- obj3OC27[i]
  } else {
    targetObject[i] <- obj3OC27[i]
    object2[i] <- obj1OC27[i]
    object3[i] <- obj2OC27[i]
  }
}  

inputData$targetObject <- targetObject
inputData$object2 <- object2
inputData$object3 <- object3


### Setting default parameters. 
notObeyInst <- 0
softPrefValue <- 0
allObjectCodes <- getAllObjectCodes(allObjects, allUtterances) # working with "dotted" NOT "polka-dotted"!!!

inputData$orderObjNum1 <- inputData$order0
for (row in c(1:length(inputData$orderObjNum1))) {
  if (!is.na(inputData$orderObjNum1[row])) {
    inputData$orderObjNum1[row] <-
      which(allObjectCodes == inputData$orderObjNum1[row])
  }
}
inputData$orderObjNum2 <- inputData$order1
for (row in c(1:length(inputData$orderObjNum2))) {
  if (!is.na(inputData$orderObjNum2[row])) {
    inputData$orderObjNum2[row] <-
      which(allObjectCodes == inputData$orderObjNum2[row])
  }
}
inputData$orderObjNum3 <- inputData$order2
for (row in c(1:length(inputData$orderObjNum3))) {
  if (!is.na(inputData$orderObjNum3[row])) {
    inputData$orderObjNum3[row] <-
      which(allObjectCodes == inputData$orderObjNum3[row])
  }
}

# shown object choice as one of the present object codes (between 1 and 27)
inputData$simulatedAnswerObjNum <- inputData$simulatedAnswer
for (row in c(1:length(inputData$simulatedAnswerObjNum))) {
  if (!is.na(inputData$simulatedAnswerObjNum[row])) {
    inputData$simulatedAnswerObjNum[row] <-
      which(allObjectCodes == inputData$simulatedAnswerObjNum[row])
  }
}

# utterance choice.
inputData$utteranceNum <- as.character(inputData$utterance)
for (row in c(1:length(inputData$utteranceNum))) {
  if (!is.na(inputData$utteranceNum[row])) {
    inputData$utteranceNum[row] <-
      as.integer(which(allUtterancesNew1 == inputData$utteranceNum[row]))
  }
}


#########################################################
inputData$ambigRatio <- NA
countAmbigUttRatio <-
  function(allPresentFeaValues,
           currentObjects,
           targetFeatureNum) {
    uniqueFeaVal <- unique(allPresentFeaValues)
    if (targetFeatureNum == 1){
      remove <- c(1, 2, 3)
    } else if(targetFeatureNum == 2){
      remove <- c(4, 5, 6)
    } else {
      remove <- c(7, 8, 9)
    }
    uniqueFeaVal <- uniqueFeaVal [! uniqueFeaVal %in% remove]
    lengthUniqueFeaVal <- length(uniqueFeaVal)
    ambigCount <- 0
    for (utt in uniqueFeaVal) {
      ambiguous <- FALSE
      utteranceWord <- allUtterancesNew1[utt]
      currentObjectsUtterances <- allObjects[currentObjects, ]
      # if(str_count(allPresentFeaValues, toString(utteranceGeneral))>1){
      if (sum(allPresentFeaValues == utt) > 1) {
        ambiguous <- TRUE
      }
      if (ambiguous) {
        possibleObjectIndex <-
          which(currentObjectsUtterances == utteranceWord, arr.ind = TRUE)[, 1]
        possibleObjects <-
          currentObjectsUtterances[possibleObjectIndex, ]
        possibleObjectTarFeaValue <-
          possibleObjects[, targetFeatureNum]
        if (length(unique(possibleObjectTarFeaValue)) > 1) {
          ambigCount <- ambigCount + 1
        }
      }
    }
    ambigRatio <- ambigCount / lengthUniqueFeaVal
    return(ambigRatio)
  }


######################################################################
#### let's get started :-) 
######################################################################
maxTrialNum <- 4
totalBlock <- 4
row <- 0

inputData$evalNumModel <- NA
inputData$allPresentFeaValues <- NA
inputData$ambiguous <- NA
inputData$preferencesPrior1 <- NA
inputData$preferencesPrior2 <- NA
inputData$preferencesPrior3 <- NA
inputData$ambiguousUtteranceCount <- NA

preferencesPriorAll <- matrix(0,length(inputData$X),9)

for (worker in c(0:totalWorker)) {
  wID <- unique(inputData$workerid)[worker + 1]
  # cat(worker)
  for (block in c(1:totalBlock)) {
    # cat("worker", worker, "\n")
    # cat(1)
    # cat(unique(inputData$workerid)[worker+1])
    blockdata <-
      subset(inputData,
             blockNr == block - 1 & workerid == wID)
    targetFeatureNum <- blockdata$targetFeatureNum[1]
    #cat("targetFeatureNum", print(targetFeatureNum))
   
    ## simulated true preferences of the imaginatory partner
    allUtterancePref <-
      getAllUtterancePref(
        c(
          blockdata$simPreference0[1],
          blockdata$simPreference1[1],
          blockdata$simPreference2[1]
        )
      )

    ambiguousUtteranceCount <- 0
    
    for (trial in c(1:maxTrialNum)) {
      if( (trial-1)%%4 == 0) {
        preferencesPriorAll <- getPreferencesPrior(blockdata[trial,"targetFeatureNum"])
      }
      preferencesPriorIndices <- which(preferencesPriorAll != 0)
      
      currentObjects <-
        c(blockdata$targetObject[trial],
          blockdata$object2[trial],
          blockdata$object3[trial])
    
      relevantUtterances <- determineValidUtterances(currentObjects)
      utteranceWord <- blockdata$utterance[trial]
      utteranceNum <- which(allUtterancesNew1 == as.character(utteranceWord))
      utterance <- which(utteranceNum == relevantUtterances)
      row <- row + 1
            
      allPresentFeaValues <- determineAllFeaValues(currentObjects)
      inputData$allPresentFeaValues[row] <- toString(allPresentFeaValues)
      utteranceGeneral <- as.integer(blockdata$utteranceNum[trial])
   #   utterance <- which(relevantUtterances == utteranceGeneral)
       ambiguous <- isAmbiguous(allPresentFeaValues,
                                utteranceGeneral,
                              currentObjects,
                             targetFeatureNum)
       ambigRatio <- countAmbigUttRatio(allPresentFeaValues, 
                                        currentObjects, 
                                        targetFeatureNum)
       inputData$ambigRatio[row] <- ambigRatio
      
       inputData$ambiguous[row] <- ambiguous
       if (ambiguous) {
         ambiguousUtteranceCount <- ambiguousUtteranceCount + 1
       }
       inputData$ambiguousUtteranceCount[row] <-   ambiguousUtteranceCount
      
       ##### Now calculate posteriors ########################################
       
       mapObjToUtt <-
        determineObjectToUtterancesMapping(currentObjects)
      
      mapUttToObjProbs <-
        determineUtteranceToObjectProbabilities(relevantUtterances,
                                                currentObjects,
                                                mapObjToUtt,
                                                notObeyInst)
      
      mapUttToPref <-
        getMapUttToPref(relevantUtterances, allObjects, allUtterancePref)
      
      objectPreferenceSoftPriors <-
        getObjectPreferencePriorsWithUttToPref(
          relevantUtterances,
          currentObjects,
          softPrefValue,
          mapUttToObjProbs,
          mapUttToPref
        )
      
      mapUttToObjToPref <-
        getMapUttToObjToPref(
          currentObjects,
          targetFeatureNum,
          relevantUtterances,
          allUtterancePref,
          allObjects,
          mapUttToPref
        )
      #cat("mapUttToObjToPref", print(mapUttToObjToPref))
      #obj <- listenerObjChoice(mapUttToObjToPref, utterance)
      # obj <- blockdata$simulatedAnswer[trial]
      # obj <- blockdata$simulatedAnswerObjNum

      obj <- 1
      
      preferencesPriorAll <-
        simplePragmaticSpeakerWithPrefPriorAll(
          utterance,
          obj,
          preferencesPriorAll, # taking posterior from previous trial
          relevantUtterances,
          currentObjects,
          mapUttToObjProbs,
          objectPreferenceSoftPriors
        )
      
      # cat("preferencesPrior", preferencesPrior, "\n")
      inputData$preferencesPrior1[row] <-
        preferencesPriorAll[preferencesPriorIndices[1]]
      inputData$preferencesPrior2[row] <-
        preferencesPriorAll[preferencesPriorIndices[2]]
      inputData$preferencesPrior3[row] <-
        preferencesPriorAll[preferencesPriorIndices[3]]
  #  }}} # temporary  



      evalNumModel <-
        evaluate(allUtterancePref, preferencesPriorAll, targetFeatureNum)
      inputData$evalNumModel[row] <- evalNumModel
      humanResponse <-
        c(
          blockdata$normResponse0[trial],
          blockdata$normResponse1[trial],
          blockdata$normResponse2[trial]
        )
      evalNum <-
        evaluate(allUtterancePref, humanResponse, targetFeatureNum)
      inputData$evalNum[row] <- evalNum
      #cat("evalNumModel", evalNumModel, "\n")
    }
  }
}



inputData$evalNum <- as.factor(inputData$evalNum)
inputData$evalNumModel <- as.factor(inputData$evalNumModel)
inputDataCondensed <-
  subset(inputData, trialNum == 3)
inputDataCondensed$ambiguousUtteranceCount <-
  as.factor(inputDataCondensed$ambiguousUtteranceCount)


inputDataCondensedCompare <-
  subset(
    inputDataCondensed,
    select = c(
      normResponse0,
      preferencesPrior1,
      normResponse1,
      preferencesPrior2,
      normResponse2,
      preferencesPrior3
    )
  )
#
# dat <- data.frame(evalNumModel = 1:400, evalNumHuman = 1:400)
# for (i in c(1:length(evalNumHumanCollected))) {
#   dat$evalNumModel[i] <- evalNumModelCollected[[i]]
#   dat$evalNumHuman[i] <- evalNumHumanCollected[[i]]
# }

ambiguityUsed <- matrix(nrow = totalWorker + 1, ncol = 2)
for (worker in c(0:totalWorker)) {
  ambiguityUsed[worker + 1, 1] <-
    unique(inputData$workerid)[worker + 1]
  ambiguityUsed[worker + 1, 2] <-
    round(sum(inputData$ambiguous[which(inputData$workerid == unique(inputData$workerid)[worker +
                                                                                           1])]) / 16 *
            100, digits = 1)
}

ambiguousWorker <-
  subset(ambiguityUsed, ambiguityUsed[, 2] > quantile(ambiguityUsed[, 2], 0.75))[, 1]
inputDataAmbiguous <-
  subset(inputData, workerid %in% ambiguousWorker)

nonAmbiguousWorker <-
  subset(ambiguityUsed, ambiguityUsed[, 2] < quantile(ambiguityUsed[, 2], 0.25))[, 1]
inputDataNonAmbiguous <-
  subset(inputData, workerid %in% nonAmbiguousWorker)

# ambiguousWorkerWOambiguityblock <- subset(ambiguityUsedWOambiguityblock, ambiguityUsedWOFirstBlock[,2]>75)[,1]
# inputDataAmbiguousWOFirstBlock <- subset(inputData, workerid %in% ambiguousWorkerWOFirstBlock)

inputDataCondensedAmbiguous <-
  subset(inputDataCondensed, workerid %in% ambiguousWorker)
inputDataCondensedAmbiguousEqual <-
  subset(inputDataCondensedAmbiguous, evalNum == evalNumModel)
inputDataCondensedEqual <-
  subset(inputDataCondensed, evalNum == evalNumModel)
# inputDataCondensedAmbiguousCompare <- subset(inputDataCondensedAmbiguous, select = c(normResponse0,preferencesPrior1, normResponse1,preferencesPrior2, normResponse2, preferencesPrior3))
# inputDataCondensedAmbiguousEqualCompare <- subset(inputDataCondensedAmbiguousEqual, select = c(normResponse0,preferencesPrior1, normResponse1,preferencesPrior2, normResponse2, preferencesPrior3))

response0 <-
  subset(inputDataCondensedAmbiguous,
         select = c(normResponse0, preferencesPrior1))
response1 <-
  subset(inputDataCondensedAmbiguous,
         select = c(normResponse1, preferencesPrior2))
response2 <-
  subset(inputDataCondensedAmbiguous,
         select = c(normResponse2, preferencesPrior3))
colnames(response0) <- c("normResponse", "preferencesPrior")
colnames(response1) <- c("normResponse", "preferencesPrior")
colnames(response2) <- c("normResponse", "preferencesPrior")
inputDataCondensedAmbiguousCompare <-
  rbind(response0, response1, response2)

# inputDataCondensedAmbiguousEqualCompare
response0 <-
  subset(inputDataCondensedAmbiguousEqual,
         select = c(normResponse0, preferencesPrior1))
response1 <-
  subset(inputDataCondensedAmbiguousEqual,
         select = c(normResponse1, preferencesPrior2))
response2 <-
  subset(inputDataCondensedAmbiguousEqual,
         select = c(normResponse2, preferencesPrior3))
colnames(response0) <- c("normResponse", "preferencesPrior")
colnames(response1) <- c("normResponse", "preferencesPrior")
colnames(response2) <- c("normResponse", "preferencesPrior")
inputDataCondensedAmbiguousEqualCompare <-
  rbind(response0, response1, response2)

# inputDataCondensedCompare
response0 <-
  subset(inputDataCondensed,
         select = c(normResponse0, preferencesPrior1))
response1 <-
  subset(inputDataCondensed,
         select = c(normResponse1, preferencesPrior2))
response2 <-
  subset(inputDataCondensed,
         select = c(normResponse2, preferencesPrior3))
colnames(response0) <- c("normResponse", "preferencesPrior")
colnames(response1) <- c("normResponse", "preferencesPrior")
colnames(response2) <- c("normResponse", "preferencesPrior")
inputDataCondensedCompare <- rbind(response0, response1, response2)
#
# write.csv(inputDataAmbiguous,'ella_total_ambiguous.csv')
# write.csv(inputDataCondensedAmbiguous,'ella_condensed_ambiguous.csv')
# write.csv(inputDataAmbiguousWOFirstBlock,'ella_total_ambiguous_wo_first_block.csv')
#
# inputDataWOFirstBlock <- subset(inputData, blockNr > 0)
# ambiguityUsedWOFirstBlock <- matrix(nrow = totalWorker+1, ncol = 2)
# for(worker in c(0:totalWorker)){
#   ambiguityUsedWOFirstBlock[worker+1,1] <- worker
#   ambiguityUsedWOFirstBlock[worker+1,2] <- round(sum(inputDataWOFirstBlock$ambiguous[which(inputDataWOFirstBlock$workerid == worker)])/12*100, digits=1)
# }
write.csv(inputData, 'after_testing_all_Data.csv')

plotting <- FALSE

if (plotting) {
  #############################################################################################################
  #____________________________________PLOTTING________________________________________________________________
  #############################################################################################################
  
  roundingDigits <- 2
  bothTables <-
    rbind(data.frame(table(inputDataCondensed$evalNum)), data.frame(table(inputDataCondensed$evalNumModel)))
  HumanOrModel = rep(c("Human", "Model"), each = length(bothTables$Freq) /
                       2)
  
  tabledEvalNum <- data.frame(bothTables, HumanOrModel)
  totalTrialsWOFirstBlock <-
    sum(tabledEvalNum$Freq[HumanOrModel == "Human"])
  tabledEvalNum$relativeFreq <-
    round(tabledEvalNum$Freq / totalTrialsWOFirstBlock, digits = roundingDigits)
  
  #
  # evalNumCompairPlot <-
  #   ggplot(data = tabledEvalNum, aes(x = Var1, y = relativeFreq, fill = HumanOrModel)) +
  #   geom_bar(stat = "identity", position = position_dodge()) +
  #   geom_text(
  #     aes(label = relativeFreq * 100),
  #     vjust = -0.3,
  #     color = "black",
  #     position = position_dodge(0.9),
  #     size = 2
  #   ) +
  #   labs(
  #     title = "Learning success compared, first block excluded\n",
  #     x = "Evaluation Number",
  #     y = "Frequency",
  #     fill = ""
  # ) + coord_cartesian(ylim = c(0, 0.65))
  
  
  learningProcessDataWithFirstBlock <-
    data.frame(
      inputData$trialNum,
      inputData$blockNr,
      inputData$evalNum,
      inputData$evalNumModel
    )
  learningProcessData <-
    subset(learningProcessDataWithFirstBlock, inputData.blockNr != 0)
  summary(learningProcessData)
  humanLearningProcess <-
    as.data.frame(
      table(
        learningProcessData$inputData.trialNum,
        learningProcessData$inputData.evalNum
      )
    )
  humanLearningProcess$relativeFreq <-
    round(humanLearningProcess$Freq / totalTrialsWOFirstBlock,
          digits = roundingDigits)
  
  humanLearnProcPlot <-
    ggplot(data = humanLearningProcess, aes(x = Var1, y = relativeFreq, fill =
                                              Var2)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(
      aes(label = relativeFreq),
      vjust = -0.3,
      color = "black",
      position = position_dodge(0.9),
      size = 2
    ) +
    labs(
      title = "Human learning process, first block excluded\n",
      x = "Trial",
      y = "Frequency",
      fill = "Evaluation\nNumber\n"
    ) + coord_cartesian(ylim = c(0, 0.65))
  
  
  modelLearningProcess <-
    as.data.frame(
      table(
        learningProcessData$inputData.trialNum,
        learningProcessData$inputData.evalNumModel
      )
    )
  modelLearningProcess$relativeFreq <-
    round(modelLearningProcess$Freq / totalTrialsWOFirstBlock,
          digits = roundingDigits)
  
  modelLearnProcPlot <-
    ggplot(data = modelLearningProcess, aes(x = Var1, y = relativeFreq, fill =
                                              Var2)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(
      aes(label = relativeFreq * 100),
      vjust = -0.3,
      color = "black",
      position = position_dodge(0.9),
      size = 2
    ) +
    labs(
      title = "Model learning process, first block excluded\n",
      x = "Trial",
      y = "Frequency",
      fill = "Evaluation\nNumber\n"
    ) + coord_cartesian(ylim = c(0, 0.65))
  
  # bla <- grid.arrange(evalNumCompairPlot, humanLearnProcPlot, modelLearnProcPlot, ncol=3)
  
  # ggsave(
  #   filename = "bla.png",
  #   plot = bla,
  #   width = 40,
  #   height = 14,
  #   units = "cm",
  #   dpi = 700
  # )
  #
  # ggsave(
  #   filename = "evalNumCompairPlot.png",
  #   plot = evalNumCompairPlot,
  #   width = 20,
  #   height = 20,
  #   units = "cm",
  #   dpi = 700
  # )
  
  # ____________________________________________________________________________________________________________________________
  bothTablesWithFirstBlock <-
    rbind(data.frame(table(inputDataCondensed$evalNum)), data.frame(table(inputDataCondensed$evalNumModel)))
  HumanOrModel = rep(c("Human", "Model"),
                     each = length(bothTablesWithFirstBlock$Freq) /
                       2)
  HumanOrModelWithFirstBlock = rep(c("Human", "Model"),
                                   each = length(bothTablesWithFirstBlock$Freq) / 2)
  tabledEvalNumWithFirstBlock <-
    data.frame(bothTablesWithFirstBlock, HumanOrModelWithFirstBlock)
  
  totalTrialsWithFirstBlock <-
    sum(tabledEvalNumWithFirstBlock$Freq[HumanOrModel == "Human"])
  tabledEvalNumWithFirstBlock$relativeFreq <-
    round(tabledEvalNumWithFirstBlock$Freq / totalTrialsWithFirstBlock,
          digits = roundingDigits)
  
  
  evalNumCompairPlotWithFirstBlock <-
    ggplot(data = tabledEvalNumWithFirstBlock,
           aes(x = Var1, y = relativeFreq, fill = HumanOrModelWithFirstBlock)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(
      aes(label = relativeFreq * 100),
      vjust = -0.3,
      color = "black",
      position = position_dodge(0.9),
      size = 2
    ) +
    labs(
      title = "Learning success compared, first block included\n",
      x = "Evaluation Number",
      y = "Frequency",
      fill = ""
    ) + coord_cartesian(ylim = c(0, 0.65))
  
  ggsave(
    filename = "evalNumCompairPlotWithFirstBlock.png" ,
    plot = evalNumCompairPlotWithFirstBlock,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 700
  )
  
  # __________________________________________________________________________
  
  for (datasetNum in c(1:3)) {
    if (datasetNum == 1) {
      dataset <- inputData
    } else if (datasetNum == 2) {
      dataset <- inputDataAmbiguous
    } else if (datasetNum == 3) {
      dataset <- inputDataNonAmbiguous
    }
    total <- length(dataset$workerid) %/% maxTrialNum
    learningProcessDataWithFirstBlock <-
      data.frame(dataset$trialNum,
                 dataset$blockNr,
                 dataset$evalNum,
                 dataset$evalNumModel)
    summary(learningProcessDataWithFirstBlock)
    colnames(learningProcessDataWithFirstBlock) <-
      c("trialNum", "blockNr", "evalNum", "evalNumModel")
    humanLearningProcessWithFirstBlock <-
      as.data.frame(
        table(
          learningProcessDataWithFirstBlock$trialNum,
          learningProcessDataWithFirstBlock$evalNum
        )
      )
    humanLearningProcessWithFirstBlock$relativeFreq <-
      round(humanLearningProcessWithFirstBlock$Freq / total,
            digits = roundingDigits)
    
    modelLearningProcessWithFirstBlock <-
      as.data.frame(
        table(
          learningProcessDataWithFirstBlock$trialNum,
          learningProcessDataWithFirstBlock$evalNumModel
        )
      )
    modelLearningProcessWithFirstBlock$relativeFreq <-
      round(modelLearningProcessWithFirstBlock$Freq / total,
            digits = roundingDigits)
    
    
    #_____________Line Plot learning trajectory all in one______________________________________________________________________________________
    
    
    humanLearningProcessWithFirstBlock$datattype <- "human"
    modelLearningProcessWithFirstBlock$datatype <- "model"
    
    colnames(humanLearningProcessWithFirstBlock) <-
      c("trialNum",
        "evalNum",
        "humanFreq",
        "relativeFreq",
        "datatype")
    colnames(modelLearningProcessWithFirstBlock) <-
      c("trialNum",
        "evalNum",
        "modelFreq",
        "relativeFreq",
        "datatype")
    
    
    titles <-
      c(
        "Learning Trajectory compared \nAll data",
        "Learning Trajectory compared \nOnly data from subjects who picked ambiguous utterances",
        "Learning Trajectory compared \nOnly data  from subjects who didn't pick ambiguous utterances"
      )
    
    
    learningProcessLinePlot <- ggplot() +
      # blue plot
      geom_line(
        data = humanLearningProcessWithFirstBlock,
        aes(
          x = trialNum,
          y = relativeFreq,
          color = evalNum,
          group = evalNum,
          linetype = datatype
        )
      ) +
      # red plot
      geom_line(
        data = modelLearningProcessWithFirstBlock,
        aes(
          x = trialNum,
          y = relativeFreq,
          color = evalNum,
          group = evalNum,
          linetype = datatype
        )
      ) +
      coord_cartesian(ylim = c(0, 0.8))  +
      labs(
        title = titles[datasetNum],
        x = "Trial Number",
        y = "Frequency [%]",
        color = "Evaluation\nNumber\n",
        group = "Evaluation\nNumber\n",
        linetype = ""
      )
    
    ggsave(
      filename = paste(
        "learningProcessLinePlot",
        as.character(datasetNum),
        ".png" ,
        sep = ""
      ) ,
      plot = learningProcessLinePlot,
      width = 20,
      height = 20,
      units = "cm",
      dpi = 700
    )
  }
  
  #____________________________non-ambiguous vs ambiguous block -> evaluation number___________________________________________________________________________
  
  humanAmbBlockEval <-
    as.data.frame(table(
      inputDataCondensed$ambiguousUtteranceCount,
      inputDataCondensed$evalNum
    ))
  modelAmbBlockEval <-
    as.data.frame(
      table(
        inputDataCondensed$ambiguousUtteranceCount,
        inputDataCondensed$evalNumModel
      )
    )
  colnames(humanAmbBlockEval) <-
    c("ambiguousUtteranceCount", "evaluationNumber", "Frequency")
  colnames(modelAmbBlockEval) <-
    c("ambiguousUtteranceCount", "evaluationNumber", "Frequency")
  
  ambiguityBlockEvaluation <- ggplot() +
    geom_line(
      humanAmbBlockEval,
      mapping = aes(
        x = evaluationNumber,
        y = Frequency,
        color = ambiguousUtteranceCount,
        group = ambiguousUtteranceCount
      )
    ) +
    geom_line(
      modelAmbBlockEval,
      mapping = aes(
        x = evaluationNumber,
        y = Frequency,
        color = ambiguousUtteranceCount,
        group = ambiguousUtteranceCount
      ),
      linetype = "dashed"
    ) + labs(
      title = "learning success depending on how many ambiguous \nutterances were picked in the block",
      x = "Evaluation Number",
      y = "Frequency",
      color = "Ambiguous \nUtterances \nper Block\n",
      group = "Ambiguous \nUtterances \nper Block\n"
    )
  
  ggsave(
    filename = "ambiguityBlockEvaluation.png" ,
    plot = ambiguityBlockEvaluation,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 700
  )
  
  #_______________RELATIVE_____________non-ambiguous vs ambiguous block -> evaluation number___________________________________________________________________________
  
  
  tabeledAmbUttCount <-
    summary(inputDataCondensed$ambiguousUtteranceCount)
  
  humanAmbBlockEval <-
    as.data.frame(table(
      inputDataCondensed$ambiguousUtteranceCount,
      inputDataCondensed$evalNum
    ))
  colnames(humanAmbBlockEval) <-
    c("ambiguousUtteranceCount", "evaluationNumber", "Frequency")
  humanAmbBlockEval$relativeFrequency <- humanAmbBlockEval$Frequency
  
  modelAmbBlockEval <-
    as.data.frame(
      table(
        inputDataCondensed$ambiguousUtteranceCount,
        inputDataCondensed$evalNumModel
      )
    )
  colnames(modelAmbBlockEval) <-
    c("ambiguousUtteranceCount", "evaluationNumber", "Frequency")
  modelAmbBlockEval$relativeFrequency <- modelAmbBlockEval$Frequency
  
  for (index in c(1:5)) {
    ambCount <- index - 1
    ambCountFreq <- tabeledAmbUttCount[[index]]
    for (row in c(1:length(humanAmbBlockEval$ambiguousUtteranceCount))) {
      if (humanAmbBlockEval$ambiguousUtteranceCount[row] == ambCount) {
        humanAmbBlockEval$relativeFrequency[row] <-
          humanAmbBlockEval$Frequency[row] / ambCountFreq
        modelAmbBlockEval$relativeFrequency[row] <-
          modelAmbBlockEval$Frequency[row] / ambCountFreq
      }
    }
  }
  
  
  ambiguityBlockEvaluationRelative <- ggplot() +
    geom_line(
      modelAmbBlockEval,
      mapping = aes(
        x = evaluationNumber,
        y = relativeFrequency,
        color = ambiguousUtteranceCount,
        group = ambiguousUtteranceCount
      ),
    ) + labs(
      title = "learning success depending on how many ambiguous \nutterances were picked in the block",
      x = "Evaluation Number",
      y = "relative Frequency [%]",
      color = "Ambiguous \nUtterances \nper Block\n",
      group = "Ambiguous \nUtterances \nper Block\n"
    )
  
  ggsave(
    filename = "ambiguityBlockEvaluationRelative.png" ,
    plot = ambiguityBlockEvaluationRelative,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 700
  )
  
  #______________________________Scatterplot Model&Human compared_______________________________________________
  
  condensedComparePlot <-
    ggplot(inputDataCondensedCompare,
           mapping = aes(preferencesPrior, normResponse)) + # January 23, 2020, flipped axes
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Last trials of block \nAll data \n",
         x = "Model Predictions",
         y = "Human Data")
  
  condensedAmbiguousComparePlot <-
    ggplot(inputDataCondensedAmbiguousCompare,
           mapping = aes(normResponse, preferencesPrior)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "Last trials of block \nData from subjects who picked ambiguous utterances\n",
         x = "Human predictions",
         y = "Model Predictions")
  
  condensedAmbiguousEqualComparePlot <-
    ggplot(
      inputDataCondensedAmbiguousEqualCompare,
      mapping = aes(normResponse, preferencesPrior)
    ) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "Last trials of block \nData from subjects who picked ambiguous utterances \nEqual Evaluation Number",
         x = "Human predictions",
         y = "Model Predictions")
  
  comparePlots <-
    arrangeGrob(
      condensedComparePlot,
      condensedAmbiguousComparePlot,
      condensedAmbiguousEqualComparePlot,
      ncol = 3
    )
  
  ggsave(
    filename = "comparePlots.png",
    plot = comparePlots,
    width = 40,
    height = 14,
    units = "cm",
    dpi = 700
  )
  
  # ____________________________________AMBIGUITY USE_________________________________________________________
  
  ambiguityUsedArranged <- as.data.frame(table(ambiguityUsed[, 2]))
  ambiguityUsePlot <-
    ggplot(ambiguityUsedArranged, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", width = 0.2) +
    labs(
      title = "Use of ambiguous utterances (of 16 Trials)\n",
      x = "Number of Trials with ambiguous utterances",
      y = "Participants using ambiguous utterances (of 100)",
      fill = "Evaluation\nNumber\n"
    ) +
    coord_flip()
  
  # ambiguityUsedArrangedWOFirstBlock <- as.data.frame(table(ambiguityUsedWOFirstBlock[, 2]))
  # ambiguityUseWOFirstBlockPlot <- ggplot(ambiguityUsedArrangedWOFirstBlock, aes(x = Var1, y = Freq)) +
  #   geom_bar(stat = "identity", width = 0.2) +
  #   labs(title = "Use of ambiguous utterances (of 12 Trials, without first block)\n",
  #        x = "Percentage of Trials with ambiguous utterances",
  #        y = "Participants using ambiguous utterances (of 100)",
  #        fill = "Evaluation\nNumber\n") +
  #   coord_flip()
  # ambiguity <-
  #   grid.arrange(
  #     ambiguityUseWOFirstBlockPlot,
  #     ambiguityUsePlot,
  #     ncol = 2
  #   )
  ggsave(
    filename = "ambiguity.png",
    plot = ambiguityUsePlot,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 700
  )
}

##############################################################################
#____________________________stat testing_____________________________________
##############################################################################

library(ordinal)

# p > 0.05 => no learning effect
#         Estimate Std. Error z value Pr(>|z|)
# blockNr  0.12546    0.09453   1.327    0.184
model <-
  clmm(evalNum ~ blockNr + (1 | workerid), data = inputDataCondensed)
summary(model)

# no effect either
#                        Estimate Std. Error z value Pr(>|z|)
# Answer.time_in_minutes  0.02682    0.03966   0.676    0.499
model <-
  clmm(evalNum ~ Answer.time_in_minutes + (1 |
                                             workerid), data = inputDataCondensed)
summary(model)

# big effect!
# Estimate Std. Error z value Pr(>|z|)
# certainty   2.4671     0.5639   4.375 1.22e-05 ***
model <-
  clmm(evalNum ~ certainty + (1 | workerid), data = inputDataCondensed)
summary(model)

# big effect"
# Estimate Std. Error z value Pr(>|z|)
# certainty   2.3298     0.3762   6.193  5.9e-10 ***
model <-
  clmm(ambiguousUtteranceCount ~ certainty + (1 |
                                                workerid), data = inputDataCondensed)
summary(model)

# effect!
# Estimate Std. Error z value Pr(>|z|)
# blockNr 0.364284   0.003463   105.2   <2e-16 ***
model <-
  clmm(ambiguousUtteranceCount ~ blockNr + (1 |
                                              workerid), data = inputDataCondensed)
summary(model)
