library(reshape2) # convert wide format data to long/tidy format
library(plyr)
library(ggplot2)
library(Rmisc) # for confidence intervals

## Prior-inference experiment (4)
## A barplot to show human behavior vs. model predictions for stimuli of type 212212, such as
## 'red striped cloud, red dotted circle, green dotted cloud'. The utterance is 'red'. The first object is picked.
## The example is just for a visualization. The graph averages over all trials of type 212212.

## The csv file below containts human data and model predicitons reordered. The non-optmized version has parameters set to 0.1,
## these are obedience and softness. We can use this graph to illustrate how the parameters work. If parameters are set to 0,
## the model predictions will be 1 for preference of a certain feature value and 0 for feature values not preferred.
## The optimized model is individually optimized for both parameters, cross-validated.

data = read.csv("x4pilotDataModelOptimizedSorted2.csv") 

sample <- subset(data, CCode == 212212)
example <- subset(sample,target == "122" & obj2 == "232" & obj3 == "133")

## First, we reshape the data table into long/tidy format

behavior <- melt(sample,
                 id.vars = c( "workerid", "item" ,"slide_number","pref1","response1","pref2", "response2","pref3","response3","pref4",
                              "response4","pref5","response5",
                              "pref6","response6", "target", "obj2","obj3","utterance", "itemCode","ambiguous", 
                              "uttFeat", "q1Feat", "q2Feat", "targetOC27","obj2OC27","obj3OC27","CCode"),
                 measure.vars = c("DataPost_1","DataPost_2","DataPost_3","DataPost_4","DataPost_5","DataPost_6","DataPost_7","DataPost_8","DataPost_9"),
                 variable.name = "sliderNumber",value.name = "sliderValue")

modelNotOpt <- melt(sample,
                 id.vars = c( "workerid", "item" ,"slide_number","pref1","response1","pref2", "response2","pref3","response3","pref4",
                              "response4","pref5","response5",
                              "pref6","response6", "target", "obj2","obj3","utterance", "itemCode","ambiguous", 
                              "uttFeat", "q1Feat", "q2Feat", "targetOC27","obj2OC27","obj3OC27","CCode"),
                 measure.vars = c("Post1_1","Post1_2","Post1_3","Post1_4","Post1_5","Post1_6","Post1_7","Post1_8","Post1_9"),
                 variable.name = "sliderNumber",value.name = "sliderValue")

model <- melt(sample,
              id.vars = c( "workerid", "item" ,"slide_number","pref1","response1","pref2", "response2","pref3","response3","pref4",
                           "response4","pref5","response5",
                           "pref6","response6", "target", "obj2","obj3","utterance", "itemCode","ambiguous", 
                           "uttFeat", "q1Feat", "q2Feat", "targetOC27","obj2OC27","obj3OC27","CCode"),
              measure.vars = c("Post2_1","Post2_2","Post2_3","Post2_4","Post2_5","Post2_6",
                               "Post2_7","Post2_8","Post2_9"),
              variable.name = "sliderNumber",value.name = "sliderValue")

behavior$sliderNumber <- gsub(paste0("DataPost_",collapse = "|"),"", behavior$sliderNumber)
model$sliderNumber <- gsub(paste0("Post2_",collapse = "|"),"", model$sliderNumber)
modelNotOpt$sliderNumber <- gsub(paste0("Post1_",collapse = "|"),"", modelNotOpt$sliderNumber)

## Now get a summary of descriptive statistics

statsBehavior <- summarySE(behavior, measurevar="sliderValue", groupvars="sliderNumber",na.rm = TRUE)
statsModel <- summarySE(model, measurevar="sliderValue", groupvars="sliderNumber")
statsModelNotOpt <- summarySE(modelNotOpt, measurevar="sliderValue", groupvars="sliderNumber")

statsBehavior$type <- "behavior"
statsModel$type <- "model"
statsModelNotOpt$type <- "modelNotOpt"

behaviorPlot <- statsBehavior[c(4,5,7,8),]
modelPlot <- statsModel[c(4,5,7,8),]
modelNotOptPlot <- statsModelNotOpt[c(4,5,7,8),]

allStats <- rbind(behaviorPlot,modelPlot,modelNotOptPlot)

## Barplot with horizontal bars to mimic sliders

ggplot(allStats, aes(x=sliderNumber, y=sliderValue, fill = type)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
#  coord_flip()+
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=sliderValue-ci, ymax=sliderValue+ci)) +
  scale_fill_manual(values = c("white","ivory3","ivory4"), labels = c("Data","Opt. model","Non-opt. model"), name = "") +
  theme_bw(base_size = 18)+
  #labs(title="Example of class 212212")+
  ylab("Slider value (normalized)")+
  xlab("")+
  scale_x_discrete(limits=c("5","4","8","7"),
                   labels=c("circle", "cloud", "dotted","striped"))+
  theme(legend.position="bottom") +
#  theme (axis.text.x=element_text(size = 16),
 #       axis.text.y=element_text(size = 16),
 #       axis.title.x=element_text(size = 16),
 #       axis.title.y=element_text(size = 16),
 #       legend.text=element_text(size = 14))+
#ggsave("examplePlot.pdf")
ggsave("december_barplot_x4.pdf",width=5,height=4.5, units = "in")

### The non-optimized model has parameters fixed at 0.1 level (obedience and softness)ß