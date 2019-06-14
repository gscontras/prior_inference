library(ggplot2)
library(reshape2)
library(lme4)
library(dplyr)

setwd("~/git/prior_inference/experiments/8-preference-type/Submiterator-master/")

source("../results/helpers.r")

d = read.csv("preference-type-trials.csv",header=T)
s = read.csv("preference-type-subject_information.csv",header=T)

d$language = s$language[match(d$workerid,s$workerid)]

unique(d$language)

# only look at English speakers
d = d[d$language!="Hindi"&d$language!="United States"&d$language!="Chinese"&d$language!="Emglish"&d$language!="Spanish"&d$language!="England",]

length(unique(d$workerid)) ## n=50

summary(d)

#write.csv(d,"../results/8-preference-type.csv")
