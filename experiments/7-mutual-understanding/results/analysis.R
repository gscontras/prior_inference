library(ggplot2)
library(reshape2)
library(lme4)
library(dplyr)

setwd("~/git/prior_inference/experiments/7-mutual-understanding/Submiterator-master/")

source("../results/helpers.r")

num_round_dirs = 6
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/mutual-understanding.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid","item","slide_number","language", "MUsharedother","target","obj2","obj3","itemCode","MUnumchoices","MUsharedlistenerpick","MUalign","utterance","response","gender","age"))

# re-factorize
d[] <- lapply( d, factor) 

unique(d$language)

# only look at "español" as the native language
d = d[d$language!="United States",]

length(unique(d$workerid)) ## n=52

summary(d)

#write.csv(d,"../results/5-combined-unique.csv")


## class plot
d_s = bootsSummary(data=t, measurevar="response", groupvars=c("class"))
# save data for aggregate plot
#write.csv(d_s,"~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/faultless.csv")

class_plot <- ggplot(d_s, aes(x=reorder(class,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,-response,mean), width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("\nadjective class") +
  ylim(0,1) +
  theme_bw()
class_plot
#ggsave("../results/class_plot.pdf",height=3)

agr_pred = aggregate(response~predicate*class,data=t,mean)

#write.csv(agr_pred,"../results/pred-subjectivity.csv")