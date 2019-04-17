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
#d[] <- lapply( d, factor) 

unique(d$language)

# only look at "español" as the native language
d = d[d$language!="United States",]

length(unique(d$workerid)) ## n=52

summary(d)

#write.csv(d,"../results/7-mutual-understanding.csv")

## results plot
d_s = bootsSummary(data=d, measurevar="response", groupvars=c("MUalign","MUnumchoices"))

p <- ggplot(d_s, aes(x=as.factor(MUnumchoices),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=as.factor(MUnumchoices), width=0.1),position=position_dodge(width=0.9))+
  ylab("understanding\n")+
  xlab("\nnumber of choices") +
  ylim(0,1) +
  facet_grid(.~MUalign,scales="free_x")+
  theme_bw()
p
#ggsave("../results/choices_plot.pdf",width=4)

## results plot by item code
i_s = bootsSummary(data=d, measurevar="response", groupvars=c("itemCode","MUalign"))

i_p <- ggplot(i_s, aes(x=itemCode,y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=itemCode, width=0.1),position=position_dodge(width=0.9))+
  ylab("understanding\n")+
  xlab("\nitem code") +
  ylim(0,1) +
  facet_grid(.~MUalign,scales="free_x")+
  theme_bw()
i_p
#ggsave("../results/item-code_plot.pdf",width=7)