library("dplyr")
library("ggplot2")

################################################## X 4 Prior inference ##################################

# Import full csv table

setwd("~/Documents/GitHub/prior_inference/RSA_2019_08/X4_Plots")
full <- read.csv("for_scatterplots_updated.csv")
head(full)

## See what models we have ##

stats <- full %>%
    group_by(softness,obedience, alpha, cross_validated,type, Nr) %>%
    summarise(n_occur = n())
ordered <- stats[order(stats$Nr),] # Sort the summary table
print.data.frame(ordered)

##########################################################################################################

# Subset data for plotting. Fill in the template

# plotData <- subset(full, softness ==  & obedience ==  & cross_validated == " " & type == "") # for simple models
# plotData <- subset(full, softness ==  & obedience ==  & alpha == & cross_validated == " " & type == "") # for full models

# Softness, obedience, and alpha are numeric
# Cross_validated can be "yes" or "no"
# Type can be "simpleRSA" or "fullRSA"
# Nr is model number in the order they are listed in the scatterplot file. 

##########################################################################################################

### Subsets for figures in the paper. Pick one plotData subset, then run the define variables part
### Then generate the plot either with ggplot or a simple plot

## Figure \label{simple-full}

# for m23
plotData <- subset(full, softness == "globally_opt" & obedience == 0 & 
                          cross_validated == "no" & type == "fullRSA" & alpha == 1) 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Full model")+
#  labs(title = bquote(atop
 #                     (.(type) ~"," ~ r^2 == .(r2),
 #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
 #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

model <- lm(formula = plotData$model~plotData$workerData)
model2 <- lm(formula = plotData$workerData~plotData$model)

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
summary(model2)

confint(model)
summary(model)
confint(model)

# for m27
plotData <- subset(full, softness == "globally_opt" & obedience == 0 & 
                     cross_validated == "no" & type == "fullRSA" & alpha == "globally_opt") 

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]

# for m13
plotData <- subset(full, softness == "globally_opt" & obedience == 0 & 
                     cross_validated == "no" & type == "simpleRSA") 

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Simple model", subtitle = "Globally optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))



#plotData <- subset(full, softness == "individually_opt" & obedience == 0.1 & cross_validated == "yes" & type == "simpleRSA") # for m9

####### Figure \label{simple-full-individual}
# for m16
plotData <- subset(full, softness == "individually_opt" & obedience == 0 & 
                                            cross_validated == "no" & type == "fullRSA" & 
                                             alpha == "individually_opt") 
model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Full model")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))


# for m3                    
plotData <- subset(full, softness == "individually_opt" & obedience == 0 & 
                     cross_validated == "no" & type == "simpleRSA") 

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = 0.5) +
  theme_bw(base_size = 14) +
  labs(title = "Simple model")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))



########## Figure \label{cross-validation} ##########
# for m5

plotData <- subset(full, softness == "individually_opt" & obedience == "individually_opt" & 
                                             cross_validated == "no" & type == "simpleRSA") 

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Simple model", subtitle = "Non-cross-validated")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))



# for m8
plotData <- subset(full, softness == "individually_opt" & obedience == "individually_opt" & 
                    cross_validated == "yes" & type == "simpleRSA") 

model <- lm(formula = plotData$model~plotData$workerData)

summary(model)
confint(model)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]


figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = .5) +
  theme_bw(base_size = 14) +
  labs(title = "Simple model", subtitle = "Individually optimized")+
  #  labs(title = bquote(atop
  #                     (.(type) ~"," ~ r^2 == .(r2),
  #                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
  #                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
print(figure)
ggsave(figure, height = 3, width = 3, units = "in", filename = paste("m", nr,".pdf", sep=""))

plotData <- subset(full, Nr == 14) 

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]

plotData <- subset(full, Nr == 27) 

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

plotData <- subset(full, Nr == 28) 

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)

plotData <- subset(full, Nr == 16) 

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)

#### Old code below #####


# Define variables to pass to plot title and ggsave

r2 <- round((summary(lm(plotData$model~plotData$workerData))$r.squared), digits = 4)
softness <- unique(as.character(plotData$softness))
obedience <- unique(as.character(plotData$obedience))
type <- unique(as.character(plotData$type))
nr <- plotData$Nr[1]

## Scatterplot with ggplot2 ##
# Contains a title with parameter values for now ##

figure <- ggplot(plotData, aes(x = model, y = workerData)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "black",
              se = FALSE,
              size = 1) +
  theme_bw(base_size = 18) +
  labs(title = bquote(atop
                     (.(type) ~"," ~ r^2 == .(r2),
                       ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
                     )))+
  ylab("human data")+
  xlab("model predictions") +
  theme(plot.title = element_text(hjust = 0.5))
print(figure)
ggsave(figure, filename = paste("m", nr,".pdf", sep=""))


## Simple plot works a little faster to look up r2 values ##

model <- lm(formula = plotData$model~plotData$workerData)
summary(model)
confint(model)

plot(plotData$model, plotData$workerData, xlab = "model", ylab = "human data")
abline(lm(formula = plotData$model~plotData$workerData), col="red") # regression line (y~x)
lines(lowess(plotData$model,plotData$workerData), col="blue") # lowess line (x,y)

title(main = bquote(atop
      (.(type) ~"," ~ r^2 == .(r2),
        ~ "softness" == .(softness) ~ "," ~ "obedience" == .(obedience)
        )))

#########################################################################################################


