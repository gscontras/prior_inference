################################### X3 Model comparison. Full RSA ###############################################################
###################################### Likelihood ratio test ###################################################################

setwd("~/Documents/GitHub/prior_inference/RSA_2019_08/Model_comaprison")
x3_full_kl <- read.csv("x3_model_comparison.csv", sep = ";")
x3_full_kl <- x3_full_kl[,c("workerid","Uniform.KL","P.only.KL","L.only.KL","OL.KL","PL.KL","POL.KL")]
colnames(x3_full_kl) <- c("workerid","uniform","preference","klFactor","obedience_klFactor","preference_klFactor","preference_obedience_klFactor")

# Does optimizing for obedience and klFactor improve fit compared to just klFactor? No
# Taking average kl values

totalDiff <- mean(x3_full_kl$preference_klFactor) - mean(x3_full_kl$klFactor) 
g2 <- totalDiff*2 # G^2 value
qchisq(.95, df=1) # to display cut-off point
g2
abs(g2) > qchisq(.95, df=1) 

totalDiff <- 444.0236 - 437.178 
g2 <- totalDiff*2 # G^2 value
qchisq(.95, df=3) # to display cut-off point
abs(g2) > qchisq(.975, df=3) 


# Does optimizing for preference softness and klFactor improve fit compared to just klFactor? No
# Summing up all differences

totalDiff <- sum(x3_full_kl$preference_klFactor - x3_full_kl$klFactor) 
g2 <- totalDiff*2
abs(g2) > qchisq(.95, df=82) 

# Does optimizing for obedience, softness, and klFactor improve fit compared to just klFactor? No

totalDiff <- sum(x3_full_kl$preference_obedience_klFactor - x3_full_kl$klFactor) 
g2 <- totalDiff*2
abs(g2) > qchisq(.95, df=82*2) 

################################### X3 Model comparison. Simple RSA ###############################################################
###################################### Likelihood ratio test ###################################################################



setwd("~/Documents/GitHub/prior_inference/RSA_2019_08")
x3_simple_kl <- read.csv("X3_Data/x3KLDivs_simpleRSA_indOpt_2019_10_11.csv")
x3_simple_kl$X <- NULL
colnames(x3_simple_kl) <- c("workerid","uniform","preference","obedience","klFactor","obedience_klFactor",
                            "preference_klFactor","preference_obedience_klFactor")

totalDiff <- sum(x3_simple_kl$preference_klFactor - x3_simple_kl$klFactor) 
g2 <- totalDiff*2
g2 > qchisq(.95, df=82) 

# Compare kl factor to uniform. Optimizing for KL factor improves the fit

totalDiff <- sum(x3_simple_kl$klFactor - x3_simple_kl$uniform) 
g2 <- totalDiff*2
abs(g2) > qchisq(.99, df=82)

######################### X4 Model comparison. Full RSA. Global optimization ####################################################
###################################### Likelihood ratio test ####################################################################

x4_full_global_kl <- read.csv("X4_Data/x4KLDivs_fullRSA_globalOpt_2019_1006.csv")
x4_full_global_kl$X <- NULL
colnames(x4_full_global_kl) <- c("workerid","uniform","preference","preference_obedience.01","obedience", "empty1","empty2",
                                 "preference_alpha","preference_obedience","preference_obedience_alpha")

## Preference and obedience vs just preference strength. No effect

totalDiff <- mean(x4_full_global_kl$preference_obedience - x4_full_global_kl$preference) 
g2 <- totalDiff*2
abs(g2) > qchisq(.95, df=1)

## Preference and obedience vs. just obedience. There is an effect but maybe driven by obedience... Check the value

totalDiff <- mean(x4_full_global_kl$preference_obedience - x4_full_global_kl$obedience) 
g2 <- totalDiff*2
abs(g2) > qchisq(.95, df=1)


## Three way parametrized vs. preference and obedience. No difference

totalDiff <- sum(x4_full_global_kl$preference_obedience_alpha - x4_full_global_kl$preference_obedience) 
g2 <- totalDiff*2
abs(g2) > qchisq(.95, df=2)

## Preference vs. uniform. Optimizing is better than uniform

totalDiff <- sum(x4_full_global_kl$preference - x4_full_global_kl$uniform) 
g2 <- totalDiff*2
abs(g2) > qchisq(.95, df=length(x4_full_global_kl$workerid))

######################### X4 Model comparison. Simple RSA. Global optimization ##################################################
###################################### Likelihood ratio test ####################################################################

x4_simple_global_kl <- read.csv("X4_Data/x4KLDivs_simpleRSA_globalOpt_2019_1009.csv")
x4_simple_global_kl$X <- NULL
colnames(x4_simple_global_kl) <- c("workerid","uniform","preference","preference_obedience.01", "preference_obedience","6","7","8","9","10")

totalDiff <- sum(x4_simple_global_kl$preference_obedience - x4_full_global_kl$preference) 
g2 <- totalDiff*2
abs(g2) > qchisq(.95, df=1) # Not sure about degrees of freedom


######################### X4 Model comparison. Full RSA. Individual optimization ################################################
###################################### Likelihood ratio test ####################################################################

x4_full_individual_kl <- read.csv("X4_Data/x4KLDivs_fullRSA_indOpt_2019_1006.csv")
x4_full_individual_kl$X <- NULL
colnames(x4_full_individual_kl) <- c("workerid","uniform","3","4","5","6","preference_alpha","preference_obedience", "preference_obedience_alpha")

totalDiff <- sum(x4_full_individual_kl$preference_obedience_alpha - x4_full_individual_kl$preference_obedience) 
g2 <- totalDiff*2
g2
abs(g2) > qchisq(.95, df=82)


######################### X4 Model comparison. Simple RSA. Individual optimization ##############################################
###################################### Likelihood ratio test ####################################################################

x4_simple_individual_kl <- read.csv("X4_Data/x4KLDivs_simpleRSA_indOpt_2019_1010.csv")
x4_simple_individual_kl$X <- NULL
colnames(x4_simple_individual_kl) <- c("workerid","uniform","preference","preference_obedience.02", "preference_obedience")


# Optimizing for softness and obedience improves the fit compared to just softness

totalDiff <- sum(x4_simple_individual_kl$preference_obedience - x4_simple_individual_kl$preference) 
g2 <- totalDiff*2
abs(g2) > qchisq(.99, df=length(x4_simple_individual_kl$workerid))

# Global vs individual

totalDiff <- sum(x4_simple_individual_kl$preference_obedience - x4_simple_global_kl$preference_obedience) 
g2 <- totalDiff*2
abs(g2) > qchisq(.99, df=(length(x4_simple_individual_kl$workerid)-1))

