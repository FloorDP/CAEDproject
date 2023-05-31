## Project Case Studies in the Analysis of Experimental Data
## Behavioral data

## Authors: Stephanie Van De Walle, Britt Di Vita & Floor Depestele

#load packages
library(pacman)
library(ltm)
library(reshape2)
library(Rmisc)
library(lavaan)
library(emmeans)
library(lme4)
library(optimx)
library(dfoptim)
library(interactions)
library(Hmisc)
library(GGally)
library(mediation)
p_load( semTools,haven,ggplot2,plotly,tidyr,dplyr,RPostgreSQL, RPostgres,magrittr,jsonlite,purrr, stringr,anytime,lubridate,
        psych,e1071,lmerTest, afex, effectsize,car,lmerTest,sjstats,mice, insight,readxl)

options(contrasts = c("contr.sum","contr.poly"))

#import datasets, available at https://osf.io/6kzx3/

# DATASET 1: ESM data: social interactions (yes or no + mode of communication)

# mywd= "C:/Users/floor/OneDrive/Documenten/MASTER/CAED" 
# mywd = "C:/Users/Gebruiker/Documents/Experimentele Psychologie/JAAR 2/Case studies"
mywd = "/Users/britt/Desktop/Case studies"

# DATASET 1: data including communication & interaction
mydata <- read.csv(str_glue(mywd,"/Study2_Wave1_ESMdata_Socinteract.csv")) 
mydata <- subset(mydata, select = c(id, created_esm, communication, interaction))  
mydata$communication[is.na(mydata$communication)] <- 0    # When people indicated "no interaction, the column of communication was empty 
# --> replaced missing value with 0

# communication: 0 = no interaction, 1 = in person, 2 = phone/chat
# interaction: 1 = yes, 2 = no

# DATASET 2: TRAIT data: extraversion (BFI-2-S) and loneliness (ULS)
mydata_trait = read.csv(str_glue(mywd, "/Study2_Wave1_Traitdata_Extraversion-Loneliness.csv"))
mydata_trait <- subset(mydata_trait, select = c(id, 
                                                bfi2s_1_t2, bfi2s_6_t2, bfi2s_11_t2, bfi2s_16_t2, bfi2s_21_t2, bfi2s_26_t2,
                                                uls_1_t2, uls_2_t2,  uls_3_t2,  uls_4_t2,  uls_5_t2,  uls_6_t2,  uls_7_t2,  uls_8_t2,  uls_9_t2))

# merge datasets of interaction/communication and BFI/ULS
data = merge(x = mydata, y = mydata_trait, by = "id") 

Numberofpp <- n_distinct(data$id) #data of 1645 participants before cleaning

# remove missing data (NA) --> exclude participants who did not fill in the BFI and ULS
data <- na.omit(data)
summary(data)
#586 participants left

# Exclude participants who filled in the questions about communication and interaction less than half of the days (< 7 days)
extra <- colsplit(data$created_esm, pattern = " ", names = c("Date", "Time"))
data <- cbind(data, extra$Date)
names(data) <- c("id", "created_esm", "communication", "interaction", "bfi2s_1_t2", "bfi2s_6_t2", "bfi2s_11_t2", "bfi2s_16_t2", "bfi2s_21_t2", "bfi2s_26_t2",
                 "uls_1_t2", "uls_2_t2",  "uls_3_t2",  "uls_4_t2",  "uls_5_t2",  "uls_6_t2",  "uls_7_t2",  "uls_8_t2",  "uls_9_t2", "date")
number_of_dates_per_id <- aggregate(date ~ id, FUN = function(x) length(unique(x)), data = data)
ppts_less7days <- subset(number_of_dates_per_id, date < 7)      # --> exclude 263 participants
data_excluded_pp <- data[!(data$id %in% ppts_less7days$id),]

# Numberofpp <- n_distinct(data_excluded_pp$id) --> 323 participants left

data <- subset(data_excluded_pp, select = c("id", "date", "communication", "interaction", "bfi2s_1_t2", "bfi2s_6_t2", "bfi2s_11_t2", "bfi2s_16_t2", "bfi2s_21_t2", "bfi2s_26_t2",
                                            "uls_1_t2", "uls_2_t2",  "uls_3_t2",  "uls_4_t2",  "uls_5_t2",  "uls_6_t2",  "uls_7_t2",  "uls_8_t2",  "uls_9_t2", "date"))
data$communication <- as.factor(data$communication)
data$id <- as.factor(data$id)

# Reverse scores extraversion & loneliness 
# Extraversion items 1, 21 and 26
# Loneliness items 2,3,5,6,7,9
reverse_cols = c("bfi2s_1_t2", "bfi2s_21_t2", "bfi2s_26_t2",
                 "uls_2_t2",  "uls_3_t2",  "uls_5_t2",  "uls_6_t2",  "uls_7_t2",  "uls_9_t2")
data[ , reverse_cols] = 6 - data[ , reverse_cols]


# 1) INTERNE CONSISTENCY + RELIABILITY: Crohnbachs alpha

#Extraversion 
Extraversion_alpha <- data.frame(data[5:10])
cronbach.alpha(Extraversion_alpha) 

#Loneliness 
Loneliness_alpha <- data.frame(data[11:19])
cronbach.alpha(Loneliness_alpha)

################################################################################

#Extraversion score per participant
data$Extraversion <- data$bfi2s_1_t2 + data$bfi2s_6_t2 + data$bfi2s_11_t2 + data$bfi2s_16_t2 + data$bfi2s_21_t2 + data$bfi2s_26_t2

#Loneliness score per participant 
data$Loneliness <- data$uls_1_t2 + data$uls_2_t2 + data$uls_3_t2 + data$uls_4_t2 + data$uls_5_t2 + data$uls_6_t2 + data$uls_7_t2 + data$uls_8_t2 + data$uls_9_t2 

################################################################################
# mean(data$Extraversion)  
# sd(data$Extraversion)    
# 
# mean(data$Loneliness)
# sd(data$Loneliness)
# 
# mean(data$interaction)
# sd(data$interaction)
# 
# mean(data$communication)
# sd(data$communication)

################################################################################
# # correlationmatrix
# data_cor = data %>% select(Extraversion, Loneliness, interaction, communication)   
# 
# res <- cor(data_cor, use = "complete.obs")                          
# round(res, 2)
# 
# # correlations significant?
# p_values <- rcorr(as.matrix(data_cor))   # p-values of the correlation matrix
# print(p_values)
# 
# ggcorr(data_cor,palette = "RdBu" ,label=TRUE,digits=3, label_color = "black"
#        , label_round=2, hjust=0.8)

################################################################################
### MAIN ANALYSIS - MODERATION ###

# MODEL 1: main effects 
Lm_mod1 = lm(Loneliness ~ Extraversion + communication, data = data) 
summary(Lm_mod1) 
confint(Lm_mod1)

# MODEL 2: moderation 
Lm_mod2 = lm(Loneliness ~ Extraversion * communication, data = data) 
summary(Lm_mod2)
confint(Lm_mod2)

# # MODEL COMPARISON
# anova(Lm_mod2, Lm_mod2)

interactions::interact_plot(Lm_mod2, "Extraversion", modx = "communication")

emmeans(Lm_mod2, pairwise ~ communication)
emtrends(Lm_mod2, pairwise ~ communication, var = "Extraversion")
# ------------------------------------------------------------------------------
# MODERATION with Mixed Models

# MODEL 1: main effects 
Lmer_mod1 = lmer(Loneliness ~ Extraversion + communication + (1 | id), data = data, 
           control = lmerControl(optimizer = 'nmkbw',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000))) 

#Anova(Lmer_mod1, type = "III", test.statistic = "F")
summary(Lmer_mod1) 
confint(Lmer_mod1)

# MODEL 2: moderation 
Lmer_mod2 = lmer(Loneliness ~ Extraversion * communication + (1 | id), data = data,
           control = lmerControl(optimizer = 'nmkbw',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000))) 
#Anova(Lmer_mod2, type = "III", test.statistic = "F")
summary(Lmer_mod2)
confint(Lmer_mod2)

interactions::interact_plot(Lmer_mod2, "Extraversion", modx = "communication")

emmeans(Lmer_mod2, pairwise ~ communication)
emtrends(Lmer_mod2, pairwise ~ communication, var = "Extraversion")


################################################################################
### MAIN ANALYSIS - MEDIATION ###

# Datasets for mediation #
# We chose to binarize the categorical mediator 'communication'

# Communication: in person vs. no communication
data_inperson <- data[data$communication != 2,]

data_inperson$communication <- as.factor(data_inperson$communication)
data_inperson$id <- as.factor(data_inperson$id)

# Communication: chat/phone vs no communication
data_chatphone <- data[data$communication != 1,]
data_chatphone$communication <- ifelse(data_chatphone$communication ==2, 1,0)

data_chatphone$communication <- as.factor(data_chatphone$communication)
data_chatphone$id <- as.factor(data_chatphone$id)

# Ik denk dat de as.factor niet nodig is omdat het in de originele data al factor was
# ----------------------------

# MEDIATION (IN PERSON COMMUNICATION)

#Step 1: direct effect --> predictor should predict the outcome                         # YES
Lm_med1 = lm(Loneliness ~ Extraversion, data = data_inperson)
summary(Lm_med1)
confint(Lm_med1)

#Step 2: effect of extraversion on communication/interaction --> predictor should predict the mediator
Lm_med2 = glm(communication ~ Extraversion, data = data_inperson, family = binomial)    # YES
summary(Lm_med2)
confint(Lm_med2)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome
Lm_med3 = lm(Loneliness ~ communication + Extraversion, data = data_inperson )          # 'In person' does have an effect
summary(Lm_med3)
confint(Lm_med3)

#Step 4: inverting the regression --> outcome should not predict our predictor, controlling for the mediator
Lm_med4 = lm(Extraversion ~ communication + Loneliness, data = data_inperson )          # IT DOES --> No mediation?
summary(Lm_med4)
confint(Lm_med4)

# ------------------------------------------------------------------------------------
# MEDIATION (IN PERSON COMMUNICATION) with Mixed Models 

#Step 1: direct effect --> predictor should predict the outcome                                   # YES
Lmer_med1 = lmer(Loneliness ~ Extraversion + (1 | id), data = data_inperson,
           control = lmerControl(optimizer = 'nmkbw',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lmer_med1)
confint(Lmer_med1)
Anova(Lmer_med1, type = "III", test.statistic = "F")

# lmall <- allFit(Lmer_med1)
# summary(lmall)

#Step 2: effect of extraversion on communication/interaction --> predictor should predict the mediator
Lmer_med2 = glmer(communication ~ Extraversion + (1 | id), data = data_inperson, family = binomial,
             control = glmerControl(optimizer = 'bobyqa',
                                    calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))        # NO
summary(Lmer_med2)
confint(Lmer_med2)


#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome  # NO
Lmer_med3 = lmer(Loneliness ~ communication + Extraversion + (1 | id), data = data_inperson,
            control = lmerControl(optimizer = 'nmkbw',
                                  calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lmer_med3)
confint(Lmer_med3)
Anova(Lmer_med3, type = "III", test.statistic = "F")

#Step 4: inverting the regression --> outcome should not predict our predictor, controlling for the mediator
Lmer_med4 = lmer(Extraversion ~ communication + Loneliness + (1 | id), data = data_inperson,
            control = lmerControl(optimizer = 'nmkbw',
                                  calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lmer_med4)
confint(Lmer_med4)
Anova(Lmer_med4, type = "III", test.statistic = "F")

# ------------------------------------------------------------------------------
# MEDIATION (ONLINE COMMUNICATION)

#Step 1: direct effect --> predictor should predict the outcome (YES)
Lm_medonl1 = lm(Loneliness ~ Extraversion, data = data_chatphone)
summary(Lm_medonl1)
confint(Lm_medonl1)

#Step 2: effect of extraversion on communication/interaction --> predictor should predict the mediator
Lm_medonl2 = glm(communication ~ Extraversion, data = data_chatphone, family = binomial)   # (YES)
summary(Lm_medonl2)
confint(Lm_medonl2)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome
Lm_medonl3 = lm(Loneliness ~ communication + Extraversion, data = data_chatphone )   # (marginally significant)
summary(Lm_medonl3)
confint(Lm_medonl3)

#Step 4: inverting the regression --> outcome should not predict our predictor, controlling for the mediator
Lm_medonl4 = lm(Extraversion ~ communication + Loneliness, data = data_chatphone )  # (IT DOES) --> No mediation
summary(Lm_medonl4)
confint(Lm_medonl4)

# ------------------------------------------------------------------------------
# MEDIATION (ONLINE COMMUNICATION) with Mixed Models 

#Step 1: direct effect --> predictor should predict the outcome # (YES)
Lmer_medonl1 = lmer(Loneliness ~ Extraversion + (1 | id), data = data_chatphone,
           control = lmerControl(optimizer = 'nmkbw',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lmer_medonl1)
confint(Lmer_medonl1)
Anova(Lmer_medonl1, type = "III", test.statistic = "F")


#Step 2: effect of extraversion on communication/interaction --> predictor should predict the mediator # (NO)
Lmer_medonl2 = glmer(communication ~ Extraversion + (1 | id), data = data_chatphone, family = binomial,
             control = glmerControl(optimizer = 'bobyqa',
                                    calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))   
summary(Lmer_medonl2)
confint(Lmer_medonl2)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome # (NO)
Lmer_medonl3 = lmer(Loneliness ~ communication + Extraversion + (1 | id), data = data_chatphone,
            control = lmerControl(optimizer = 'bobyqa',
                                  calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lmer_medonl3)
confint(Lmer_medonl3)
#Anova(Lmer_medonl3, type = "III", test.statistic = "F")


#Step 4: inverting the regression --> outcome should not predict our predictor, controlling for the mediator
Lmer_medonl4 = lmer(Extraversion ~ communication + Loneliness + (1 | id), data = data_chatphone,
            control = lmerControl(optimizer = 'bobyqa',
                                  calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lmer_medonl4)
confint(Lmer_medonl4)
Anova(Lmer_medonl4, type = "III", test.statistic = "F")


#try it with lavaan
model212<-'
level:1
communication~1
level:2
Loneliness~b*communication + c*Extraversion
communication~a*Extraversion
#indirect and total effects
ab:=a*b
total:=ab+c'

fit212<- sem(model212, data=data_chatphone, cluster="id")
summary(fit212)
lavInspect(fit212, icc)

################################################################################
#### Mediation package
data_inperson$communication <- as.numeric(data_inperson$communication)
data_chatphone$communication <- as.numeric(data_chatphone$communication)

# IN PERSON
# Mediator model
Med1 = lm(communication ~ Extraversion, data = data_inperson)

# Outcome model
Med2 = lm(Loneliness ~ communication + Extraversion, data = data_inperson )

mediation_person <- mediation::mediate(Med1, # Mediator model
                                       Med2, # Outcome model
                                sims=2000, # Number of bootstrap samples
                                boot = TRUE, # Ask for bootstrapped confidence intervals
                                treat ="Extraversion", # Name of the x variable
                                mediator="communication", # Name of the m variable)
                                robustSE = FALSE)
summary(mediation_person) 
# ACME (Average Causal Mediation Effects) should be significant (yes)


# IN PERSON
# Mediator model
Med3 = lm(communication ~ Extraversion, data = data_chatphone)

# Outcome model
Med4 = lm(Loneliness ~ communication + Extraversion, data = data_chatphone)

mediation_online <- mediation::mediate(Med3, # Mediator model
                                       Med4, # Outcome model
                                       sims=2000, # Number of bootstrap samples
                                       boot = TRUE, # Ask for bootstrapped confidence intervals
                                       treat ="Extraversion", # Name of the x variable
                                       mediator="communication", # Name of the m variable)
                                       robustSE = FALSE)
summary(mediation_online) # No mediation
# ACME (Average Causal Mediation Effects) should be significant (no)
