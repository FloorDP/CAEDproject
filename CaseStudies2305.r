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
p_load( semTools,haven,ggplot2,plotly,tidyr,dplyr,RPostgreSQL, RPostgres,magrittr,jsonlite,purrr, stringr,anytime,lubridate,
        psych,e1071,lmerTest, afex, effectsize,car,lmerTest,sjstats,mice, insight,readxl)

options(contrasts = c("contr.sum","contr.poly"))

#import datasets, available at https://osf.io/6kzx3/

# DATASET 1: ESM data: social interactions (yes or no + mode of communication)

mywd= "C:/Users/floor/OneDrive/Documenten/MASTER/CAED" 
# mywd = "C:/Users/Gebruiker/Documents/Experimentele Psychologie/JAAR 2/Case studies"
# mywd = "/Users/britt/Desktop/Case studies"

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
### Datasets for mediation ###
# we chose to binarize the categorical mediator 'communication'

# Communication: in person vs. no communication
data_inperson <- data[data$communication != 2,]

# Communication: chat/phone vs no communication
data_chatphone <- data[data$communication != 1,]
data_chatphone$communication <- ifelse(data_chatphone$communication ==2, 1,0)


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
# correlatiematrix
data = data %>% select(Extraversion, Loneliness, interaction, communication)   
install.packages("GGally")
library(GGally)

res <- cor(data, use = "complete.obs")                          
round(res, 2)

# correlaties significant?
install.packages("Hmisc")
library("Hmisc")
p_values <- rcorr(as.matrix(data))   #p values of the correlation matrix
print(p_values)

ggcorr(data,palette = "RdBu" ,label=TRUE,digits=3, label_color = "black"
       , label_round=2, hjust=0.8)

################################################################################
### MAIN ANALYSIS ##

data$communication <- as.factor(data$communication)
data$id <- as.factor(data$id)

# MODERATION
# MODEL 1: main effects 
Lm1 = lm(Loneliness ~ Extraversion + communication, data = data) 
summary(Lm1) 
confint(Lm1)

# MODEL 2: moderation 
Lm2 = lm(Loneliness ~ Extraversion * communication, data = data) 
summary(Lm2)
confint(Lm2)

# MODEL COMPARISON
anova(Lm1, Lm2)

library(interactions)
interactions::interact_plot(Lm2, "Extraversion", modx = "communication")

emmeans(Lm2, pairwise ~ communication)
emtrends(Lm2, pairwise ~ communication, var = "Extraversion")

# ------------------------------------------------------------------------------
# MEDIATION 1

#Step 1: direct effect --> predictor should predict the outcome (YES)
Lm3 = lm(Loneliness ~ Extraversion, data = data)
summary(Lm3)
confint(Lm3)

#Step 2: effect of extraversion on communication/interaction --> predictor should predict the mediator
Lm4 = glm(communication ~ Extraversion, data = data, family = binomial)   # (YES)
summary(Lm4)
confint(Lm4)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome
Lm5 = lm(Loneliness ~ communication + Extraversion, data = data )   # ('In person' does have an effect)
summary(Lm5)
confint(Lm5)

#Step 4: inverting the regression --> outcome should not predict our predictor, controlling for the mediator
Lm6 = lm(Extraversion ~ communication + Loneliness, data = data )  # (IT DOES) --> No mediation
summary(Lm6)
confint(Lm6)


#### Mediation package (klopt niet momenteel)
# library(mediation)
# mediation <- mediate(Lm4, # Mediator model
#                      Lm5, # Outcome model
#                      sims=1000, # Number of bootstrap samples
#                      boot = TRUE, # Ask for bootstrapped confidence intervals
#                      treat ="Extraversion", # Name of the x variable
#                      mediator="communication", # Name of the m variable)
#                      ) 
# summary(mediation) # No mediation


################################################################################
# USING (G)LMER() INSTEAD OF (G)LM() 
# Britt: KLOPT NIET MOMENTEEL

# MODERATION 
# MODEL 1: main effects 
Lm7 = lmer(Loneliness ~ Extraversion + communication + (1 | id), data = data, 
           control = lmerControl(optimizer = 'bobyqa',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000))) 
summary(Lm7) 
confint(Lm7)

# MODEL 2: moderation 
Lm8 = lmer(Loneliness ~ Extraversion * communication + (1 | id), data = data,
           control = lmerControl(optimizer = 'nloptwrap',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000))) 
summary(Lm8)
confint(Lm8)

interactions::interact_plot(Lm8, "Extraversion", modx = "communication")

emmeans(Lm8, pairwise ~ communication)
emtrends(Lm8, pairwise ~ communication, var = "Extraversion")

# ------------------------------------------------------------------------------
# MEDIATION 1: effect of in person interactions

#Step 1: direct effect --> predictor should predict the outcome (NO)
Lm9 = lmer(Loneliness ~ Extraversion + (1 | id), data = data_inperson,
           control = lmerControl(optimizer = 'nloptwrap',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lm9)
confint(Lm9)

#Step 2: effect of extraversion on communication/interaction --> predictor should predict the mediator
Lm10 = glmer(communication ~ Extraversion + (1 | id), data = data_inperson, family = binomial,
             control = glmerControl(optimizer = 'bobyqa',
                                   calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))   # (NO)
summary(Lm10)
confint(Lm10)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome
Lm11 = lmer(Loneliness ~ communication + Extraversion + (1 | id), data = data_inperson,
            control = lmerControl(optimizer = 'nloptwrap',
                                  calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lm11)
confint(Lm11)

#Step 4: inverting the regression --> outcome should not predict our predictor, controlling for the mediator
Lm12 = lmer(Extraversion ~ communication + Loneliness + (1 | id), data = data_inperson,
            control = lmerControl(optimizer = 'nloptwrap',
                                  calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lm12)
confint(Lm12)

# ------------------------------------------------------------------------------
# MEDIATION 2: effect of chat/phone interactions

#Step 1: direct effect --> predictor should predict the outcome (NO)
Lm9 = lmer(Loneliness ~ Extraversion + (1 | id), data = data_chatphone,
           control = lmerControl(optimizer = 'nloptwrap',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lm9)
confint(Lm9)

#Step 2: effect of extraversion on communication/interaction --> predictor should predict the mediator
Lm10 = glmer(communication ~ Extraversion + (1 | id), data = data_chatphone, family = binomial,
             control = glmerControl(optimizer = 'bobyqa',
                                    calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))   # (NO)
summary(Lm10)
confint(Lm10)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome
Lm11 = lmer(Loneliness ~ communication + Extraversion + (1 | id), data = data_chatphone,
            control = lmerControl(optimizer = 'nloptwrap',
                                  calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lm11)
confint(Lm11)

#Step 4: inverting the regression --> outcome should not predict our predictor, controlling for the mediator
Lm12 = lmer(Extraversion ~ communication + Loneliness + (1 | id), data = data_inperson,
            control = lmerControl(optimizer = 'nloptwrap',
                                  calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))
summary(Lm12)
confint(Lm12)


#### Mediation package
# mediation <- mediate(Lm10, # Mediator model
#                      Lm11, # Outcome model
#                      sims=1000, # Number of bootstrap samples
#                      boot = FALSE, # Ask for bootstrapped confidence intervals
#                      treat ="Extraversion", # Name of the x variable
#                      mediator="communication" # Name of the m variable
# )
# summary(mediation) # No mediation

#try it with lavaan
model212<-'
level:1
interaction~1
level:2
Loneliness~b*interaction + c*Extraversion
interaction~a*Extraversion
#indirect and total effects
ab:=a*b
total:=ab+c'

fit212<- sem(model212, data=data, cluster="id")
summary(fit212)
lavInspect(fit212, icc)
