## Project Case Studies in the Analysis of Experimental Data
## Behavioral data

## Authors: Stephanie Van De Walle, Britt Di Vita & Floor Depestele

#load packages
library(stringr)
library(reshape2)
library(dplyr)
library(Rmisc)
library(ltm)
library(emmeans)
library(interactions)
library(lme4)
library(afex)
library(car)
library(lmerTest)
library(lavaan)
library(optimx)
library(dfoptim)
library(mediation)
library(ggplot2)
library(lattice)
library(DHARMa)
library(performance)

mywd= "C:/Users/floor/OneDrive/Documenten/MASTER/CAED" 
# mywd = "C:/Users/Gebruiker/Documents/Experimentele Psychologie/JAAR 2/Case studies"
#mywd = "/Users/britt/Desktop/Case studies"

### Import datasets, available at https://osf.io/6kzx3/ ###

# DATASET 1: data including communication
mydata <- read.csv(str_glue(mywd,"/Study2_Wave1_ESMdata_Socinteract.csv")) 
mydata <- subset(mydata, select = c(id, created_esm, communication))  
mydata$communication[is.na(mydata$communication)] <- 0    # When people indicated "no interaction, the column of communication was empty 
                                                          # --> replaced missing value with 0

# communication: 0 = no interaction, 1 = in person, 2 = phone/chat

# DATASET 2: TRAIT data: extraversion (BFI-2-S) and loneliness (ULS)
mydata_trait = read.csv(str_glue(mywd, "/Study2_Wave1_Traitdata_Extraversion-Loneliness.csv"))
mydata_trait <- subset(mydata_trait, select = c(id, 
                                                bfi2s_1_t2, bfi2s_6_t2, bfi2s_11_t2, bfi2s_16_t2, bfi2s_21_t2, bfi2s_26_t2,
                                                uls_1_t2, uls_2_t2,  uls_3_t2,  uls_4_t2,  uls_5_t2,  uls_6_t2,  uls_7_t2,  uls_8_t2,  uls_9_t2))

# Merge datasets of communication and BFI/ULS
data = merge(x = mydata, y = mydata_trait, by = "id") 
# n_distinct(data$id) # --> data of 1645 participants before cleaning

# Remove missing data (NA) --> exclude participants who did not fill in the BFI and ULS
data <- na.omit(data)
summary(data)
# n_distinct(data$id) # --> 586 participants left

# Exclude participants who filled out the questions about communication and interaction less than half of the days (< 7 days)
extra <- colsplit(data$created_esm, pattern = " ", names = c("Date", "Time"))
data <- cbind(data, extra$Date)
names(data) <- c("id", "created_esm", "communication", "bfi2s_1_t2", "bfi2s_6_t2", "bfi2s_11_t2", "bfi2s_16_t2", "bfi2s_21_t2", "bfi2s_26_t2",
                 "uls_1_t2", "uls_2_t2", "uls_3_t2", "uls_4_t2", "uls_5_t2", "uls_6_t2","uls_7_t2", "uls_8_t2","uls_9_t2", "date")
number_of_dates_per_id <- aggregate(date ~ id, FUN = function(x) length(unique(x)), data = data)
ppts_less7days <- subset(number_of_dates_per_id, date < 7)      # --> exclude 263 participants
data_excluded_pp <- data[!(data$id %in% ppts_less7days$id),]
# n_distinct(data_excluded_pp$id) # --> 323 participants left

data <- subset(data_excluded_pp, select = c("id", "communication", "bfi2s_1_t2", "bfi2s_6_t2", "bfi2s_11_t2", "bfi2s_16_t2", "bfi2s_21_t2", "bfi2s_26_t2",
                                            "uls_1_t2", "uls_2_t2", "uls_3_t2", "uls_4_t2", "uls_5_t2", "uls_6_t2", "uls_7_t2", "uls_8_t2", "uls_9_t2"))

# Participants excluded based on non-normal results (Normality assumption)
data <- subset(data, id != "849" & id != "653" & id != "1588")

data$communication <- as.factor(data$communication)
data$id <- as.factor(data$id)

# Reverse scores extraversion & loneliness 
# Extraversion items 1, 21 and 26
# Loneliness items 2,3,5,6,7,9
reverse_cols = c("bfi2s_1_t2", "bfi2s_21_t2", "bfi2s_26_t2",
                 "uls_2_t2",  "uls_3_t2",  "uls_5_t2",  "uls_6_t2",  "uls_7_t2",  "uls_9_t2")
data[ , reverse_cols] = 6 - data[ , reverse_cols]

################################################################################
# INTERN CONSISTENCY + RELIABILITY: Crohnbachs alpha
# Extraversion 
Extraversion_alpha <- data.frame(data[3:8])
cronbach.alpha(Extraversion_alpha) 

# Loneliness 
Loneliness_alpha <- data.frame(data[9:17])
cronbach.alpha(Loneliness_alpha)

################################################################################
# Extraversion score per participant
data$Extraversion <- data$bfi2s_1_t2 + data$bfi2s_6_t2 + data$bfi2s_11_t2 + data$bfi2s_16_t2 + data$bfi2s_21_t2 + data$bfi2s_26_t2

# Loneliness score per participant 
data$Loneliness <- data$uls_1_t2 + data$uls_2_t2 + data$uls_3_t2 + data$uls_4_t2 + data$uls_5_t2 + data$uls_6_t2 + data$uls_7_t2 + data$uls_8_t2 + data$uls_9_t2 

################################################################################
# Descriptives 
mean(data$Extraversion) # 19.61
sd(data$Extraversion)   # 3.89

mean(data$Loneliness)   # 21.78 
sd(data$Loneliness)     # 5.81

# Histogram Extraversion
Plot_Extraversion = summarySE(data, measurevar="Extraversion", groupvars = 'id')
ggplot(Plot_Extraversion, aes(x=Extraversion)) + 
  labs(x = "Score BFI Extraversion")+
  theme_classic()+
  geom_bar(color="steelblue", fill="steelblue")

# Histogram Loneliness
Plot_Loneliness = summarySE(data, measurevar="Loneliness", groupvars = 'id')
ggplot(Plot_Loneliness, aes(x=Loneliness)) + 
  labs(x = "Score UCLA Loneliness scale")+
  theme_classic()+
  geom_bar(color="steelblue", fill="steelblue")

################################################################################
# Standardize continuous variables (log-transforming did not affect homoscedasticity or normality)
# data$Loneliness <- c(log(data$Loneliness))
# data$Extraversion <- c(log(data$Extraversion))
data$Loneliness <- c(scale(data$Loneliness))
data$Extraversion <- c(scale(data$Extraversion))

################################################################################
### MAIN ANALYSIS - MODERATION ###
options(contrasts = c("contr.sum","contr.poly"))

# MODEL 1: main effects 
Lm_mod1 = lm(Loneliness ~ Extraversion + communication, data = data)    # Extraversion & communication have an effect
Anova(Lm_mod1, type = "III")
summary(Lm_mod1) 
confint(Lm_mod1)

# MODEL 2: moderation 
Lm_mod2 = lm(Loneliness ~ Extraversion * communication, data = data)    # Interaction effect of Extraversion & communication
Anova(Lm_mod2, type = "III")
summary(Lm_mod2)
confint(Lm_mod2)

# MODEL COMPARISON
anova(Lm_mod1, Lm_mod2) 
# = Anova above
# Adding the interaction effect led to an improvement of the model fit (p = 0.02299)

# Plot interaction model
interact_plot(Lm_mod2, "Extraversion", modx = "communication", 
              x.label = "Extraversion (z-scored)", y.label = "Loneliness (z-scored)",
              legend.main = "Communication type", modx.labels = c("No communication", "In person", "Chat/phone"))

# Post hoc pairwise comparison of the interaction effect
emtrends(Lm_mod2, pairwise ~ communication, var = "Extraversion") 
# Difference between no communication0:Extraversion and in person communication1:Extraversion is significant
# i.e., no communication vs. in person

# ------------------------------------------------------------------------------
# MODERATION with Mixed Models

# MODEL 1: main effects 
Lmer_mod1 = lmer(Loneliness ~ Extraversion + communication + (1 | id), data = data,        # Only Extraversion has an effect 
           control = lmerControl(optimizer = 'nmkbw',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000))) 
Anova(Lmer_mod1, type = "III", test.statistic = "F")
summary(Lmer_mod1) 
confint(Lmer_mod1)

# MODEL 2: moderation 
Lmer_mod2 = lmer(Loneliness ~ Extraversion * communication + (1 | id), data = data,        # No interaction effect of Extraversion & communication
           control = lmerControl(optimizer = 'nmkbw',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000))) 
Anova(Lmer_mod2, type = "III", test.statistic = "F")
summary(Lmer_mod2)
confint(Lmer_mod2)

################################################################################
### MAIN ANALYSIS - MEDIATION - Linear Models ###

# Datasets for mediation #
# We chose to binarize the categorical mediator 'communication' and create seperate datasets 
# for 'in person communication' and 'online communication'

# Communication: in person vs. no communication
data_inperson <- data[data$communication != 2,]

# Communication: chat/phone vs no communication
data_chatphone <- data[data$communication != 1,]
data_chatphone$communication <- ifelse(data_chatphone$communication ==2, 1,0)

# ------------------------------------------------------------------------------
# MEDIATION (IN PERSON COMMUNICATION)

#Step 1: direct effect --> predictor should predict the outcome                         # YES
Lm_med1 = lm(Loneliness ~ Extraversion, data = data_inperson)
Anova(Lm_med1, type = "III")
summary(Lm_med1)
confint(Lm_med1)

#Step 2: effect of extraversion on communication --> predictor should predict the mediator
Lm_med2 = glm(communication ~ Extraversion, data = data_inperson, family = binomial)    # YES
Anova(Lm_med2, type = "III")
summary(Lm_med2)
confint(Lm_med2)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome
#Step 4: Is there an effect of extraversion on loneliness after controlling for for communication (if yes: partial mediation)
Lm_med3 = lm(Loneliness ~ communication + Extraversion, data = data_inperson )          # YES
Anova(Lm_med3, type = "III")
summary(Lm_med3)
confint(Lm_med3)

# ------------------------------------------------------------------------------
# MEDIATION (ONLINE COMMUNICATION)

#Step 1: direct effect --> predictor should predict the outcome                           # YES
Lm_medonl1 = lm(Loneliness ~ Extraversion, data = data_chatphone)
Anova(Lm_medonl1, type = "III")
summary(Lm_medonl1)
confint(Lm_medonl1)

#Step 2: effect of extraversion on communication --> predictor should predict the mediator
Lm_medonl2 = glm(communication ~ Extraversion, data = data_chatphone, family = binomial)  # YES
Anova(Lm_medonl2, type = "III")
summary(Lm_medonl2)
confint(Lm_medonl2)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome
#Step 4: Is there an effect of extraversion on loneliness after controlling for for communication (if yes: partial mediation)
Lm_medonl3 = lm(Loneliness ~ communication + Extraversion, data = data_chatphone )        # Marginally significant
Anova(Lm_medonl3, type = "III")
summary(Lm_medonl3)
confint(Lm_medonl3)

################################################################################
#### Mediation package ###
# https://github.com/cran/mediation/blob/master/R/mediate.R
# "For binary response models, the 'mediator' must be a numeric variable with values 0 or 1 as opposed to a factor."
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
# ACME (Average Causal Mediation Effects) should be significant (yes) --> Mediation

# ONLINE
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
# ACME (Average Causal Mediation Effects) should be significant (no) --> No mediation

################################################################################
#### Lavaan package ###
# "Categorical (binary) variables must be ordered and coded as 0 and 1."
data_inperson$communication <- factor(data_inperson$communication, ordered = TRUE)
data_chatphone$communication <- factor(data_chatphone$communication, ordered = TRUE)

model <- ' # direct effect
             Loneliness ~ c*Extraversion
           # mediator
             communication ~ a*Extraversion
             Loneliness ~ b*communication
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit_inpersion <- lavaan::sem(model, data = data_inperson)
summary(fit_inpersion)
# Indirect effect (ab) --> Mediation

fit_online <- lavaan::sem(model, data = data_chatphone)
summary(fit_online)
# No indirect effect --> No mediation

################################################################################
# MEDIATION (IN PERSON COMMUNICATION) with Mixed Models 
data_inperson$communication <- factor(data_inperson$communication)
data_chatphone$communication <- factor(data_chatphone$communication)

#Step 1: direct effect --> predictor should predict the outcome                                               # YES
Lmer_med1 = lmer(Loneliness ~ Extraversion + (1 | id), data = data_inperson,
                 control = lmerControl(optimizer = 'nmkbw',
                                       calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))

Anova(Lmer_med1, type = "III", test.statistic = "F")
summary(Lmer_med1)
confint(Lmer_med1)

# Check which optimizer performs best
# lmall <- allFit(Lmer_med1)
# summary(lmall)

#Step 2: effect of extraversion on communication --> predictor should predict the mediator                    # NO
Lmer_med2 = mixed(communication ~ Extraversion + (1 | id), data = data_inperson, family = binomial,
                  control = glmerControl(optimizer = 'bobyqa', calc.derivs = FALSE, 
                                         optCtrl = list(maxfun = 100000)),
                  type = 3, method = "LRT", test_intercept = TRUE)               
anova(Lmer_med2)
summary(Lmer_med2)
confint(Lmer_med2)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome              # NO
Lmer_med3 = lmer(Loneliness ~ communication + Extraversion + (1 | id), data = data_inperson,
                 control = lmerControl(optimizer = 'nmkbw',
                                       calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))

Anova(Lmer_med3, type = "III", test.statistic = "F")
summary(Lmer_med3)
confint(Lmer_med3)

# ------------------------------------------------------------------------------
# MEDIATION (ONLINE COMMUNICATION) with Mixed Models 

#Step 1: direct effect --> predictor should predict the outcome                                               # YES
Lmer_medonl1 = lmer(Loneliness ~ Extraversion + (1 | id), data = data_chatphone,
           control = lmerControl(optimizer = 'nmkbw',
                                 calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))

Anova(Lmer_medonl1, type = "III", test.statistic = "F")
summary(Lmer_medonl1)
confint(Lmer_medonl1)

#Step 2: effect of extraversion on communication --> predictor should predict the mediator                    # NO
Lmer_medonl2 = mixed(communication ~ Extraversion + (1 | id), data = data_chatphone, family = binomial,
             control = glmerControl(optimizer = 'bobyqa', calc.derivs = FALSE, 
                                    optCtrl = list(maxfun = 100000)),
             type = 3, method = "LRT", test_intercept = TRUE)   

anova(Lmer_medonl2)
summary(Lmer_medonl2)
confint(Lmer_medonl2)

#Step 3: Regress loneliness on mediator and extraversion --> mediator should predict the outcome              # NO
Lmer_medonl3 = lmer(Loneliness ~ communication + Extraversion + (1 | id), data = data_chatphone,
            control = lmerControl(optimizer = 'nmkbw',
                                  calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))

Anova(Lmer_medonl3, type = "III", test.statistic = "F")
summary(Lmer_medonl3)
confint(Lmer_medonl3)

################################################################################
#### Mediation package ###
# https://github.com/cran/mediation/blob/master/R/mediate.R
# "For binary response models, the 'mediator' must be a numeric variable with values 0 or 1 as opposed to a factor."
data_inperson$communication <- as.numeric(data_inperson$communication)
data_chatphone$communication <- as.numeric(data_chatphone$communication)

# lmerTest must be unloaded for the mediation package to use merModLmerTest instead of lmerTest. 
# Every package using lmerTest must therefore also be unloaded.
detach('package:afex', unload = TRUE)
detach('package:lmerTest', unload = TRUE)

# IN PERSON
# Mediator model
Med_lmer1 = lmer(communication ~ Extraversion + (1 | id), data = data_inperson,
                  control = lmerControl(optimizer = 'nmkbw',
                                         calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))

# Outcome model
Med_lmer2 = lmer(Loneliness ~ communication + Extraversion + (1 | id), data = data_inperson,
                 control = lmerControl(optimizer = 'nmkbw',
                                       calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))

mediation_lmer_person <- mediation::mediate(Med_lmer1, # Mediator model
                                            Med_lmer2, # Outcome model
                                            sims=2000, # Number of bootstrap samples
                                            boot = FALSE, # Ask for bootstrapped confidence intervals
                                            treat ="Extraversion", # Name of the x variable
                                            mediator="communication", # Name of the m variable)
                                            robustSE = FALSE)
summary(mediation_lmer_person) 
# ACME (Average Causal Mediation Effects) should be significant (no) --> No mediation

# ONLINE
# Mediator model
Med_lmer3 = lmer(communication ~ Extraversion + (1 | id), data = data_chatphone,
                  control = lmerControl(optimizer = 'nmkbw',
                                         calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))

# Outcome model
Med_lmer4 = lmer(Loneliness ~ communication + Extraversion + (1 | id), data = data_chatphone,
                 control = lmerControl(optimizer = 'nmkbw',
                                       calc.derivs = FALSE, optCtrl = list(maxfun = 100000)))

mediation_lmer_online <- mediation::mediate(Med_lmer1, # Mediator model
                                            Med_lmer2, # Outcome model
                                            sims=2000, # Number of bootstrap samples
                                            boot = FALSE, # Ask for bootstrapped confidence intervals
                                            treat ="Extraversion", # Name of the x variable
                                            mediator="communication", # Name of the m variable)
                                            robustSE = FALSE)
summary(mediation_lmer_online) 
# ACME (Average Causal Mediation Effects) should be significant (no) --> No mediation

# DON'T FORGET TO FACTORIZE COMMUNICATION AGAIN WHEN REFITTING OTHER MODELS ABOVE!!

################################################################################
# Assumption checks
# We checked the linear mixed models for normality, homoscedasticity, linearity and multicollinearity by 
# replacing the model name (and dataframe) in the script below. 

### LMER ###
# Check for outliers 
sum(abs(resid(Lmer_mod2, scaled = TRUE)) > 3.5) / length(resid(Lmer_med1))    # Should be 0
sum(abs(resid(Lmer_mod2, scaled = TRUE)) > 2.5) / length(resid(Lmer_med1))    # Max 1%
sum(abs(resid(Lmer_mod2, scaled = TRUE)) > 2) / length(resid(Lmer_med1))      # Max 5%

# Check normality
qqnorm(resid(Lmer_medonl1))
qqline(resid(Lmer_medonl1))
# qqPlot(resid(Lmer_medonl1))
densityplot(resid(Lmer_medonl1,scaled=TRUE), xlab = "Residuals")

# Check homoscedasticity + linearity
ggplot(data_chatphone, aes(x = fitted(Lmer_medonl1), y = resid(Lmer_medonl1))) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Check multicollinearity in case of 2 predictors
vif(Lmer_mod1)

### GLMER ###
# Check for outliers
sum(abs(resid(Lmer_med2$full_model, scaled = TRUE)) > 3.5) / length(resid(Lmer_med2$full_model))    # Should be 0
sum(abs(resid(Lmer_med2$full_model, scaled = TRUE)) > 2.5) / length(resid(Lmer_med2$full_model))    # Max 1%
sum(abs(resid(Lmer_med2$full_model, scaled = TRUE)) > 2) / length(resid(Lmer_med2$full_model))      # Max 5%

# Check linearity 
# data_chatphone$probabilities <- predict(Lmer_medonl2$full_model, type = "response")
# data_chatphone$Logit = log(data_chatphone$probabilities / (1 - data_chatphone$probabilities))
# ggplot(data_chatphone, aes(x = Logit, y = Extraversion)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   theme_minimal()

# DHARMA package
simulationOutput <- simulateResiduals(fittedModel = Lmer_med2$full_model, plot = F, n = 5000)
plot(simulationOutput)

# ROC Curve
performance_roc(Lmer_med2$full_model)
performance_roc(Lmer_medonl2$full_model)
plot(performance_roc(Lmer_med2$full_model))
plot(performance_roc(Lmer_medonl2$full_model))

# Check multicollinearity     
# (No glmers with multiple predictors in our case)
