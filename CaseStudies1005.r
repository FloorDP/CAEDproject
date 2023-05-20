## Project Case Studies in the Analysis of Experimental Data
## Behavioral data

## Authors: Stephanie Van De Walle, Britt Di Vita & Floor Depestele

#load packages
library(pacman)
library(ltm)
library(reshape2)
library(Rmisc)

p_load( semTools,haven,ggplot2,plotly,tidyr,dplyr,RPostgreSQL, RPostgres,magrittr,jsonlite,purrr, stringr,anytime,lubridate,
        psych,e1071,lmerTest, afex, effectsize,car,lmerTest,sjstats,mice, insight,readxl)
set.seed(123)
options(contrasts = c("contr.sum","contr.poly"))
options(scipen=999)

#import datasets, available at https://osf.io/6kzx3/

# DATASET 1: ESM data: social interactions (yes or no + mode of communication)

# mywd= "C:/Users/floor/OneDrive/Documenten/MASTER/CAED" 
# mywd = "C:/Users/Gebruiker/Documents/Experimentele Psychologie/JAAR 2/Case studies"
mywd = "/Users/britt/Desktop/Case studies"

#mydata <- read.csv(str_glue(mywd,"/Study2_Wave1_ESMdata_Socinteract.csv")) 
mydata <- read.csv(str_glue(mywd,"/Study2_Wave1_ESMdata_Socinteract.csv")) 
mydata <- subset(mydata, select = c(id, created_esm, communication, interaction))  
mydata$communication[is.na(mydata$communication)] <- 0

# communication: 1 = in person, 2 = phone/chat
# interaction: 1 = yes, 2 = no

# DATASET 2: TRAIT data: extraversion (BFI-2-S) and loneliness (ULS)
mydata_trait = read.csv(str_glue(mywd, "/Study2_Wave1_Traitdata_Extraversion-Loneliness.csv")
)
mydata_trait <- subset(mydata_trait, select = c(id, 
                                                bfi2s_1_t2, bfi2s_6_t2, bfi2s_11_t2, bfi2s_16_t2, bfi2s_21_t2, bfi2s_26_t2,
                                                uls_1_t2, uls_2_t2,  uls_3_t2,  uls_4_t2,  uls_5_t2,  uls_6_t2,  uls_7_t2,  uls_8_t2,  uls_9_t2))

# merge datasets of interaction/communication and BFI/ULS
data = merge(x = mydata, y = mydata_trait, by = "id") 

#factorize relevant variables
#data$id <- as.factor(data$id)
#data$communication <- as.factor(data$communication)
#data$interaction <- as.factor(data$interaction)

# remove missing data (NA) --> exlude participants who did not fill in the BFI and ULS
data <- na.omit(data)
summary(data)
#558 participants left

# Exclude participants who filled in the questions about communication and interaction less than half of the days (< 7 days)
extra <- colsplit(data$created_esm, pattern = " ", names = c("Date", "Time"))
data <- cbind(data, extra$Date)
names(data) <- c("id", "created_esm", "communication", "interaction", "bfi2s_1_t2", "bfi2s_6_t2", "bfi2s_11_t2", "bfi2s_16_t2", "bfi2s_21_t2", "bfi2s_26_t2",
                 "uls_1_t2", "uls_2_t2",  "uls_3_t2",  "uls_4_t2",  "uls_5_t2",  "uls_6_t2",  "uls_7_t2",  "uls_8_t2",  "uls_9_t2", "date")
number_of_dates_per_id <- aggregate(date ~ id, FUN = function(x) length(unique(x)), data = data)
ppts_less7days <- subset(number_of_dates_per_id, date < 7) 
data_excluded_pp <- data[!(data$id %in% ppts_less7days$id),]

# Numberofpp <- n_distinct(data_excluded_pp$id) # 323 participants left

data <- subset(data_excluded_pp, select = c("id", "date", "communication", "interaction", "bfi2s_1_t2", "bfi2s_6_t2", "bfi2s_11_t2", "bfi2s_16_t2", "bfi2s_21_t2", "bfi2s_26_t2",
               "uls_1_t2", "uls_2_t2",  "uls_3_t2",  "uls_4_t2",  "uls_5_t2",  "uls_6_t2",  "uls_7_t2",  "uls_8_t2",  "uls_9_t2", "date"))

#Reverse code extraversion scores on items 1, 21 and 26
#Loneliness items 2,3,5,6,7,9
#define columns to reverse code
reverse_cols = c("bfi2s_1_t2", "bfi2s_21_t2", "bfi2s_26_t2",
                "uls_2_t2",  "uls_3_t2",  "uls_5_t2",  "uls_6_t2",  "uls_7_t2",  "uls_9_t2")

#reverse code items
data[ , reverse_cols] = 6 - data[ , reverse_cols]


### Mss nieuwe kolom met "gemiddeld aantal interacties" per participant?
## Floor: communication en interaction zijn discrete variabelen met 2 categorieën
# gemiddelde lijkt me minder zinvol, misschien de modus (meest voorkomende categorie)

# 1) INTERNE CONSISTENCY + RELIABILITY: Crohnbachs alpha

#Extraversion 
Extraversion_alpha <- data.frame(data[5:10])
cronbach.alpha(Extraversion_alpha) 

#Loneliness 
Loneliness_alpha <- data.frame(data[11:19])
cronbach.alpha(Loneliness_alpha)

################################################################################
############# DESCRIPTIVE ANALYSIS #############################################

#Extraversion score per participant
data$Extraversion <- data$bfi2s_1_t2 + data$bfi2s_6_t2 + data$bfi2s_11_t2 + data$bfi2s_16_t2 + data$bfi2s_21_t2 + data$bfi2s_26_t2

#Loneliness score per participant 
data$Loneliness <- data$uls_1_t2 + data$uls_2_t2 + data$uls_3_t2 + data$uls_4_t2 + data$uls_5_t2 + data$uls_6_t2 + data$uls_7_t2 + data$uls_8_t2 + data$uls_9_t2 

#interaction: Calculate how often participants indicated to have interaction
data$interaction <- ifelse(data$interaction == "2", 0, 1)               # 1 = interaction / 0 = no interaction
interactions_pp = summarySE(data, measurevar="interaction", groupvars= "id")

#communication: Mean amount of interactions per mode? (chat/in person)
data_communication <- subset(data, communication %in% c(1, 2)) 
data_communication$communication <- ifelse(data_communication$communication == "2", 0, 1)               # 1 = in person / 0 = chat
type_com_pp = summarySE(data_communication, measurevar="communication", groupvars= "id")                # Percentage of interactions that were in person

# Create dataframe which contains Extraversion, Loneliness, variable which indicates how often participants indicated to have interaction, 
# and variable which indicates how often that interaction was in person (indireclty also indicates how much of the interactions were online)
data <- cbind(data$id, data$Extraversion, data$Loneliness, type_com_pp$communication, interactions_pp$interaction, by = "id")
data <- data.frame(data)
names(data) <- c("id", "Extraversion", "Loneliness", "communication", "interaction", "by")

data$Extraversion <- as.numeric(data$Extraversion)
data$Loneliness <- as.numeric(data$Loneliness)
data$communication <- as.numeric(data$communication)
data$interaction <- as.numeric(data$interaction)

################################################################################
mean(data$Extraversion)  
sd(data$Extraversion)    

mean(data$Loneliness)
sd(data$Loneliness)

mean(data$interaction)
sd(data$interaction)

mean(data$communication)
sd(data$communication)

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
## main analysis 


# MODEL 1: main effects 
Lm1 = lm(Loneliness ~ Extraversion + interaction + communication, data = data) 
summary(Lm1) 
confint(Lm1)

# MODEL 2: moderation 
Lm2 = lm(Loneliness ~ Extraversion + interaction + communication + Extraversion:interaction + Extraversion:communication, data = data) 
summary(Lm2)
confint(Lm2)

# MEDIATION
#Step 1: direct effect
Lm3 = lm(Loneliness ~ Extraversion, data = data)
summary(Lm3)
confint(Lm3)

#Step 2: effect of extraversion on communication/interaction (mediator?)
Lm4 = lm(interaction ~ Extraversion, data = data)     
summary(Lm4)
confint(Lm4)
Lm5 = lm(communication ~ Extraversion, data = data)  
summary(Lm5)
confint(Lm5)

#Step 3 + 4: Regress loneliness on mediator and extraversion
Lm6 = lm(Loneliness ~ interaction + Extraversion, data = data)       
summary(Lm6)
confint(Lm6)
Lm7 = lm(Loneliness ~ communication + Extraversion, data = data )  
summary(Lm7)
confint(Lm7)






