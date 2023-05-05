## Project Case Studies in the Analysis of Experimental Data
## Behavioral data

## Authors: Stephanie Van De Walle, Britt Di Vita & Floor Depestele

#load packages
library(pacman)
p_load( semTools,haven,ggplot2,plotly,tidyr,dplyr,RPostgreSQL, RPostgres,magrittr,jsonlite,purrr, stringr,anytime,lubridate,psych,e1071,lmerTest, afex, effectsize,car,lmerTest,sjstats,mice, insight,readxl)
set.seed(123)
options(contrasts = c("contr.sum","contr.poly"))
options(scipen=999)

#import datasets, available at https://osf.io/6kzx3/

# DATASET 1: ESM data: social interactions (yes or no + mode of communication)

#mywd= "C:/Users/floor/OneDrive/Documenten/MASTER/CAED" 
mywd = "C:/Users/Gebruiker/Documents/Experimentele Psychologie/JAAR 2/Case studies"

#mydata <- read.csv(str_glue(mywd,"/Study2_Wave1_ESMdata_Socinteract.csv")) 
mydata <- read.csv(str_glue(mywd,"/Study2_Wave1_ESMdata_Socinteract.csv")) 
mydata <- subset(mydata, select = c(id, communication, interaction))  

# DATASET 2: TRAIT data: extraversion (BFI-2-S) and loneliness (ULS)
mydata_trait = read.csv(str_glue(mywd, "/Study2_Wave1_Traitdata_Extraversion-Loneliness.csv")
)
mydata_trait <- subset(mydata_trait, select = c(id, 
                                                bfi2s_1_t2, bfi2s_6_t2, bfi2s_11_t2, bfi2s_16_t2, bfi2s_21_t2, bfi2s_26_t2,
                                                uls_1_t2, uls_2_t2,  uls_3_t2,  uls_4_t2,  uls_5_t2,  uls_6_t2,  uls_7_t2,  uls_8_t2,  uls_9_t2))

# put datasets together 
data = merge(x = mydata, y = mydata_trait, by = "id")

# remove missing data (NA)
data <- na.omit(data)
summary(data) #no missing data anymore :)

#Reverse code extraversion scores on items 1, 21 and 26
#Loneliness items 2,3,5,6,7,9
#define columns to reverse code
reverse_cols = c("bfi2s_1_t2", "bfi2s_21_t2", "bfi2s_26_t2",
                "uls_2_t2",  "uls_3_t2",  "uls_5_t2",  "uls_6_t2",  "uls_7_t2",  "uls_9_t2")

#reverse code items
data[ , reverse_cols] = 6 - data[ , reverse_cols]



### ESM: soc interacties: 14 dagen meerdere keren per dag, mr niet iedereen elke dag ingevuld ?? <-> extraversie en loneliness
### Hoe oplossen? Enkel participanten includeren die alles elke dag invulde of? 
### Mss nieuwe kolom met "gemiddeld aantal interacties" per participant?
## Floor: communication en interaction zijn discrete variabelen met 2 categorieën
# gemiddelde lijkt me minder zinvol, misschien de modus (meest voorkomende categorie)

# 1) INTERNE CONSISTENCTY + RELIABILITY: Crohnbachs alpha
library(ltm)

#Extraversion 
Extraversion_alpha <- data.frame(data[4:9])
cronbach.alpha(Extraversion_alpha) 

#Loneliness 
Loneliness_alpha <- data.frame(data[10:18])
cronbach.alpha(Loneliness_alpha)

################################################################################
############# DESCRIPTIVE ANALYSIS #############################################
## create variables (per participant, mean score for each construct)

#score vr extraversie per participant
data$Extraversion <- rowMeans(data[,c('bfi2s_1_t2', 'bfi2s_6_t2', 'bfi2s_11_t2', 'bfi2s_16_t2', 'bfi2s_21_t2', 'bfi2s_26_t2',)], na.rm=TRUE)

#score vr loneliness per participant 
data$Loneliness <- rowMeans (data[,c('uls_1_t2', 'uls_2_t2',  'uls_3_t2',  'uls_4_t2',  'uls_5_t2',  'uls_6_t2',  'uls_7_t2',  'uls_8_t2',  'uls_9_t2',)], na.rm=TRUE)

#interaction: eerst: enkel degene includeren die minstens 5 keer '1' (ja) antwoordden? 

#interaction: modus berekenen per participant 

#communication: modus berekenen per participant 

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

# MODEL 1: hoofd effecten   
Lm1 = lm(Loneliness ~ Extraversion + interaction + communication, data = data) 
summary(Lm1) 
confint(Lm1)

# MODEL 2: moderatie 
Lm2 = lm(Loneliness ~ Extraversion, Extraversion:Interaction, Extraversion:Communication, data = data) 
summary(Lm2)
confint(Lm2)

# MODEL 3: mediatie 
Lm3 = lm(Loneliness ~ Extraversion, data = data)
summary(Lm3)
confint(Lm3)

Lm4 = lm(Interaction ~ Extraversion, data = data)     #pijl van extraversie nr interactie
summary(Lm4)
confint(Lm4)
Lm5 = lm(communication ~ Extraversion, data = data)   #pijl van extraversie nr communicatie 
summary(Lm5)
confint(Lm5)

Lm6 = lm(Loneliness ~ Interaction, data = data)       #pijl van interactie nr loneliness 
summary(Lm6)
confint(Lm6)
Lm7 = lm(Loneliness ~ communication, data = data )    #pijl van communicatie nr loneliness 
summary(Lm7)
confint(Lm7)






