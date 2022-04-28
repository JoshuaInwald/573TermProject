#Packages and working directory
library(psych)
library(tidyverse)
library(lme4)
library(lmerTest)
library(dplyr)
library(MASS)
library(foreign)
library(randtests)
library(pscl)
library(stats)
library(modelsummary)
library(ggplot2)
library(brms)
library(GPArotation)
# setwd("~/Documents/Social Identities X Climate Change")
setwd("/media/sabdurah/51fd1956-269d-4031-b807-9e08cd7e39ed/suhaib/Classes/PSYC573/Homeworks/HW9")

data <- read.csv("clean EnvironmentIdentities March9.csv")
#View(data)
data <- data %>%
  filter(WritTask1 != "" ) #Remove participants who didn't answer the prompts
#Only at 239 responses when filtering on Qualtrics' "finished" variable

#Coerce categorical variables into factors
data$Generation <- as.factor(data$Generation)
data$Politics <- as.factor(data$Politics)
data$Gender <- as.factor(data$Gender)
data$Race <- as.factor(data$Race)
data$Education <- as.factor(data$Education)
data$SexualOrientation <- as.factor(data$SexualOrientation)
data$Serious <- as.factor(data$Serious)
data$LibCon <- data$Politics.1 #just rename for comprehension

VarsToFix <- colnames(data)[9:40] #Get names of all 7-scale Likert variables
for (i in 1:length(VarsToFix)){
  data[,i+8] <- recode(data[,VarsToFix[i]], #Note we have to offset the index by 8
                       "Strongly disagree" = 1, "Disagree" = 2,
                       "Somewhat disagree" = 3, "Neither agree nor disagree" = 4,
                       "Somewhat agree" = 5, "Agree" = 6,
                       "Strongly agree" = 7)  #use recode from dplyr to change Likert strings to numbers
  #Note: syntax here was a bit tricky, as recode takes a string, not a data frame
}

OtherVarsToFix <- colnames(data)[47:58] #Get names of all 5-scale Likert variables
for (i in 1:length(OtherVarsToFix)){
  if(recode(data[,OtherVarsToFix[i]],
            "Never" = 0, "Occassionally" = 1,
            "Most of the time" = 2, "Always" = 3,
            "No answer" = 98) == 98) {
    data[,i+46] <- NA #Note we have to offset the index by 46
  } else {
    data[,i+46] <- recode(data[,OtherVarsToFix[i]], #Note we have to offset the index by 46
                          "Never" = 0, "Occassionally" = 1,
                          "Most of the time" = 2, "Always" = 3)  
  }
}

#Reverse-code necessary variables
recodeVars <- c("CSES1", "CSES5", "CSES7", "NEP2", "NEP4", 
                "NEP8", "NEP10", "NEP12","MWB2","MWB4")
for (i in 1:length(recodeVars)){
  data[,recodeVars[i]] <- 8 - data[,recodeVars[i]] #recode from  1-to-7 to 7-to-1
}

#Create scales from items
data$CSEStotal <- rowMeans(data[, c("CSES1", "CSES2", "CSES3", "CSES4", "CSES5", "CSES6", "CSES7", "CSES8")], na.rm = T)
data$EnvConcern <- rowMeans(data[, c("SC1", "SC2", "SC4", "SC3", "SC5", "SC7", "SC8", "SC10", "SC11")], na.rm = T)
data$NEP <- rowMeans(data[, c("NEP1", "NEP11", "NEP2", "NEP4", "NEP8", "NEP10", "NEP12", "NEP3", "NEP5", "NEP9", "NEP15")], na.rm = T)
data$VerbCommit <- rowMeans(data[, c("MWB1", "MWB2", "MWB4", "MWB6")], na.rm = T)
data$RecallTotal <- rowMeans(data[, c("recall1", "recall2", "recall3", "recall4", "recall5", "recall6")], na.rm = T)
data$IntentTotal <- rowMeans(data[, c("intent1", "intent2", "intent3", "intent4", "intent5", "intent6")], na.rm = T)

# data <- data %>% rowwise() %>%
#   mutate(CSEStotal = mean(c(CSES1, CSES2, CSES3, CSES4, CSES5, CSES6, CSES7, CSES8), na.rm = T),
#          EnvConcern = mean(c(SC1, SC2, SC4, SC3, SC5, SC7, SC8, SC10, SC11), na.rm = T),
#          NEP = mean(c(NEP1, NEP11, NEP2, NEP4, NEP8, NEP10, NEP12, NEP3, NEP5, NEP9, NEP15), na.rm = T),
#          VerbCommit = mean(c(MWB1, MWB2, MWB4, MWB6), na.rm = T),
#          RecallTotal = mean(c(recall1, recall2, recall3, recall4, recall5, recall6), na.rm = T),
#          IntentTotal = mean(c(intent1, intent2, intent3, intent4, intent5, intent6), na.rm = T)
#          )

#Do our scales check out?  Yes, all look great except Verbal Commitment, which is still fine
omega(data[,c("CSES1", "CSES2", "CSES3" , "CSES4", 
              "CSES5", "CSES6", "CSES7", "CSES8")]) #Total omega = 0.96
omega(data[,c("SC1", "SC2", "SC4", "SC3", "SC5", "SC7",
              "SC8", "SC10", "SC11")]) #Total omega = 0.98
omega(data[,c("NEP1", "NEP11", "NEP2", "NEP4" ,"NEP8", "NEP10",
               "NEP12" ,"NEP3", "NEP5", "NEP9", "NEP15")]) #Total omega = 0.89
omega(data[,c("MWB1" ,"MWB2" ,"MWB4", "MWB6")]) #Total omega = 0.77
omega(data[,c("recall1" ,"recall2" ,"recall3" ,
              "recall4", "recall5","recall6")]) #Total omega = 0.91
omega(data[,c("intent1" ,"intent2" ,"intent3" ,
              "intent4", "intent5","intent6")]) #Total omega = 0.91

data <- data %>%
  filter(Serious == "Yes",#Remove participants who didn't answer seriously
         Identity %in% c("a conservative", "a Gen-Zer" , "a Millennial")) #Remove participants who didn't self-identify into young/conservative
  
x <- data[,"Age"]
y <- data[,"EnvConcern"]


cor(data[,"Age"],data[,"charity_WWF"], use = "complete.obs")


#Descriptive statistics -- DV by Identity prime condition
data %>%
  group_by(Identity) %>%
  summarise(n = n(), CSEStot = mean(CSEStotal, na.rm = TRUE), EnvConcern = mean(EnvConcern, na.rm = TRUE), NEP = mean(NEP, na.rm = TRUE), VerbCommit = mean(VerbCommit, na.rm = TRUE), 
            RecallTotal = mean(RecallTotal, na.rm = TRUE), IntentTotal = mean(IntentTotal, na.rm = TRUE), charity_WWF = mean(charity_WWF, na.rm = TRUE))

#DVs to regress on: EnvConcern, NEP, VerbCommit, RecallTotal, IntentTotal, charity_WWF
hist(data$EnvConcern)
#All skewed distributions. NEP is close to normal, but everything else is skewed (right/left)
#perhaps OLS isn't the appropriate tool here?

#Regress various DV scores on manipulation category
m0 <- lm(NEP ~ as.factor(Identity), data = data)
summary(m0) #no significant raw main effect

#Control for strength of manipulation (CSES total score)
m1 <- lm(NEP ~ as.factor(Identity) + CSEStotal, data = data)
summary(m1)
#Nothing is reaching significance with CSES as a control
#try including number of words written into prompt?

#more controls
m2 <- lm(NEP ~ as.factor(Identity) + CSEStotal + LibCon + Age + CCpercent, data = data)
summary(m2)
#still no dice -- though political orientation (LibCon) works in the way we'd expect

# #let's try lumping Zoomers + Millennials together?
# data$Identity[data$Identity == "a Gen-Zer"] <- "a Millennial"
# m3 <- lm(RecallTotal ~ as.factor(Identity) + CSEStotal, data = data)
# summary(m3)
# # Still nothing


### Bayesian

#weak priors, only specify intercept because it is not grounded at 0 but instead at 5
m0_b <- brm(NEP ~ as.factor(Identity) + CSEStotal + LibCon + Age + CCpercent, data = data,
          prior = prior(normal(0, 5), class = "b") +
            prior(normal(4, 5), class = "Intercept") + # scale is from 1-7, therefore centered at 4
            prior(student_t(4, 0, 5), class = "sigma"))

# informed priors from pre-test (use SE for coefficient priors)
m1_b <- brm(NEP ~ as.factor(Identity) + CSEStotal + LibCon + Age + CCpercent, data = data,
          prior = prior(normal(0, 5), class = "b") +
            prior(normal(-0.421219, 0.045845), class = "b", coef = "LibCon") +
            prior(normal(0.009764, 0.012115), class = "b", coef = "Age") +
            prior(normal(5, 3), class = "Intercept") +
            prior(student_t(4, 2.057011, 2.5), class = "sigma"))

# very strong priors from pre-test (add SE for intercept prior)
m2_b <- brm(NEP ~ as.factor(Identity) + CSEStotal + LibCon + Age + CCpercent, data = data,
            prior = prior(normal(0, 5), class = "b") +
              prior(normal(-0.421219, 0.045845), class = "b", coef = "LibCon") +
              prior(normal(0.009764, 0.012115), class = "b", coef = "Age") +
              prior(normal(5, 0.1673972), class = "Intercept") +
              prior(student_t(4, 2.057011, 0.5), class = "sigma"))

# very strong priors from pre-test (remove CCpercent variable -> check contribution to R2)
m3_b <- brm(NEP ~ as.factor(Identity) + CSEStotal + LibCon + Age, data = data,
            prior = prior(normal(0, 5), class = "b") +
              prior(normal(-0.421219, 0.045845), class = "b", coef = "LibCon") +
              prior(normal(0.009764, 0.012115), class = "b", coef = "Age") +
              prior(normal(5, 0.1673972), class = "Intercept") +
              prior(student_t(4, 2.057011, 0.5), class = "sigma"))


print(m0_b)
bayes_R2(m0_b)

print(m1_b)
bayes_R2(m1_b)

print(m2_b)
bayes_R2(m2_b)

print(m3_b)
bayes_R2(m3_b) #Without CCpercent still ~18% variance




