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
setwd("/media/sabdurah/51fd1956-269d-4031-b807-9e08cd7e39ed/suhaib/Classes/PSYC573/Homeworks/HW9")

preData <- read.csv("Identities Pretest.csv")
#View(preData)
preData <- preData %>%
  filter(Response.Type == "IP Address" ) #Remove preview responses

#Start with calculating scales from environmental concern subscales
#dataScales <- preData %>% dplyr::select(BSC1,BSC2,BSC3,BSC4,EcoDam1,EcoDam2,EcoDam3,EcoDam4,EnvDeg1,EnvDeg2,EnvDeg3,EnvDeg4)
#omega(dataScales) #All 12 items: alpha = 0.93, omega = 0.96
#omega(dataScales[1:4], nfactors = 1) #Biospheric Concerns items: alpha = 0.94, omega = 0.94
#omega(dataScales[5:8]) #Ecological Damage items: alpha = 0.83, omega = 0.87
#omega(dataScales[9:12]) #Environmental Degradation  items: alpha = 0.85, omega = 0.91

preData <- preData %>%
  rowwise() %>% 
  mutate(BiosphericConcern = sum(BSC1,BSC2,BSC3,BSC4, na.rm = TRUE))
preData <- preData %>%
  rowwise() %>% 
  mutate(EcologicalDamage = mean(EcoDam1,EcoDam2,EcoDam3,EcoDam4, na.rm = TRUE))
preData <- preData %>%  #Reverse score two items
  mutate(EnvDeg1 = -1 * EnvDeg1,
         EnvDeg4 = -1 * EnvDeg4) %>%
  rowwise() %>% 
  mutate(EnvironmentalDegrade = mean(EnvDeg1,EnvDeg2,EnvDeg3,EnvDeg4, na.rm = TRUE))

preData$EnvironmentalDegrade <- preData$EnvironmentalDegrade + 4 #rescale: -3, 3 to 1,7 

#Leaderboards for liberal-conservative, climate change concern, valence

LibConBoard <- round(sapply(preData[,c(3:43)], mean),2)
CCCBoard <- round(sapply(preData[,c(44:84)], mean),2)
ValenceBoard <- round(sapply(preData[,c(85:125)], mean),2)

m0_pre <- lm(EnvironmentalDegrade ~ LibCon + Age, data = preData)
summary(m0_pre)

# Priors 
c(mean(preData$CCCGenZ) - mean(preData$CCCConservatives), SD(preData$CCCGenZ)/sqrt(nrow(preData)))
c(mean(preData$CCCMillenials) - mean(preData$CCCConservatives), SD(preData$CCCMillenials)/sqrt(nrow(preData)))
c(mean(preData$CCCBoomers) - mean(preData$CCCConservatives), SD(preData$CCCBoomers)/sqrt(nrow(preData)))
c(mean(preData$CCCLiberals) - mean(preData$CCCConservatives), SD(preData$CCCLiberals)/sqrt(nrow(preData)))

SD_env = sd(preData$EnvironmentalDegrade)
M_env = mean(preData$EnvironmentalDegrade)
SE_env = sd(preData$EnvironmentalDegrade)/sqrt(nrow(preData))
N = nrow(preData)
df = N-1
alpha = 0.05

print(c(SD_env, M_env, SE_env))

CI_SD = sqrt(c((N-1)*SD_env^2/qchisq(alpha/2, df, lower.tail=TRUE), (N-1)*SD_env^2/qchisq(1-alpha/2, df, lower.tail=TRUE)))
SE_var = sqrt(2*SD_env^4/(N-1))

CI_SD
SE_var





