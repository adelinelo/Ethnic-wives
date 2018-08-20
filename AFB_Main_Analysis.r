###############################################
#
#  The Spousal Bump: 
#  Do cross-ethnic marriages increase political support in multiethnic democracies?
#
#  Data Replication
#
#  Claire Adida
#  Nathan Combes
#  Adeline Lo
#  Alex Verink
#
###############################################

###############################################
# This code generates all of the main results reported in the paper.
###############################################

library(foreign)
library(Zelig)
library(ZeligChoice)
library(stargazer)
library(plm)
rm(list=ls())

###############################################
# Read in Data
###############################################
## read in file (subset of (data,(prescoethnic==0 & preswifesame==0)))
data <- read.csv(file = "DataforAnalysisSub.csv")
## read in file full data, not subsetted to (prescoethnic==0 & preswifesame==0)
data2<- read.csv(file = "DataforAnalysisFull.csv")

## Summary statistics for full data ##
# Footnote 29
# Avg support for leader coethnics
lc<-subset(data2,(prescoethnic==1))
mean(lc$vote,na.rm=TRUE) 
# Avg support for spouse coethnics
sc<-subset(data2,(wifecoethnic==1))
mean(sc$vote,na.rm=TRUE) 
# Average support for non coethnics
nc<-subset(data2,(wifecoethnic!=1 & prescoethnic!=1))
mean(nc$vote,na.rm=TRUE) 


# Average support for spouse coethnics
benin<-subset(data,(country=="benin"))
sc<-subset(benin,(wifecoethnic==1))
mean(sc$vote,na.rm=TRUE)
# Average support for non coethnics
nc<-subset(benin,(wifecoethnic!=1 & prescoethnic!=1))
mean(nc$vote,na.rm=TRUE) 
#Footnote 17:... "This is consistent with the AB data where only 29% of Fon express support for Yayi"
mean(benin$vote[benin$wifecoethnic==1],na.rm=TRUE) 

## "An afrobarometer respondent's coethnicity with the leader's spouse occurs about 22% of the time, a slightly higher rate than that of coethnicity with the leader"
table(data2$wifecoethnic, useNA="always")

##################### MAIN MODELS ######################

# Table SI-3
stargazer(data2) #full data (not subsetting)

#Table 4: Determinants of Support for Leader in AB Survey
### DV=vote for incumbent party ###
#fe basic
vote1 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ round + as.factor(country), model="ls", data=data, robust=FALSE)
summary(vote1) 
#fe controls
vote2 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + iage + ieducation + igender + as.factor(country), model="ls", data=data, robust=FALSE)
summary(vote2) 

## output to LaTeX ##
stargazer(vote1, vote2, title="Wife Coethnic effect on Vote", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))

### Robustness check ## 
# robustness
# probit  
vote3 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), model="probit", data=data)
summary(vote3) 
# logit
vote4 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), model="logit", data=data)
summary(vote4) 
## output to LaTeX ##
stargazer(vote3, vote4, title="Wife Coethnic effect on Vote (probit/logit)", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))

##############################
### DV=approval (job perf) ###
##############################
#fe basic
approval1 <- zelig(as.numeric(approval) ~ wifecoethnic + oppcoethnic+ethnicpercent + round +as.factor(country), model="ls", data=data, robust=FALSE)
summary(approval1) 
#fe controls
approval2 <- zelig(as.numeric(approval) ~ wifecoethnic+ oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage + ieducation + igender + as.factor(country), model="ls", data=data, robust=FALSE)
summary(approval2)
## output to LaTeX ##
stargazer(approval1, approval2, title="Wife Coethnic effect on Approval)", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))

# robustness
# oprobit 
approval3 <- zelig(approval ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + 
                     fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV 
                   + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), 
                   model="oprobit", data=data)
summary(approval3) 
# ologit 
approval4 <- zelig(approval ~ wifecoethnic + oppcoethnic + ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender + as.factor(country), model="ologit", data=data)
summary(approval4)
## output to LaTeX ##
stargazer(approval3, approval4, title="Wife Coethnic effect on Approval (oprobit/ologit)", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))



############################################################
### DV=believe ethnic group has pol representation #########
############################################################
#fe basic
ethnicPolitical1 <- zelig(as.numeric(ethnicPolitical) ~ wifecoethnic+ oppcoethnic + ethnicpercent + round + as.factor(country), model="ls", data=data,robust=FALSE)
summary(ethnicPolitical1)
#fe controls
ethnicPolitical2 <- zelig(as.numeric(ethnicPolitical) ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ  + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender+ as.factor(country), model="ls", data=data,robust=FALSE)
summary(ethnicPolitical2)

## output to LaTeX ##
stargazer(ethnicPolitical1, ethnicPolitical2, title="Wife Coethnic effect on Ethnic Political)", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))


# robustness
# oprobit 
ethnicPolitical3 <- zelig(ethnicPolitical ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ  + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender + as.factor(country), model="oprobit", data=data)
summary(ethnicPolitical3) 
# ologit 
ethnicPolitical4 <- zelig(ethnicPolitical ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ  + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender + as.factor(country), model="ologit", data=data)
summary(ethnicPolitical4)
## output to LaTeX ##
stargazer(ethnicPolitical3, ethnicPolitical4, title="Wife Coethnic effect on Ethnic Political (oprobit/ologit)", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))



##################################################################
### DV=believe ethnic group is treated unfairly (ethnicUnfair) ###
##################################################################
#fe basic
ethnicUnfair1 <- zelig(as.numeric(ethnicUnfair) ~ wifecoethnic + oppcoethnic + ethnicpercent + round +as.factor(country), model="ls", data=data, robust=FALSE)
summary(ethnicUnfair1)
#fe controls
ethnicUnfair2 <- zelig(as.numeric(ethnicUnfair) ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender +as.factor(country), model="ls", data=data,robust=FALSE)
summary(ethnicUnfair2) 

## output to LaTeX ##
stargazer(ethnicUnfair1, ethnicUnfair2, title="Wife Coethnic effect on Ethnic Unfair", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))


# robustness
# oprobit 
ethnicUnfair3 <- zelig(ethnicUnfair ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender+ as.factor(country), model="oprobit", data=data)
summary(ethnicUnfair3) 
# ologit 
ethnicUnfair4 <- zelig(ethnicUnfair ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender +as.factor(country), model="ologit", data=data)
summary(ethnicUnfair4)
## output to LaTeX ##
stargazer(ethnicUnfair3, ethnicUnfair4, title="Wife Coethnic effect on Ethnic Unfair (oprobit/ologit)", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))

#output all 8 main models into LaTex
stargazer(vote1, vote2,approval1, approval2,ethnicPolitical1, ethnicPolitical2, ethnicUnfair1, ethnicUnfair2, 
          title="Determinants of Support for Leader in AB Survey", align=TRUE, ci=FALSE, keep=c("wifecoethnic"))

