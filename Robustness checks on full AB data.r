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
# This code generates some robustness checks and 
# additional results shown in the paper.
###############################################

library(foreign)
library(Zelig)
library(ZeligChoice)
library(stargazer)
library(plm)
library(lmtest)
rm(list=ls())

###############################################
# Read in Data
###############################################

data <- read.csv(file = "DataforAnalysisSub.csv")
data2<-read.csv(file="DataforAnalysisFull.csv")

############## R&R comments #############
# Footnote 17
# average support for the leader (would you vote for) among spousal coethnics? 

  # Among spouse coethnics (prescoethnic==0 and preswifesame==0)
table(data$wifecoethnic, data$vote,useNA="always") 

# when opponent is coethnic with wife
data.oppwifecoethnic1<-subset(data,(oppwifecoethnic==1))
table(data.oppwifecoethnic1$wifecoethnic, data.oppwifecoethnic1$vote,useNA="always") 
# when opponent is NOT coethnic with wife
data.oppwifecoethnic0<-subset(data,(oppwifecoethnic==0))
table(data.oppwifecoethnic0$wifecoethnic, data.oppwifecoethnic0$vote,useNA="always")



# DIFF-MEANS #
t.test(data$vote~data$wifecoethnic) 
t.test(data$approval~data$wifecoethnic) 
t.test(data$ethnicPolitical~data$wifecoethnic) 
t.test(data$ethnicUnfair~data$wifecoethnic) 

#just on prescoethnic==0 data
data.1<-subset(data2,(prescoethnic==0))
t.test(data.1$vote~data.1$wifecoethnic) 
t.test(data.1$approval~data.1$wifecoethnic) 
t.test(data.1$ethnicPolitical~data.1$wifecoethnic) 
t.test(data.1$ethnicUnfair~data.1$wifecoethnic) 

#just on prescoethnic==1 data
data.2<-subset(data2,(prescoethnic==1))
t.test(data.2$vote~data.2$wifecoethnic) 
t.test(data.2$approval~data.2$wifecoethnic) 
t.test(data.2$ethnicPolitical~data.2$wifecoethnic)
t.test(data.2$ethnicUnfair~data.2$wifecoethnic) 


#############################################
######## MLE models for all DVs #############
#############################################

### DV=vote for incumbent party ###
# probit  vote wifecoethnic round4 age female rural race head cecon recon fcecon frecon interest favorowngroup nofood nowater hasradio hastv hasvehicle voted educ iage ieducation igender CountryFE1-CountryFE19 if prescoethnic==0 & preswifesame==0, robust
vote7 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), model="probit", data=data)
summary(vote7) 
# logit
vote8 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), model="logit", data=data)
summary(vote8) 
## output to LaTeX ##
stargazer(vote4, vote5, title="Wife Coethnic effect on Vote (probit/logit)", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))
##############################
### DV=approval (job perf) ###
##############################
# oprobit approval wifecoethnic round4 age female rural race head cecon recon fcecon frecon interest favorowngroup nofood nowater hasradio hastv hasvehicle voted educ iage ieducation igender CountryFE1-CountryFE19 if prescoethnic==0 & preswifesame==0, robust
approval7 <- zelig(approval ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + 
                     fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV 
                   + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), 
                   model="oprobit", data=data)
summary(approval7) 
# ologit approval wifecoethnic round4 age female rural race head cecon recon fcecon frecon interest favorowngroup nofood nowater hasradio hastv hasvehicle voted educ iage ieducation igender CountryFE1-CountryFE19 if prescoethnic==0 & preswifesame==0, robust
approval8 <- zelig(approval ~ wifecoethnic + oppcoethnic + ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender + as.factor(country), model="ologit", data=data)
summary(approval8)
stargazer(approval7, approval8, title="Wife Coethnic effect on approval", align=TRUE, ci=FALSE, keep=c("wifecoethnic"))


######################################################
### DV=believe ethnic group has pol representation ###
######################################################
# oprobit ethnicPolitical wifecoethnic round4 age female rural race head cecon recon fcecon frecon interest favorowngroup nofood nowater hasradio hastv hasvehicle voted educ iage ieducation igender CountryFE1-CountryFE19 if prescoethnic==0 & preswifesame==0, robust
ethnicPolitical7 <- zelig(ethnicPolitical ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ  + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender + as.factor(country), model="oprobit", data=data)
summary(ethnicPolitical7) 
# ologit ethnicPolitical wifecoethnic round4 age female rural race head cecon recon fcecon frecon interest favorowngroup nofood nowater hasradio hastv hasvehicle voted educ iage ieducation igender CountryFE1-CountryFE19 if prescoethnic==0 & preswifesame==0, robust
ethnicPolitical8 <- zelig(ethnicPolitical ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ  + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender + as.factor(country), model="ologit", data=data)
summary(ethnicPolitical8)
stargazer(ethnicPolitical7,ethnicPolitical8, title="Wife Coethnic effect on ethnicPolitical", align=TRUE, ci=FALSE, keep=c("wifecoethnic"))
##################################################################
### DV=believe ethnic group is treated unfairly (ethnicUnfair) ###
##################################################################
# oprobit ethnicUnfair wifecoethnic round4 age female rural race head cecon recon fcecon frecon interest favorowngroup nofood nowater hasradio hastv hasvehicle voted educ iage ieducation igender CountryFE1-CountryFE19 if prescoethnic==0 & preswifesame==0, robust
ethnicUnfair7 <- zelig(ethnicUnfair ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender+ as.factor(country), model="oprobit", data=data)
summary(ethnicUnfair7) 
# ologit ethnicUnfair wifecoethnic round4 age female rural race head cecon recon fcecon frecon interest favorowngroup nofood nowater hasradio hastv hasvehicle voted educ iage ieducation igender CountryFE1-CountryFE19 if prescoethnic==0 & preswifesame==0, robust
ethnicUnfair8 <- zelig(ethnicUnfair ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender +as.factor(country), model="ologit", data=data)
summary(ethnicUnfair8)
stargazer(ethnicUnfair7,ethnicUnfair8, title="Wife Coethnic effect on ethnicUnfair", align=TRUE, ci=FALSE, keep=c("wifecoethnic"))

###################################################
## REG ANALYSIS MINUS Tanzania (- Polity Score)####
###################################################

# for prescoethnic==0 and preswifesame==0
data<-subset(data,(country!="tanzania"))
data<-droplevels(data)

### DV=vote for incumbent party ###
# fe controls, no robust
vote3 <- zelig(as.numeric(vote) ~ wifecoethnic + oppcoethnic + ethnicpercent + prescoethnic + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), model="ls", data=data, robust=TRUE)
summary(vote3) 

### DV=approval  ###
# fe controls, no robust
approval3 <- zelig(as.numeric(approval) ~ wifecoethnic + oppcoethnic + ethnicpercent + prescoethnic + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), model="ls", data=data, robust=TRUE)
summary(approval3) 

### DV=ethnicUnfair  ###
# fe controls, no robust
ethnicUnfair3 <- zelig(as.numeric(ethnicUnfair) ~ wifecoethnic + oppcoethnic + ethnicpercent + prescoethnic + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), model="ls", data=data, robust=TRUE)
summary(ethnicUnfair3) 

### DV=ethnicPolitical ###
# fe controls, no robust
ethnicPolitical3 <- zelig(as.numeric(ethnicPolitical) ~ wifecoethnic + oppcoethnic + ethnicpercent + prescoethnic + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + educ + iage + ieducation + igender + as.factor(country), model="ls", data=data, robust=TRUE)
summary(ethnicPolitical3) 

stargazer(vote3, approval3, ethnicUnfair3, ethnicPolitical3, title="Wife Coethnic effect on Vote/Approval/Ethnic Unfair/Ethnic Power (Exclude Tanzania)", align=TRUE, ci=FALSE, keep=c("wifecoethnic", "ethnicpercent", "oppcoethnic","Constant"))


##########################################################################
######################### Further robustness checks ######################

##### Excluding Kenya (more than 1 wife) ######
## Kenya already excluded when subset down to preswifesame==0 because Kenya wife-leader in AB3 and AB4 are always kikuyu-kikuyu


#### Robustness check where code president of Mozambique R3 as Makua (instead of Ronga). Use variable prescoethnic2 ###
## This requires using the dataset "DataforAnalysisSubMAKUA.csv" ##
dataMAKUA <- read.csv(file = "DataforAnalysisSubMAKUA.csv")
### DV=vote for incumbent party ###
#fe basic
vote1 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ round + as.factor(country), model="ls", data=dataMAKUA, robust=FALSE)
summary(vote1) 
#fe controls
vote2 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + iage + ieducation + igender + as.factor(country), model="ls", data=dataMAKUA, robust=FALSE)
summary(vote2) 
### DV=approval (job perf) ###
#fe basic
approval1 <- zelig(as.numeric(approval) ~ wifecoethnic + oppcoethnic+ethnicpercent + round +as.factor(country), model="ls", data=dataMAKUA, robust=FALSE)
summary(approval1) 
#fe controls
approval2 <- zelig(as.numeric(approval) ~ wifecoethnic+ oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage + ieducation + igender + as.factor(country), model="ls", data=dataMAKUA, robust=FALSE)
summary(approval2)
### DV=believe ethnic group has pol representation #####
#fe basic
ethnicPolitical1 <- zelig(as.numeric(ethnicPolitical) ~ wifecoethnic+ oppcoethnic + ethnicpercent + round + as.factor(country), model="ls", data=dataMAKUA,robust=FALSE)
summary(ethnicPolitical1)
#fe controls
ethnicPolitical2 <- zelig(as.numeric(ethnicPolitical) ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ  + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender+ as.factor(country), model="ls", data=dataMAKUA,robust=FALSE)
summary(ethnicPolitical2)
### DV=believe ethnic group is treated unfairly (ethnicUnfair) ###
#fe basic
ethnicUnfair1 <- zelig(as.numeric(ethnicUnfair) ~ wifecoethnic + oppcoethnic + ethnicpercent + round +as.factor(country), model="ls", data=dataMAKUA, robust=FALSE)
summary(ethnicUnfair1)
#fe controls
ethnicUnfair2 <- zelig(as.numeric(ethnicUnfair) ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender +as.factor(country), model="ls", data=dataMAKUA,robust=FALSE)
summary(ethnicUnfair2) 


#### Robustness check where code spouse of president in Mali is Bambara AND Peulh. Use variable wifecoethnic2 ###
### DV=vote for incumbent party ###
#fe basic
vote1 <- zelig(as.numeric(vote) ~ wifecoethnic2 +oppcoethnic+ethnicpercent+ round + as.factor(country), model="ls", data=data, robust=FALSE)
summary(vote1) 
#fe controls
vote2 <- zelig(as.numeric(vote) ~ wifecoethnic2 +oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + iage + ieducation + igender + as.factor(country), model="ls", data=data, robust=FALSE)
summary(vote2) 
### DV=approval (job perf) ###
#fe basic
approval1 <- zelig(as.numeric(approval) ~ wifecoethnic2 + oppcoethnic+ethnicpercent + round +as.factor(country), model="ls", data=data, robust=FALSE)
summary(approval1) 
#fe controls
approval2 <- zelig(as.numeric(approval) ~ wifecoethnic2+ oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage + ieducation + igender + as.factor(country), model="ls", data=data, robust=FALSE)
summary(approval2)
### DV=believe ethnic group has pol representation #####
#fe basic
ethnicPolitical1 <- zelig(as.numeric(ethnicPolitical) ~ wifecoethnic2+ oppcoethnic + ethnicpercent + round + as.factor(country), model="ls", data=data,robust=FALSE)
summary(ethnicPolitical1)
#fe controls
ethnicPolitical2 <- zelig(as.numeric(ethnicPolitical) ~ wifecoethnic2 + oppcoethnic + ethnicpercent + age + female + educ  + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender+ as.factor(country), model="ls", data=data,robust=FALSE)
summary(ethnicPolitical2)
### DV=believe ethnic group is treated unfairly (ethnicUnfair) ###
#fe basic
ethnicUnfair1 <- zelig(as.numeric(ethnicUnfair) ~ wifecoethnic2 + oppcoethnic + ethnicpercent + round +as.factor(country), model="ls", data=data, robust=FALSE)
summary(ethnicUnfair1)
#fe controls
ethnicUnfair2 <- zelig(as.numeric(ethnicUnfair) ~ wifecoethnic2 + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender +as.factor(country), model="ls", data=data,robust=FALSE)
summary(ethnicUnfair2) 

### Robustness check: exclude Nigeria Round 3 (changed Obasanjo's wife from Yoruba to Edo) ###
excl<-data[!(data$country=="nigeria" & data$round==3),]
table(excl$country,excl$round)
### DV=vote for incumbent party ###
#fe basic
vote1 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ round + as.factor(country), model="ls", data=excl, robust=FALSE)
summary(vote1) 
#fe controls
vote2 <- zelig(as.numeric(vote) ~ wifecoethnic +oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + interest + favorOwnGroup + noFood + noWater + hasRadio + hasTV + hasVehicle + voted + iage + ieducation + igender + as.factor(country), model="ls", data=excl, robust=FALSE)
summary(vote2) 
### DV=approval (job perf) ###
#fe basic
approval1 <- zelig(as.numeric(approval) ~ wifecoethnic + oppcoethnic+ethnicpercent + round +as.factor(country), model="ls", data=excl, robust=FALSE)
summary(approval1)  #not significant
#fe controls
approval2 <- zelig(as.numeric(approval) ~ wifecoethnic+ oppcoethnic+ethnicpercent+ age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage + ieducation + igender + as.factor(country), model="ls", data=excl, robust=FALSE)
summary(approval2)
### DV=believe ethnic group has pol representation #####
#fe basic
ethnicPolitical1 <- zelig(as.numeric(ethnicPolitical) ~ wifecoethnic+ oppcoethnic + ethnicpercent + round + as.factor(country), model="ls", data=excl,robust=FALSE)
summary(ethnicPolitical1)
#fe controls
ethnicPolitical2 <- zelig(as.numeric(ethnicPolitical) ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ  + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender+ as.factor(country), model="ls", data=excl,robust=FALSE)
summary(ethnicPolitical2)
### DV=believe ethnic group is treated unfairly (ethnicUnfair) ###
#fe basic
ethnicUnfair1 <- zelig(as.numeric(ethnicUnfair) ~ wifecoethnic + oppcoethnic + ethnicpercent + round +as.factor(country), model="ls", data=excl, robust=FALSE)
summary(ethnicUnfair1)
#fe controls
ethnicUnfair2 <- zelig(as.numeric(ethnicUnfair) ~ wifecoethnic + oppcoethnic + ethnicpercent + age + female + educ + round + rural + race + head + cecon + recon + fcecon + interest + favorOwnGroup + noFood + noWater + hasRadio +hasTV+hasVehicle+voted+educ+iage+ieducation+igender +as.factor(country), model="ls", data=excl,robust=FALSE)
summary(ethnicUnfair2) 
##########################################################################

test<-subset(data2,(preswifesame==0))
test<-subset(data2,(preswifesame==1))
round3<-subset(test,(round==3))
round4<-subset(test,(round==4))
# benin 
#r3/r4
benin<-subset(round4,(country=="benin"))
benin<-droplevels(benin)
table(benin$prescoethnic)
table(benin$wifecoethnic)
table(benin$oppcoethnic)
t.test(benin$vote~benin$prescoethnic) # where y is numeric and x is a binary factor
t.test(benin$vote~benin$wifecoethnic)
t.test(benin$vote~benin$oppcoethnic)

#botswana
#r3
botswana<-subset(round3,(country=="botswana"))
botswana<-droplevels(botswana)
table(botswana$prescoethnic)
table(botswana$wifecoethnic)
table(botswana$oppcoethnic)
t.test(botswana$vote~botswana$prescoethnic) # where y is numeric and x is a binary factor
t.test(botswana$vote~botswana$wifecoethnic)
t.test(botswana$vote~botswana$oppcoethnic)

#mali
#r3/r4
mali<-subset(round4,(country=="mali"))
mali<-droplevels(mali)
table(mali$prescoethnic)
table(mali$wifecoethnic)
table(mali$oppcoethnic)
t.test(mali$vote~mali$prescoethnic) # where y is numeric and x is a binary factor
t.test(mali$vote~mali$wifecoethnic)
t.test(mali$vote~mali$oppcoethnic)

#mozambique
#r3/r4
mozambique<-subset(round4,(country=="mozambique"))
mozambique<-droplevels(mozambique)
table(mozambique$prescoethnic)
table(mozambique$wifecoethnic)
table(mozambique$oppcoethnic)
t.test(mozambique$vote~mozambique$prescoethnic) # where y is numeric and x is a binary factor
t.test(mozambique$vote~mozambique$wifecoethnic)
t.test(mozambique$vote~mozambique$oppcoethnic)

#south africa
#r3/r4
southafrica<-subset(round4,(country=="south africa"))
southafrica<-droplevels(southafrica)
table(southafrica$prescoethnic)
table(southafrica$wifecoethnic)
table(southafrica$oppcoethnic)
t.test(southafrica$vote~southafrica$prescoethnic) # where y is numeric and x is a binary factor
t.test(southafrica$vote~southafrica$wifecoethnic)
t.test(southafrica$vote~southafrica$oppcoethnic)

#tanzania
#r3/r4
tanzania<-subset(round4,(country=="tanzania"))
tanzania<-droplevels(tanzania)
table(tanzania$prescoethnic)
table(tanzania$wifecoethnic)
table(tanzania$oppcoethnic)
t.test(tanzania$vote~tanzania$prescoethnic) # where y is numeric and x is a binary factor
t.test(tanzania$vote~tanzania$wifecoethnic)
t.test(tanzania$vote~tanzania$oppcoethnic)

#zambia
#r3/r4
zambia<-subset(round4,(country=="zambia"))
zambia<-droplevels(zambia)
table(zambia$prescoethnic)
table(zambia$wifecoethnic)
table(zambia$oppcoethnic)
t.test(zambia$vote~zambia$prescoethnic) # where y is numeric and x is a binary factor
t.test(zambia$vote~zambia$wifecoethnic)
t.test(zambia$vote~zambia$oppcoethnic)


## for when preswifesame==1
#ghana
#r3/r4
ghana<-subset(round4,(country=="ghana"))
ghana<-droplevels(ghana)
table(ghana$prescoethnic)
table(ghana$wifecoethnic)
table(ghana$oppcoethnic)
t.test(ghana$vote~ghana$prescoethnic) # where y is numeric and x is a binary factor
t.test(ghana$vote~ghana$wifecoethnic)
t.test(ghana$vote~ghana$oppcoethnic)

#kenya
#r3/r4
kenya<-subset(round4,(country=="kenya"))
kenya<-droplevels(kenya)
table(kenya$prescoethnic)
table(kenya$wifecoethnic)
table(kenya$oppcoethnic)
t.test(kenya$vote~kenya$prescoethnic) # where y is numeric and x is a binary factor
t.test(kenya$vote~kenya$wifecoethnic)
t.test(kenya$vote~kenya$oppcoethnic)

#madagascar
#r3/r4
madagascar<-subset(round4,(country=="madagascar"))
madagascar<-droplevels(madagascar)
table(madagascar$prescoethnic)
table(madagascar$wifecoethnic)
table(madagascar$oppcoethnic)
t.test(madagascar$vote~madagascar$prescoethnic) # where y is numeric and x is a binary factor
t.test(madagascar$vote~madagascar$wifecoethnic)
t.test(madagascar$vote~madagascar$oppcoethnic)

#namibia
#r3/r4
namibia<-subset(round4,(country=="namibia"))
namibia<-droplevels(namibia)
table(namibia$prescoethnic)
table(namibia$wifecoethnic)
table(namibia$oppcoethnic)
t.test(namibia$vote~namibia$prescoethnic) # where y is numeric and x is a binary factor
t.test(namibia$vote~namibia$wifecoethnic)
t.test(namibia$vote~namibia$oppcoethnic)

#nigeria
#r3/r4
nigeria<-subset(round4,(country=="nigeria"))
nigeria<-droplevels(nigeria)
table(nigeria$prescoethnic)
table(nigeria$wifecoethnic)
table(nigeria$oppcoethnic)
t.test(nigeria$vote~nigeria$prescoethnic) # where y is numeric and x is a binary factor
t.test(nigeria$vote~nigeria$wifecoethnic)
t.test(nigeria$vote~nigeria$oppcoethnic)

#south africa
#r4
southafrica<-subset(round4,(country=="south africa"))
southafrica<-droplevels(southafrica)
table(southafrica$prescoethnic)
table(southafrica$wifecoethnic)
table(southafrica$oppcoethnic)
t.test(southafrica$vote~southafrica$prescoethnic) # where y is numeric and x is a binary factor
t.test(southafrica$vote~southafrica$wifecoethnic)
t.test(southafrica$vote~southafrica$oppcoethnic)

#uganda
#r3/r4
uganda<-subset(round4,(country=="uganda"))
uganda<-droplevels(uganda)
table(uganda$prescoethnic)
table(uganda$wifecoethnic)
table(uganda$oppcoethnic)
t.test(uganda$vote~uganda$prescoethnic) # where y is numeric and x is a binary factor
t.test(uganda$vote~uganda$wifecoethnic)
t.test(uganda$vote~uganda$oppcoethnic)

#zambia
#r3
zambia<-subset(round3,(country=="zambia"))
zambia<-droplevels(zambia)
table(zambia$prescoethnic)
table(zambia$wifecoethnic)
table(zambia$oppcoethnic)
t.test(zambia$vote~zambia$prescoethnic) # where y is numeric and x is a binary factor
t.test(zambia$vote~zambia$wifecoethnic)
t.test(zambia$vote~zambia$oppcoethnic)

#zimbabwe
#r3/r4
zimbabwe<-subset(round3,(country=="zimbabwe"))
zimbabwe<-droplevels(zimbabwe)
table(zimbabwe$prescoethnic)
table(zimbabwe$wifecoethnic)
table(zimbabwe$oppcoethnic)
t.test(zimbabwe$vote~zimbabwe$prescoethnic) # where y is numeric and x is a binary factor
t.test(zimbabwe$vote~zimbabwe$wifecoethnic)
t.test(zimbabwe$vote~zimbabwe$oppcoethnic)

#####################################
## Table 3: Comparing cross-ethnic and coethnic marriage cases
## Edo are ethnic=39
nigeria=subset(data2, (round==3&country=="nigeria"))
unique(nigeria$country)
nigeria=droplevels(nigeria)
table(nigeria$ethnic, useNA="always") 