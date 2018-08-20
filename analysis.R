###############################################################################################################################
# Benin 2012 experiment analysis
# March 13, 2015, by Adeline Lo
# updated April 22, 2015 by AL
# updated May 14, 2015 AL
###############################################################################################################################
library(foreign)
library(Zelig)
library(ZeligChoice)
library(coefplot)
library(arm)
library(boot)
library(plyr)
library(stargazer)
rm(list=ls())
setwd("/Users/Adeline/Dropbox/Replication/Benin_experiment/")
#benin <- read.csv(file = "Benin2012survey.csv")#regular dataset Fon is Goun
benin <-read.dta(file="/Users/Adeline/Dropbox/Ethnic Wives/Writing/CPS Submission Spring 2015/Data for Survey Experiment Analysis/dataFonIsNotGoun.dta")#dataset where FonIsNotGoun
benin<-benin[which(benin$passage=="Control"|benin$passage=="Femme"|benin$passage=="FonFemme"),]
benin<-droplevels(benin)
benin$FonGroupNonCoethnics<-ifelse(benin$FonGroup==1,1,ifelse(benin$NonCoethnics==1,0,NA)) #variable 1 if Wifecoethnic, 0 if Noncoethnic
benin$pop=ifelse((benin$Control==1|benin$Femme==1|benin$FemmeFon==1),1,0)
#nordsud1 as numerical version of NordSud, North=0, South=1
benin$nordsud1<-as.numeric(ifelse(benin$nordsud=="Nord",0,ifelse(benin$nordsud=="Sud",1,NA)))
#code for numeric version of importance
benin$importance1<-benin$importance
benin$importance1[benin$importance=="1"]<-1
benin$importance1[benin$importance=="2"]<-2
benin$importance1[benin$importance=="3"]<-3
benin$importance1[benin$importance=="4"]<-4
benin$importance1[benin$importance=="NA"]<-NA
benin$importance1<-as.numeric(benin$importance1)
#code for numeric version of education
benin$education1<-benin$education
benin$education1[benin$education1=="1"]<-1
benin$education1[benin$education1=="2"]<-2
benin$education1[benin$education1=="3"]<-3
benin$education1[benin$education1=="4"]<-4
benin$education1[benin$education1=="5"]<-5
benin$education1[benin$education1=="6"]<-6
benin$education1[benin$education1=="7"]<-7
benin$education1[benin$education1=="NA"]<-NA
benin$education1<-as.numeric(benin$education1)
unique(benin$education1)
#code for numeric version of importance
benin$malnourriture1<-benin$malnourriture
benin$malnourriture1[benin$malnourriture1=="1"]<-1
benin$malnourriture1[benin$malnourriture1=="2"]<-2
benin$malnourriture1[benin$malnourriture1=="3"]<-3
benin$malnourriture1[benin$malnourriture1=="4"]<-4
benin$malnourriture1[benin$malnourriture1=="NA"]<-NA
benin$malnourriture1[benin$malnourriture1=="NR"]<-NA
benin$malnourriture1<-as.numeric(benin$malnourriture1)

unique(benin$malnourriture1)

control<-benin[which(benin$passage=="Control"),] #subset passage=Control
femme<-benin[which(benin$passage=="Femme"),] #subset passage=Femme
femmefon<-benin[which(benin$passage=="FonFemme"),] #subset passage=FemmeFon

#only look at fon
fonbenin<-benin[which(benin$FonGroup==1),]

#only look at noncoethnics
noncoethnics<-benin[which(benin$NonCoethnics==1),]

#only look at yayicoethnics
yayicoethnics<-benin[which(benin$YayiCoethnics==1),]

#for bootstrapping later:
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 


#"In the control condition...71% of non-coethnics would vote for Yayi compared to merely 19% of Chantal coethnics"
mean(control$BoniVote[control$NonCoethnics==1],na.rm=TRUE) #0.7142857
mean(control$BoniVote[control$FonGroup==1],na.rm=TRUE) #0.1935484
#"This more than fifty percentage point difference is statistically significant beyond the 99% confidence level"
t.test(control$BoniVote~control$FonGroupNonCoethnics) #pvalue=1.683e-09

# Table 1
# Control Chantal Coethnics
t.test(control$BoniVote~control$FonGroupNonCoethnics,unpaired=TRUE)
table(control$BoniVote,control$FonGroupNonCoethnics)
# Wife Chantal Coethnics
t.test(femme$BoniVote~femme$FonGroupNonCoethnics,unpaired=TRUE)
table(femme$BoniVote,femme$FonGroupNonCoethnics)
# Fon Chantal Coethnics
t.test(femmefon$BoniVote~femmefon$FonGroupNonCoethnics,unpaired=TRUE)
table(femmefon$BoniVote,femmefon$FonGroupNonCoethnics)
# Wife-Control, Chantal Coethnics
t<-t.test(fonbenin$BoniVote~fonbenin$ControlFemme,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t
# Fon-Control, Chantal Coethnics
t<-t.test(fonbenin$BoniVote~fonbenin$ControlFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t
# Fon-Wife, Chantal Coethnics
t<-t.test(fonbenin$BoniVote~fonbenin$FemmeFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t

# Non-coethnics
# Wife-Control, Non-coethnics
t<-t.test(noncoethnics$BoniVote~noncoethnics$ControlFemme,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t
# Fon-Control, Non-coethnics
t<-t.test(noncoethnics$BoniVote~noncoethnics$ControlFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t
# Fon-Wife, Chantal Coethnics
t<-t.test(noncoethnics$BoniVote~noncoethnics$FemmeFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t

#Table 2: Determinants of Yayi Vote Choice on Chantal 
#coethnics and non-coethnics in Benin Survey Experiment
#Chantal coethnics Model (1)
data<-benin[which(benin$FonGroup==1 & benin$pop==1),]
vote1 <- zelig(BoniVote ~ Femme + FemmeFon, model="ls", data=data, robust=TRUE)
  #logit
  vote1 <- zelig(BoniVote ~ Femme + FemmeFon, model="logit", data=data, robust=TRUE)
summary(vote1)
stargazer(vote1, title="", align=TRUE, ci=FALSE, keep=c("FemmeFon"))
#bootstrapped (for Footnote 21)
vote1boot<-boot(data=data,statistic=bs,R=1000, formula=BoniVote~Femme+FemmeFon)
#results
vote1boot
#95% confidence intervals 
boot.ci(vote1boot, type="bca", index=1) # intercept 
boot.ci(vote1boot, type="bca", index=2) # Femme 
boot.ci(vote1boot, type="bca", index=3) # FemmeFon

#Chantal coethnics Model (2)
vote2 <- zelig(BoniVote ~ Femme + FemmeFon + RAdummy1 + RAdummy2 + RAdummy3, model="ls", data=data, robust=TRUE)
  #logit
  vote2 <- zelig(BoniVote ~ Femme + FemmeFon, model="logit", data=data, robust=TRUE)
summary(vote2)
stargazer(vote2, title="", align=TRUE, ci=FALSE, keep=c("FemmeFon"))
#bootstrapped
vote2boot<-boot(data=data,statistic=bs,R=1000, formula=BoniVote~Femme+FemmeFon+ RAdummy1 + RAdummy2 + RAdummy3)
#results
vote2boot
#95% confidence intervals 
boot.ci(vote2boot, type="bca", index=1) # intercept 
boot.ci(vote2boot, type="bca", index=2) # Femme 
boot.ci(vote2boot, type="bca", index=3) # FemmeFon

#noncoethnics Model (1)
#data<-benin[which(benin$YayiCoethnics!=1&benin$FonGroup!=1&benin$AdjaGroup!=1),]
data<-benin[which(benin$YayiCoethnics!=1&benin$FonGroup!=1),]
vote1 <- zelig(BoniVote ~ Femme + FemmeFon, model="ls", data=data, robust=TRUE)
  #logit
  vote1 <- zelig(BoniVote ~ Femme + FemmeFon, model="logit", data=data, robust=TRUE)
summary(vote1)
stargazer(vote1, title="", align=TRUE, ci=FALSE, keep=c("FemmeFon"))
#bootstrapped (for Footnote 21)
vote1boot<-boot(data=data,statistic=bs,R=1000, formula=BoniVote~Femme+FemmeFon)
#results
vote1boot

#noncoethnics Model (2)
vote2 <- zelig(BoniVote ~ Femme + FemmeFon + RAdummy1 + RAdummy2 + RAdummy3, model="ls", data=data, robust=TRUE)

  #logit
  vote2 <- zelig(BoniVote ~ Femme + FemmeFon + RAdummy1 + RAdummy2 + RAdummy3, model="logit", data=data, robust=TRUE)
summary(vote2)
stargazer(vote2, title="", align=TRUE, ci=FALSE, keep=c("FemmeFon"))
#bootstrapped
vote2boot<-boot(data=data,statistic=bs,R=1000, formula=BoniVote~Femme+FemmeFon+ RAdummy1 + RAdummy2 + RAdummy3)
#results
vote2boot
#95% confidence intervals 
boot.ci(vote2boot, type="bca", index=1) # intercept 
boot.ci(vote2boot, type="bca", index=2) # Femme 
boot.ci(vote2boot, type="bca", index=3) # FemmeFon


#among noncoethnics: do they believe Yayi favors South?
t.test(noncoethnics$nordsud1~noncoethnics$ControlFemmeFon)
t.test(noncoethnics$nordsud1~noncoethnics$FemmeFemmeFon)
# Among Yayi’s coethnics?
t.test(yayicoethnics$nordsud1~yayicoethnics$ControlFemmeFon)
t.test(yayicoethnics$nordsud1~yayicoethnics$FemmeFemmeFon)



#North/South
# Page 16: In our sample, just over 1/3 of respondents claimed that President Yayi favors Southerners over Northerners. 
table(benin$nordsud1) #(162/(162+303))
# Yet, only 9% of those are Fon [...] 
table(benin$nordsud1,benin$ethnie) #(15/162)
#Indeed, approximately 9.8% of Fon respondents in the control condition claimed that Yayi favors Southerners. 
table(fonbenin$nordsud1,fonbenin$Control) #(6/61)
#This number increases to 13.7% for Fon respondents who received the Fon Wife cue
table(fonbenin$nordsud1,fonbenin$FemmeFon) # (7/51)
# difference is not statistically distinguishable from zero (p=0.513).
t.test(fonbenin$nordsud1~fonbenin$ControlFemmeFon)


# ttest1: Femme against FemmeFon "FemmeFemmeFon"
mean(benin$nordsud1,na.rm=TRUE)
mean(fonbenin$nordsud1,na.rm=TRUE)
# (% responded southerners, % responded Southerners who are Fon, and ttest on responding Southerner for the Fon exposed to fon wife cue v. control)
t.test(benin$nordsud1~benin$FemmeFemmeFon)
t.test(fonbenin$nordsud1~fonbenin$FemmeFemmeFon) # not significant

#plot ttest1: boxplot of probability picking South as being favored by receiving Femme or FemmeFon treatment
pdf("boxplot ttest FemmeFemmeFon.pdf")
boxplot(fonbenin$FemmeFemmeFon~fonbenin$nordsud1, data=fonbenin, main="Instrumentalism Mechanism Test", xlab="Treatment (Wife or Fon Wife)", ylab="Probability of Selecting South as Favored", ylim=c(0,1),names=c("Wife", "Fon Wife"), col=c("lightskyblue1","royalblue"))
dev.off()

# ttest2: Control against FemmeFon "ControlFemmeFon"
t.test(fonbenin$nordsud1~fonbenin$ControlFemmeFon) # not significant
#plot ttest2: boxplot of probability picking South as being favored by receiving Control or FemmeFon treatment
pdf("boxplot ttest.pdf")
boxplot(fonbenin$ControlFemmeFon~fonbenin$nordsud1, data=fonbenin, main="Instrumentalism Mechanism Test", xlab="Treatment (Control or Fon Wife)", ylab="Probability of Selecting South as Favored", names=c("Control", "Fon Wife"),col=c("lightskyblue1","royalblue"))
dev.off()
# with various controls, OLS
ns1<-lm(nordsud1~FemmeFemmeFon+age+Christian+Muslim+Education+sexe+WithoutFood, data=fonbenin)
summary(ns1)
# with various controls, logit
ns2<-glm(nordsud1~FemmeFemmeFon+age+Christian+Muslim+Education+sexe+WithoutFood, data=fonbenin, family=binomial(logit))
summary(ns2)
# with various controls, probit
ns3<-glm(nordsud1~FemmeFemmeFon+age+Christian+Muslim+Education+sexe+WithoutFood, data=fonbenin, family=binomial(probit))
summary(ns3)
#plot OLS and logit
pdf("coeffs.pdf")
coefplot(ns1,col.pts="seagreen3", xlim=c(-5.5,3), intercept=TRUE, main="Instrumentalism Mechanism Test", varnames=c("(Intercept)", "Fon Wife", "Age", "Christian", "Muslim", "Education", "Sex", "Food"))
coefplot(ns2, add=TRUE, col.pts="orange1", intercept=TRUE, offset=.1)
coefplot(ns3, add=TRUE, col.pts="royalblue1", intercept=TRUE, offset=.3)
dev.off()

## Extra Mechanism tests ##
# Instrumental mechanism test 1: do Adja (southern group) respond to cue?
#Adja group creation
adja<-benin[which(benin$AdjaGroup==1),]
t.test(adja$nordsud1~adja$ControlFemmeFon)
t.test(adja$nordsud1~adja$ControlFemme)
t.test(adja$nordsud1~adja$FemmeFemmeFon)

t.test(adja$BoniVote~adja$ControlFemmeFon) 
t.test(adja$BoniVote~adja$ControlFemme)
t.test(adja$BoniVote~adja$FemmeFemmeFon)

# Instrumental mechanism test 2: do Southern departments respond to cue? (Zou,Kouffo,Mono,Atlantique)
unique(benin$department)
#create southern variable
benin$southern<-as.numeric(ifelse(benin$department=="Zou"|benin$department=="Couffo"
              |benin$department=="Mono"|benin$department=="Atlantique",1,0))
southern<-benin[which(benin$southern==1),]
t.test(southern$nordsud1~southern$ControlFemmeFon) 
t.test(southern$nordsud1~southern$ControlFemme)
t.test(southern$nordsud1~southern$FemmeFemmeFon)

t.test(southern$BoniVote~southern$ControlFemmeFon) 
t.test(southern$BoniVote~southern$ControlFemme)
t.test(southern$BoniVote~southern$FemmeFemmeFon)

# Cosmopolitical mechanism test (heterogeneous): are highly educated people more likely to respond to priming?
# test only on highly educated-non-coethnics

names(benin)
unique(benin$education)
#create high education variable (with education level 6 and 7 "Secondaire (avec Bac)" and "Universite (completee ou non)")
benin$higheducation1<-as.numeric(ifelse(benin$education=="7"|benin$education=="6",1,0))
#create high education variable 2 (with education level 7 "Universite (completee ou non)")
benin$higheducation2<-as.numeric(ifelse(benin$education=="7",1,0))
#highed1
highed1<-benin[which(benin$higheducation1==1 & benin$NonCoethnics==1),] #higheducation1 and noncoethnics
highed1<-droplevels(highed1)

t.test(highed1$nordsud1~highed1$ControlFemmeFon) 
t.test(highed1$nordsud1~highed1$ControlFemme)
t.test(highed1$nordsud1~highed1$FemmeFemmeFon)

t.test(highed1$BoniVote~highed1$ControlFemmeFon) 
t.test(highed1$BoniVote~highed1$ControlFemme) # significant
t.test(highed1$BoniVote~highed1$FemmeFemmeFon) 

#highed2
highed2<-benin[which(benin$higheducation2==1 & benin$NonCoethnics==1),] #higheducation2 and noncoethnics
highed2<-droplevels(highed2)

t.test(highed2$nordsud1~highed2$ControlFemmeFon) 
t.test(highed2$nordsud1~highed2$ControlFemme)
t.test(highed2$nordsud1~highed2$FemmeFemmeFon)

t.test(highed2$BoniVote~highed2$ControlFemmeFon) 
t.test(highed2$BoniVote~highed2$ControlFemme) # significant
t.test(highed2$BoniVote~highed2$FemmeFemmeFon) 

# Look at only Control group and see who is favoring the South
control<-benin[which(benin$passage=="Control"),]

table(control$nordsud,control$ethnie)
table(control$nordsud,control$religion)
table(control$nordsud,control$education)

# Yayi’s coethnics don’t respond negatively to the cue. 
# Run tests on Bariba and Nago ethnic groups.

bariba<-benin[which(benin$BaribaGroup==1),]
nago<-benin[which(benin$NagoGroup==1),]
both<-benin[which(benin$BaribaGroup==1 | benin$NagoGroup==1),]

t.test(bariba$BoniVote~bariba$ControlFemme)
t.test(bariba$BoniVote~bariba$ControlFemmeFon)
t.test(bariba$BoniVote~bariba$FemmeFemmeFon)

t.test(nago$BoniVote~nago$ControlFemme)
t.test(nago$BoniVote~nago$ControlFemmeFon)
t.test(nago$BoniVote~nago$FemmeFemmeFon)

t.test(both$BoniVote~both$ControlFemme)
t.test(both$BoniVote~both$ControlFemmeFon)
t.test(both$BoniVote~both$FemmeFemmeFon)

# Page 14: In the control condition: 71% of non-coethnics would vote for Yayi compared to merely 19% of Chantal coethnics. 
# This more than fifty-percentage-point difference is statistically significant... (EXPERIMENT)
control<-benin[which(benin$passage=="Control"),]
#create group 0 is non-coethnics, 1 is FonGroup
control$NonCoethnicsFonGroup<-as.numeric(ifelse(control$FonGroup==1,1,"NA"))
control$NonCoethnicsFonGroup<-ifelse(control$NonCoethnics==1,0,control$NonCoethnicsFonGroup)

t.test(control$BoniVote~control$NonCoethnicsFonGroup)


#Footnote 18: Table SI-4 summarizes balance tests between the three survey conditions 
# and indicates that balance was achieved over a lare set of pretreatment covariates.(EXPERIMENT)

#Male
t.test(benin$male~benin$Control)
sum(count(control$male)[2])
t.test(benin$male~benin$Femme)
sum(count(femme$male)[2])
t.test(benin$male~benin$FemmeFon)
sum(count(femmefon$male)[2])

t<-t.test(benin$male~benin$ControlFemme)
t<-t.test(benin$male~benin$ControlFemmeFon)
t<-t.test(benin$male~benin$FemmeFemmeFon)
t$estimate[1]-t$estimate[2]
t

#Age
t.test(benin$age~benin$Control)
sum(count(control$age)[2])
t.test(benin$age~benin$Femme)
sum(count(femme$age)[2])
t.test(benin$age~benin$FemmeFon)
sum(count(femmefon$age)[2])

t<-t.test(benin$age~benin$ControlFemme)
t<-t.test(benin$age~benin$ControlFemmeFon)
t<-t.test(benin$age~benin$FemmeFemmeFon)
t$estimate[1]-t$estimate[2]
t

#Fon
t.test(benin$FonGroup~benin$Control)
sum(count(control$FonGroup)[2])
t.test(benin$FonGroup~benin$Femme)
sum(count(femme$FonGroup)[2]) #1NA
t.test(benin$FonGroup~benin$FemmeFon)
sum(count(femmefon$FonGroup)[2]) #1NA

t<-t.test(benin$FonGroup~benin$ControlFemme)
t<-t.test(benin$FonGroup~benin$ControlFemmeFon)
t<-t.test(benin$FonGroup~benin$FemmeFemmeFon)
t$estimate[1]-t$estimate[2]
t

#Yoruba
t.test(benin$YorubaGroup~benin$Control)
sum(count(control$YorubaGroup)[2])
t.test(benin$YorubaGroup~benin$Femme)
sum(count(femme$YorubaGroup)[2]) #1NA
t.test(benin$YorubaGroup~benin$FemmeFon)
sum(count(femmefon$YorubaGroup)[2]) #1NA

t<-t.test(benin$YorubaGroup~benin$ControlFemme)
t<-t.test(benin$YorubaGroup~benin$ControlFemmeFon)
t<-t.test(benin$YorubaGroup~benin$FemmeFemmeFon)
t$estimate[1]-t$estimate[2]
t

#Bariba
t.test(benin$BaribaGroup~benin$Control)
sum(count(control$BaribaGroup)[2])
t.test(benin$BaribaGroup~benin$Femme)
sum(count(femme$BaribaGroup)[2]) #1NA
t.test(benin$BaribaGroup~benin$FemmeFon)
sum(count(femmefon$BaribaGroup)[2]) #1NA

t<-t.test(benin$BaribaGroup~benin$ControlFemme)
t<-t.test(benin$BaribaGroup~benin$ControlFemmeFon)
t<-t.test(benin$BaribaGroup~benin$FemmeFemmeFon)
t$estimate[1]-t$estimate[2]
t

#Christian
t.test(benin$Christian~benin$Control)
sum(count(control$Christian)[2])
t.test(benin$Christian~benin$Femme)
sum(count(femme$Christian)[2]) #2NA
t.test(benin$Christian~benin$FemmeFon)
sum(count(femmefon$Christian)[2]) #2NA

t<-t.test(benin$Christian~benin$ControlFemme)
t<-t.test(benin$Christian~benin$ControlFemmeFon)
t<-t.test(benin$Christian~benin$FemmeFemmeFon)
t$estimate[1]-t$estimate[2]
t

#Muslim
t.test(benin$Muslim~benin$Control)
sum(count(control$Muslim)[2])
t.test(benin$Muslim~benin$Femme)
sum(count(femme$Muslim)[2]) #2NA
t.test(benin$Muslim~benin$FemmeFon)
sum(count(femmefon$Muslim)[2]) #2NA

t<-t.test(benin$Muslim~benin$ControlFemme)
t<-t.test(benin$Muslim~benin$ControlFemmeFon)
t<-t.test(benin$Muslim~benin$FemmeFemmeFon)
t$estimate[1]-t$estimate[2]
t

#Animist
t.test(benin$Animist~benin$Control)
sum(count(control$Animist)[2])
t.test(benin$Animist~benin$Femme)
sum(count(femme$Animist)[2]) #2NA
t.test(benin$Animist~benin$FemmeFon)
sum(count(femmefon$Animist)[2]) #2NA

t<-t.test(benin$Animist~benin$ControlFemme)
t<-t.test(benin$Animist~benin$ControlFemmeFon)
t<-t.test(benin$Animist~benin$FemmeFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t


#Religiosity (variable called "importance")
t.test(benin$importance1~benin$Control)
sum(count(control$importance1)[2]) #1NA
t.test(benin$importance1~benin$Femme)
sum(count(femme$importance1)[2]) #3NA
t.test(benin$importance1~benin$FemmeFon)
sum(count(femmefon$importance1)[2]) #2NA

t<-t.test(benin$importance1~benin$ControlFemme,unpaired=TRUE)
t<-t.test(benin$importance1~benin$ControlFemmeFon)
t<-t.test(benin$importance1~benin$FemmeFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t


#Education
t.test(benin$education1~benin$Control)
sum(table(benin$education1,benin$Control)[,2])
t.test(benin$education1~benin$Femme)
sum(table(femme$education1))
t.test(benin$education1~benin$FemmeFon)
sum(count(femmefon$education1)[2]) #1NA

t<-t.test(benin$education1~benin$ControlFemme)
t<-t.test(benin$education1~benin$ControlFemmeFon)
t<-t.test(benin$education1~benin$FemmeFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t

#Without Food (malnourriture variable)
t.test(benin$malnourriture1~benin$Control)
sum(count(control$malnourriture1)[2]) #1NA
t.test(benin$malnourriture1~benin$Femme)
sum(count(femme$malnourriture1)[2]) #1NA
t.test(benin$malnourriture1~benin$FemmeFon)
sum(count(femmefon$malnourriture1)[2]) #1NA

t<-t.test(benin$malnourriture1~benin$ControlFemme)
t<-t.test(benin$malnourriture1~benin$ControlFemmeFon)
t<-t.test(benin$malnourriture1~benin$FemmeFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t


#Table SI-5 : Manipulation check in Benin Survey Experiment
# Control Chantal Coethnics
t.test(control$WifeIsFon~control$FonGroupNonCoethnics,unpaired=TRUE)
table(control$WifeIsFon,control$FonGroupNonCoethnics)
# Wife Chantal Coethnics
t.test(femme$WifeIsFon~femme$FonGroupNonCoethnics,unpaired=TRUE)
table(femme$WifeIsFon,femme$FonGroupNonCoethnics)
# Fon Chantal Coethnics
t.test(femmefon$WifeIsFon~femmefon$FonGroupNonCoethnics,unpaired=TRUE)
table(femmefon$WifeIsFon,femmefon$FonGroupNonCoethnics)
# Wife-Control, Chantal Coethnics (all 1s)

# Fon-Control, Chantal Coethnics
t<-t.test(fonbenin$WifeIsFon~fonbenin$ControlFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t
# Fon-Wife, Chantal Coethnics
t<-t.test(fonbenin$WifeIsFon~fonbenin$FemmeFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t

# Non-coethnics
# Wife-Control, Non-coethnics
t<-t.test(noncoethnics$WifeIsFon~noncoethnics$ControlFemme,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t
# Fon-Control, Non-coethnics
t<-t.test(noncoethnics$WifeIsFon~noncoethnics$ControlFemmeFon,unpaired=TRUE)
t$estimate[1]-t$estimate[2]
t
# Fon-Wife, Chantal Coethnics
#all 1s


# Average support in the Control condition for leader coethnics
mean(control$BoniVote[control$BaribaGroup==1|control$NagoGroup==1],na.rm=TRUE) #0.8571429
mean(control$BoniVote[control$BaribaGroup==1|control$YorubaGroup==1],na.rm=TRUE)#0.7105263
