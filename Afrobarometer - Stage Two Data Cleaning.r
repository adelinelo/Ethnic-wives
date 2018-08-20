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
# This code reads in the merged and cleaned Afrobarometer 
# Rounds 3 and 4 data, along with a dataset of the ethnicities of 
# political opponents.  It merges these two datasets,  recodes
# the ethnicity variables incorporating new information, and 
# creates a variable for the portion of the population constituted by 
# each ethnic group.
###############################################

library(foreign)
rm(list=ls())

###############################################
# Read in Data
###############################################

afbData <-read.dta(file="ABR3and4Data.dta") 
ethData <- read.csv(file = "Opponents and Wives.csv")

###############################################
# Merge two datasets 
###############################################

#change country label in afb to lower-case to match, preserve upper case in Country variable
afbData$country <- tolower(afbData$country)

#add the AfB code for the ethnicity(ethnicities) of the opponent to each row
afbDataMerge <- merge(afbData,ethData[,c("country","Round","Opponent.AfB.Ethnic.Code","Opponent.AfB.Ethnic.Code.2")],by.x=c("country","round"),by.y=c("country","Round"),all.x=T)

###############################################
# Create Opponent Coethnic Variable
###############################################

#replace NA with 0 for opponent ethnic code variables
afbDataMerge$Opponent.AfB.Ethnic.Code[is.na(afbDataMerge$Opponent.AfB.Ethnic.Code)]<-0
afbDataMerge$Opponent.AfB.Ethnic.Code.2[is.na(afbDataMerge$Opponent.AfB.Ethnic.Code.2)]<-0

# oppcoethnic NA if there was no opponent ethnicity in the dataset from nathan (both opponent variables are zero)
# 1 if matched with either row of opponent ethnicity
# 0 if it is unmatched
afbDataMerge$oppcoethnic <- ifelse(afbDataMerge$Opponent.AfB.Ethnic.Code+afbDataMerge$Opponent.AfB.Ethnic.Code.2==0,NA,  
                                   ifelse(afbDataMerge$round==3,
                                          ifelse(afbDataMerge$Opponent.AfB.Ethnic.Code==afbDataMerge$R3Ethnic|
                                                 afbDataMerge$Opponent.AfB.Ethnic.Code.2==afbDataMerge$R3Ethnic,1,0),
                                          ifelse(afbDataMerge$Opponent.AfB.Ethnic.Code==afbDataMerge$R4Ethnic|
                                                 afbDataMerge$Opponent.AfB.Ethnic.Code.2==afbDataMerge$R4Ethnic,1,0)))

afbDataMerge$oppcoethnic <- ifelse(afbDataMerge$round==3,
                                   ifelse(afbDataMerge$R3Ethnic<0|afbDataMerge$R3Ethnic>990,NA,afbDataMerge$oppcoethnic),
                                   ifelse(afbDataMerge$R4Ethnic<0|afbDataMerge$R4Ethnic>990,NA,afbDataMerge$oppcoethnic))

                                  
###############################################
# Create ethfrac variable (ethnic group % of population)
###############################################
library(Zelig)
library(ZeligChoice)

afb <- afbDataMerge
rm(list=c("afbData","afbDataMerge","ethData"))

#variable for weights is Withinwt for rd4, withinwt for rd3, it is the fraction or number of individuals each row should be weighted by
# where withinwt sums up to the total number of individuals
# check Tanzania
vars<-c("country","oppcoethnic","round","q79")
tz<-afb[vars]
tz3<-tz[which(tz$round==3),] #Note madagascar has no oppcoethnic==1 (not15%), Zim has no oppcoethnic==1 (no Shona/Karanga)
tz4<-tz[which(tz$round==4),] #Note madagascar has no oppcoethnic==1 (not15%)
table(tz3$country,tz3$oppcoethnic)

#sum up across individuals of the same ethnicity, then that is the weight for each ethnic group out of total # of individuals.
#test round 4
table(afb$country,afb$round)
#Withinwt<-as.numeric(levels(afb$Withinwt))[afb$Withinwt] #change factor Withinwt to numeric
#afb$Withinwt<-Withinwt
testa<-aggregate(afb$Withinwt, list(Country=afb$country, Round=afb$round), sum) #check that this sums up to # of respondents per country
testa<-testa[19:38,] #just round4
ethnicfrac<-aggregate(afb$Withinwt, list(Country=afb$country, Ethnic=afb$R4Ethnic), sum)
#check that summing across one country gets full # of obs per country
testb<-aggregate(ethnicfrac$x,list(Country=ethnicfrac$Country),sum) #testa$x should be same as testb$x


#if we take the weight of each ethnic group/# individuals ==> estimated percentage of population for given ethnic group 
#each country # of individuals
#merge
ethnicfracR4<-merge(ethnicfrac,testa, by.x=c("Country"),by.y=c("Country"),all.x=T)
ethnicfracR4$ethnicpercent<-(ethnicfracR4$x.x/ethnicfracR4$x.y)
aggregate(ethnicfracR4$ethnicpercent, list(Country=ethnicfracR4$Country), sum) #check that ethnicpercent adds up to 1 for each country

####### do it for R3 ##

#sum up across individuals of the same ethnicity, then that is the weight for each ethnic group out of total # of individuals.
testa<-aggregate(afb$withinwt, list(Country=afb$country, Round=afb$round), sum)#check that this sums up to # of respondents per country
testa<-testa[1:18,] #just round 3
ethnicfrac<-aggregate(afb$withinwt, list(Country=afb$country, Ethnic=afb$R3Ethnic), sum)
#check that summing across one country gets full # of obs per country
testb<-aggregate(ethnicfrac$x,list(Country=ethnicfrac$Country),sum) #testa$x should be same as testb$x

#if we take the weight of each ethnic group/# individuals ==> estimated percentage of population for given ethnic group 
#each country # of individuals
#merge
ethnicfracR3<-merge(ethnicfrac,testa, by.x=c("Country"),by.y=c("Country"),all.x=T)
ethnicfracR3$ethnicpercent<-(ethnicfracR3$x.x/ethnicfracR3$x.y)
aggregate(ethnicfracR3$ethnicpercent, list(Country=ethnicfracR3$Country), sum) #check that ethnicpercent adds up to 1 for each country

#combine R3 and R4
ethnicfrac<-rbind(ethnicfracR3,ethnicfracR4)
#head(ethnicfrac)
#tail(ethnicfrac)
names<-c("Country", "ethniccode", "withinwt", "round","countryroundn", "ethnicpercent")
names(ethnicfrac)<-names
#head(ethnicfrac)

# merge in ethnic weights and percentages
ethnicfrac4<-subset(ethnicfrac,round==4)
ethnicfrac3<-subset(ethnicfrac,round==3)
mergedata <- merge(afb,ethnicfrac4,by.x=c("country","round","R4Ethnic"),by.y=c("Country","round","ethniccode"),all.x=T)
#tail(mergedata)
#head(mergedata)
mergedata2 <- merge(mergedata,ethnicfrac3,by.x=c("country","round","R3Ethnic"),by.y=c("Country","round","ethniccode"),all.x=T)
mergedata2$ethnicpercent<-ifelse(is.na(mergedata2$ethnicpercent.y),mergedata2$ethnicpercent.x,mergedata2$ethnicpercent.y)
sum(is.na(mergedata2$ethnicpercent)) #should be no NAs
mergedata2$withinwt<-ifelse(is.na(mergedata2$withinwt.y),mergedata2$withinwt.x,mergedata2$withinwt.y)
sum(is.na(mergedata2$withinwt)) #should be no NAs
mergedata2$countryroundn<-ifelse(is.na(mergedata2$countryroundn.y),mergedata2$countryroundn.x,mergedata2$countryroundn.y)
sum(is.na(mergedata2$countryroundn)) #should be no NAs

#drop excess variables: withinwt.x, withinwt.y, Withinwt, ethnicpercent.x, ethnicpercent.y, countryroundn.x, countryroundn.y
exclude<-names(mergedata2) %in% c("withinwt.x", "withinwt.y", "Withinwt", "ethnicpercent.x", "ethnicpercent.y", "countryroundn.x", "countryroundn.y")
mergedata3<-mergedata2[!exclude]
#head(mergedata3)

###############################################
# Drop Unused Observations
###############################################

rm(list=ls()[ls()!="mergedata3"])

data <- mergedata3
names(data)

# Keep only 14 countries that are nonnational marriages in rounds 3 and 4: to verify, pls check "DatasetInclusionDecision.xlsx" file
#mozambique BOTH rounds

data1 <- data[which(data$country=='benin'|data$country=='botswana' & data$round==3| data$country=='ghana'|data$country=='kenya'
                    |data$country=='madagascar'|data$country=='mali'|data$country=='mozambique'#& data$round==4
                    |data$country=='namibia'|data$country=='nigeria'|data$country=='south africa'& data$round==3|data$country=='uganda'
                    |data$country=='tanzania'|data$country=='zambia'|data$country=='zimbabwe'&data$round==4), ]


#check that correct country/years included
table(data1$country,data1$round)

###############################################
# Recodes ethnicity variable given new information
###############################################

#Recode Botswana r3 (see Amanda Robinson email 4/9/15) so 140-144, 146, 149-150, 153, 156, and 159 are Tswana sub-tribes.

table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==140&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic)
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==141&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==142&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==143&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==144&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==146&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==149&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==150&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==153&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==156&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==159&data1$country=='botswana',1,data1$prescoethnic) 
table(data1$prescoethnic) 
#Recode Benin R3 (anyone who is Ditamari (125) is coethnic with president)
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==125&data1$country=='benin',1,data1$prescoethnic) 
table(data1$prescoethnic)
#Recode Tanzania R3 (such that anyone who is Mmakonde (387) is coethnic with president)
data1$prescoethnic <- ifelse(data1$round==3&data1$q79==387&data1$country=='tanzania',1,data1$prescoethnic)
table(data1$prescoethnic) 

###############################################
# Code cases where Opponent and Spouse are Coethnic
###############################################
# divide it up btwn cases where opponent is coethnic of spouse & where the opponent is not?
#need new variable: oppwifecoethnic; use "Opponents and Wives - AB3 and 4.xlsx"
# pairs for each country-round are opponent ethnicity first, then wife ethnicity
# [1,0] benin-3: (Fon, Fon); benin-4: (Goun, Fon)
# [0] botswana-3: (Tswana/Mongwato, Kalanga)
# [1,1] ghana-3: (Akan, Akan); ghana-4: (Akan, Akan) # (Wife is Ashanti but coded as Akan since subgroup of Akan)
# [1,1] kenya-3: (Kikuyu, Kikuyu); kenya-4: (Kikuyu, Kikuyu)
# [NA,NA] madagascar-3: (*Not 15%, Merina); madagascar-4: (*Not 15%, Merina) [Note both treated as NA]
# [0,0] mali-3: (Sonrhai, Bambara); mali-4: (Sonrhai, Bambara)
# [1,1] mozambique-3: (Ndau, Ndau), mozambique-4: (Ndau, Ndau)
# [1,1] namibia-3: (Ovambo, Ovambo); namibia-4: (Ovambo, Ovambo)
# [0,1] nigeria-3: (Hausa, Edo); nigeria-4: (Hausa, Hausa-Fulani) [Note Nigeria-4 treated as 1]
# [0] safrica-3: (Afrikaner, Zulu)
# [1,0] tanzania-3: (Chaga, Chagga); (Nyamwezi, Coastal Muslim);  [tanzania-4 treated as 1]
# [0,0] uganda-3: (Bunyankole, Ankole); uganda-4: (Bunyankole, Ankole) # check to make sure these are NOT same
# [0,0] zambia-3: (Tonga, Lenje); zambia-4: (Bisa, Tumbuka)
# [1,1] zimbabwe-3: (Shona/Karanga, Shona); (Shona/Karanga, Shona) [treated as 1]
data1$oppwifecoethnic<-ifelse(#1 values
  data1$country=="benin" & data1$round==3|
    data1$country=="ghana"|
    data1$country=="kenya"|
    data1$country=="mozambique" |
    data1$country=="namibia"|
    data1$country=="nigeria" & data1$round==4|
    data1$country=="tanzania" & data1$round==3|
    data1$country=="zimbabwe"
  ,
  1,      #code as 1 since oppwifecoethnic
  ifelse(#0 values
    data1$country=="benin" & data1$round==4|
      data1$country=="botswana"&data1$round==3|
      data1$country=="mali"|
      data1$country=="nigeria" & data1$round==3|
      data1$country=="south africa" & data1$round==3|
      data1$country=="tanzania" & data1$round==4|
      data1$country=="uganda"|
      data1$country=="zambia"
    ,
    0,    #code as 0 since NOT oppwifecoethnic
    NA))  #otherwise code as NA since not all info available

###############################################
# Drop Unused Data and Save
###############################################
#drop unnecessary (empty) levels (data1 full)
data1<-droplevels(data1)

#keep only variables we need for models
keep<-c("wifecoethnic","ethnic","oppcoethnic", "ethnicpercent","round", "rural","race", "head","cecon","recon",
		"fcecon", "frecon","interest","favorOwnGroup","noFood","noWater","hasRadio","hasTV","hasVehicle","voted",
		"educ","iage","ieducation","igender","country","vote", "approval", "ethnicUnfair", "ethnicPolitical", 
		"prescoethnic", "preswifesame","age","female","educ", "oppwifecoethnic", "prescoethnic2", "wifecoethnic2")
data2<-data1[keep]

data<-data2 #data with all cases

data1<-subset(data,(prescoethnic==0 & preswifesame==0)) #subversion of data
dataMAKUA<-subset(data,(prescoethnic2==0 & preswifesame==0)) #subversion of data for robustness check: Mozambique R3 president coded as MAKUA (not Ronga)
# dataMAKUA smaller dataset than data1 because there are more prescoethnics (adding in Mozambique R3 Makuas), so using only the prescoethnics==0 means fewer people
data1<-droplevels(data1)
dataMAKUA<-droplevels(dataMAKUA)

# save files
write.csv(data, "DataforAnalysisFull.csv") # full data, not subsetted to (prescoethnic==0 & preswifesame==0)
write.csv(data1,"DataforAnalysisSub.csv") #subset data (prescoethnic==0 & preswifesame==0)
write.csv(dataMAKUA, "DataforAnalysisSubMAKUA.csv") #subset data (prescoethnic2==0 & preswifesame==0)

                                   
                                   

