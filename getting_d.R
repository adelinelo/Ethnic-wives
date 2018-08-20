library("foreign")
library("plyr")
setwd("~DHSEW/dta_files")
#files=c("BFCR21.dta", "BFCR31.dta", "BFCR41.dta", "BFCR61.dta", "BJCR31.dta", "BJCR41.dta","BJCR51.dta", "GHCR4B.dta", "GHCR5A.dta", "GHCR31.dta", "GHCR41.dta", "KECR3A.dta", "KECR32.dta","KECR42.dta", "KECR52.dta", "LBCR51.dta", "LSCR41.dta", "LSCR60.dta", "MDCR41.dta", "MDCR51.dta","MLCR31.dta", "MLCR41.dta", "MLCR52.dta", "MWCR4D.dta", "MWCR21.dta", "MWCR41.dta", "MWCR61.dta","MZCR31.dta", "MZCR41.dta", "MZCR61.dta", "NGCR4A.dta", "NGCR41.dta", "NGCR52.dta", "NMCR41.dta","NMCR51.dta", "SNCR4H.dta", "SNCR21.dta", "SNCR31.dta", "SNCR60.dta", "TZCR3A.dta", "TZCR4I.dta","TZCR21.dta", "TZCR41.dta", "TZCR62.dta", "UGCR33.dta", "UGCR41.dta", "UGCR52.dta", "UGCR60.dta", "ZMCR31.dta", "ZMCR42.dta", "ZMCR51.dta", "ZWCR31.dta", "ZWCR41.dta", "ZWCR51.dta", "ZWCR62")
temp = list.files(pattern="*.DTA") # recognize .dta files
myfiles = lapply(temp, read.dta, convert.factors = FALSE)
keep=c("v000", "v007", "v131", "mv131") # variables we want to keep
# remove keep variables from each item in myfiles, then rbind the lot
foo=function(data,kept){  # function for keeping
  data[kept]  
}
keptfiles = lapply(myfiles,foo, keep)
####################
# correct labels/values (just ordered)
temp
#[1] "BFCR21FL.DTA" 
a=read.dta("BFCR21FL.dta",convert.factors = FALSE) #keep numbers as factor
#a=read.dta("BFCR21FL.dta",convert.factors = NA)
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
#resp
a$v131 <- ifelse(a$v131==98, 11, a$v131) 
#spouse
data1=a

#[2] "BFCR31FL.DTA" 
head(keptfiles[2])
a=read.dta("BFCR31FL.dta",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
#resp
a$v131 <- ifelse(a$v131==15, 14, a$v131)
a$v131 <- ifelse(a$v131==16, 15, a$v131)
a$v131 <- ifelse(a$v131==18, 17, a$v131)
a$v131 <- ifelse(a$v131==19, 18, a$v131)
a$v131 <- ifelse(a$v131==20, 19, a$v131)
a$v131 <- ifelse(a$v131==21, 20, a$v131)
a$v131 <- ifelse(a$v131==22, 21, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==15, 14, a$mv131)
a$mv131 <- ifelse(a$mv131==16, 15, a$mv131)
a$mv131 <- ifelse(a$mv131==19, 18, a$mv131)
a$mv131 <- ifelse(a$mv131==22, 21, a$mv131)
data2=a

#[3]"BFCR41FL.DTA" 
a=read.dta("BFCR41FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
head(keptfiles[3])
#resp
a$v131 <- ifelse(a$v131==96, 11, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 11, a$mv131)
data3=a

#[4]"BFCR61FL.DTA" 
a=read.dta("BFCR61FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
head(keptfiles[4])
#spouse
a$mv131 <- ifelse(a$mv131==98, 16, a$mv131)
a$mv131 <- ifelse(a$mv131==96, 15, a$mv131)
data4=a

#[5]"BJCR31FL.DTA"
a=read.dta("BJCR31FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
head(keptfiles[5])
#resp
a$v131 <- ifelse(a$v131==96, 10, a$v131)
a$v131 <- ifelse(a$v131==8, 9, a$v131)
a$v131 <- ifelse(a$v131==7, 8, a$v131)
a$v131 <- ifelse(a$v131==6, 7, a$v131)
a$v131 <- ifelse(a$v131==5, 6, a$v131)
a$v131 <- ifelse(a$v131==4, 5, a$v131)
a$v131 <- ifelse(a$v131==3, 4, a$v131)
a$v131 <- ifelse(a$v131==2, 3, a$v131)
a$v131 <- ifelse(a$v131==1, 2, a$v131)
a$v131 <- ifelse(a$v131==0, 1, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 10, a$mv131)
a$mv131 <- ifelse(a$mv131==8, 9, a$mv131)
a$mv131 <- ifelse(a$mv131==7, 8, a$mv131)
a$mv131 <- ifelse(a$mv131==6, 7, a$mv131)
a$mv131 <- ifelse(a$mv131==5, 6, a$mv131)
a$mv131 <- ifelse(a$mv131==4, 5, a$mv131)
a$mv131 <- ifelse(a$mv131==3, 4, a$mv131)
a$mv131 <- ifelse(a$mv131==2, 3, a$mv131)
a$mv131 <- ifelse(a$mv131==1, 2, a$mv131)
a$mv131 <- ifelse(a$mv131==0, 1, a$mv131)
data5=a

#[6] "BJCR51FL.DTA" 
a=read.dta("BJCR51FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
head(keptfiles[6])
data6=a

#[7]"GHCR31FL.DTA" 
a=read.dta("GHCR31FL.DTA",convert.factors = TRUE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
head(keptfiles[7])
data7=a

#[8]"GHCR41FL.DTA" 
a=read.dta("GHCR41FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
head(keptfiles[8])
#resp
a$v131 <- ifelse(a$v131==96, 13, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 13, a$mv131)
data8=a

#[9]"GHCR4BFL.DTA" 
a=read.dta("GHCR4BFL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
head(keptfiles[9])
data9=a

#[10]"GHCR5AFL.DTA"
a=read.dta("GHCR5AFL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
head(keptfiles[10])
data10=a

#[11] "KECR32FL.DTA" 
a=read.dta("KECR32FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data11=a

#[12]"KECR3AFL.DTA" 
a=read.dta("KECR3AFL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
head(keptfiles[12])
#resp
a$v131 <- ifelse(a$v131==96, 12, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 12, a$mv131)
data12=a

#[13]"KECR42FL.DTA" 
a=read.dta("KECR42FL.DTA",convert.factors = TRUE) #keep names
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data13=a

#[14]"KECR52FL.DTA" 
a=read.dta("KECR52FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
#spouse
a$mv131 <- ifelse(a$mv131==96, 13, a$mv131)
data14=a

#[15]"LBCR51FL.DTA"
a=read.dta("LBCR51FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data15=a

#[16] "LSCR41FL.DTA" 
a=read.dta("LSCR41FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data16=a

#[17]"LSCR60FL.DTA" 
a=read.dta("LSCR60FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data17=a

#[18]"MDCR41FL.DTA" 
a=read.dta("MDCR41FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data18=a

#[19]"MDCR51FL.DTA" 
a=read.dta("MDCR51FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data19=a


#[20]"MLCR31FL.DTA"
a=read.dta("MLCR31FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
#resp
a$v131 <- ifelse(a$v131==96, 11, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 11, a$mv131)
data20=a


#[21] "MLCR41FL.DTA" 
a=read.dta("MLCR41FL.DTA",convert.factors = TRUE) #keep ethnic names
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data21=a

#[22]"MLCR52FL.DTA" 
a=read.dta("MLCR52FL.DTA",convert.factors = FALSE) 
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data22=a

#[23]"MWCR21FL.DTA" 
a=read.dta("MWCR21FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data23=a

#[24]"MWCR41FL.DTA" 
a=read.dta("MWCR41FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
#resp
a$v131 <- ifelse(a$v131==96, 10, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 10, a$mv131)
data24=a

#[25]"MWCR4DFL.DTA"
a=read.dta("MWCR4DFL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
#resp
a$v131 <- ifelse(a$v131==96, 9, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 9, a$mv131)
data25=a

#[26] "MWCR61FL.DTA" 
a=read.dta("MWCR61FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data26=a

#[27]"MZCR31FL.DTA" 
a=read.dta("MZCR31FL.DTA",convert.factors = TRUE) #keep ethnicities
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data27=a

#[28]"MZCR41FL.DTA" 
a=read.dta("MZCR41FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data28=a

#[29]"MZCR61FL.DTA" 
a=read.dta("MZCR61FL.DTA",convert.factors = TRUE) #keep ethnicities
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data29=a

#[30] "NGCR41FL.DTA"
a=read.dta("NGCR41FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data30=a

#[31] "NGCR4AFL.DTA" 
a=read.dta("NGCR4AFL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data31=a

#[32]"NGCR52FL.DTA" 
a=read.dta("NGCR52FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data32=a

#[33]"NMCR41FL.DTA" 
a=read.dta("NMCR41FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
#resp
a$v131 <- ifelse(a$v131==96, 10, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 10, a$mv131)
data33=a

#[34]"NMCR51FL.DTA" 
a=read.dta("NMCR51FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data34=a

#[35]"SNCR21FL.DTA"
a=read.dta("SNCR21FL.DTA",convert.factors = FALSE) #keep numbers as factor
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data35=a

#[36] "SNCR31FL.DTA" 
a=read.dta("SNCR31FL.DTA",convert.factors = TRUE) #keep ethnic
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data36=a

#[37]"SNCR4HFL.DTA" 
a=read.dta("SNCR4HFL.DTA",convert.factors =FALSE) #keep ethnic
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
#resp
a$v131 <- ifelse(a$v131==96, 8, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 8, a$mv131)
data37=a

#[38]"SNCR60FL.DTA" 
a=read.dta("SNCR60FL.DTA",convert.factors =TRUE) #keep ethnic
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data38=a

#[39]"TZCR21FL.DTA" 
a=read.dta("TZCR21FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data39=a

#[40]"TZCR3AFL.DTA"
a=read.dta("TZCR3AFL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data40=a

#[41] "TZCR41FL.DTA" 
a=read.dta("TZCR41FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data41=a

#[42]"TZCR4IFL.DTA" 
a=read.dta("TZCR4IFL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data42=a

#[43]"TZCR62FL.DTA" 
a=read.dta("TZCR62FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data43=a

#[44]"UGCR33FL.DTA" 
a=read.dta("UGCR33FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
#resp
a$v131 <- ifelse(a$v131==96, 34, a$v131)
#spouse
a$mv131 <- ifelse(a$mv131==96, 34, a$mv131)
data44=a

#[45]"UGCR41FL.DTA"
a=read.dta("UGCR41FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data45=a

#[46] "UGCR52FL.DTA" 
a=read.dta("UGCR52FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data46=a

#[47]"UGCR60FL.DTA" 
a=read.dta("UGCR60FL.DTA",convert.factors =TRUE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data47=a


#[48]"ZMCR31FL.DTA" 
a=read.dta("ZMCR31FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data48=a

#[49]"ZMCR42FL.DTA" 
a=read.dta("ZMCR42FL.DTA",convert.factors =TRUE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data49=a

#[50]"ZMCR51FL.DTA"
a=read.dta("ZMCR51FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data50=a


#[51] "ZWCR31FL.DTA" 
a=read.dta("ZWCR31FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data51=a

#[52]"ZWCR41FL.DTA" 
a=read.dta("ZWCR41FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data52=a

#[53]"ZWCR51FL.DTA" 
a=read.dta("ZWCR51FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data53=a

#[54]"ZWCR62FL.DTA"
a=read.dta("ZWCR62FL.DTA",convert.factors =FALSE) #
keep=c("v000", "v007", "v131", "mv131")
a=a[keep]
data54=a
############################################################################################
data=rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12,
           data13, data14, data15, data16, data17, data18, data19, data20, data21, data22, data23,
           data24, data25, data26, data27, data28, data29, data30, data31, data32, data33, data34,
           data35, data36, data37, data38, data39, data40, data41, data42, data43, data44, data45,
           data46, data47, data48, data49, data50, data51, data52, data53, data54)

save(data, file = "~d.RData")