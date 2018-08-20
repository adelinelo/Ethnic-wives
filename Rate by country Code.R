load("C:/Users/ajv/Dropbox/Ethnic Wives/Datasets/DHSEW/d.rdata")
# Make a country variable
library(stringr)
d$country <- str_sub(d$V1,1,2)

# Check correspondance in names
#new <- data.frame(as.character(unique(d$V3)))
#new[,2] <- c(as.character(unique(d$V4)),rep(NA,length(unique(d$V3))-length(unique(d$V4))))

#write.csv(new,"C:/Users/ajv/Desktop/names.csv")

# Make corrected variables
d$V3cha <- as.character(d$V3)
d$V4cha <- as.character(d$V4)

# Make some corrections to match values

#### Correct V3 ####
# V3 other --> NA
# V3 outra --> NA
# V3 other zambia --> NA
# V3 other african --> NA
# V3 not senegalese --> NA
# V3 not a senegalese --> NA
# V3 missing --> NA
# V3 xitswa & simili --> xitswa
# V3 xitsonga & simili --> xitsonga
# V3 emakua & simili --> emakua
# V3 elomue & emarenjo --> elomue
# V3 cisena & simili --> cisena

d$V3clean <- 
  ifelse(d$V3cha=="other", NA,
  ifelse(d$V3cha=="outra", NA,
  ifelse(d$V3cha=="other zambia", NA,       
  ifelse(d$V3cha=="other african", NA,
  ifelse(d$V3cha=="not senegalese", NA,
  ifelse(d$V3cha=="not a senegalese", NA,
  ifelse(d$V3cha=="missing", NA,
  ifelse(d$V3cha=="xitswa & simili","xitswa",      
  ifelse(d$V3cha=="xitsonga & simili","xitsonga",
  ifelse(d$V3cha=="emakua & simili","emakua",
  ifelse(d$V3cha=="elomue & emarenjo","elomue",
  ifelse(d$V3cha=="cisena & simili","cisena",
  d$V3cha))))))))))))
        
#### Correct V4 ####
# V4 elomue --> elomwe
# V4 central togo tribes --> NA
# V4 akwapin --> akwapim
# V4 ga /adangbe --> ga.adangbe
# V4 sarkol,/ --> sarkole
# V4 other --> NA
# V4 outra --> NA
# V4 other zambian --> NA
# V4 other non-gh.tribes --> NA
# V4 other ghanaian tr. --> NA
# V4 other african --> NA
# V4 other (senegalese) --> NA
# V4 other (non-senegalese) --> NA
# V4 soninké --> soninke
# V4 sarakol‚/sonink‚/marka --> sarakole
# V4 s‚noufo/minianka --> senoufo/minianka
# V4 not a senegalese --> NA
# V4 xitsonga & simili --> xitsonga
         
d$V4clean <-
  ifelse(d$V4cha=="elomue","elomwe",
  ifelse(d$V4cha=="central togo tribes", NA,
  ifelse(d$V4cha=="akwapin","akwapim",
  ifelse(d$V4cha=="ga /adangbe", "ga.adangbe",
  ifelse(d$V4cha=="sarkol,/","sarkole",
  ifelse(d$V4cha=="other",NA,
  ifelse(d$V4cha=="outra",NA,
  ifelse(d$V4cha=="other zambian",NA,
  ifelse(d$V4cha=="other non-gh.tribes",NA,
  ifelse(d$V4cha=="other ghanaian tr.",NA,
  ifelse(d$V4cha=="other african",NA,
  ifelse(d$V4cha=="other (senegalese)",NA,
  ifelse(d$V4cha=="other (non-senegalese)",NA,
  ifelse(d$V4cha=="soninké","soninke",
  ifelse(d$V4cha=="sarakol‚/sonink‚/marka","sarakole/soninke/marka",
  ifelse(d$V4cha=="s‚noufo/minianka","senoufo/minianka",
  ifelse(d$V4cha=="not a senegalese", NA,
  ifelse(d$V4cha=="xitsonga & simili","xitsonga",
  d$V4cha))))))))))))))))))                 


# Get rates by country
exy <- data.frame(NULL)
dat <- NULL

for(i in 1:length(unique(d$country))){
  exy[i,1] <- unique(d$country)[i]
  dat <- subset(d,d$country==unique(d$country)[i]&!is.na(d$V3)&!is.na(d$V4))
  exy[i,2] <- sum(dat$V3cha!=dat$V4cha,na.rm=T)/nrow(dat)
  exy[i,3] <- sum(dat$V3clean!=dat$V4clean,na.rm=T)/nrow(dat)
}
names(exy) <- c("Country","Original Rate", "Clean Rate")

exyC <- exy

# Get rates by round

exy <- data.frame(NULL)
dat <- NULL

for(i in 1:length(unique(d$V1))){
  exy[i,1] <- unique(d$V1)[i]
  dat <- subset(d,d$V1==unique(d$V1)[i]&!is.na(d$V3)&!is.na(d$V4))
  exy[i,2] <- sum(dat$V3cha!=dat$V4cha,na.rm=T)/nrow(dat)
  exy[i,3] <- sum(dat$V3clean!=dat$V4clean,na.rm=T)/nrow(dat)
}
names(exy) <- c("Round","Original Rate", "Clean Rate")

exyR <- exy

write.csv(exyC,"C:/Users/ajv/Desktop/Exogamy Rates by Country.csv")
write.csv(exyR,"C:/Users/ajv/Desktop/Exogamy Rates by Round.csv")
