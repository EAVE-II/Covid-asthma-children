##########################################################
# Name of file: estimate for population that needs vaccine.R
# Data release (if applicable):
# Original author(s): Jiafeng Pan jiafeng.pan@phs.scot
# Original date: 06 August 2020
# Latest update author (if not using version control) - jiafeng.pan@phs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: estimate population size that needs vaccine
# Approximate run time: Unknown
##########################################################
smr <- readRDS(paste0(Location,"EAVE/GPanalysis/data/smr01_JP_2021-08-26.rds"))
z <- as.data.frame(smr)
z$ADMISSION_DATE <- as.Date(substr(z$ADMISSION_DATE,1,10))
summary(z$ADMISSION_DATE)#2021-08-10
#z$dis_date <- as.Date(substr(z$dis_date,1,10))
#table(z$main_diag_admit)
#all children with prior asthma hosp
#select main condition J45 or J46 
z <- subset(z,ADMISSION_DATE>=as.Date("2019-09-01"))
z <- subset(z, substr(z$MAIN_CONDITION,1,3)%in% c("J45","J46"))
smr2 <- z


pis <-  readRDS("/conf/EAVE/GPanalysis/data/PIS_ASTHMA_2021-09-03.rds")
z <- pis
z$prescribed_full_date <- as.Date(z$prescribed_full_date, "%Y%m%d")
summary(z$prescribed_full_date)#2021-07-06
z <-subset(z, prescribed_full_date>=as.Date("2019-09-01"))
z.agg <- aggregate(z$no_items, list(z$EAVE_LINKNO),sum)
names(z.agg) <- c("EAVE_LINKNO","no_pres")
pis2 <- subset(z.agg, no_pres>=2)
z <- subset(z, approved_name=="PREDNISOLONE")
z.agg <- aggregate(z$no_items, list(z$EAVE_LINKNO),sum)
names(z.agg) <- c("EAVE_LINKNO","no_pres")
pis3 <- subset(z.agg, no_pres>=2)

z <- pis
z$prescribed_full_date <- as.Date(z$prescribed_full_date, "%Y%m%d")
z <-subset(z, prescribed_full_date>=as.Date("2018-03-01") &prescribed_full_date<as.Date("2020-03-01"))
#z <- subset(z, approved_name=="DEXAMETHASONE")
z <- subset(z, approved_name=="PREDNISOLONE")
#z <- subset(z, approved_name=="PREDNISONE")
z.agg <- aggregate(z$no_items, list(z$EAVE_LINKNO),sum)
names(z.agg) <- c("EAVE_LINKNO","no_pres")
z.agg <- subset(z.agg, EAVE_LINKNO%in% df$EAVE_LINKNO )
table(z.agg$no_pres)

#children 12-15 years
#using df.org created from children cohort asthma.r

df <- subset(df.org,ageYear>=12 & ageYear<=15)
df$hosp <- ifelse(df$EAVE_LINKNO %in% smr2$EAVE_LINKNO, 1, 0)
df$pres <- ifelse(df$EAVE_LINKNO %in% pis3$EAVE_LINKNO, 1, 0)
df$hosporpres <- ifelse(df$hosp==1 | df$pres==1, df$eave_weight, 0)
sum(df$hosporpres)

df$pres <- ifelse(df$EAVE_LINKNO %in% pis2$EAVE_LINKNO, 1, 0)
df$hosporpres <- ifelse(df$hosp==1 | df$pres==1, df$eave_weight, 0)
sum(df$hosporpres)



#adult
#using df.org created from adult asthma.r
#df$hosp <- ifelse(df$EAVE_LINKNO %in% smr2$EAVE_LINKNO, 1, 0)
#df$pres <- ifelse(df$EAVE_LINKNO %in% pis2$EAVE_LINKNO, 1, 0)
#df$hosporpres <- ifelse(df$hosp==1 | df$pres==1, df$eave_weight, 0)
#sum(df$hosporpres)



