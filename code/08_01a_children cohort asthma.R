##########################################################
# Name of file: 08_01a_children.R
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 06 August 2020
# Latest update author (if not using version control) - Jiafeng Pan jiafeng.pan@phs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: reads in the cohort and merges in the Q Covid risk groups; set up asthma severity marker; produce tables
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop
project_path <- paste0(Location,"EAVE/GPanalysis/progs/JP/covid hosp in children")
setwd(project_path)




EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/Cohort_Demog_Endpoints_Times2021-07-28.rds"))
EAVE_cohort <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))

table(EAVE_cohort$death_covid, is.na(EAVE_cohort$NRS.Date.Death), exclude=NULL)
table(EAVE_cohort$icu_death, is.na(EAVE_cohort$date_icu_death), exclude=NULL)
table(EAVE_cohort$hosp_covid, is.na(EAVE_cohort$date_hosp_covid), exclude=NULL)

#z <- filter(EAVE_cohort, (icu_death==1 & is.na(date_icu_death)) )
#correct errors
EAVE_cohort <- EAVE_cohort %>% 
  mutate(death_covid = if_else(death_covid==1 & is.na(NRS.Date.Death), 0,death_covid)) %>% 
  mutate(icu_death = if_else((icu_death==1) & is.na(date_icu_death), 0, icu_death))

a_begin <- as.Date("2020-03-01")
#remove all who have died before the beginning
EAVE_cohort <- filter(EAVE_cohort, is.na(NRS.Date.Death) | (!is.na(NRS.Date.Death) & NRS.Date.Death > a_begin))

EAVE_Weights <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/CR_Cohort_Weights.rds"))
EAVE_cohort  <- EAVE_cohort %>% left_join(EAVE_Weights, by="EAVE_LINKNO")
EAVE_cohort$eave_weight[is.na(EAVE_cohort$eave_weight)] <- mean(EAVE_cohort$eave_weight, na.rm=T)

#adjust inconsistencies in the endpoints and times - all hosp have an admission date
z_max_date_death <- max(EAVE_cohort$NRS.Date.Death, na.rm=T)
z_max_date_icu <- max(EAVE_cohort$date_icu_death, na.rm=T)
#EAVE_cohort <- EAVE_cohort %>% mutate(NRS.Date.Death = case_when(death_covid==1 & is.na(NRS.Date.Death) ~ SpecimenDate + 21,
#                                                       TRUE ~ NRS.Date.Death),
#                            date_icu_death = case_when(icu_death==1 & is.na(date_icu_death) ~ SpecimenDate + 14,
#                                                       TRUE ~ date_icu_death ) ) %>% 
#  mutate(NRS.Date.Death = case_when(NRS.Date.Death > z_max_date_death  ~ z_max_date_death,
#                                    TRUE ~ NRS.Date.Death),
#         date_icu_death = case_when(date_icu_death > z_max_date_icu ~ z_max_date_icu,
#                                    TRUE ~ date_icu_death ) )
EAVE_cohort <- EAVE_cohort %>% mutate(death_covid = case_when(death_covid==1 & is.na(NRS.Date.Death) ~ 0,
                                                              TRUE ~ death_covid),
                                      icu_death = case_when(icu_death==1 & is.na(date_icu_death) ~ 0,
                                                            TRUE ~ icu_death ) )

#z <- readRDS(paste0(Location,"EAVE/GPanalysis/data/ECOSS_deduped_linked.rds"))

#rg only has data on individuals who have at least one condition
rg <- readRDS( paste0(Location,"EAVE/GPanalysis/data/temp_allQcovid.rds"))
rg <- filter(rg,!duplicated(EAVE_LINKNO))

#Get the children Only
#df is the main analysis data frame
df <- EAVE_cohort %>% filter(ageYear >= 5 & ageYear < 18)


#individuals with no values in rg have no risk conditions
z <- df %>% left_join(rg, by="EAVE_LINKNO")
z <- z %>% mutate_at(vars(Q_DIAG_AF:Q_DIAG_CKD_LEVEL), ~replace(., is.na(.), 0))
z <- z %>% mutate_at(vars(Q_DIAG_AF:Q_DIAG_CKD_LEVEL), ~as.numeric(.))

df <- z

#get the names of all the Q covid risk groups
z_vars <- names(df)[grepl("^Q_", names(df))]

#rename the risk groups
name.map <- read.csv("condition_name_map.csv")
table(names(df)[names(df)%in%z_vars] == as.character(name.map$names))#all true
names(df)[names(df)%in%z_vars] <- as.character(name.map$full_name)
z_vars <- as.character(name.map$full_name)
#remove the risk groups - either not relevent to children or have data quality issues
z_vars_use <- z_vars[z_vars %in% c("BMI", "coronary heart disease", "COPD", 
"dementia","parkinson's disease","ethnicity", "accomodation")==FALSE]
zz <- z_vars_use[z_vars_use != "learning disability"]
#change the levels in z_vars_use - 0 is no 1 as yes
#for chronic kidney disease combine 3-5 to 1
zz <- z_vars_use[z_vars_use != "learning disability"]
df$'chronic kidney disease' <- ifelse(df$'chronic kidney disease'%in%c(3,4,5),1,df$'chronic kidney disease')
table(df$'chronic kidney disease')
my.fun <- function(x){as.factor(ifelse(x==1,"Yes","No"))}
z <- df %>% mutate_at(vars(zz), funs(my.fun))
#no learning disability, 1 means learning disability apart from Down’s syndrome and 2 means Down’s syndrome
z$`learning disability` <- factor(z$`learning disability`,level=0:2,
                                  labels=c("No","Yes - not Downs syndrome", "Yes - Downs syndrome"))
table(z$`learning disability`)
df <- z



#find the number with a value in each risk group
#z <- df %>% dplyr::select_at(z_vars)
#z <- z %>% pivot_longer(cols=everything())

#z.df <- z %>% group_by(name) %>% 
#  dplyr::summarise(N=sum(value>0))

#find the risk groups with at least 500 children
#drop bmi and ethnicity as they are not reliable
#z_vars_use <- z.df %>% filter(N>=500) %>% pull(name)
#z_vars_use <- z_vars_use[z_vars_use != "Q_BMI"]
#z_vars_use <- z_vars_use[z_vars_use != "Q_ETHNICITY"]




#link in hospital history at baseline
z <- readRDS(paste0(Location,"EAVE/GPanalysis/data/smr01_JP_2021-08-26.rds"))
hh.df <- z
#z <- subset(z, substr(z$MAIN_CONDITION,1,3)%in% c("J45","J46")==FALSE)
#z <- subset(z, ADMISSION_DATE >= as.Date("2018-03-01")&ADMISSION_DATE <as.Date("2020-03-01"))
#z.agg <- aggregate(z$CIS_MARKER, list(z$EAVE_LINKNO), function(x)length(unique(x)))#slow
#names(z.agg) <- c("EAVE_LINKNO","no_hosp_2yr")
#save the hosp history out
#saveRDS(z.agg, "hosp_hist_noasthma.rds")
z.agg <- readRDS("hosp_hist_noasthma.rds")

df <- left_join(df, z.agg)
summary(df$no_hosp_2yr)#majority have 1
df$no_hosp_2yr <- ifelse(is.na(df$no_hosp_2yr), 0, df$no_hosp_2yr)
df$no_hosp_2yrgp <- as.factor(ifelse(df$no_hosp_2yr>=1, "1+", 0))

df.org <- df

#link in the discharge date for the covid hosp
df <- df.org
z<- subset(hh.df, ADMISSION_DATE >= as.Date("2020-03-01"))
z$ADMISSION_DATE <- as.Date(substr(z$ADMISSION_DATE,1,10))
z$DISCHARGE_DATE <- as.Date(substr(z$DISCHARGE_DATE,1,10))
z.agg <- z %>% group_by( EAVE_LINKNO,CIS_MARKER) %>% 
  dplyr::summarise(doa = min(ADMISSION_DATE), dodis= max(DISCHARGE_DATE))

df <- left_join(df,z.agg[c("EAVE_LINKNO","doa","dodis")], by = c("date_hosp_covid" = "doa", "EAVE_LINKNO"="EAVE_LINKNO"))
df <- subset(df, !duplicated(EAVE_LINKNO))
table(is.na(subset(df, !is.na(date_hosp_covid))$dodis))#jumberdont have discharge following covid hosp
summary(subset(df, !is.na(date_hosp_covid) & is.na(dodis))$date_hosp_covid)  

z <- readRDS(paste0(Location,"EAVE/GPanalysis/data/covid_hospitalisations.rds"))
names(z)[2] <- "dodis2"
df <- left_join(df,z[c("EAVE_LINKNO","admission_date","dodis2")], by = c("date_hosp_covid" = "admission_date", "EAVE_LINKNO"="EAVE_LINKNO"))
table(is.na(subset(df, !is.na(date_hosp_covid))$dodis2))#number dont have discharge following covid hosp

z <- readRDS(paste0(Location,"EAVE/GPanalysis/data/any_hospitalisation_post_01022020.rds"))
names(z)[2] <- "dodis3"
df <- left_join(df,z[c("EAVE_LINKNO","admission_date","dodis3")], by = c("date_hosp_covid" = "admission_date", "EAVE_LINKNO"="EAVE_LINKNO"))
table(is.na(subset(df, !is.na(date_hosp_covid))$dodis3))#number dont have discharge following covid hosp

df$dodis4 <- ifelse(is.na(df$dodis), as.character(df$dodis2), as.character(df$dodis))
df$dodis4 <- ifelse(is.na(df$dodis4), as.character(df$dodis3), as.character(df$dodis4))
table(is.na(subset(df, !is.na(date_hosp_covid))$dodis4))#number unknown
df$dodis4 <- as.Date(df$dodis4)
summary(df$dodis4)


df.org <- df


#get the numbers and rate per 100,000 for all the response variables
#don't use icu_death and covid_death as too few observations
#z_resp_vars <- c("tested","result","hosp_covid","icu_death","death_covid")
z_resp_vars <- c("tested","result","hosp_covid")


z <- df %>% dplyr::select_at(c(z_vars_use, z_resp_vars, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars), ~ sum(.))) 
z1 <- z.df %>% ungroup() %>%  mutate(across(all_of(z_resp_vars), ~round(./N*100000,1), .names="rate_{col}"))
z1 <- z.df %>%  mutate(rate_tested = round(tested/N*100000,1),
                       rate_positive = round(result/N*100000,1),
                       rate_hosp_covid = round(hosp_covid/N*100000,1) ) %>% ungroup() %>% as.data.frame()

z1$agegp <- "5-17 years old"
z.r <- z1

z_vars_use_a <- subset(z1, value!="No" & hosp_covid>=5)$name


df <- subset(df.org, ageYear>=5&ageYear<=11)
z <- df %>% dplyr::select_at(c(z_vars_use, z_resp_vars, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars), ~ sum(.))) 
z1 <- z.df %>% ungroup() %>%  mutate(across(all_of(z_resp_vars), ~round(./N*100000,1), .names="rate_{col}"))
z1 <- z.df %>%  mutate(rate_tested = round(tested/N*100000,1),
                       rate_positive = round(result/N*100000,1),
                       rate_hosp_covid = round(hosp_covid/N*100000,1) ) %>% ungroup() %>% as.data.frame()
z1$agegp <- "5-11 years old"
z.r <- rbind(z.r,z1)


df <- subset(df.org, ageYear>=12&ageYear<=17)
z1$agegp <- "5-17 years old"
df <- subset(df.org, ageYear>=5&ageYear<=11)
z <- df %>% dplyr::select_at(c(z_vars_use, z_resp_vars, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars), ~ sum(.))) 
z1 <- z.df %>% ungroup() %>%  mutate(across(all_of(z_resp_vars), ~round(./N*100000,1), .names="rate_{col}"))
z1 <- z.df %>%  mutate(rate_tested = round(tested/N*100000,1),
                       rate_positive = round(result/N*100000,1),
                       rate_hosp_covid = round(hosp_covid/N*100000,1) ) %>% ungroup() %>% as.data.frame()

z1$agegp <- "12-17 years old"
z.r <- rbind(z.r,z1)


write.csv(z.r, "table1.csv")

table(df$ageYear)
nrow(df)#764195
table(df$result)#44570
table(df$result)/nrow(df)*100#5.83%
table(df$hosp_covid)#449


##asthma severity marker

smr01_asthma <- readRDS(paste0(Location,"EAVE/GPanalysis/data/smr01_child_asthma_admits.rds"))
z <- as.data.frame(smr01_asthma)
z$admit_date <- as.Date(substr(z$admit_date,1,10))
z$dis_date <- as.Date(substr(z$dis_date,1,10))
#table(z$main_diag_admit)
#all children with prior asthma hosp
zs <- subset(z,admit_date>=as.Date("2020-03-01")-365.25*2 & admit_date<as.Date("2020-03-01"))
#zs <- subset(z,admit_date>=as.Date("2019-09-01"))
zs <- subset(zs, EAVE_LINKNO %in% df.org$EAVE_LINKNO)
z.agg <- zs %>% group_by(EAVE_LINKNO) %>% 
  dplyr::summarise(admit_date_2yr=max(admit_date))
zz <- left_join(df.org, z.agg)
dim(zz)

zz$asthma_hosp_2yr <- ifelse(is.na(zz$admit_date_2yr)==FALSE, "prior hosp 2yr", as.character(zz$asthma))
table(zz$asthma_hosp_2yr)
zz$asthma_hosp_2yr <- factor(zz$asthma_hosp_2yr, level=c("No", "Yes", "prior hosp 2yr"))
levels(zz$asthma_hosp_2yr)[1:2] <- c("no asthma", "mild asthma")
df.org2 <- zz



zs <- subset(z,admit_date>=as.Date("2020-03-01")-365.25*1 & admit_date<as.Date("2020-03-01"))
zs <- subset(zs, EAVE_LINKNO %in% df.org$EAVE_LINKNO)
z.agg <- zs %>% group_by(EAVE_LINKNO) %>% 
  dplyr::summarise(admit_date_1yr=max(admit_date))
zz <- left_join(df.org2, z.agg)
dim(zz)
dim(z.agg)

zz$asthma_hosp_1yr <- ifelse(is.na(zz$admit_date_1yr)==FALSE, "prior hosp 1yr", as.character(zz$asthma))
table(zz$asthma_hosp_1yr)
zz$asthma_hosp_1yr <- factor(zz$asthma_hosp_1yr, level=c("No", "Yes", "prior hosp 1yr"))
levels(zz$asthma_hosp_1yr)[1:2] <- c("no asthma", "mild asthma")
df.org2 <- zz


z <- readRDS(paste0(Location,"EAVE/GPanalysis/data/child_asthma_PIS/PIS_update_20210813.rds"))
z <- as.data.frame(z)
dim(z)#145449
table(z$`PI Approved Name`)
z$`Presc Date` <- as.Date(z$`Presc Date`, format="%m/%d/%y")#
summary(z$`Presc Date`)#
z <- subset(z, `PI Approved Name` %in% c("PREDNISOLONE"))#
zs <-subset(z,`Presc Date`>=as.Date("2020-03-01")-365.25*1 & `Presc Date`<as.Date("2020-03-01"))
z.agg <- zs %>% group_by(EAVE_LINKNO) %>% 
  dplyr::summarise( no_pres_1yr=length(EAVE_LINKNO))
table(z.agg$no_pres_1yr)

zz <- left_join(df.org2, z.agg)
dim(zz)
table(zz$no_pres_1yr)

zz$no_pres_1yrgp1 <- ifelse(zz$no_pres_1yr>=3,"3+",zz$no_pres_1yr)
#zz$no_pres_1yrgp2 <- ifelse(zz$no_pres_1yr>=2,"2+",zz$no_pres_1yr)

zz$asthma_pres_1yrgp1 <- ifelse(is.na(zz$no_pres_1yrgp1)==FALSE, zz$no_pres_1yrgp1, as.character(zz$asthma))
zz$asthma_pres_1yrgp1 <- factor(zz$asthma_pres_1yrgp1, level=c("No", "Yes", "1" ,"2" , "3+" ))
levels(zz$asthma_pres_1yrgp1)[1:2] <- c("no asthma", "0")
table(zz$asthma_pres_1yrgp1)


#zz$asthma_pres_1yrgp2 <- ifelse(is.na(zz$no_pres_1yrgp2)==FALSE, zz$no_pres_1yrgp2, as.character(zz$asthma))
#zz$asthma_pres_1yrgp2<- factor(zz$asthma_pres_1yrgp2, level=c("No", "Yes", "1" ,"2+"  ))
#levels(zz$asthma_pres_1yrgp2)[1:3] <- c("no asthma", "mild asthma", "mild asthma")
#table(zz$asthma_pres_1yrgp2)

#zz$asthma_pres_1yrgp3 <- ifelse(zz$asthma_pres_1yrgp2=="2+"|zz$asthma_hosp_1yr=="prior hosp 1yr", "2+ or prior hosp 1yr",as.character(zz$asthma_pres_1yrgp2))
#zz$asthma_pres_1yrgp3 <- factor(zz$asthma_pres_1yrgp3, level=c("no asthma", "mild asthma", "2+ or prior hosp 1yr"  ))
#table(zz$asthma_pres_1yrgp3)
df.org2 <- zz


zs <-subset(z,`Presc Date`>=as.Date("2020-03-01")-365.25*2 & `Presc Date`<as.Date("2020-03-01"))
#zs <-subset(z,`Presc Date`>=as.Date("2019-09-01"))
z.agg <- zs %>% group_by(EAVE_LINKNO) %>% 
  dplyr::summarise( no_pres_2yr=length(EAVE_LINKNO))
table(z.agg$no_pres_2yr)

zz <- left_join(df.org2, z.agg)
dim(zz)
table(zz$no_pres_2yr)

zz$no_pres_2yrgp1 <- ifelse(zz$no_pres_2yr>=3,"3+",zz$no_pres_2yr)
#zz$no_pres_2yrgp2 <- ifelse(zz$no_pres_2yr>=2,"2+",zz$no_pres_2yr)

zz$asthma_pres_2yrgp1 <- ifelse(is.na(zz$no_pres_2yrgp1)==FALSE, zz$no_pres_2yrgp1, as.character(zz$asthma))
zz$asthma_pres_2yrgp1 <- factor(zz$asthma_pres_2yrgp1, level=c("No", "Yes", "1" ,"2" , "3+" ))
levels(zz$asthma_pres_2yrgp1)[1:2] <- c("no asthma", "0")
table(zz$asthma_pres_2yrgp1)


#zz$asthma_pres_2yrgp2 <- ifelse(is.na(zz$no_pres_2yrgp2)==FALSE, zz$no_pres_2yrgp2, as.character(zz$asthma))
#zz$asthma_pres_2yrgp2<- factor(zz$asthma_pres_2yrgp2, level=c("No", "Yes", "1" ,"2+"  ))
#levels(zz$asthma_pres_2yrgp2)[1:3] <- c("no asthma", "mild asthma", "mild asthma")
#table(zz$asthma_pres_2yrgp2)

#zz$asthma_pres_2yrgp3 <- ifelse(zz$asthma_pres_2yrgp2=="2+"|zz$asthma_hosp_2yr=="prior hosp 2yr", "2+ or prior hosp 2yr",as.character(zz$asthma_pres_2yrgp2))
#zz$asthma_pres_2yrgp3 <- factor(zz$asthma_pres_2yrgp3, level=c("no asthma", "mild asthma", "2+ or prior hosp 2yr"  ))
#table(zz$asthma_pres_2yrgp3)
df.org2 <- zz



z_resp_vars <- c("tested","result","hosp_covid")
df <- subset(df.org2, ageYear>=12 & ageYear<=17)
#df <- subset(df.org2, ageYear>=12 & ageYear<=15)
#df <- df.org2

z_vars_use2 <- c("asthma","asthma_hosp_1yr","asthma_hosp_2yr", "asthma_pres_1yrgp1", "asthma_pres_1yrgp2", 
                 "asthma_pres_1yrgp3", "asthma_pres_2yrgp1", "asthma_pres_2yrgp2", 
                 "asthma_pres_2yrgp3")
#z_vars_use2 <- c("asthma_hosp_2yr",  
#                 "asthma_pres_2yrgp1", "asthma_pres_2yrgp2", 
#                 "asthma_pres_2yrgp3")
z <- df %>% dplyr::select_at(c(z_vars_use2, z_resp_vars, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use2))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars), ~ sum(.))) 
z1 <- z.df %>% ungroup() %>%  mutate(across(all_of(z_resp_vars), ~round(./N*100000,1), .names="rate_{col}"))
z1 <- z.df %>%  mutate(rate_tested = round(tested/N*100000,1),
                       rate_positive = round(result/N*100000,1),
                       rate_hosp_covid = round(hosp_covid/N*100000,1) ) %>% ungroup() %>% as.data.frame()

z1

#demog table
z <- df.org2
z$agegp <- cut(z$ageYear, breaks=c(0,11,200), labels=c("5-11 years old","12-17 years old"))
#table(z$agegp,exclude=NULL)
z$Sex <- as.factor(z$Sex)
#table(z$agegp,exclude=NULL)
#table(z$Sex,exclude=NULL)


z$asthma_yes <- ifelse(z$asthma=="Yes", z$eave_weight,0)
#table(z$asthma_yes,exclude=NULL)
z$asthma_hosp_2yr_no <- ifelse(z$asthma_hosp_2yr=="mild asthma",z$eave_weight,0)
z$asthma_hosp_2yr <- ifelse(z$asthma_hosp_2yr=="prior hosp 2yr",z$eave_weight,0)
z$asthma_pres_2yr0 <- ifelse(z$asthma_pres_2yrgp1=="0",z$eave_weight,0)
z$asthma_pres_2yr1 <- ifelse(z$asthma_pres_2yrgp1=="1",z$eave_weight,0)
z$asthma_pres_2yr2 <- ifelse(z$asthma_pres_2yrgp1=="2",z$eave_weight,0)
z$asthma_pres_2yrover3 <- ifelse(z$asthma_pres_2yrgp1=="3+",z$eave_weight,0)

df <- z

zz <- z_vars_use
my.fun <- function(x){as.factor(x)}
df <- df %>% mutate_at(vars(zz), my.fun)

z_resp_vars2 <- c("asthma_yes","asthma_hosp_2yr_no","asthma_hosp_2yr",
                  "asthma_pres_2yr0","asthma_pres_2yr1","asthma_pres_2yr2","asthma_pres_2yrover3")
z_vars_use2 <- c("agegp", "Sex", "simd2020_sc_quintile", "no_hosp_2yrgp")
z <- df %>% dplyr::select_at(c(z_vars_use2, z_resp_vars2, "eave_weight"))
z1 <- z %>%  dplyr::summarise(N = round(sum(eave_weight)),
                              across(all_of(z_resp_vars2), ~ round(sum(.))) )
z1 <- cbind(data.frame(name="Total", value="Total"),z1)
z <- z %>% pivot_longer(cols=all_of(z_vars_use2))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars2), ~ round(sum(.))) )
z1 <- rbind.data.frame(z1,z.df)

z_vars_use2<-z_vars_use_a[z_vars_use_a!="asthma"]
z <- df %>% dplyr::select_at(c(z_vars_use2, z_resp_vars2, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use2))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars2), ~ round(sum(.))) )
z1 <- rbind.data.frame(z1,z.df)

zz <- c("N", z_resp_vars2)
my.fun <- function(x){round(x/x[1]*100,1)}
z2 <- z1 %>% mutate_at(vars(zz), my.fun)
for(i in zz){
  z1[,i] <- paste( z1[,i], " (", z2[,i],")", sep="")
}



write.csv(z1,"demog.asthma.children.csv")








