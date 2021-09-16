##########################################################
# Name of file: subset analysis tested positive.R
# Data release (if applicable):
# Original author(s): Jiafeng Pan jiafeng.pan@phs.scot
# Original date: 30 August 2020
# Latest update author (if not using version control) - Jiafeng Pan jiafeng.pan@phs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: model the risk for those with severe asthma among those tested postive for covid
# Approximate run time: Unknown
##########################################################
#for asthma severity analysis
#only for thoes tested positive
#look back 2 years from the date of test
#cox model using date of test as starting point
#add a spline of time between the date of test and 01-03-2020


#use df.org from 08_01a_children_cohort_asthma.r

table(df.org$result,df.org$hosp_covid)#covid hosp with no previous positive test results
table(df.org$hosp_covid)#53/449
table(df.org$tested,df.org$hosp_covid)#covid hsop with no previous test
zz <- subset(df.org, hosp_covid==1 & result==0)[c("EAVE_LINKNO", "SpecimenDate","result",
                                          "tested","date_hosp_covid", "hosp_covid","DATE_OF_DEATH","death_covid")]
write.csv(zz, "covid hosp with no previous positive test.csv")
zz$diff <- as.numeric(zz$date_hosp_covid- zz$SpecimenDate)
summary(zz$diff)


zz <- subset(df.org, hosp_covid==1 & result==1)
zz$diff <- as.numeric(zz$date_hosp_covid- zz$SpecimenDate)
summary(zz$diff)


#For rhose with no positive test give them an impute specimen date 7 days before the date of admission
#Some people are in hospital before the day of the test and get a negative time
#reset the time to hospitalisation to zero – remove the ones that are very negative
zz <- subset(df.org, result==1 |hosp_covid==1)
table(zz$result)
zz$end <- ifelse(!is.na(zz$date_hosp_covid),as.character(zz$date_hosp_covid),"2021-07-26")#"2021-07-26" is the max specimendate
zz$end <- as.Date(zz$end)
zz$time.test.to.hosp <- as.numeric(zz$end-zz$SpecimenDate)
summary(zz$time.test.to.hosp)
zz[zz$result==0, "time.test.to.hosp"] <- 7
length(zz$time.test.to.hosp[zz$time.test.to.hosp< -28]) 
zz <- subset(zz, time.test.to.hosp>-28)
zz[zz$time.test.to.hosp <0,"time.test.to.hosp"] <- 0
dim(zz)#44601
zz[is.na(zz$SpecimenDate), "SpecimenDate"] <- zz[is.na(zz$SpecimenDate), "date_hosp_covid"]-7
subset(zz, EAVE_LINKNO %in% c("EAVE1225164", "EAVE3569078"))
df.org3 <- zz

smr01_asthma <- readRDS(paste0(Location,"EAVE/GPanalysis/data/smr01_child_asthma_admits.rds"))
z <- as.data.frame(smr01_asthma)
z$admit_date <- as.Date(substr(z$admit_date,1,10))
#z$dis_date <- as.Date(substr(z$dis_date,1,10))
#table(z$main_diag_admit)
#all children with prior asthma hosp
dim(z)
z <- merge(z, df.org3[,c("EAVE_LINKNO", "SpecimenDate")])
zs <- subset(z,admit_date>=SpecimenDate-365.25*2 & admit_date<SpecimenDate)
z.agg <- zs %>% group_by(EAVE_LINKNO) %>% 
  dplyr::summarise(admit_date_2yr=max(admit_date))
zz <- left_join(df.org3, z.agg)
dim(zz)

zz$asthma_hosp_2yr <- ifelse(is.na(zz$admit_date_2yr)==FALSE, "prior hosp 2yr", as.character(zz$asthma))
table(zz$asthma_hosp_2yr)
zz$asthma_hosp_2yr <- factor(zz$asthma_hosp_2yr, level=c("No", "Yes", "prior hosp 2yr"))
levels(zz$asthma_hosp_2yr)[1:2] <- c("no asthma", "mild asthma")
df.org3 <- zz




z <- readRDS("/conf/EAVE/GPanalysis/data/child_asthma_PIS/PIS_update_20210813.rds")
z <- as.data.frame(z)
dim(z)#145449
table(z$`PI Approved Name`)
z$`Presc Date` <- as.Date(z$`Presc Date`, format="%m/%d/%y")#
summary(z$`Presc Date`)#
z <- subset(z, `PI Approved Name` %in% c("PREDNISOLONE"))#
zs <-subset(z,`Presc Date`>=as.Date("2020-03-01")-365.25*2 & `Presc Date`<as.Date("2020-03-01"))
z.agg <- z %>% group_by(EAVE_LINKNO) %>% 
  dplyr::summarise( no_pres_2yr=length(EAVE_LINKNO))
table(z.agg$no_pres_2yr)

zz <- left_join(df.org3, z.agg)
dim(zz)
table(zz$no_pres)


zz$no_pres_2yrgp1 <- ifelse(zz$no_pres_2yr>=3,"3+",zz$no_pres_2yr)
#zz$no_pres_2yrgp2 <- ifelse(zz$no_pres_2yr>=2,"2+",zz$no_pres_2yr)

zz$asthma_pres_2yrgp1 <- ifelse(is.na(zz$no_pres_2yrgp1)==FALSE, zz$no_pres_2yrgp1, as.character(zz$asthma))
zz$asthma_pres_2yrgp1 <- factor(zz$asthma_pres_2yrgp1, level=c("No", "Yes", "1" ,"2" , "3+" ))
levels(zz$asthma_pres_2yrgp1)[1:2] <- c("no asthma", "0")
table(zz$asthma_pres_2yrgp1)

zz$time <- as.numeric(zz$SpecimenDate - as.Date("2020-03-01"))
summary(zz$time)
df.org3 <- zz

zz$time <- zz$SpecimenDate - as.Date("2020-03-01")

df <- df.org3

z_vars_use2 <- c("asthma","asthma_hosp_2yr", "asthma_pres_2yrgp1")
z_resp_vars2 <- c("result" ,"hosp_covid")
z <- df %>% dplyr::select_at(c(z_vars_use2, z_resp_vars2, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use2))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(across(all_of(z_resp_vars2), ~ sum(.))) 
z1 <- z.df %>%  mutate(rate_hosp_covid = round(hosp_covid/result*1000,1) ) %>% ungroup() %>% as.data.frame()

z1





#cox model

z_vars_use_a <- chartr(" ", "_", z_vars_use_a)



#if (exists("z.out")) rm(z.out)
df <-df.org3
#df <- subset(df.org2, ageYear>=5&ageYear<=11)
#df <- subset(df.org2, ageYear>=12&ageYear<=17)
#df <- subset(df, ageYear>=12&ageYear<=17)
names(df) <- chartr(" ","_",names(df))


z.rv <- "hosp_covid"
z.rv.time <- "Time.To.Hosp"
z_vars_use_a3 <- chartr(" ", "_", z_vars_use_a)

#z_vars_use_a2 <- z_vars_use_a[z_vars_use_a!="severe_mental_illness"]
#z_vars_use_a2 <- z_vars_use_a[z_vars_use_a!="blood_cancer"]
z_vars_use_a2 <- z_vars_use_a3[z_vars_use_a3!="asthma"]
z_vars_use_a2 <- c("asthma_hosp_2yr",z_vars_use_a2)
z_vars_use_a2 <- c("asthma_pres_2yrgp1",z_vars_use_a2)
#z_vars_use_a2 <- z_vars_use_a2[z_vars_use_a2!="severe_mental_illness"]
#z_vars_use_a2 <- z_vars_use_a2[z_vars_use_a2!="blood_cancer"]
##cox model
#z.fmla <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + Sex + simd2020_sc_quintile + ",
#                           paste(z_vars_use_a, collapse= "+")))

z.fmla <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + pspline(time)+ Sex + simd2020_sc_quintile + no_hosp_2yrgp+",
                           paste(z_vars_use_a2, collapse= "+")))

z.fit <- coxph(z.fmla , data=df, weights = eave_weight)
#summary(z.fit)
z.r <- fun.cox(z.rv,z.fit)
z.r
#ptemp <- termplot(z.fit, se=TRUE, plot=FALSE)
#ageterm <- ptemp$ageYear  # this will be a data frame
#center <- with(ageterm, y[x==5])
#ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96),'*')
#matplot(ageterm$x, exp(ytemp - center), log='y',type='l', lty=c(1,2,2), col=1,
#xlab="Age at Positive Test", ylab="Hospitalisation Hazard Ratio")
#abline(h=1, lty=2)


fun.cox <- function(z.var,z) {
  #z is a cox model fitted
  z.coefs <- summary(z)$coefficients
  z.est <- z.coefs[,"coef"]
  z.se <- z.coefs[,"se(coef)"]
  z.p <- z.coefs[,"p"]
  z.out <- cbind.data.frame(levels=dimnames(z.coefs)[[1]],HR=exp(z.est),LCL=exp(z.est-1.96*z.se),
                            UCL=exp(z.est+1.96*z.se),p=z.p,outcome=z.var)
  z.out$hrci <- paste(round(z.out$HR,2), "(", round(z.out$LCL,2), ",",round(z.out$UCL,2),")", sep="")
  
  return(z.out)
}












