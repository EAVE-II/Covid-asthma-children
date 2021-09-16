##########################################################
# Name of file: sensitivity analysis.R
# Data release (if applicable):
# Original author(s): Jiafeng Pan jiafeng.pan@phs.scot
# Original date: 30 August 2020
# Latest update author (if not using version control) - Jiafeng Pan jiafeng.pan@phs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: sensitivity analysis covid hosp with piror positive testy
#                          sensitivity analysis only count covid hosp with los>1
# Approximate run time: Unknown
##########################################################
#for asthma severity analysis
#only for thoes tested positive
#look back 2 years from the date of test
#cox model using date of test as starting point
#add a spline of time between the date of test and 01-03-2020


#use df.org2 from 08_01a_children_cohort_asthma.r

#only count covid hosp with prior postive test as event - those who dont will censor at the date of admission
#this is because we only count the first covid hosp if there is multiple so need to censor at they are no longer at risk
df <- df.org2

#zz <- df[which((df$hosp_covid==1 & df$result==0)|(df$hosp_covid==1 & df$result==1 & df$SpecimenDate>=df$date_hosp_covid)),]
#summary(as.numeric(zz$SpecimenDate-zz$date_hosp_covid))

df[which((df$hosp_covid==1 & df$result==0)|(df$hosp_covid==1 & df$result==1 & df$SpecimenDate>=df$date_hosp_covid)),"hosp_covid"] <- 0

#only count covid hosp with los>1 as event - those who dont will count as no event will censor at the date of admission

df$los.covid.hosp <- as.numeric(df$dodis4 - df$date_hosp_covid)
summary(df$los.covid.hosp)
#zz <- df[which(df$hosp_covid==1& (df$los.covid.hosp<=1|is.na(df$los.covid.hosp))),]
#summary(zz$los.covid.hosp)
df[which(df$hosp_covid==1& (df$los.covid.hosp<=1|is.na(df$los.covid.hosp))),"hosp_covid"] <- 0

#rate

z_resp_vars <- c("hosp_covid")

z_vars_use2 <- c("asthma","asthma_hosp_2yr",  "asthma_pres_2yrgp1")

z <- df %>% dplyr::select_at(c(z_vars_use2, z_resp_vars, "eave_weight"))
z <- z %>% pivot_longer(cols=all_of(z_vars_use2))
z.df <- z %>% group_by(name, value) %>% 
  dplyr::summarise(N = round(sum(eave_weight)),
                   across(all_of(z_resp_vars), ~ sum(.))) 
z1 <- z.df %>% ungroup() %>%  mutate(across(all_of(z_resp_vars), ~round(./N*100000,1), .names="rate_{col}"))
z1 <- z.df %>%  mutate( rate_hosp_covid = round(hosp_covid/N*100000,1) ) %>% ungroup() %>% as.data.frame()

z1


#cox model


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

z.fmla <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + Sex + simd2020_sc_quintile + no_hosp_2yrgp+",
                           paste(z_vars_use_a2, collapse= "+")))

#z.fit <- coxph(Surv(Time.To.Hosp, hosp_covid) ~ pspline(ageYear) + Sex + simd2020_sc_quintile + no_hosp_2yrgp+asthma_hosp_2yr , data=df, weights = eave_weight)
z.fit <- coxph(z.fmla , data=df, weights = eave_weight)
#summary(z.fit)
z.r <- fun.cox(z.rv,z.fit)
z.r



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












