##########################################################
# Name of file: 08_01c_children_asthma_Modelling.R
# Data release (if applicable):
# Original author(s): Chris Robertson chrisobertson@nhs.net
# Original date: 30 August 2020
# Latest update author (if not using version control) - Jiafeng Pan jiafeng.pan@phs.scot
# Latest update date (if not using version control) - 
# Latest update description (if not using version control)
# Type of script: Descriptive stats
# Written/run onL: R Studio SERVER
# Version of R that the script was most recently run on: R 3.6.1
# Description of content: Run 08_01a_children cohort asthma.R to get the cohort  
#                         multivriate cox model
# Approximate run time: Unknown
##########################################################

# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
library(survminer)
#Load data
#use df.org2 from 08_01a_children_cohort_asthma.r
Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop

fun.cox <- function(z.var,z) {
  #z is a cox model fitted
  z.coefs <- summary(z)$coefficients
  z.est <- z.coefs[,"coef"]
  z.se <- z.coefs[,"se(coef)"]
  z.p <- z.coefs[,"p"]
  z.out <- cbind.data.frame(levels=dimnames(z.coefs)[[1]],HR=exp(z.est),LCL=exp(z.est-1.96*z.se),
                            UCL=exp(z.est+1.96*z.se),p=z.p,outcome=z.var)
  z.out$hrci <- paste(round(z.out$HR,2), " (", round(z.out$LCL,2), "-",round(z.out$UCL,2),")", sep="")
  
  return(z.out)
}

z.response.vars <- c("tested","result","hosp_covid")
z_vars_use_a3 <- chartr(" ", "_", z_vars_use_a)

#z_vars_use2 <- c("asthma","asthma_hosp_1yr","asthma_hosp_2yr", "asthma_pres_1yrgp1", "asthma_pres_1yrgp2", 
#                 "asthma_pres_1yrgp3", "asthma_pres_2yrgp1", "asthma_pres_2yrgp2", 
#                 "asthma_pres_2yrgp3")

#if (exists("z.out")) rm(z.out)
df <-df.org2
#subset analysis  compare severe asthma to mild asthma
#dont run the below 2 line for main analysis
levels(df$asthma_hosp_2yr)[1] <- NA
levels(df$asthma_pres_2yrgp1)[1] <- NA
###
df <- subset(df, ageYear>=5&ageYear<=11)
df <- subset(df, ageYear>=12&ageYear<=17)
#df <- subset(df, ageYear>=12&ageYear<=17)
names(df) <- chartr(" ","_",names(df))


z.rv <- "hosp_covid"
z.rv.time <- "Time.To.Hosp"

#z_vars_use_a2 <- z_vars_use_a[z_vars_use_a!="severe_mental_illness"]
#z_vars_use_a2 <- z_vars_use_a[z_vars_use_a!="blood_cancer"]
z_vars_use_a2 <- z_vars_use_a3[z_vars_use_a3!="asthma"]
z_vars_use_a2 <- c("asthma_hosp_2yr",z_vars_use_a2)
z_vars_use_a2 <- c("asthma_pres_2yrgp1",z_vars_use_a2)
z_vars_use_a2 <- c("asthma_hosp_1yr",z_vars_use_a2)
z_vars_use_a2 <- c("asthma_pres_1yrgp1",z_vars_use_a2)
#z_vars_use_a2 <- z_vars_use_a2[z_vars_use_a2!="severe_mental_illness"]
z_vars_use_a2 <- z_vars_use_a2[z_vars_use_a2!="blood_cancer"]
##cox model

z.fmla <- as.formula(paste("Surv(",z.rv.time,",",z.rv,") ~  pspline(ageYear) + Sex + simd2020_sc_quintile + no_hosp_2yrgp +",
                           paste(z_vars_use_a2, collapse= "+")))

z.fit <- coxph(z.fmla , data=df, weights = eave_weight)
z.r <- fun.cox(z.rv,z.fit)
z.r

#summary(z.fit)

#ptemp <- termplot(z.fit, se=TRUE, plot=FALSE)
#ageterm <- ptemp$ageYear  # this will be a data frame
#center <- with(ageterm, y[x==5])
#ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96),'*')
#matplot(ageterm$x, exp(ytemp - center), log='y',type='l', lty=c(1,2,2), col=1,
        #xlab="Age at Positive Test", ylab="Hospitalisation Hazard Ratio")
#abline(h=1, lty=2)






