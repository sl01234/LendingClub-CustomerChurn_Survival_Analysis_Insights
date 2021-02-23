# Import Libraries
library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
install.packages('cmprsk', dependencies=T) # install Competing Risk package
library(cmprsk) # Competing Risk library, 

setwd('~/Desktop/NYCDSA/LENDINGCLUB_Proj_GITHUB')

# CSV files too large, so created Dropbox links to dataframes
# Read this in for 36 month term
df_accepted4a_36m_no_0_cox <- read.csv('https://www.dropbox.com/s/m8pnpz5anojf3tb/df_accepted4a_36m_no_0_cox.csv?dl=1')
# Read this in for 60 month term
df_accepted4a_60m_no_0_cox <- read.csv('https://www.dropbox.com/s/iewlcsolsni81ui/df_accepted4a_60m_no_0_cox.csv?dl=1')

## SURVIVAL ANALYSIS: COMPETING RISKS --> 

# Choice of methodology: We decided to do survival analysis in order to predict the time to default or prepayment.  
# This type of analysis is commonly used in business analytics when assessing customer churn, epidemiology when predicting 
# time-to-relapse or death, and performance optimization for predicting time-to-equipment failure.  

# Whereas the more common method when doing survival analysis is to use either Kaplan-Meier analysis (KM) or Cox-Proportional 
# analysis (for time-to-event for a single event of interest), this would conflate default and prepayment into one event group 
# when there are actually two. Since we are coding for two competing events (prepayment or default) where the occurrence of one 
# event competes with the other, we will instead use competing risk analysis, where one set of curves (dotted) represent the 
# prepaid borrowers, and the second set of curves (solid) represent the default/delinquent group).  We subdivide the 
# analysis between 36 month term and 60 month term (second half of R script). 
# The library used is 'cmprsk'. 

####################################################
## 36 MONTH TERM ############
####################################################
# Competing Risks 1a: GRADE 36mnth 

# Output table 1a: GRADE 36m (probability estimates)
print(cif2_36m <- cuminc(ftime =
                             df_accepted4a_36m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_36m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_36m_no_0_cox$GRADE))

# Plot for Competing Risks 1a: GRADE 36m
plot(cif2_36m, 
     lty = c(1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 2, 2, 2, 2, 2, 2, 2),
     
     col = c("black", "red", "green", "pink", "yellow", "blue", "purple", 
             "black", "red", "green", "pink", "yellow", "blue", "purple"),
     main = "Competing Risks: Grade (36 mnth)", 
     
     xlab="Month",
     ylab="Probablility",
     xlim=c(0, 40),
     cex=0.7)


# Competing Risks 2a: FICO 36m 

# Output table 2a: FICO 36m (probability estimates)
print(cif3_36m <- cuminc(ftime = df_accepted4a_36m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_36m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_36m_no_0_cox$FICO_BINNED1))

# Plot for Competing Risks 2a: FICO 36m
plot(cif3_36m, 
     lty = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 0.5, 2, 2, 2, 2, 2),
     col = c("green", "red", "blue", "pink", "brown", 
             "green", "red", "blue", "pink", "brown"),
     main = "Competing Risks: FICO (36mnth)", 
     xlim=c(0, 40),
     xlab="Month",
     ylab="Probablility",
     cex=0.7)


# Competing Risks 3a: Bankcard Utilization 36m
print(cif4_36m <- cuminc(ftime = df_accepted4a_36m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_36m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_36m_no_0_cox$BC_UTIL1))

plot(cif4_36m, 
     lty = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 0.5, 2, 2, 2, 2, 2),
     col = c("green", "red", "blue", "pink", "brown", 
             "green", "red", "blue", "pink", "brown"),
     main = "Competing Risks: Bankcard Utilization (36mnth)", 
     xlim=c(0, 40),
     xlab="Month",
     ylab="Probablility",
     cex=0.7)

# Competing Risks 4a: Home Ownership 36m
print(cif5_36m <- cuminc(ftime = df_accepted4a_36m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_36m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_36m_no_0_cox$HOME_OWNERSHIP_OWN_IS_0))

plot(cif5_36m, 
     lty = c(1, 1, 1, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 2, 2, 2),
     col = c("green", "red", "blue", 
             "green", "red", "blue"),
     main = "Competing Risks: Home Ownership (36mnth: 0 is own, 1 is mortgage, 2 is rent)", 
     xlim=c(0, 40),
     xlab="Month",
     ylab="Probablility",
     cex=0.7)


# Competing Risks 5a: Employment Length 36m
print(cif6_36m <- cuminc(ftime = df_accepted4a_36m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_36m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_36m_no_0_cox$EMP_LENGTH_BINNED1))

plot(cif6_36m, 
     lty = c(1, 1, 1, 1, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 2.5, 2.5, 2.5, 2.5),
     col = c("green", "red", "blue", "orange", 
             "green", "red", "blue", "orange"),
     main = "Competing Risks: Employment Length (36mnth)", 
     xlim=c(0, 40),
     xlab="Month",
     ylab="Probablility",
     cex=0.7)


# Competing Risks 6a: Interest Rate 36m
print(cif7_36m <- cuminc(ftime = df_accepted4a_36m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_36m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_36m_no_0_cox$INT_RATE_BINNED1))

plot(cif7_36m, 
     lty = c(1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5),
     col = c("brown", "red", "blue", "orange", "yellow", "purple",  
             "brown", "red", "blue", "orange", "yellow", "purple"),
     main = "Competing Risks: Interest Rate (36mnth)", 
     xlim=c(0, 40),
     xlab="Month",
     ylab="Probablility",
     cex=0.7)

##################################################################
## 60 Month Term #######
##################################################################


# Competing Risks 1b: GRADE 60m

# Output table 1b: GRADE 60m (probability estimates)
print(cif2_60m <- cuminc(ftime = df_accepted4a_60m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_60m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_60m_no_0_cox$GRADE))

# Plot for Competing Risks 1b: GRADE 60m
plot(cif2_60m, 
     lty = c(1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 2, 2, 2, 2, 2, 2, 2),
     
     col = c("black", "red", "green", "pink", "yellow", "blue", "purple", 
             "black", "red", "green", "pink", "yellow", "blue", "purple"),
     main = "Competing Risks: Grade (60 mnth)", 
     xlab="Month",
     ylab="Probablility",
     cex=0.7)


## Competing Risk 2b: FICO 60m
print(cif3_60m <- cuminc(ftime = df_accepted4a_60m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_60m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_60m_no_0_cox$FICO_BINNED1))

plot(cif3_60m, 
     lty = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 0.5, 2, 2, 2, 2, 2),
     col = c("green", "red", "blue", "pink", "brown", 
             "green", "red", "blue", "pink", "brown"),
     main = "Competing Risks: FICO (60mnth)", 
     xlab="Month",
     ylab="Probablility",
     cex=0.7)


# Competing Risks 3b: Bankcard Utilization 60m

print(cif4_60m <- cuminc(ftime = df_accepted4a_60m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_60m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_60m_no_0_cox$BC_UTIL1))

plot(cif4_60m,     
     lty = c(1, 1, 1, 1, 1, 3, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 0.5, 2, 2, 2, 2, 2),
     col = c("green", "red", "blue", "pink", "brown", 
             "green", "red", "blue", "pink", "brown"),
     main = "Competing Risks: Bankcard Utilization (60mnth)", 
     xlab="Month",
     ylab="Probablility",
     cex=0.7)


# Competing Risks 4b: Home Ownership 60m

print(cif5_60m <- cuminc(ftime = df_accepted4a_60m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_60m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_60m_no_0_cox$HOME_OWNERSHIP_OWN_IS_0))

plot(cif5_60m, 
     lty = c(1, 1, 1, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 2, 2, 2),
     col = c("green", "red", "blue", 
             "green", "red", "blue"),
     main = "Competing Risks: Home Ownership (60mnth: 0 is own, 1 is mortgage, 2 is rent)", 
     
     xlab="Month",
     ylab="Probablility",
     cex=0.7)


# Competing Risks 5b: Employment Length 60m
print(cif6_60m <- cuminc(ftime = df_accepted4a_60m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_60m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_60m_no_0_cox$EMP_LENGTH_BINNED1))

plot(cif6_60m, 
     lty = c(1, 1, 1, 1, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 2.5, 2.5, 2.5, 2.5),
     col = c("green", "red", "blue", "orange", 
             "green", "red", "blue", "orange"),
     main = "Competing Risks: Employment Length (60mnth)", 
     xlab="Month",
     ylab="Probablility",
     cex=0.7)


# Competing Risks 6b: Interest Rate
print(cif7_60m <- cuminc(ftime = df_accepted4a_60m_no_0_cox$surv1_timedelta_mnth, 
                         fstatus = df_accepted4a_60m_no_0_cox$loan_status_bool2,
                         group = df_accepted4a_60m_no_0_cox$INT_RATE_BINNED1))

plot(cif7_60m, 
     lty = c(1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 3),
     lwd = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5),
     col = c("black", "red", "blue", "orange", "yellow", "purple",  
             "black", "red", "blue", "orange", "yellow", "purple"),
     main = "Competing Risks: Interest Rate (60mnth)", 
     xlab="Month",
     ylab="Probablility",
     cex=0.5)
