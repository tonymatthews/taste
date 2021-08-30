
setwd("I:/EPI/Data/Anthony Matthews/taste/")

# Packages
library(tableone)
library(labelled)
library(haven)
library(survival)
library(ggplot2)
library(dplyr)
library(survminer)
library(gridExtra)
library(ggpubr)
library(geepack)
library(splitstackshape)
library(boot)
library(tidyr)
library(stringr)


# Time code starts
starttime <- Sys.time()

# Set outcomes
outcomes <- c("death", "mi")

# Bootstraps
numboot <- 200

## loop different analysis data sets

x <- c("sep07tojan16",
       "mar13tojan16",
       "sep07tojun10",
       "misens_sep07tojan16", 
       "nostenosiseligibility_sep07tojan16",
       "over65_sep07tojan16",
       "under65_sep07tojan16",
       "male_sep07tojan16",
       "female_sep07tojan16",
       "diabetes_sep07tojan16",
       "nodiabetes_sep07tojan16",
       "prevpci_sep07tojan16",
       "noprevpci_sep07tojan16",
       "previnfarction_sep07tojan16",
       "noprevinfarction_sep07tojan16"
       )

for (period in x) { 

######## IDENTIFY BASE DATA #######
dat <- data.frame(read_dta(paste0("mydata/cr_finaldataset_", period, ".dta")))


###### CREATE DATA FRAMES FOR EACH OUTCOME ########

# Create a data frame that only contains the information for each outcome, and one for covariates
source("scripts/cr_data.R")


######## RUN ANALYSES ########

#Table one
source("scripts/an_tableone.R")

#Find missing N and % for continuous vars
source("scripts/ex_missing.R")

# create confounder lists for different analyses
source("scripts/cr_confounder_list.R")

#G formula - all models (with different confounders)
source("scripts/an_gformula_allmodels.R")

}


##### 3Y ANALYSIS #####

period <- "sep07tojan16"
outcomes <- c("death", "mi")

# Get data
dat <- data.frame(read_dta(paste0("mydata/cr_finaldataset_", period, ".dta")))

# Orgaise data
source("scripts/cr_data.R")

# create confounder list
source("scripts/cr_confounder_list.R")

# Run analysis
source("scripts/an_gformula_3y.R")



######## SENSITIVITIES ##########

period <- "sep07tojan16"

# Base data
dat <- data.frame(read_dta(paste0("mydata/cr_finaldataset_", period, ".dta")))
source("scripts/cr_data.R")

### CENSOR AT DEATH
source("scripts/an_gformula_censdeath.R")
  
### COMPLETE CASE 
source("scripts/an_gformula_completecase.R")
  
### ADJUSTMENT FOR PERIOD (BEFORE OR AFTER TASTE)
source("scripts/an_gformula_period.R")
  
### Stratify by exposure and fit models in each arm
source("scripts/an_gformula_expstrat.R")


### ADJUST FOR KILLIP CLASS AFTER TASTE (change period and rerun data first)
period <- "mar13tojan16"

# Base data
dat <- data.frame(read_dta(paste0("mydata/cr_finaldataset_", period, ".dta")))
source("scripts/cr_data.R")
  
# analysis
source("scripts/an_gformula_killipclass.R")



# Time code ends
endtime <- Sys.time()

#time to run all of the code
endtime-starttime



## OLD SCRIPTS

# IP Weights functions
#source("scripts/func_ipweights.R")

# Explore predicrtors of treatment
#source("scripts/an_exp_predictors.R")

#Kaplan Meier plots
#source("scripts/an_kaplanmeier.R")

#IPW Kaplan Meier plots
#source("scripts/an_kaplanmeier_ipw.R")


