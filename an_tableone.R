###################################################################################
#Author - Anthony Matthews
#Date created - 12/11/2019
#Date of last update - 12/11/2019
#Purpose - Create a table one for a certain time period in TASTE
#How to use - Put the name of the time period datafile extension in the function
#What is returned - text file that can be copied into a table
###################################################################################

#Use original data that still has missing data
dat <- data.frame(read_dta(paste("mydata/cr_finaldataset_", period, ".dta", sep="")))


#LABEL CATEGORICAL VARIABLES

#Exposure 
dat$exp <- factor(dat$exp, levels=c(1,0),
          labels=c("PCI + Thrombus Aspiration", "PCI only"))

#Smoking
dat$smoking_status <- factor(dat$smoking_status, levels=c(0,1,2,9),
                          labels=c("Never","Ex smoker (> 1 month)", "Current smoker", "Missing"))

#Stenosis Class
dat$stenosklass <- factor(dat$stenosklass, levels=c(1,2,3,4,5,6,7,88),
                             labels=c("A","B1", "B2", "C", "B1 Bifurcation", "B2 Bifurcation", "C Bifurcations", "Other"))

# Stenosis in culprit artery
dat<- dat %>% 
      mutate(segment_stenos= replace(segment_stenos, segment_stenos==0, NA)) 
dat$segment_stenos <- factor(dat$segment_stenos, levels=c(1,2,3,4,5,6), 
                            labels=c("0-29%","30-49%","50-69%","70-89%", "90-99%", "100%"))


#Angiography finding

dat$fynd <- factor(dat$fynd, levels=c(1,2,3,4,5,9),
                          labels=c("Normal","1 vessel", "2 vessels", "3 vessels", "Left main", "Missing"))


# Make categorical variables factors
varsToFactor <- c("centre_g", 
                  "gender", 
                  "stenosklass",
                  "fynd",
                  "smoking_status",
                  "segment_stenos",
                  "diabetes",
                  "hyperlip", 
                  "hyperton",
                  "prev_infarction",
                  "prev_pci",
                  "prev_cabg",
                  "tro", 
                  "war", 
                  "asa", 
                  "clo", 
                  "pra", 
                  "hep", 
                  "lowhep", 
                  "biv", 
                  "gpiibiiia")
dat[varsToFactor] <- lapply(dat[varsToFactor], factor)

# Create variable list
# First check the variables that are in the dataset


# Variable labels
varLabelList <- list(exp = "Exposure",
                     centre_g = "Hospital",
                     gender = "Female",
                     age = "Age (yrs)",
                     stenosklass = "Stenosis class",
                     segment_stenos = "Stenosis in culprit artery",
                     diabetes = "Diabetes", 
                     bmi = "BMI (kg/m^2)", 
                     smoking_status = "Smoking status", 
                     hyperlip = "Hyperlipidemia treatment", 
                     hyperton = "Hypertension treatment", 
                     prev_infarction = "Previous myocardial infarction",
                     prev_pci= "Previous percutaneous coronary intervention", 
                     prev_cabg = "Previous coronary artery bypass grafting", 
                     tro = "Thrombolysis", 
                     war = "Warfarin", 
                     asa = "Aspirin", 
                     clo = "Clopidogrel or ticlopidine", 
                     pra = "Prasugrel", 
                     hep = "Heparin", 
                     lowhep = "Low-molecular weight heparin", 
                     biv = "Bivalirudin", 
                     gpiibiiia = "Glycoprotein IIb/IIIa inhibitors",
                     #timefromecg = "Time from ECG to percutaneous coronary intervention (mins)",
                     fynd = "Angiography finding", 
                     heart_rate = "Heart rate", 
                     systolic_blood_pressure = "Systolic blood pressure", 
                     diastolic_blood_pressure = "Diastolic blood pressure"
)

var_label(dat) <-  varLabelList

# Create variable list
# First check the variables that are in the dataset
#dput(names(dat))
vars <- c("age",
          "gender", 
          "centre_g", 
          "stenosklass", 
          "segment_stenos",
          "fynd",
          "bmi",
          "smoking_status",
          "diabetes",
          "hyperlip", 
          "hyperton",
          "prev_infarction",
          "prev_pci",
          "prev_cabg",
          "tro", 
          "war", 
          "asa", 
          "clo", 
          "pra", 
          "hep", 
          "lowhep", 
          "biv", 
          "gpiibiiia", 
          #"timefromecg",
          "heart_rate",
          "systolic_blood_pressure", 
          "diastolic_blood_pressure")

median <- c("age", 
            "bmi", 
            "heart_rate", 
            "systolic_blood_pressure", 
            "diastolic_blood_pressure") 
            #"timefromecg")

#Create table one stratified by exp
tableOne <- CreateTableOne(vars = vars, strata = c("exp"), includeNA = TRUE, data = dat) 
tableOnePrint <- print(tableOne, nonnormal=median, contDigits = 1, test = FALSE, missing = TRUE, quote = FALSE, varLabel = TRUE, dropEqual = TRUE, noSpace = TRUE)
write.table(tableOnePrint, file=paste("logfiles/table1_", period, ".txt", sep=""), sep="\t", quote = FALSE, row.names=TRUE, col.names=NA)
        
rm(dat, tableOnePrint, vars, tableOne, median, varsToFactor, varLabelList)


