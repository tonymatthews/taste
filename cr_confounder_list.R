#################### DEFINE CONFOUNDERS ###################
 if (period %in% c("male_sep07tojan16", "female_sep07tojan16")) {
  confounder_list <- as.list(c(
    # model 1 - age and sex
    "+ age + I(age^2)",
    
    # model 2 - fully adjusted
    "+ age + I(age^2) + as.factor(centre_num) + as.factor(stenosklass) + as.factor(segment_stenos) + as.factor(fynd) + bmi + I(bmi^2) + as.factor(smoking_status) + as.factor(diabetes) + as.factor(prev_infarction) + as.factor(prev_pci) + as.factor(prev_cabg) + as.factor(hyperlip) + as.factor(hyperton) + as.factor(tro) + as.factor(war) + as.factor(asa) + as.factor(clo) + as.factor(pra) + as.factor(hep) + as.factor(lowhep) + as.factor(biv) + as.factor(gpiibiiia) + heart_rate + I(heart_rate^2) + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2)"))
 
# age
} else if (period %in% c("over65_sep07tojan16", "under65_sep07tojan16")) {
  confounder_list <- as.list(c(
    # model 1 - age and centre_num
    "+ as.factor(gender) + age + I(age^2)",
    
    # model 2 - fully adjusted
    "+ as.factor(gender) + age + I(age^2) + as.factor(centre_num) + as.factor(stenosklass) + as.factor(segment_stenos) + as.factor(fynd) + bmi + I(bmi^2) + as.factor(smoking_status) + as.factor(diabetes) + as.factor(prev_infarction) + as.factor(prev_pci) + as.factor(prev_cabg) + as.factor(hyperlip) + as.factor(hyperton) + as.factor(tro) + as.factor(war) + as.factor(asa) + as.factor(clo) + as.factor(pra) + as.factor(hep) + as.factor(lowhep) + as.factor(biv) + as.factor(gpiibiiia) + heart_rate + I(heart_rate^2) + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2)"))
 
# stenosclass C stratification analysis doesn't include stenosclass variable
} else if (period %in% c("stenosc_sep07tojan16")) {
  confounder_list <- as.list(c(
    # model 1 - age and sex
    "+ as.factor(gender) + age + I(age^2)",
    
    # model 2 - fully adjusted
    "+ as.factor(gender) + age + I(age^2) + as.factor(centre_num) + as.factor(segment_stenos) + as.factor(fynd) + bmi + I(bmi^2) + as.factor(smoking_status) + as.factor(diabetes) + as.factor(prev_infarction) + as.factor(prev_pci) + as.factor(prev_cabg) + as.factor(hyperlip) + as.factor(hyperton) + as.factor(tro) + as.factor(war) + as.factor(asa) + as.factor(clo) + as.factor(pra) + as.factor(hep) + as.factor(lowhep) + as.factor(biv) + as.factor(gpiibiiia) + heart_rate + I(heart_rate^2) + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2)"))
  
# stenosclass AB
} else if (period %in% c("stenosab_sep07tojan16")) {
  confounder_list <- as.list(c(
    # model 1 - age and sex
    "+ as.factor(gender) + age + I(age^2)",
    
    # model 2 - fully adjusted
    "+ as.factor(gender) + age + I(age^2) + as.factor(centre_num) + as.factor(stenosklass) + as.factor(segment_stenos) + as.factor(fynd) + bmi + I(bmi^2) + as.factor(smoking_status) + as.factor(diabetes) + as.factor(prev_infarction) + as.factor(prev_pci) + as.factor(prev_cabg) + as.factor(hyperlip) + as.factor(hyperton) + as.factor(tro) + as.factor(war) + as.factor(asa) + as.factor(clo) + as.factor(pra) + as.factor(hep) + as.factor(lowhep) + as.factor(biv) + as.factor(gpiibiiia) + heart_rate + I(heart_rate^2) + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2)"))
  

# diabetes
} else if (period %in% c("diabetes_sep07tojan16", "nodiabetes_sep07tojan16")) {
  confounder_list <- as.list(c(
    # model 1 - age and sex
    "+ as.factor(gender) + age + I(age^2)",
    
    # model 2 - fully adjusted
    "+ as.factor(gender) + age + I(age^2) + as.factor(centre_num) + as.factor(stenosklass) + as.factor(segment_stenos) + as.factor(fynd) + bmi + I(bmi^2) + as.factor(smoking_status) + as.factor(prev_infarction) + as.factor(prev_pci) + as.factor(prev_cabg) + as.factor(hyperlip) + as.factor(hyperton) + as.factor(tro) + as.factor(war) + as.factor(asa) + as.factor(clo) + as.factor(pra) + as.factor(hep) + as.factor(lowhep) + as.factor(biv) + as.factor(gpiibiiia) + heart_rate + I(heart_rate^2) + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2)"))

# prev pci
} else if (period %in% c("prevpci_sep07tojan16", "noprevpci_sep07tojan16")) {
  
  confounder_list <- as.list(c(
    # model 1 - age and sex
    "+ as.factor(gender) + age + I(age^2)",
    
    # model 2 - fully adjusted
    "+ as.factor(gender) + age + I(age^2) + as.factor(centre_num) + as.factor(stenosklass) + as.factor(segment_stenos) + as.factor(fynd) + bmi + I(bmi^2) + as.factor(smoking_status) + as.factor(diabetes) + as.factor(prev_infarction) + as.factor(prev_cabg) + as.factor(hyperlip) + as.factor(hyperton) + as.factor(tro) + as.factor(war) + as.factor(asa) + as.factor(clo) + as.factor(pra) + as.factor(hep) + as.factor(lowhep) + as.factor(biv) + as.factor(gpiibiiia) + heart_rate + I(heart_rate^2) + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2)"))

# prev_infarction
} else if (period %in% c("previnfarction_sep07tojan16", "noprevinfarction_sep07tojan16")) {
  
  confounder_list <- as.list(c(
    # model 1 - age and sex
    "+ as.factor(gender) + age + I(age^2)",
    
    # model 2 - fully adjusted
    "+ as.factor(gender) + age + I(age^2) + as.factor(centre_num) + as.factor(stenosklass) + as.factor(segment_stenos) + as.factor(fynd) + bmi + I(bmi^2) + as.factor(smoking_status) + as.factor(diabetes) + as.factor(prev_pci) + as.factor(prev_cabg) + as.factor(hyperlip) + as.factor(hyperton) + as.factor(tro) + as.factor(war) + as.factor(asa) + as.factor(clo) + as.factor(pra) + as.factor(hep) + as.factor(lowhep) + as.factor(biv) + as.factor(gpiibiiia) + heart_rate + I(heart_rate^2) + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2)"))
  
} else {
  
  confounder_list <- as.list(c(
    # model 1 - age and sex
    "+ as.factor(gender) + age + I(age^2)",
    
    # model 2 - fully adjusted
    "+ as.factor(gender) + age + I(age^2) + as.factor(centre_num) + as.factor(stenosklass) + as.factor(segment_stenos) + as.factor(fynd) + bmi + I(bmi^2) + as.factor(smoking_status) + as.factor(diabetes) + as.factor(prev_infarction) + as.factor(prev_pci) + as.factor(prev_cabg) + as.factor(hyperlip) + as.factor(hyperton) + as.factor(tro) + as.factor(war) + as.factor(asa) + as.factor(clo) + as.factor(pra) + as.factor(hep) + as.factor(lowhep) + as.factor(biv) + as.factor(gpiibiiia) + heart_rate + I(heart_rate^2) + systolic_blood_pressure + I(systolic_blood_pressure^2) + diastolic_blood_pressure + I(diastolic_blood_pressure^2)"))
  
}