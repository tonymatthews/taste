
missing <- data.frame()

dat <- data.frame(read_dta(paste("mydata/cr_finaldataset_", period, ".dta", sep="")))

miss <- dat %>% 
        select(exp, bmi, heart_rate, systolic_blood_pressure, diastolic_blood_pressure) %>% 
        group_by(exp) %>%
        summarise_each(funs(sum(is.na(.)), 100*mean(is.na(.)))) %>% 
        arrange(-exp)
  
  
exp <- cbind("", "1", "0")
bmi <- cbind("BMI", 
             paste0(miss$bmi_sum[1], " (", format(round(miss$`bmi_*`[1], 1), nsmall=1),")"), 
             paste0(miss$bmi_sum[2], " (", format(round(miss$`bmi_*`[2], 1), nsmall=1),")")) 
heart_rate <- cbind("Heart rate", 
                    paste0(miss$heart_rate_sum[1], " (", format(round(miss$`heart_rate_*`[1], 1), nsmall=1), ")"), 
                    paste0(miss$heart_rate_sum[2], " (", format(round(miss$`heart_rate_*`[2], 1), nsmall=1), ")")) 
systolic_blood_pressure <- cbind("Systolic blood pressure", 
                                 paste0(miss$systolic_blood_pressure_sum[1], " (", format(round(miss$`systolic_blood_pressure_*`[1], 1), nsmall=1), ")"), 
                                 paste0(miss$systolic_blood_pressure_sum[2], " (", format(round(miss$`systolic_blood_pressure_*`[2], 1), nsmall=1), ")")) 
diastolic_blood_pressure <- cbind("Diastolic blood pressure", 
                                  paste0(miss$diastolic_blood_pressure_sum[1], " (", format(round(miss$`diastolic_blood_pressure_*`[1], 1), nsmall=1),")"), 
                                  paste0(miss$diastolic_blood_pressure_sum[2], " (", format(round(miss$`diastolic_blood_pressure_*`[2], 1), nsmall=1),")")) 

missing <- rbind(exp, bmi, heart_rate, systolic_blood_pressure, diastolic_blood_pressure) 

write.table(missing, paste0("logfiles/missing_", period, ".txt"), sep="\t", quote=FALSE, row.names=FALSE, col.names = FALSE)

rm(dat, miss, exp, bmi, heart_rate, systolic_blood_pressure, diastolic_blood_pressure, missing)
