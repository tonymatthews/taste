

dat_death <- dat %>%
            rename(date_event = date_death) %>%
            rename(event_30 = death_30) %>%
            rename(event_1y = death_1y) %>%
            rename(event_3y = death_3y) %>%
            select(lopnr, interdat, exp, date_event, event_30, event_1y, event_3y)

dat_mi <- dat %>%
          rename(date_event = date_mi) %>%
          rename(event_30 = mi_30) %>%
          rename(event_1y = mi_1y) %>%
          rename(event_3y = mi_3y) %>%
          select(lopnr, interdat, exp, date_event, event_30, event_1y, event_3y)


dat_covariates <- dat %>%
                  select(lopnr, interdat, exp, contains("before_after"), gender, age, weight, centre_num, uni_hosp, stenosklass,
                         segment_stenos, killipklass, timefromecg, diabetes, bmi, smoking_status, 
                         hyperlip, hyperton, prev_infarction, prev_pci, prev_cabg, 
                         fynd, heart_rate, systolic_blood_pressure, diastolic_blood_pressure,
                         tro, war, asa, clo, tic, pra, hep, lowhep, biv, gp2b, gpiibiiia) %>% 
          
                  
                  #Organise missing variable for analysis
                  mutate(smoking_status= replace(smoking_status, is.na(smoking_status), 9)) %>%
                  mutate(killipklass= replace(killipklass, is.na(killipklass), 9)) %>%
                  mutate(fynd= replace(fynd, is.na(fynd), 9)) %>%
                  mutate(stenosklass= replace(stenosklass, is.na(stenosklass), 9)) %>% 
                  mutate(heart_rate= replace(heart_rate, is.na(heart_rate), median(heart_rate, na.rm=TRUE))) %>% 
                  mutate(systolic_blood_pressure= replace(systolic_blood_pressure, is.na(systolic_blood_pressure), median(systolic_blood_pressure, na.rm=TRUE))) %>% 
                  mutate(diastolic_blood_pressure= replace(diastolic_blood_pressure, is.na(diastolic_blood_pressure), median(diastolic_blood_pressure, na.rm=TRUE))) %>%  
                  mutate(bmi= replace(bmi, is.na(bmi), median(bmi, na.rm=TRUE))) %>% 
                  mutate(timefromecg= replace(timefromecg, is.na(timefromecg), median(timefromecg, na.rm=TRUE))) %>%
                  mutate(segment_stenos= replace(segment_stenos, is.na(segment_stenos), 9)) %>%
                  mutate(segment_stenos= replace(segment_stenos, segment_stenos==0, 9)) %>%
                  mutate(after = case_when(
                    interdat < as.Date("2010-06-14") ~ 0L,
                    interdat > as.Date("2013-03-26") ~ 1L
                  ))


#if (period %in% c("sep07tojun10", "mar13tojan16",
#                  "stenosab_sep07tojan16", "stenosc_sep07tojan16",
#                  "over65_sep07tojan16", "under65_sep07tojan16",
#                  "male_sep07tojan16", "female_sep07tojan16",
#                  "diabetes_sep07tojan16", "nodiabetes_sep07tojan16",
#                  "prevpci_sep07tojan16", "noprevpci_sep07tojan16",
#                  "previnfarction_sep07tojan16", "noprevinfarction_sep07tojan16")) {
#  dat_covariates <- dat_covariates %>% 
#    filter(fynd!= 9 & fynd!=1) %>% 
#    filter(stenosklass!= 9 & stenosklass != 88)
#                    
#}
                  


dat_complete <- dat %>%
              select(lopnr, interdat, exp, contains("before_after"), gender, age, weight, centre_num, uni_hosp, stenosklass,
                     segment_stenos, killipklass, timefromecg, diabetes, bmi, smoking_status, 
                     hyperlip, hyperton, prev_infarction, prev_pci, prev_cabg, 
                     fynd, heart_rate, systolic_blood_pressure, diastolic_blood_pressure,
                     tro, war, asa, clo, tic, pra, hep, lowhep, biv, gpiibiiia) %>% 
              
              
              #Organise missing variable for analysis
              filter(smoking_status!= is.na(smoking_status)) %>% 
              filter(fynd!= is.na(fynd)) %>% 
              filter(stenosklass!= is.na(stenosklass) & stenosklass != 88) %>%
              filter(heart_rate!= is.na(heart_rate)) %>%
              filter(systolic_blood_pressure!= is.na(systolic_blood_pressure)) %>%
              filter(diastolic_blood_pressure!= is.na(diastolic_blood_pressure)) %>%
              filter(bmi!= is.na(bmi)) 

rm(dat)