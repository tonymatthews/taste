
# CREATE IP WEIGHTS AT BASELINE


#### ESTIMATION OF IP WEIGHTS

func_ipweights <- function(input_data) { 

  
      #ipw denominator model
      p_denom <- glm(exp ~ 
                       as.factor(gender) + 
                       age + I(age^2) + 
                       as.factor(centre_num) + 
                       as.factor(stenosklass) + 
                       as.factor(segment_stenos) + 
                       as.factor(fynd) + 
                       bmi + I(bmi^2) + 
                       as.factor(smoking_status) + 
                       as.factor(diabetes) + 
                       as.factor(prev_infarction) + 
                       as.factor(prev_pci) + 
                       as.factor(prev_cabg) + 
                       as.factor(hyperlip) + 
                       as.factor(hyperton) + 
                       as.factor(tro) + 
                       as.factor(war) + 
                       as.factor(asa) + 
                       as.factor(clo) + 
                       as.factor(pra) + 
                       as.factor(hep) + 
                       as.factor(lowhep) + 
                       as.factor(biv) + 
                       as.factor(gpiibiiia) + 
                       heart_rate + I(heart_rate^2) + 
                       systolic_blood_pressure + I(systolic_blood_pressure^2) + 
                       diastolic_blood_pressure + I(diastolic_blood_pressure^2), 
                    family=binomial(), 
                    data=input_data)
      
      
      
      # ipw numerator model
      p_num <- glm(exp ~ 1, family=binomial(), data=input_data)
      
      input_data$pd_exp <- predict(p_denom, input_data, type="response")
      
      input_data$pn_exp <- predict(p_num, input_data, type="response")
      
      #Compute estimated weights
      input_data$sw <- ifelse(input_data$exp==1, input_data$pn_exp/input_data$pd_exp, 
                             (1-input_data$pn_exp)/(1-input_data$pd_exp))
      
      #Truncate to 99th percentile, and keep only weights and lopnr
      input_data <- input_data %>%
                    mutate(sw = ifelse(sw >= quantile(sw, 0.99), quantile(sw, 0.99),sw)) %>%
                    mutate(sw = ifelse(sw <= quantile(sw, 0.01), quantile(sw, 0.01),sw)) %>%
                    select(lopnr, sw)
      
      rm(p_denom, p_num)
      
      ipweights <<- input_data

}

