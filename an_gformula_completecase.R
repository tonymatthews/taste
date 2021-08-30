
results_1y <- data.frame()

outcome_sens <- c("death","mi")


for(i in outcome_sens) {
  
#Create an outcome value that is the analysis that is taking palce
  
  
  if (i == "death") {
    outcome <- "death"
    outcome1 <- "Death"
    dat_base <- dat_death
  }
  
  if (i == "mi") {
    outcome <- "mi"
    outcome1 <- "Myocardial infarction"
    dat_base <- dat_mi
  }
  
  
  dat <- merge(dat_base, dat_complete, by=c("lopnr", "interdat", "exp")) 
  
  #Expand to a row per week until event
  dat_model <- dat %>%
    mutate(survtime = ifelse(event_1y==0 | (date_event-interdat)/7>52 , 52,
                             ceiling((date_event-interdat)/7)))  %>%
    expandRows("survtime", drop=F) %>%
    mutate(time = sequence(rle(lopnr)$lengths)-1) %>% 
    mutate(event = ifelse(time==survtime-1 & event_1y==1, 1, 0)) %>%
    mutate(timesq = time^2)  %>%
    select(-event_30, -event_1y, -date_event, -interdat, -survtime)
  
  #model
  gf_model <- glm(event==0 ~ 
                      exp + 
                      I(exp*time) + I(exp*timesq) +
                      time + timesq +
                      as.factor(gender)  + 
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
                    data=dat_model, 
                    family=binomial())
  
 
  #create dataset with all time points for each individual under each treatment
  gf_exp0 <- expandRows(dat, count=52, count.is.col=F)
  gf_exp0$time <- rep(seq(0,51), nrow(dat))
  gf_exp0$timesq <- gf_exp0$time^2
  gf_exp0$exp <- 0
  
  gf_exp1 <- gf_exp0
  gf_exp1$exp <- 1
  
  #use the model to predict the probability of no event for each dataset
  #one where everyone is exposed, the other where no one is exposed
  gf_exp0$p_noevent0 <- predict(gf_model, gf_exp0, type="response")
  gf_exp1$p_noevent1 <- predict(gf_model, gf_exp1, type="response")
  
  gf_exp0_surv <- gf_exp0 %>%
    group_by(lopnr) %>%
    mutate(surv0 = cumprod(p_noevent0))
  
  gf_exp1_surv <- gf_exp1 %>%
    group_by(lopnr) %>%
    mutate(surv1 = cumprod(p_noevent1))
  
  gf_surv0 <- aggregate(gf_exp0_surv, by=list(gf_exp0_surv$time), FUN=mean)[c("exp", "time", "surv0")]
  gf_surv1 <- aggregate(gf_exp1_surv, by=list(gf_exp1_surv$time), FUN=mean)[c("exp", "time", "surv1")]
  
  gf <- merge(gf_surv0, gf_surv1, by=c("time"))
  gf$survdiff <- gf_surv1-gf_surv0
  
  gf <- gf %>%
    arrange(time)
  
  surv0 <- gf$surv0[52]
  surv1 <- gf$surv1[52]
  Y_0 <- 1-surv0
  Y_1 <- 1-surv1
  riskdiff <- Y_1 - Y_0
  riskratio <- Y_1/Y_0
  
  gf_surv0 <- gf_surv0 %>% 
    rename(surv = surv0)
  
  gf_surv1 <- gf_surv1 %>% 
    rename(surv = surv1) 
    
    
  gf_plot <- bind_rows(gf_surv0, gf_surv1) 

  
  rm(dat_model, gf_exp0, gf_exp0_surv, gf_exp1, gf_exp1_surv, gf_model, gf_surv0, gf_surv1, gf)
  
  
  ###### bootstraps ########
  res <- NULL
  set.seed(1)
  
  for (z in 1:numboot) {
    
    index <- sample(1:nrow(dat), nrow(dat), replace=T)
    boot_dat <- dat[index, ]
    
    #Expand to a row per week until event
    dat_model <- boot_dat %>%
      mutate(survtime = ifelse(event_1y==0 | (date_event-interdat)/7>52 , 52,
                               ceiling((date_event-interdat)/7)))  %>%
      expandRows("survtime", drop=F) %>%
      mutate(time = sequence(rle(lopnr)$lengths)-1) %>% 
      mutate(event = ifelse(time==survtime-1 & event_1y==1, 1, 0)) %>%
      mutate(timesq = time^2)  %>%
      select(-event_30, -event_1y, -date_event, -interdat, -survtime)
    
    #model
    gf_model <- glm(event==0 ~ 
                      exp + 
                      I(exp*time) + I(exp*timesq) +
                      time + timesq +
                      as.factor(gender)  + 
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
                      data=dat_model, 
                      family=binomial())
    
    
    #create dataset with all time points for each individual under each treatment
    gf_exp0 <- expandRows(boot_dat, count=52, count.is.col=F)
    gf_exp0$time <- rep(seq(0,51), nrow(dat))
    gf_exp0$timesq <- gf_exp0$time^2
    gf_exp0$exp <- 0
    
    gf_exp1 <- gf_exp0
    gf_exp1$exp <- 1
    
    #use the model to predict the probability of no event for each dataset
    #one where everyone is exposed, the other where no one is exposed
    gf_exp0$p_noevent0 <- predict(gf_model, gf_exp0, type="response")
    gf_exp1$p_noevent1 <- predict(gf_model, gf_exp1, type="response")
    
    gf_exp0_surv <- gf_exp0 %>%
      group_by(lopnr) %>%
      mutate(surv0 = cumprod(p_noevent0))
    
    gf_exp1_surv <- gf_exp1 %>%
      group_by(lopnr) %>%
      mutate(surv1 = cumprod(p_noevent1))
    
    gf_surv0 <- aggregate(gf_exp0_surv, by=list(gf_exp0_surv$time), FUN=mean)[c("exp", "time", "surv0")]
    gf_surv1 <- aggregate(gf_exp1_surv, by=list(gf_exp1_surv$time), FUN=mean)[c("exp", "time", "surv1")]
    
    gf <- merge(gf_surv0, gf_surv1, by=c("time"))
    gf$survdiff <- gf_surv1-gf_surv0
    
    gf_boot <- gf %>%
      arrange(time) %>% 
      filter(time==51) %>%
      mutate(risk0 = 1-surv0) %>%
      mutate(risk1 = 1-surv1) %>%
      mutate(riskdiff = risk1-risk0) %>%
      mutate(logriskratio = log(risk1/risk0)) %>%
      select(risk0,risk1,riskdiff, logriskratio)
    
    res <- rbind(res, cbind(gf_boot$risk0, gf_boot$risk1, gf_boot$riskdiff, gf_boot$logriskratio))
    
    rm(dat_model, gf_exp0, gf_exp0_surv, gf_exp1, gf_exp1_surv, gf_model, gf_surv0, gf_surv1, gf_boot)
    
  }
  
  ######### CREATE ESTIMATES AND CIS ########
  res_sd <- apply(res,2,sd)
  
  lclY_0 <- Y_0 - 1.96*res_sd[1]
  uclY_0 <- Y_0 + 1.96*res_sd[1]
  
  lclY_1 <- Y_1 - 1.96*res_sd[2]
  uclY_1 <- Y_1 + 1.96*res_sd[2]
  
  lcldiff <- riskdiff - 1.96*res_sd[3]
  ucldiff <- riskdiff + 1.96*res_sd[3]
  
  lclratio <- exp(log(riskratio) - 1.96*res_sd[4])
  uclratio <- exp(log(riskratio) + 1.96*res_sd[4])
  
  result1 <- cbind(paste0(outcome1), "Y_1=1", paste0(format(round(Y_1*100,1), nsmall=1) ,
                                                     " (" , format(round(lclY_1*100,1), nsmall=1), "," ,
                                                     format(round(uclY_1*100,1), nsmall=1), ")" ))
  result2 <- cbind(paste0(outcome1), "Y_0=1", paste0(format(round(Y_0*100,1), nsmall=1) ,
                                                     " (" , format(round(lclY_0*100,1), nsmall=1), "," ,
                                                     format(round(uclY_0*100,1), nsmall=1), ")" ))  
  result3 <- cbind(paste0(outcome1), "RD", paste0(format(round(riskdiff*100,1), nsmall=1), 
                                                  " (" , format(round(lcldiff*100,1), nsmall=1), "," ,
                                                  format(round(ucldiff*100,1), nsmall=1), ")" ))
  result4 <- cbind(paste0(outcome1), "RR", paste0(format(round(riskratio,2), nsmall=2), 
                                                  " (" , format(round(lclratio,2), nsmall=2), "," ,
                                                  format(round(uclratio,2), nsmall=2), ")" ))
  
  results <- rbind(result1, result2, result3, result4)
  results_1y <- rbind(results_1y, results)
  

  #REMOVE EVERYTHING THAT ISN'T NEEDED
  rm(gf_plot, gf, res, result1, result2, result3, result4, results, index, 
     lcldiff, lclratio, lclY_0, lclY_1, res_sd, riskdiff, riskratio, surv0, 
     surv1, ucldiff, uclratio, uclY_0, uclY_1, Y_0, Y_1, z)
  
}


# BRING TOGETHER ALL RESULTS IN TXT FILE
results_1y <- results_1y %>%
  rename(outcome=V1, measure=V2, effect=V3) %>%
  spread(measure, effect) %>%
  select(outcome, "Y_1=1", "Y_0=1", "RD", "RR") %>%
  rename("Outcome" = "outcome",
         "PCI + Thrombus Aspiration"="Y_1=1", 
         "PCI only"="Y_0=1", 
         "365-day risk difference (95% CI)"="RD", 
         "365-day risk ratio (95% CI)"="RR")

write.table(results_1y, paste0("logfiles/an_gformula_completecase_", period, ".txt"), sep="\t", quote=FALSE, row.names=FALSE)





