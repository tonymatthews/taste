

func_strat_exp <- function(x) {

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
  
  
  dat <- merge(dat_base, dat_covariates, by=c("lopnr", "interdat", "exp")) %>% 
    filter(exp==x) 
  
  #Expand to a row per week until event
  dat_model <- dat %>%
    mutate(survtime = ifelse(event_1y==0 | (date_event-interdat)/7>52 , 52,
                             ceiling((date_event-interdat)/7))) %>%
    expandRows("survtime", drop=F) %>%
    mutate(time = sequence(rle(lopnr)$lengths)-1) %>% 
    mutate(event = ifelse(time==survtime-1 & event_1y==1, 1, 0)) %>%
    mutate(timesq = time^2)  %>%
    select(-event_30, -event_1y, -date_event, -interdat, -survtime)
  
  #model
  gf_model <- glm(event==0 ~ 
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
  gf_exp <- expandRows(dat, count=52, count.is.col=F)
  gf_exp$time <- rep(seq(0,51), nrow(dat))
  gf_exp$timesq <- gf_exp$time^2
  gf_exp$exp <- x
  
  
  #use the model to predict the probability of no event for each dataset
  #one where everyone is exposed, the other where no one is exposed
  gf_exp$p_noevent <- predict(gf_model, gf_exp, type="response")

  
  gf_exp_surv <- gf_exp %>%
    group_by(lopnr) %>%
    mutate(surv = cumprod(p_noevent))

  gf_surv <- aggregate(gf_exp_surv, by=list(gf_exp_surv$time), FUN=mean)[c("exp", "time", "surv")]

  
  surv <- gf_surv$surv[52]
  Y <- 1-surv

  
  
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
    gf_exp <- expandRows(boot_dat, count=52, count.is.col=F)
    gf_exp$time <- rep(seq(0,51), nrow(dat))
    gf_exp$timesq <- gf_exp$time^2
    gf_exp$exp <- x
    
    
    #use the model to predict the probability of no event for each dataset
    #one where everyone is exposed, the other where no one is exposed
    gf_exp$p_noevent <- predict(gf_model, gf_exp, type="response")
    
    
    gf_exp_surv <- gf_exp %>%
      group_by(lopnr) %>%
      mutate(surv = cumprod(p_noevent))
    
    gf_surv <- aggregate(gf_exp_surv, by=list(gf_exp_surv$time), FUN=mean)[c("exp", "time", "surv")]
    
    
    gf_boot <- gf_surv %>%
      arrange(time) %>% 
      filter(time==51) %>%
      mutate(risk = 1-surv) 
    
    res <- rbind(res, cbind(gf_boot$risk))
    
  }
  
  ######### CREATE ESTIMATES AND CIS ########
  res_sd <- apply(res,2,sd)
  
  lclY <- Y - 1.96*res_sd[1]
  uclY <- Y + 1.96*res_sd[1]

  
  results <- cbind(paste0(outcome1), paste0("Y_",x,"=1"), paste0(format(round(Y*100,1), nsmall=1) ,
                                                     " (" , format(round(lclY*100,1), nsmall=1), "," ,
                                                     format(round(uclY*100,1), nsmall=1), ")" ))

  
  results_1y <- rbind(results_1y, results)
  

}

return(results_1y)

}


exp_val <- c(0,1)

results_strat_exp <- lapply(exp_val, func_strat_exp)


# BRING TOGETHER ALL RESULTS IN TXT FILE
results_1y_exp0 <- results_strat_exp[[1]] %>% 
  rename(outcome=V1, measure=V2, effect=V3) %>% 
  spread(measure, effect) %>%
  select(outcome, "Y_0=1") %>%
  rename("Outcome" = "outcome",
         "No thrombus aspiration"="Y_0=1")

results_1y_exp1 <- results_strat_exp[[2]] %>% 
  rename(outcome=V1, measure=V2, effect=V3) %>% 
  spread(measure, effect) %>%
  select("Y_1=1") %>%
  rename("Thrombus aspiration"="Y_1=1")

results_expstrat <- cbind(results_1y_exp0, results_1y_exp1)

write.table(results_expstrat, paste0("logfiles/an_gformula_expstrat_", period, ".txt"), sep="\t", quote=FALSE, row.names=FALSE)





