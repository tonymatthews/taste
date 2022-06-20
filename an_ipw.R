
results_1y <- data.frame()


################## EXPAND DATA AND RUN MODELS ##################

for(i in outcomes) {
  
  #Create an outcome value that is the analysis that is taking palce
  
  if (i == "death") {
    outcome_title <- "Death"
    dat_base <- dat_death
  }
  
  if (i == "mi") {
    outcome_title <- "Myocardial infarction"
    dat_base <- dat_mi
  }
  
  
  dat <- merge(dat_base, dat_covariates, by=c("lopnr", "interdat", "exp")) 
  
  # Get Ip weights 
  func_ipweights(dat)
  
  #Expand to a row per week until event
  dat_model <- dat %>%
    left_join(ipweights, by=c("lopnr")) %>% 
    mutate(survtime = ifelse(event_1y==0 | (date_event-interdat)/7>52 , 52,
                             ceiling((date_event-interdat)/7)))  %>%
    expandRows("survtime", drop=F) %>%
    mutate(time = sequence(rle(lopnr)$lengths)-1) %>% 
    mutate(event = ifelse(time==survtime-1 & event_1y==1, 1, 0)) %>%
    mutate(timesq = time^2)  %>%
    select(-event_30, -event_1y, -date_event, -interdat, -survtime)
  
  ###### Model to predict IP weighted survival curves & risks ########
  ipw_model <- glm(event==0 ~ exp + I(exp*time) + I(exp*timesq) + 
                     time + timesq, family=binomial(), weight=sw,
                   data=dat_model)
  
  
  #Create datasets with all time points under each treatment level
  ipw_exp0 <- data.frame(cbind(seq(0,51),0,(seq(0,51))^2))
  ipw_exp1 <- data.frame(cbind(seq(0,51),1,(seq(0,51))^2))
  
  colnames(ipw_exp0) <- c("time", "exp", "timesq")
  colnames(ipw_exp1) <- c("time", "exp", "timesq")
  
  # assignment of estimates (1-hazard) to each person day
  ipw_exp0$p_noevent0 <- predict(ipw_model, ipw_exp0, type="response")
  ipw_exp1$p_noevent1 <- predict(ipw_model, ipw_exp1, type="response")
  
  #Compute survival for each person day
  ipw_exp0$surv0 <- cumprod(ipw_exp0$p_noevent0)
  ipw_exp1$surv1 <- cumprod(ipw_exp1$p_noevent1)
  
  #merge both datasets together
  ipw_graph <- merge(ipw_exp0, ipw_exp1, by=c("time", "timesq"))
  
  #create the differece in survival
  ipw_graph$survdiff <- ipw_graph$surv1 - ipw_graph$surv0
  
  ipw_graph <- ipw_graph %>%
    arrange(time)
  
  surv0 <- ipw_graph$surv0[52]
  surv1 <- ipw_graph$surv1[51]
  Y_0 <- 1-surv0
  Y_1 <- 1-surv1
  riskdiff <- Y_1 - Y_0
  riskratio <- Y_1/Y_0
  
  ####### BOOTSTRAP CIS ########
  
  res <- NULL
  set.seed(1)
  
  #Create a data frame that will be used to take samples from
  #this includes outcomes and covariates, so can calculate ipweights again
  dat_boot <- merge(dat_base, dat_covariates, by=c("lopnr", "interdat", "exp")) 
  
  for (z in 1:numboot) {
    
    index <- sample(1:nrow(dat_boot), nrow(dat_boot), replace=T)
    boot_dat <- dat_boot[index, ]
    
    
    #Create ipweights 
    func_ipweights(boot_dat)
    
    #Expand to a row per week until event
    dat_model <- dat %>%
      left_join(ipweights, by=c("lopnr")) %>% 
      mutate(survtime = ifelse(event_1y==0 | (date_event-interdat)/7>52 , 52,
                               ceiling((date_event-interdat)/7)))  %>%
      expandRows("survtime", drop=F) %>%
      mutate(time = sequence(rle(lopnr)$lengths)-1) %>% 
      mutate(event = ifelse(time==survtime-1 & event_1y==1, 1, 0)) %>%
      mutate(timesq = time^2)  %>%
      select(-event_30, -event_1y, -date_event, -interdat, -survtime)
    
    ###### Model to predict IP weighted survival curves & risks ########
    ipw_model <- glm(event==0 ~ exp + I(exp*time) + I(exp*timesq) + 
                       time + timesq, family=binomial(), weight=sw,
                     data=dat_model)  
    
    #Create datasets with all time points undr each treatment level
    ipw_exp0_boot <- data.frame(cbind(seq(0,51),0,(seq(0,51))^2))
    ipw_exp1_boot <- data.frame(cbind(seq(0,51),1,(seq(0,51))^2))
    
    colnames(ipw_exp0_boot) <- c("time", "exp", "timesq")
    colnames(ipw_exp1_boot) <- c("time", "exp", "timesq")
    
    
    # assignment of estimates (1-hazard) to each person day
    ipw_exp0_boot$p_noevent0 <- predict(ipw_model, ipw_exp0_boot, type="response")
    ipw_exp1_boot$p_noevent1 <- predict(ipw_model, ipw_exp1_boot, type="response")
    
    #Compute survival for each person day
    ipw_exp0_boot$surv0 <- cumprod(ipw_exp0_boot$p_noevent0)
    ipw_exp1_boot$surv1 <- cumprod(ipw_exp1_boot$p_noevent1)
    
    #merge both datasets together
    ipw_boot <- merge(ipw_exp0_boot, ipw_exp1_boot, by=c("time", "timesq"))
    
    
    ipw_boot <- ipw_boot %>%
      filter(time==51) %>%
      mutate(risk0 = 1-surv0) %>%
      mutate(risk1 = 1-surv1) %>%
      mutate(riskdiff = risk1-risk0) %>%
      mutate(logriskratio = log(risk1/risk0)) %>%
      select(risk0,risk1,riskdiff, logriskratio)
    
    
    res <- rbind(res, cbind(ipw_boot$risk0, ipw_boot$risk1, ipw_boot$riskdiff, ipw_boot$logriskratio))
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
  
  result1 <- cbind(paste0(outcome_title), "Y_1=1", paste0(format(round(Y_1*100,1), nsmall=1) ,
                                                     " (" , format(round(lclY_1*100,1), nsmall=1), "," ,
                                                     format(round(uclY_1*100,1), nsmall=1), ")" ))
  result2 <- cbind(paste0(outcome_title), "Y_0=1", paste0(format(round(Y_0*100,1), nsmall=1) ,
                                                     " (" , format(round(lclY_0*100,1), nsmall=1), "," ,
                                                     format(round(uclY_0*100,1), nsmall=1), ")" ))  
  result3 <- cbind(paste0(outcome_title), "RD", paste0(format(round(riskdiff*100,1), nsmall=1), 
                                                  " (" , format(round(lcldiff*100,1), nsmall=1), "," ,
                                                  format(round(ucldiff*100,1), nsmall=1), ")" ))
  result4 <- cbind(paste0(outcome_title), "RR", paste0(format(round(riskratio,2), nsmall=2), 
                                                  " (" , format(round(lclratio,2), nsmall=2), "," ,
                                                  format(round(uclratio,2), nsmall=2), ")" ))
  results <- rbind(result1, result2, result3, result4)
  results_1y <- rbind(results_1y, results)
  

  
} # end for(i in outcomes)  
  
# Organise final results data frame
results_1y <- results_1y %>%
  rename(outcome=V1, measure=V2, effect=V3) %>%
  spread(measure, effect) %>%
  select(outcome, "Y_1=1", "Y_0=1", "RD", "RR") %>%
  rename("Outcome" = "outcome",
         "Thrombus aspiration"="Y_1=1", 
         "No thrombus aspiration"="Y_0=1", 
         "Risk difference (95% CI)"="RD", 
         "Risk ratio (95% CI)"="RR") 

#write the results to txt file
write.table(results_1y, paste0("logfiles/an_ipw_", period, ".txt"), sep="\t", quote=FALSE, row.names=FALSE)
  
  

