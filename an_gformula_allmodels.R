
results_1y <- data.frame()
difference_results_1y <- data.frame()


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
  
  if (i == "stentthromb") {
    outcome_title <- "Stent thrombosis"
    dat_base <- dat_stentthromb
  }
  
  dat <- merge(dat_base, dat_covariates, by=c("lopnr", "interdat", "exp")) 
  
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
  gf_model_list <- lapply(paste("event==0 ~ exp + I(exp*time) + I(exp*timesq) + time + timesq", confounder_list), as.formula)
  gf_model_results <- lapply(gf_model_list, function(x) glm(x, data=dat_model, family=binomial()))
  

  all_results <- data.frame(model=integer(), risk_0=numeric(), risk_1=numeric(), riskdiff=numeric(), riskratio=numeric())
 
  
  for(m in 1:length(confounder_list)) {
    
        #create dataset with all time points for each individual under each treatment
        gf_exp0 <- expandRows(dat, count=52, count.is.col=F)
        gf_exp0$time <- rep(seq(0,51), nrow(dat))
        gf_exp0$timesq <- gf_exp0$time^2
        gf_exp0$exp <- 0
        
        gf_exp1 <- gf_exp0
        gf_exp1$exp <- 1
      
        gf_exp0$p_noevent0 <- predict(gf_model_results[[m]], gf_exp0, type="response")
        gf_exp1$p_noevent1 <- predict(gf_model_results[[m]], gf_exp1, type="response")
        
        gf_exp0_surv <- gf_exp0 %>%
                        group_by(lopnr) %>%
                        mutate(surv_0 = cumprod(p_noevent0))
        
        gf_exp1_surv <- gf_exp1 %>%
                        group_by(lopnr) %>%
                        mutate(surv_1 = cumprod(p_noevent1))
        
        gf_surv_0 <- aggregate(gf_exp0_surv, by=list(gf_exp0_surv$time), FUN=mean)[c("exp", "time", "surv_0")]
        gf_surv_1 <- aggregate(gf_exp1_surv, by=list(gf_exp1_surv$time), FUN=mean)[c("exp", "time", "surv_1")]
        
        gf <- merge(gf_surv_0, gf_surv_1, by=c("time")) %>%
              arrange(time)
        
        surv_0 <- gf$surv_0[52]
        surv_1 <- gf$surv_1[52]
        Y_0 <- 1-surv_0
        Y_1 <- 1-surv_1
        riskdiff <- Y_1 - Y_0
        riskratio <- Y_1/Y_0
        
        all_results[nrow(all_results)+1,] <- c(as.integer(m), Y_0, Y_1, riskdiff, riskratio)
  }
  
  #Difference in risk difference
  difference_results <- all_results  %>% 
                        select(model, riskdiff) %>% 
                        mutate(diffindiff = (riskdiff[2] - riskdiff)) %>%
                        select(-riskdiff)
                        
  
  #DATA FOR SURVIVAL CURVES - ONLY NEED THIS FOR FULLY ADJUSTED - TAKES THE LAST ENTRIES FROM ABOVE
  gf_surv_0 <- gf_surv_0 %>% 
               rename(surv = surv_0)
  
  gf_surv_1 <- gf_surv_1 %>% 
               rename(surv = surv_1) 
  
  gf_plot <- bind_rows(gf_surv_0, gf_surv_1) 
  
  
  rm(dat_model, gf_model_list, gf_model_results, gf_exp0, gf_exp0_surv, gf_exp1, gf_exp1_surv, 
     gf_surv_0, gf_surv_1, gf, surv_0, surv_1, Y_0, Y_1, riskdiff, riskratio)
  
  
  #################################### BOOTSTRAPS ############################################
  res <- NULL
  res_diff <- NULL
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
    gf_model_list <- lapply(paste("event==0 ~ exp + I(exp*time) + I(exp*timesq) + time + timesq", confounder_list), as.formula)
    gf_model_results <- lapply(gf_model_list, function(x) glm(x, data=dat_model, family=binomial()))
    
    
    boot_all_results <- data.frame(model=integer(), risk_0=numeric(), risk_1=numeric(), riskdiff=numeric(), logriskratio=numeric())
    
    
    for(m in 1:length(confounder_list)) {
      
      #create dataset with all time points for each individual under each treatment
      gf_exp0 <- expandRows(boot_dat, count=52, count.is.col=F)
      gf_exp0$time <- rep(seq(0,51), nrow(dat))
      gf_exp0$timesq <- gf_exp0$time^2
      gf_exp0$exp <- 0
      
      gf_exp1 <- gf_exp0
      gf_exp1$exp <- 1
      
      gf_exp0$p_noevent0 <- predict(gf_model_results[[m]], gf_exp0, type="response")
      gf_exp1$p_noevent1 <- predict(gf_model_results[[m]], gf_exp1, type="response")
      
      gf_exp0_surv <- gf_exp0 %>%
                      group_by(lopnr) %>%
                      mutate(surv_0 = cumprod(p_noevent0))
      
      gf_exp1_surv <- gf_exp1 %>%
                      group_by(lopnr) %>%
                      mutate(surv_1 = cumprod(p_noevent1))
      
      gf_surv_0 <- aggregate(gf_exp0_surv, by=list(gf_exp0_surv$time), FUN=mean)[c("exp", "time", "surv_0")]
      gf_surv_1 <- aggregate(gf_exp1_surv, by=list(gf_exp1_surv$time), FUN=mean)[c("exp", "time", "surv_1")]
      
      gf <- merge(gf_surv_0, gf_surv_1, by=c("time")) %>%
            arrange(time)
      
      surv_0 <- gf$surv_0[52]
      surv_1 <- gf$surv_1[52]
      Y_0 <- 1-surv_0
      Y_1 <- 1-surv_1
      riskdiff <- Y_1 - Y_0
      logriskratio <- log(Y_1/Y_0)
      
      boot_all_results[nrow(boot_all_results)+1,] <- c(as.integer(m), Y_0, Y_1, riskdiff, logriskratio)
    }
    
  res <- rbind(res, boot_all_results)
  
  #Difference in risk difference
  boot_difference_results <- boot_all_results  %>% 
                              select(model, riskdiff) %>% 
                              mutate(diffindiff = (riskdiff[2] - riskdiff)) %>%
                              select(-riskdiff)
  
  res_diff <- rbind(res_diff, boot_difference_results)
    
  }
  
  rm(boot_dat, dat_model, gf_model_list, gf_model_results, gf_exp0, gf_exp0_surv, gf_exp1, gf_exp1_surv, 
     gf_surv_0, gf_surv_1, gf, surv_0, surv_1, Y_0, Y_1, riskdiff, logriskratio, boot_all_results)
  
  ######### CREATE ESTIMATES AND CIS ########
  
  # get sd's from bootstraps - full results 
  res_sd <- data.frame()
  for (m in 1:length(confounder_list)) {
     res_sd_tmp <- res %>% 
                    filter(model==m) %>% 
                    select(-model) %>% 
                    apply(2,sd)
  
     res_sd <- rbind(res_sd, res_sd_tmp)
  }
  res_sd <- res_sd %>% 
            rename(risk_0_sd=1, risk_1_sd=2, riskdiff_sd=3, logriskratio_sd=4)
  
  #get sd's from bootstraps - difference in risk difference
  res_diff_sd <- data.frame()
  for (m in 1:length(confounder_list)) {
    res_diff_sd_tmp <- res_diff %>% 
                      filter(model==m) %>% 
                      select(-model) %>% 
                      apply(2,sd)
    
    res_diff_sd <- rbind(res_diff_sd, res_diff_sd_tmp)
  }
  res_diff_sd <- res_diff_sd %>% 
                  rename(diffindiff_sd=1)
     
  #BRING ALL REASULTS TOGETHER AND CALCULATE CI'S
  # Main
  all_results_full <- cbind(all_results, res_sd) %>% 
    
                      #Calculate 95% CIs based on bootstapped sd's
                      mutate(lcl_risk_0 = risk_0 - 1.96*risk_0_sd) %>% 
                      mutate(ucl_risk_0 = risk_0 + 1.96*risk_0_sd) %>% 
                      mutate(lcl_risk_1 = risk_1 - 1.96*risk_1_sd) %>% 
                      mutate(ucl_risk_1 = risk_1 + 1.96*risk_1_sd) %>% 
                      mutate(lcl_riskdiff = riskdiff - 1.96*riskdiff_sd) %>% 
                      mutate(ucl_riskdiff = riskdiff + 1.96*riskdiff_sd) %>%
                      mutate(lcl_riskratio = exp(log(riskratio) - 1.96*logriskratio_sd)) %>% 
                      mutate(ucl_riskratio = exp(log(riskratio) + 1.96*logriskratio_sd)) %>% 
                      select(-risk_0_sd, -risk_1_sd, -riskdiff_sd, -logriskratio_sd) %>% 
    
                      # Create results in string vars
                      mutate(outcome = paste0(outcome_title)) %>% 
                      mutate(risk1 = paste0(format(round(risk_1*100,1), nsmall=1) ,
                                              " (" , format(round(lcl_risk_1*100,1), nsmall=1), ", " ,
                                              format(round(ucl_risk_1*100,1), nsmall=1), ")" )) %>% 
                      mutate(risk0 = paste0(format(round(risk_0*100,1), nsmall=1) ,
                                              " (" , format(round(lcl_risk_0*100,1), nsmall=1), ", " ,
                                              format(round(ucl_risk_0*100,1), nsmall=1), ")" )) %>%
                      mutate(rd = paste0(format(round(riskdiff*100,1), nsmall=1) ,
                                              " (" , format(round(lcl_riskdiff*100,1), nsmall=1), ", " ,
                                              format(round(ucl_riskdiff*100,1), nsmall=1), ")" )) %>% 
                      mutate(rr = paste0(format(round(riskratio,2), nsmall=2) ,
                                              " (" , format(round(lcl_riskratio,2), nsmall=2), ", " ,
                                              format(round(ucl_riskratio,2), nsmall=2), ")" )) %>% 
                      select(outcome, model, risk1, risk0, rd, rr)
  
  results_1y <- rbind(results_1y, all_results_full)

  # Difference in risk difference
  difference_results_full <- cbind(difference_results, res_diff_sd) %>%
                              
                              #Calculate 95% CIs based on bootstapped sd's
                              mutate(lcl_diffindiff = diffindiff - 1.96*diffindiff_sd) %>% 
                              mutate(ucl_diffindiff = diffindiff + 1.96*diffindiff_sd) %>%
                              select(-diffindiff_sd) %>% 
    
                              # Create results in string vars
                              mutate(outcome = paste0(outcome_title)) %>% 
                              mutate(rd_difftofullyadj = paste0(format(round(diffindiff*100,1), nsmall=1) ,
                                                 " (" , format(round(lcl_diffindiff*100,1), nsmall=1), ", " ,
                                               format(round(ucl_diffindiff*100,1), nsmall=1), ")" )) %>% 
                              select(outcome, model, rd_difftofullyadj)
    
  difference_results_1y <- rbind(difference_results_1y, difference_results_full)

  
}

# WRITE RESULTS TO .TXT
write.table(results_1y, paste0("logfiles/an_gformula_allmodels_", period, ".txt"), sep="\t", quote=FALSE, row.names=FALSE)
write.table(difference_results_1y, paste0("logfiles/an_gformula_rddiff_", period, ".txt"), sep="\t", quote=FALSE, row.names=FALSE)

