

policy_horizon <- 5 #for now, implement over 5 years and calculate costs/outcomes over 5 years
analytic_horizon <- 5

#options to be added later
contact_only <- 0 #base case = include TPT too (0); sensitivity analysis = only contact investigation (1); or TPT Vs. CI (2)
reinfect <- 0 #0=base case, or annual risk of infection
visits_3hp <- 1 #1 or 3 visits (1 in main analysis). initiation visit is separate (part of screening)
cxr_screen_5plus <- 1 #whether to include CXR in the screening algorithm for contacts aged 5+ (w/ symptom screen, vs. symptom screen alone)
tb_test_child <- "cxr" #cxr or xpert as a test for children < 5
tpt_wastage <- "full courses"  #wastage factor for drugs (e.g. 0.1), or cost out "full courses" for all initiators

#implementation of options
child_params$cxr_screen <- 0 #only option built in currently - CXR is only used as a test, not screening tool, for children
adol_params$cxr_screen <- cxr_screen_5plus #built into sensitivity/specificity of screening - so only affects costs
adult_params$cxr_screen <- cxr_screen_5plus
child_params$tb_test <- tb_test_child
adol_params$tb_test <- "xpert" #only option built in currently
adult_params$tb_test <- "xpert" #only option built in currently
if(reinfect!=0) {
  child_params$p_infect <- reinfect
  adol_params$p_infect <- reinfect
  adult_params$p_infect <- reinfect
}
child_params$n_visit_3hp <- visits_3hp
adol_params$n_visit_3hp <- visits_3hp
adult_params$n_visit_3hp <- visits_3hp

child_params$p_screen <- child_params$p_symptom
child_params$p_tb_screen <- child_params$p_tb_symptom
child_params$p_negscreen_tb <- child_params$p_asymptom_tb
child_params$sens <- child_params$cxr_sens*(tb_test_child=="cxr") + child_params$xpert_sens*(tb_test_child=="xpert")
child_params$spec <- child_params$cxr_spec*(tb_test_child=="cxr") + child_params$xpert_spec*(tb_test_child=="xpert")
adol_params$p_screen <- adol_params$p_symptom_cxr*(cxr_screen_5plus==1) + adol_params$p_symptom*(cxr_screen_5plus==0)
adol_params$p_tb_screen <- adol_params$p_tb_symptom_cxr*(cxr_screen_5plus==1) + adol_params$p_tb_symptom*(cxr_screen_5plus==0)
adol_params$p_negscreen_tb <- adol_params$p_asymptom_negcxr_tb*(cxr_screen_5plus==1) + 
  adol_params$p_asymptom_tb*(cxr_screen_5plus==0)
adol_params$sens <- adol_params$xpert_sens
adol_params$spec <- adol_params$xpert_spec
adult_params$p_screen <- adult_params$p_symptom_cxr*(cxr_screen_5plus==1) + adult_params$p_symptom*(cxr_screen_5plus==0)
adult_params$p_tb_screen <- adult_params$p_tb_symptom_cxr*(cxr_screen_5plus==1) + adult_params$p_tb_symptom*(cxr_screen_5plus==0)
adult_params$p_negscreen_tb <- adult_params$p_asymptom_negcxr_tb*(cxr_screen_5plus==1) + 
  adult_params$p_asymptom_tb*(cxr_screen_5plus==0)
adult_params$sens <- adult_params$xpert_sens
adult_params$spec <- adult_params$xpert_spec
p_hh_child_tpt <- 0*(child_params$cxr_screen==1)*(child_params$tb_test=="xpert") +
  other_params$p_hh_child_xpert_tpt*(child_params$cxr_screen==0)*(child_params$tb_test=="xpert") +
  other_params$p_hh_child_cxr_tpt*(child_params$cxr_screen==0)*(child_params$tb_test=="cxr")
p_hh_adol_tpt <- other_params$p_hh_adol_symptom_cxr_tpt*(adol_params$cxr_screen==1)*(adol_params$tb_test=="xpert") +
  other_params$p_hh_adol_symptom_tpt*(adol_params$cxr_screen==0)*(adol_params$tb_test=="xpert") +
  0*(adol_params$cxr_screen==0)*(adol_params$tb_test=="cxr")
p_hh_adult_tpt <- other_params$p_hh_adult_symptom_cxr_tpt*(adult_params$cxr_screen==1)*(adult_params$tb_test=="xpert") +
  other_params$p_hh_adult_symptom_tpt*(adult_params$cxr_screen==0)*(adult_params$tb_test=="xpert") +
  0*(adult_params$cxr_screen==0)*(adult_params$tb_test=="cxr")

child_params$wastage <- ifelse(tpt_wastage=="full courses", 0, 
                               as.double(tpt_wastage))
child_params$part_course_cost <- ifelse(tpt_wastage=="full courses",
                                        1, 0.5)
adol_params$wastage <- child_params$wastage
adol_params$part_course_cost <- child_params$part_course_cost
adult_params$wastage <- child_params$wastage
adult_params$part_course_cost <- child_params$part_course_cost

if(type=="i4tb") {
  comp_scen <- "sq"
  start_yr <- 2020
  end_yr <- start_yr+analytic_horizon
  policy_end_yr <- start_yr + policy_horizon 
  #reshape targets so that scenario and treatment types are columns
  names(targets) <- sub("_covg$", "", names(targets)) #remove "_covg" from col names - makes pivoting easier
  #add in columns for 0 1HP under the sq, dir, and  cat scenarios
  targets <- targets %>% 
    filter(!(code %in% exclusions_i4tb))
  #truncate impact of direct scenario to specified year
  targets <- targets %>% mutate(child_3hp_dir=if_else(year>dir_end, 0, child_3hp_dir),
                                adol_3hp_dir=if_else(year>dir_end, 0, adol_3hp_dir),
                                adult_3hp_dir=if_else(year>dir_end, 0, adult_3hp_dir),
                                plhiv_new_3hp_dir=if_else(year>dir_end, 0, plhiv_new_3hp_dir),
                                plhiv_backlog_3hp_dir=if_else(year>dir_end, 0, plhiv_backlog_3hp_dir),
                                child_ipt_dir=if_else(year>dir_end, child_ipt_sq, child_ipt_dir),
                                adol_ipt_dir=if_else(year>dir_end, adol_ipt_sq, adol_ipt_dir),
                                adult_ipt_dir=if_else(year>dir_end, adult_ipt_sq, adult_ipt_dir),
                                plhiv_new_ipt_dir=if_else(year>dir_end, plhiv_new_ipt_sq, plhiv_new_ipt_dir),
                                plhiv_backlog_ipt_dir=if_else(year>dir_end, plhiv_backlog_ipt_sq, plhiv_backlog_ipt_dir))
  targets <- pivot_longer(targets, cols=c(ends_with("sq"), ends_with("dir"), 
                                          ends_with("cat"), ends_with("cat1")),
                          names_to=c(".value", "scenario"),
                          names_sep="_(?=[^_]+$)") #this is regex for "last underscore only"
}
if(type=="pub") {
  comp_scen <- "none"
  start_yr <- 2023
  end_yr <- start_yr + analytic_horizon - 1
  policy_end_yr <- start_yr + policy_horizon - 1
  pop_calcs <- pivot_longer(pop_calcs %>% filter(year>=start_yr & !(code %in% exclusions_pub)), 
                            cols=c(ends_with("tpt"), 
                                   ends_with("none")),
                            names_to=c(".value", "scenario"),
                            names_sep="_(?=[^_]+$)") #this is regex for "last underscore only"
  pop_calcs <- pop_calcs %>% mutate(tpt_type=tpt_type)
  pop_calcs <- pop_calcs %>% 
    mutate(child_ipt=if_else(tpt_type=="ipt", child_covg, 0),
           adol_ipt=if_else(tpt_type=="ipt", adol_covg, 0),
           adult_ipt=if_else(tpt_type=="ipt", adult_covg, 0),
           child_3hp=if_else(tpt_type=="3hp", child_covg, 0),
           adol_3hp=if_else(tpt_type=="3hp", adol_covg, 0),
           adult_3hp=if_else(tpt_type=="3hp", adult_covg, 0),
           child_1hp=if_else(tpt_type=="1hp", child_covg, 0),
           adol_1hp=if_else(tpt_type=="1hp", adol_covg, 0),
           adult_1hp=if_else(tpt_type=="1hp", adult_covg, 0))
  targets <- pop_calcs
  if(contact_only==2) {
    targets <- targets %>% filter(scenario=="tpt")
    targets <- rbind(targets, targets %>% mutate(scenario="contact"))
    comp_scen <- "contact"
    #no TPT, but we'll use covg to estimate #s investigated. wrong for PWH but that's fine. 
  }
}

#loop over params in PSA 
child_params_all <- child_params
adol_params_all <- adol_params
adult_params_all <- adult_params
#save all output
child_comb_all <- list()
adol_comb_all <- list()
adult_comb_all <- list()
#save CEA output (for CEAC only)
child_cea_all_all <- list()
adol_cea_all_all <- list()
adult_cea_all_all <- list()
contacts_cea_all_all <- list()
if(type=="i4tb") {
  #compare each scenario against status quo
  child_cea_dir_all <- list()
  child_cea_cat_all <- list()
  child_cea_cat1_all <- list()
  adol_cea_dir_all <- list()
  adol_cea_cat_all <- list()
  adol_cea_cat1_all <- list()
  adult_cea_dir_all <- list()
  adult_cea_cat_all <- list()
  adult_cea_cat1_all <- list()
  contacts_cea_dir_all <- list()
  contacts_cea_cat_all <- list()
  contacts_cea_cat1_all <- list()
}

#warnings here are fine - for params that aren't in use
if(price_tpt=="vary") { 
  #vary 3hp price only - all other params fixed at country-specific means
  child_params <- sapply(names(child_params_all), function(x)
    mean(child_params_all[[x]]), simplify=F, USE.NAMES=T)
  adol_params <- sapply(names(adol_params_all), function(x)
    mean(adol_params_all[[x]]), simplify=F, USE.NAMES=T)
  adult_params <- sapply(names(adult_params_all), function(x)
    mean(adult_params_all[[x]]), simplify=F, USE.NAMES=T)
  child_params$tb_test <- child_params_all$tb_test #character variable
  adol_params$tb_test <- adol_params_all$tb_test #character variable
  adult_params$tb_test <- adult_params_all$tb_test #character variable
  cost_params <- sapply(names(cost_params), function(x)
    as.data.frame(t(colMeans(cost_params[[x]]))), simplify=F, USE.NAMES=T)
  p_notif_child_use <- unlist(sapply(names(p_notif_child), function(x)
    mean(p_notif_child[[x]]), simplify=F, USE.NAMES=T))
  p_notif_adol_use <- unlist(sapply(names(p_notif_adol), function(x)
    mean(p_notif_adol[[x]]), simplify=F, USE.NAMES=T))
  p_notif_1524_use <- unlist(sapply(names(p_notif_1524), function(x)
    mean(p_notif_1524[[x]]), simplify=F, USE.NAMES=T))
  p_notif_adult_use <- unlist(sapply(names(p_notif_adult), function(x)
    mean(p_notif_adult[[x]]), simplify=F, USE.NAMES=T))
  p_success_child_use <- unlist(sapply(names(p_success_child), function(x)
    mean(p_success_child[[x]]), simplify=F, USE.NAMES=T))
  p_success_adult_use <- unlist(sapply(names(p_success_adult), function(x)
    mean(p_success_adult[[x]]), simplify=F, USE.NAMES=T))
  other_params <- sapply(names(other_params), function(x)
    mean(other_params[[x]]), simplify=F, USE.NAMES=T)
  #3hp price is determined by array value - only 1 model run per array, only 500 arrays
  c_3hp_vec <- seq(from=0.1, to=50, by=0.1)
  index_start <- 1
  index_end <- 1
}
if(cost_tbtx=="vary") { 
  #vary tb treatment cost only - all other params fixed at country-specific means
  child_params <- sapply(names(child_params_all), function(x)
    mean(child_params_all[[x]]), simplify=F, USE.NAMES=T)
  adol_params <- sapply(names(adol_params_all), function(x)
    mean(adol_params_all[[x]]), simplify=F, USE.NAMES=T)
  adult_params <- sapply(names(adult_params_all), function(x)
    mean(adult_params_all[[x]]), simplify=F, USE.NAMES=T)
  child_params$tb_test <- child_params_all$tb_test #character variable
  adol_params$tb_test <- adol_params_all$tb_test #character variable
  adult_params$tb_test <- adult_params_all$tb_test #character variable
  cost_params <- sapply(names(cost_params), function(x)
    as.data.frame(t(colMeans(cost_params[[x]]))), simplify=F, USE.NAMES=T)
  p_notif_child_use <- unlist(sapply(names(p_notif_child), function(x)
    mean(p_notif_child[[x]]), simplify=F, USE.NAMES=T))
  p_notif_adol_use <- unlist(sapply(names(p_notif_adol), function(x)
    mean(p_notif_adol[[x]]), simplify=F, USE.NAMES=T))
  p_notif_1524_use <- unlist(sapply(names(p_notif_1524), function(x)
    mean(p_notif_1524[[x]]), simplify=F, USE.NAMES=T))
  p_notif_adult_use <- unlist(sapply(names(p_notif_adult), function(x)
    mean(p_notif_adult[[x]]), simplify=F, USE.NAMES=T))
  p_success_child_use <- unlist(sapply(names(p_success_child), function(x)
    mean(p_success_child[[x]]), simplify=F, USE.NAMES=T))
  p_success_adult_use <- unlist(sapply(names(p_success_adult), function(x)
    mean(p_success_adult[[x]]), simplify=F, USE.NAMES=T))
  other_params <- sapply(names(other_params), function(x)
    mean(other_params[[x]]), simplify=F, USE.NAMES=T)
  #tb tx cost is determined by array value - only 1 model run per array, only 500 arrays
  c_tbtx_vec <- seq(from=10, to=5000, by=10) #length of 500
  index_start <- 1
  index_end <- 1
}
for(j in index_start:index_end) {
  print(j)
  
  if(price_tpt!="vary" & cost_tbtx!="vary") {
    #extract jth elements of params that are varied in the PSA only (costs of 3HP and 1HP vary by yr, not param set)
    child_params <- sapply(names(child_params_all), function(x)
      (if(length(child_params_all[[x]])==1 | x %in% c("c_3hp", "c_1hp")) 
        child_params_all[[x]] else child_params_all[[x]][j]),
      simplify=F, USE.NAMES=T)
    adol_params <- sapply(names(adol_params_all), function(x)
      (if(length(adol_params_all[[x]])==1 | x %in% c("c_3hp", "c_1hp")) 
        adol_params_all[[x]] else adol_params_all[[x]][j]),    
      simplify=F, USE.NAMES=T)
    adult_params <- sapply(names(adult_params_all), function(x)
      (if(length(adult_params_all[[x]])==1 | x %in% c("c_3hp", "c_1hp")) 
        adult_params_all[[x]] else adult_params_all[[x]][j]),    
      simplify=F, USE.NAMES=T)
    p_notif_child_use <- unlist(sapply(names(p_notif_child), function(x)
      p_notif_child[[x]][[j]], simplify=F, USE.NAMES=T))
    p_notif_adol_use <- unlist(sapply(names(p_notif_adol), function(x)
      p_notif_adol[[x]][[j]], simplify=F, USE.NAMES=T))
    p_notif_1524_use <- unlist(sapply(names(p_notif_1524), function(x)
      p_notif_1524[[x]][[j]], simplify=F, USE.NAMES=T))
    p_notif_adult_use <- unlist(sapply(names(p_notif_adult), function(x)
      p_notif_adult[[x]][[j]], simplify=F, USE.NAMES=T))
    p_success_child_use <- unlist(sapply(names(p_success_child), function(x)
      p_success_child[[x]][[j]], simplify=F, USE.NAMES=T))
    p_success_adult_use <- unlist(sapply(names(p_success_adult), function(x)
      p_success_adult[[x]][[j]], simplify=F, USE.NAMES=T))
  }
  if(meta_psa==1|meta_psa==2) {
    child_params$c_3hp <- child_params$c_3hp_psa
    child_params$c_1hp <- child_params$c_1hp_psa
    adol_params$c_3hp <- adol_params$c_3hp_psa
    adol_params$c_1hp <- adol_params$c_1hp_psa
    adult_params$c_3hp <- adult_params$c_3hp_psa
    adult_params$c_1hp <- adult_params$c_1hp_psa
  }
  if(meta_psa==2) {
    p_child_die[["ALL"]] <- p_child_die_all[["ALL"]][[j]]
    p_adol_die[["ALL"]] <- p_adol_die_all[["ALL"]][[j]]
    p_adult_die[["ALL"]] <- p_adult_die_all[["ALL"]][[j]]
    life_exp_child[["ALL"]] <- life_exp_child_all[["ALL"]][[j]]
    life_exp_adol[["ALL"]] <- life_exp_adol_all[["ALL"]][[j]]
    life_exp_adult[["ALL"]] <- life_exp_adult_all[["ALL"]][[j]]
  }
  if(price_tpt=="vary") {
    child_params$c_3hp <- c_3hp_vec[[array]]
    adol_params$c_3hp <- c_3hp_vec[[array]]
    adult_params$c_3hp <- c_3hp_vec[[array]]
  }
  if(cost_tbtx=="vary") {
    cost_params <- sapply(names(cost_params), function(x) {
      cost_params[[x]][["tb_tx"]] <- c_tbtx_vec[[array]]
      cost_params[[x]]
    }, USE.NAMES=T, simplify=F)
  }
  #separate into 3 dataframes - one for each target population group
  #hh contacts - targets include initiation, so backcalculate # contacts investigated
  child <- targets %>% select(-pop) %>% 
    rename("initiate_ipt_covg"="child_ipt", 
           "initiate_3hp_covg"="child_3hp",
           "initiate_1hp_covg"="child_1hp",
           "pop"="pop04", "hh"="hh_child") %>%
    mutate(total=tb_notif_new*hh,
           hh_contact_covg = initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg, #initiate_covg is now before adjusting for screening/testing negative and initiation ratios
           prop_ipt=if_else(initiate_ipt_covg==0|contact_only==1|scenario=="contact", 0,
                            initiate_ipt_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
           prop_3hp=if_else(initiate_3hp_covg==0|contact_only==1|scenario=="contact", 0, 
                            initiate_3hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
           prop_1hp=if_else(initiate_1hp_covg==0|contact_only==1|scenario=="contact", 0, 
                            initiate_1hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
           contact_invest_num = hh_contact_covg*total,
           initiate_num=if_else(scenario=="contact", 0, contact_invest_num*p_hh_child_tpt[j]*child_params$p_initiate),
           p_initiate=if_else(contact_only==1|scenario=="contact", 0, child_params$p_initiate)) %>%
    select(country, code, year, scenario,
           pop, hh, total, prop_ipt, prop_3hp, prop_1hp, contact_invest_num, initiate_num, p_initiate)
  
  adol <- targets %>% select(-pop) %>% 
    rename("initiate_ipt_covg"="adol_ipt", "initiate_3hp_covg"="adol_3hp",
           "initiate_1hp_covg"="adol_1hp",
           "pop"="pop514", "hh"="hh_adol") %>%
    mutate(total=tb_notif_new*hh,
           hh_contact_covg = initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg, #initiate_covg is now before adjusting for screening/testing negative and initiation ratios
           prop_ipt=if_else(initiate_ipt_covg==0|contact_only==1|scenario=="contact", 0,
                            initiate_ipt_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
           prop_3hp=if_else(initiate_3hp_covg==0|contact_only==1|scenario=="contact", 0, 
                            initiate_3hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
           prop_1hp=if_else(initiate_1hp_covg==0|contact_only==1|scenario=="contact", 0, 
                            initiate_1hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
           contact_invest_num = hh_contact_covg*total,
           initiate_num=if_else(scenario=="contact", 0, contact_invest_num*p_hh_adol_tpt[j]*adol_params$p_initiate),
           p_initiate=if_else(contact_only==1|scenario=="contact", 0, adol_params$p_initiate)) %>%
    select(country, code, year, scenario, 
           pop, hh, total, prop_ipt, prop_3hp, prop_1hp, contact_invest_num, initiate_num, p_initiate)
  
  adult <- targets %>% select(-pop) %>% 
    rename("initiate_ipt_covg"="adult_ipt", "initiate_3hp_covg"="adult_3hp",
           "initiate_1hp_covg"="adult_1hp",
           "pop"="popadult", "hh"="hh_adult") %>%
    mutate(total=tb_notif_new*hh,
           hh_contact_covg = initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg, #initiate_covg is now before adjusting for screening/testing negative and initiation ratios
           prop_ipt=if_else(initiate_ipt_covg==0|contact_only==1|scenario=="contact", 0,
                            initiate_ipt_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
           prop_3hp=if_else(initiate_3hp_covg==0|contact_only==1|scenario=="contact", 0, 
                            initiate_3hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
           prop_1hp=if_else(initiate_1hp_covg==0|contact_only==1|scenario=="contact", 0, 
                            initiate_1hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
           contact_invest_num = hh_contact_covg*total,
           initiate_num=if_else(scenario=="contact", 0, contact_invest_num*p_hh_adult_tpt[j]*adult_params$p_initiate),
           p_initiate=if_else(contact_only==1|scenario=="contact", 0, adult_params$p_initiate)) %>%
    select(country, code, year, scenario, 
           pop, hh, total, prop_ipt, prop_3hp, prop_1hp, contact_invest_num, initiate_num, p_initiate)
  
  child <- child %>% mutate(p_notif=p_notif_child_use[code],
                            p_success=p_success_child_use[code],
                            p_die=p_child_die[code])
  adol <- adol %>% mutate(p_notif=p_notif_adol_use[code],
                          p_success=p_success_child_use[code],
                          p_die=p_adol_die[code])
  adult <- adult %>% mutate(p_notif=p_notif_adult_use[code],
                            p_success=p_success_adult_use[code],
                            p_die=p_adult_die[code])
  
  if(contact_only==1) {
    #child_params$p_initiate <- 0
    #adol_params$p_initiate <- 0
    #adult_params$p_initiate <- 0
    child <- child %>% mutate(initiate_num=0)
    adol <- adol %>% mutate(initiate_num=0)
    adult <- adult %>% mutate(initiate_num=0)
  }
  #estimate outcomes of contact investigation
  child <- calc_contact_invest(child, child_params)
  adol <- calc_contact_invest(adol, adol_params)
  adult <- calc_contact_invest(adult, adult_params)
  #confirmed that initiate_ipt + initiate_3hp + initiate_1hp = initiate_num from targets for all 3 contact groups
  #confirmed that contact_invest_num + no_contact_tb + no_contact_ltbi + no_contact_no_tb = total
  #confirmed that contact_invest_tb_tp + contact_invest_tb_tn + contact_invest_tb_fp + contact_invest_tb_fn = contact_invest_num
  
  #calculate TPT-related events and outcomes (toxicity, completion) to calculate costs
  #calculate breakdown of completion and latent TB status
  child <- calc_tpt_outcomes_contacts(child, child_params) 
  adol <- calc_tpt_outcomes_contacts(adol, adol_params)
  adult <- calc_tpt_outcomes_contacts(adult, adult_params)
  if(FALSE) {
    #confirmed that ltbi + active tb + no tb + tb_deaths + non_tb_deaths = total
    child$ltbi + child$active_tb + child$no_tb + child$tb_deaths + child$non_tb_deaths - child$total
    #graph TB status by year, country, scenario
    child_test <- child %>% pivot_longer(cols=c("ltbi", "active_tb", "no_tb", "tb_deaths" , "non_tb_deaths"),
                                         names_to="tb_state", values_to="num")
    ggplot(child_test %>% filter(country %in% unique(child$country)[1:12]), aes(x=year, y=num, fill=tb_state)) +
      geom_col(position="stack") + facet_wrap(~country+scenario) + theme_bw()
    #more notifications w/ I4TB because of more contact investigations, but total cases should be equal - check this
    ggplot(child %>% filter(country %in% unique(child$country)[1:12]), 
           aes(x=year, y=cases)) +
      geom_line() + facet_wrap(~country+scenario) + theme_bw()
    
  }
  
  #calculate out "horizon" years for each group - child contacts
  child_all <- child %>% mutate(yrs_out=0, 
                                cum_tb_deaths=tb_deaths,
                                cum_non_tb_deaths=non_tb_deaths)
  for(i in 1:analytic_horizon) {
    #children: mortality and reactivation rates vary over time, notification and mortality varies by country
    child_prev <- child_all %>% filter(yrs_out==i-1)
    if(i <=2) {
      child_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(child_prev %>% filter(code==x), p_notif_child_use[[x]], p_success_child_use[[x]], 
                          child_params$p_reactivate_02, child_params$p_die_tb, 
                          child_params$p_die_tb_tx, p_child_die[[x]],
                          child_params$p_infect, child_params$ltbi_protect,
                          child_params$prop_fast))
    } else if(i==3) {
      child_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(child_prev %>% filter(code==x), p_notif_child_use[[x]], p_success_child_use[[x]],  
                          child_params$p_reactivate_25, child_params$p_die_tb, 
                          child_params$p_die_tb_tx, p_child_die[[x]],
                          child_params$p_infect, child_params$ltbi_protect,
                          child_params$prop_fast))
    } else if(i==4|i==5) {
      child_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(child_prev %>% filter(code==x), 
                          rowMeans(cbind(p_notif_child_use, p_notif_adol_use))[[x]],
                          p_success_child_use[[x]], 
                          child_params$p_reactivate_25, 
                          mean(c(child_params$p_die_tb, adol_params$p_die_tb)),
                          mean(c(child_params$p_die_tb_tx, adol_params$p_die_tb_tx)),
                          rowMeans(cbind(p_child_die, p_adol_die))[[x]],
                          child_params$p_infect, child_params$ltbi_protect,
                          child_params$prop_fast))
    } else if(i>5 & i<=10) {
      child_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(child_prev %>% filter(code==x), p_notif_adol_use[[x]], p_success_child_use[[x]],  
                          child_params$p_reactivate_510, adol_params$p_die_tb, 
                          adol_params$p_die_tb_tx, p_adol_die[[x]],
                          child_params$p_infect, child_params$ltbi_protect,
                          child_params$prop_fast))
    } else if(i>10) {
      child_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(child_prev %>% filter(code==x), p_notif_1524_use[[x]], 
                          p_success_adult_use[[x]], 
                          child_params$p_reactivate_10plus, 
                          mean(c(adol_params$p_die_tb, adult_params$p_die_tb)),
                          mean(c(adol_params$p_die_tb_tx, adult_params$p_die_tb_tx)), 
                          rowMeans(cbind(p_adult_die, p_adol_die))[[x]],
                          child_params$p_infect, child_params$ltbi_protect,
                          child_params$prop_fast))
    }
    child_cur <- bind_rows(child_cur)
    child_cur <- child_cur %>% mutate(yrs_out=i)
    child_all <- bind_rows(child_all, child_cur)
  }
  
  #adolescent contacts
  adol_all <- adol %>% mutate(yrs_out=0,
                              cum_tb_deaths=tb_deaths,
                              cum_non_tb_deaths=non_tb_deaths) 
  for(i in 1:analytic_horizon) {
    #adolescents: reactivation rates vary over time, notification and mortality varies by country
    adol_prev <- adol_all %>% filter(yrs_out==i-1)
    if(i <=2) {
      adol_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(adol_prev %>% filter(code==x), p_notif_adol_use[[x]], p_success_child_use[[x]], 
                          adol_params$p_reactivate_02, adol_params$p_die_tb, 
                          adol_params$p_die_tb_tx, p_adol_die[[x]],
                          adol_params$p_infect, adol_params$ltbi_protect,
                          adol_params$prop_fast))
    } else if(i>2 & i<=5) {
      adol_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(adol_prev %>% filter(code==x), p_notif_adol_use[[x]], p_success_child_use[[x]],  
                          adol_params$p_reactivate_25, adol_params$p_die_tb, 
                          adol_params$p_die_tb_tx, p_adol_die[[x]],
                          adol_params$p_infect, adol_params$ltbi_protect,
                          adol_params$prop_fast))
    } else if(i>5 & i<=10) {
      adol_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(adol_prev %>% filter(code==x),
                          rowMeans(cbind(p_notif_1524_use, p_notif_adol_use))[[x]],
                          rowMeans(cbind(p_success_child_use, p_success_adult_use))[[x]],
                          adol_params$p_reactivate_510, 
                          mean(c(adol_params$p_die_tb, adult_params$p_die_tb)),
                          mean(c(adol_params$p_die_tb_tx, adult_params$p_die_tb_tx)), 
                          rowMeans(cbind(p_adult_die, p_adol_die))[[x]],
                          adol_params$p_infect, adol_params$ltbi_protect,
                          adol_params$prop_fast))
    } else if(i>10) {
      adol_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(adol_prev %>% filter(code==x),
                          rowMeans(cbind(p_notif_1524_use, p_notif_adult_use))[[x]], 
                          p_success_adult_use[[x]],
                          adol_params$p_reactivate_10plus, 
                          mean(c(adol_params$p_die_tb, adult_params$p_die_tb)),
                          mean(c(adol_params$p_die_tb_tx, adult_params$p_die_tb_tx)), 
                          rowMeans(cbind(p_adult_die, p_adol_die))[[x]],
                          adol_params$p_infect, adol_params$ltbi_protect,
                          adol_params$prop_fast))
    }
    adol_cur <- bind_rows(adol_cur)
    adol_cur <- adol_cur %>% mutate(yrs_out=i)
    adol_all <- bind_rows(adol_all, adol_cur)
  }
  
  #adult contacts
  adult_all <- adult %>% mutate(yrs_out=0,
                                cum_tb_deaths=tb_deaths,
                                cum_non_tb_deaths=non_tb_deaths) 
  for(i in 1:analytic_horizon) {
    #adults: reactivation rates vary over time, notification and mortality varies by country
    adult_prev <- adult_all %>% filter(yrs_out==i-1)
    if(i <=2) {
      adult_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(adult_prev %>% filter(code==x), p_notif_adult_use[[x]], p_success_adult_use[[x]],
                          adult_params$p_reactivate_02, adult_params$p_die_tb, 
                          adult_params$p_die_tb_tx, p_adult_die[[x]],
                          adult_params$p_infect, adult_params$ltbi_protect,
                          adult_params$prop_fast))
    } else if(i>2 & i<=5) {
      adult_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(adult_prev %>% filter(code==x), p_notif_adult_use[[x]], p_success_adult_use[[x]],
                          adult_params$p_reactivate_25, adult_params$p_die_tb, 
                          adult_params$p_die_tb_tx, p_adult_die[[x]],
                          adult_params$p_infect, adult_params$ltbi_protect,
                          adult_params$prop_fast))
    } else if(i>5 & i<=10) {
      adult_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(adult_prev %>% filter(code==x), p_notif_adult_use[[x]], p_success_adult_use[[x]], 
                          adult_params$p_reactivate_510, adult_params$p_die_tb, 
                          adult_params$p_die_tb_tx, p_adult_die[[x]],
                          adult_params$p_infect, adult_params$ltbi_protect,
                          adult_params$prop_fast))
    } else if(i>10) {
      adult_cur <- lapply(unique(targets$code), function(x)
        model_tb_contacts(adult_prev %>% filter(code==x), p_notif_adult_use[[x]], p_success_adult_use[[x]], 
                          adult_params$p_reactivate_10plus, adult_params$p_die_tb, 
                          adult_params$p_die_tb_tx, p_adult_die[[x]],
                          adult_params$p_infect, adult_params$ltbi_protect,
                          adult_params$prop_fast))
    }
    adult_cur <- bind_rows(adult_cur)
    adult_cur <- adult_cur %>% mutate(yrs_out=i)
    adult_all <- rbind(adult_all, adult_cur)
  }
  
  #calculate totals per year (combining cohorts from different years) and include info from contact investigation
  child_comb <- combine_yrs(child_all, "child", policy_end_yr, end_yr)
  adol_comb <- combine_yrs(adol_all, "adol", policy_end_yr, end_yr)
  adult_comb <- combine_yrs(adult_all, "adult", policy_end_yr, end_yr)
  #confirm cum_tb_deaths + cum_non_tb_deaths + ltbi + active_tb + no_tb =total (contacts) or plhiv
  if(FALSE) {
    adult_comb$cum_tb_deaths + adult_comb$cum_non_tb_deaths + adult_comb$ltbi +
      adult_comb$active_tb + adult_comb$no_tb - (adult_comb$total)
    sum(adult_comb$cum_tb_deaths + adult_comb$cum_non_tb_deaths + adult_comb$ltbi +
          adult_comb$active_tb + adult_comb$no_tb  - (adult_comb$total))
  }
  #quick graphical checks on epi output
  if(FALSE) {
    #graph output for testing - for country, by scenario, multiple outcomes
    ggplot(adult_comb %>% filter(country=="Cambodia"), 
           aes(x=year)) +
      geom_line(aes(y=active_tb, color="Active TB")) +
      geom_line(aes(y=no_tb, color="No TB")) +
      geom_line(aes(y=ltbi, color="Latent TB")) +
      geom_line(aes(y=cum_tb_deaths, color="Cumulative TB Deaths")) +
      facet_wrap(~scenario, ncol=3) +
      labs(x="Years", y="Population", color="") +
      theme_bw() + theme(panel.grid=element_blank())
    #deaths (or cases), by country and scenario
    plot_list <- list()
    for(i in unique(targets$country)) {
      fig <- ggplot(adult_comb %>% filter(country==i), 
                    aes(x=year, y=cases, color=scenario)) +
        geom_line() +
        labs(x="Years", y="cases", color="") + ggtitle(i) +
        theme_bw() + theme(panel.grid=element_blank())
      plot_list[[i]] <- fig
    }
    plot <- ggarrange(plotlist=plot_list, common.legend=T, legend="right", align="hv")
  }
  
  #calculate DALYs and discounted DALYs
  child_comb <-  calc_dalys(as.data.table(child_comb), "child", life_exp_child, other_params$dw_tb[[j]], 
                            other_params$dw_notb, 1, other_params$dur_tb_tx, other_params$disc_fac[[j]],
                            start_yr)
  adol_comb <-  calc_dalys(as.data.table(adol_comb), "adol", life_exp_adol, other_params$dw_tb[[j]], 
                           other_params$dw_notb, 1, other_params$dur_tb_tx, other_params$disc_fac[[j]],
                           start_yr)
  adult_comb <-  calc_dalys(as.data.table(adult_comb), "adult", life_exp_adult, other_params$dw_tb[[j]], 
                            other_params$dw_notb, 1, other_params$dur_tb_tx, other_params$disc_fac[[j]],
                            start_yr)

  #calculate costs (stratified by type) and discounted costs (total)
  child_comb <- lapply(unique(child_comb$code), function(x) 
    calc_costs(child_comb %>% filter(code==x), "child", cost_params[[x]][j, ], 
               costs_i4tb_impl %>% filter(code==x), child_params, 
               other_params$disc_fac[[j]], start_yr))
  child_comb <- as.data.table(rbindlist(child_comb))
  adol_comb <- lapply(unique(adol_comb$code), function(x) 
    calc_costs(adol_comb %>% filter(code==x), "adol", cost_params[[x]][j, ], 
               costs_i4tb_impl %>% filter(code==x), adol_params, 
               other_params$disc_fac[[j]], start_yr))
  adol_comb <- as.data.table(rbindlist(adol_comb))
  adult_comb <- lapply(unique(adult_comb$code), function(x) 
    calc_costs(adult_comb %>% filter(code==x), "adult", cost_params[[x]][j, ], 
               costs_i4tb_impl %>% filter(code==x), adult_params, 
               other_params$disc_fac[[j]], start_yr))
  adult_comb <- as.data.table(rbindlist(adult_comb))
  
  if(type=="i4tb") {
    #remove direct scenario for non-I4TB countries
    i4tb_countries <- country_info %>% filter(i4tb==1) %>% pull(code)
    child_comb <- left_join(child_comb, country_info %>% select(code, i4tb),
                            by="code")
    child_comb <- child_comb %>% filter(i4tb==1|scenario!="dir")
    adol_comb <- left_join(adol_comb, country_info %>% select(code, i4tb),
                           by="code")
    adol_comb <- adol_comb %>% filter(i4tb==1|scenario!="dir")
    adult_comb <- left_join(adult_comb, country_info %>% select(code, i4tb),
                            by="code")
    adult_comb <- adult_comb %>% filter(i4tb==1|scenario!="dir")
    
    #calculate cost-effectiveness for household contacts 
    id_cols <- child_comb %>% filter(year==(end_yr)) %>% select(country, code, scenario)
    child_cea <- child_comb %>% filter(year==(end_yr)) %>% ungroup() %>%
      select(cum_tb_deaths, cum_yll, cum_dalys, cum_costs,
             cum_disc_dalys, cum_disc_costs)
    adol_cea <- adol_comb %>% filter(year==(end_yr)) %>% ungroup() %>%
      select(cum_tb_deaths, cum_yll, cum_dalys, cum_costs,
             cum_disc_dalys, cum_disc_costs)
    adult_cea <- adult_comb %>% filter(year==(end_yr)) %>% ungroup() %>%
      select(cum_tb_deaths, cum_yll, cum_dalys, cum_costs,
             cum_disc_dalys, cum_disc_costs)
    contacts <- child_cea + adol_cea + adult_cea
    contacts <- bind_cols(id_cols, contacts)
    
    #CEA for status quo vs. catalytic w/ 3HP vs. catalytic w/ 1HP
    contacts_cea <- lapply(unique(country_info$code), function(x)
      calc_CEA(as.data.table(contacts)[code==x & scenario!="dir", ], 3, comp_scen))
    contacts_cea <- bind_rows(contacts_cea)
    
    #catalytic vs. direct isn't really one vs the other - they are different scenarios of I4TB vs. status quo
    #so calculate separately
    contacts_cea_dir <- contacts %>% 
      filter(scenario %in% c("sq", "dir")) %>% ungroup() %>%
      select(country, code, scenario, 
             cum_tb_deaths, cum_yll, cum_dalys, cum_costs,
             cum_disc_dalys, cum_disc_costs)
    contacts_cea_dir <- lapply(i4tb_countries, function(x)
      calc_CEA(as.data.table(contacts_cea_dir)[code==x, ], 2, comp_scen))
    contacts_cea_dir <- bind_rows(contacts_cea_dir)
    
    contacts_cea_cat <- contacts %>% 
      filter(scenario %in% c("sq", "cat")) %>% ungroup() %>%
      select(country, code, scenario, 
             cum_tb_deaths, cum_yll, cum_dalys, cum_costs,
             cum_disc_dalys, cum_disc_costs)
    contacts_cea_cat <- lapply(unique(country_info$code), function(x)
      calc_CEA(as.data.table(contacts_cea_cat)[code==x, ], 2, comp_scen))
    contacts_cea_cat <- bind_rows(contacts_cea_cat)
    
    contacts_cea_cat1 <- contacts %>% 
      filter(scenario %in% c("sq", "cat1")) %>% ungroup() %>%
      select(country, code, scenario, 
             cum_tb_deaths, cum_yll, cum_dalys, cum_costs,
             cum_disc_dalys, cum_disc_costs)
    contacts_cea_cat1 <- lapply(unique(country_info$code), function(x)
      calc_CEA(as.data.table(contacts_cea_cat1)[code==x, ], 2, comp_scen))
    contacts_cea_cat1 <- bind_rows(contacts_cea_cat1)
    
    #also do age groups separately - turned on for now
    if(TRUE) {
      child_cea <- bind_cols(id_cols, child_cea)
      child_cea_all <- lapply(unique(country_info$code), function(x)
        calc_CEA(as.data.table(child_cea)[code==x & scenario!="dir", ], 3, comp_scen))
      child_cea_cat <- lapply(unique(country_info$code), function(x)
        calc_CEA(as.data.table(child_cea)[code==x & scenario %in% c("sq", "cat"), ], 2, comp_scen))
      child_cea_dir <- lapply(i4tb_countries, function(x)
        calc_CEA(as.data.table(child_cea)[code==x & scenario %in% c("sq", "dir"), ], 2, comp_scen))
      child_cea_cat1 <- lapply(unique(country_info$code), function(x)
        calc_CEA(as.data.table(child_cea)[code==x & scenario %in% c("sq", "cat1"), ], 2, comp_scen))
      child_cea_all <- bind_rows(child_cea_all)
      child_cea_cat <- bind_rows(child_cea_cat)
      child_cea_dir <- bind_rows(child_cea_dir)
      child_cea_cat1 <- bind_rows(child_cea_cat1)
      
      adol_cea <- bind_cols(id_cols, adol_cea)
      adol_cea_all <- lapply(unique(country_info$code), function(x)
        calc_CEA(as.data.table(adol_cea)[code==x & scenario!="dir", ], 3, comp_scen))
      adol_cea_cat <- lapply(unique(country_info$code), function(x)
        calc_CEA(as.data.table(adol_cea)[code==x & scenario %in% c("sq", "cat"), ], 2, comp_scen))
      adol_cea_dir <- lapply(i4tb_countries, function(x)
        calc_CEA(as.data.table(adol_cea)[code==x & scenario %in% c("sq", "dir"), ], 2, comp_scen))
      adol_cea_cat1 <- lapply(unique(country_info$code), function(x)
        calc_CEA(as.data.table(adol_cea)[code==x & scenario %in% c("sq", "cat1"), ], 2, comp_scen))
      adol_cea_all <- bind_rows(adol_cea_all)
      adol_cea_cat <- bind_rows(adol_cea_cat)
      adol_cea_dir <- bind_rows(adol_cea_dir)
      adol_cea_cat1 <- bind_rows(adol_cea_cat1)
      
      adult_cea <- bind_cols(id_cols, adult_cea)
      adult_cea_all <- lapply(unique(country_info$code), function(x)
        calc_CEA(as.data.table(adult_cea)[code==x & scenario!="dir", ], 3, comp_scen))
      adult_cea_cat <- lapply(unique(country_info$code), function(x)
        calc_CEA(as.data.table(adult_cea)[code==x & scenario %in% c("sq", "cat"), ], 2, comp_scen))
      adult_cea_dir <- lapply(i4tb_countries, function(x)
        calc_CEA(as.data.table(adult_cea)[code==x & scenario %in% c("sq", "dir"), ], 2, comp_scen))
      adult_cea_cat1 <- lapply(unique(country_info$code), function(x)
        calc_CEA(as.data.table(adult_cea)[code==x & scenario %in% c("sq", "cat1"), ], 2, comp_scen))
      adult_cea_all <- bind_rows(adult_cea_all)
      adult_cea_cat <- bind_rows(adult_cea_cat)
      adult_cea_dir <- bind_rows(adult_cea_dir)
      adult_cea_cat1 <- bind_rows(adult_cea_cat1)
    }
  }
  if(type=="pub") {
    #calculate cost-effectiveness for household contacts 
    id_cols <- child_comb %>% filter(year==(end_yr)) %>% select(country, code, scenario)
    child_cea <- child_comb %>% filter(year==(end_yr)) %>% ungroup() %>%
      select(cum_tb_deaths, cum_yll, cum_dalys, cum_costs,
             cum_disc_dalys, cum_disc_costs)
    adol_cea <- adol_comb %>% filter(year==(end_yr)) %>% ungroup() %>%
      select(cum_tb_deaths, cum_yll, cum_dalys, cum_costs,
             cum_disc_dalys, cum_disc_costs)
    adult_cea <- adult_comb %>% filter(year==(end_yr)) %>% ungroup() %>%
      select(cum_tb_deaths, cum_yll, cum_dalys, cum_costs,
             cum_disc_dalys, cum_disc_costs)
    contacts <- child_cea + adol_cea + adult_cea
    contacts <- bind_cols(id_cols, contacts)
    
    #CEA for status quo vs. catalytic w/ 3HP vs. catalytic w/ 1HP
    contacts_cea <- lapply(unique(contacts$code), function(x)
      calc_CEA(as.data.table(contacts)[code==x, ], length(unique(contacts$scenario)), comp_scen))
    contacts_cea <- bind_rows(contacts_cea)
    
    #also do age groups separately - turned on for now
    if(TRUE) {
      child_cea <- bind_cols(id_cols, child_cea)
      child_cea_all <- lapply(unique(contacts$code), function(x)
        calc_CEA(as.data.table(child_cea)[code==x, ], length(unique(contacts$scenario)), comp_scen))
      child_cea_all <- bind_rows(child_cea_all)
      
      adol_cea <- bind_cols(id_cols, adol_cea)
      adol_cea_all <- lapply(unique(contacts$code), function(x)
        calc_CEA(as.data.table(adol_cea)[code==x, ], length(unique(contacts$scenario)), comp_scen))
      adol_cea_all <- bind_rows(adol_cea_all)
      
      adult_cea <- bind_cols(id_cols, adult_cea)
      adult_cea_all <- lapply(unique(contacts$code), function(x)
        calc_CEA(as.data.table(adult_cea)[code==x, ], length(unique(contacts$scenario)), comp_scen))
      adult_cea_all <- bind_rows(adult_cea_all)
    }
  }
  
  
  #save output to lists
  child_comb_all[[as.character(j)]] <- child_comb
  adol_comb_all[[as.character(j)]] <- adol_comb
  adult_comb_all[[as.character(j)]] <- adult_comb

  child_cea_all_all[[as.character(j)]] <- child_cea_all
  adol_cea_all_all[[as.character(j)]] <- adol_cea_all
  adult_cea_all_all[[as.character(j)]] <- adult_cea_all
  contacts_cea_all_all[[as.character(j)]] <- contacts_cea
  
  if(type=="i4tb") {
    child_cea_dir_all[[as.character(j)]] <- child_cea_dir
    child_cea_cat_all[[as.character(j)]] <- child_cea_cat
    child_cea_cat1_all[[as.character(j)]] <- child_cea_cat1
    adol_cea_dir_all[[as.character(j)]] <- adol_cea_dir
    adol_cea_cat_all[[as.character(j)]] <- adol_cea_cat
    adol_cea_cat1_all[[as.character(j)]] <- adol_cea_cat1
    adult_cea_dir_all[[as.character(j)]] <- adult_cea_dir
    adult_cea_cat_all[[as.character(j)]] <- adult_cea_cat
    adult_cea_cat1_all[[as.character(j)]] <- adult_cea_cat1
    contacts_cea_dir_all[[as.character(j)]] <- contacts_cea_dir
    contacts_cea_cat_all[[as.character(j)]] <- contacts_cea_cat
    contacts_cea_cat1_all[[as.character(j)]] <- contacts_cea_cat1
  }
}

#combine output
child_comb_all <- bind_rows(child_comb_all, .id="sample")
adol_comb_all <- bind_rows(adol_comb_all, .id="sample")
adult_comb_all <- bind_rows(adult_comb_all, .id="sample")
child_cea_all_all <- bind_rows(child_cea_all_all, .id="sample")
adol_cea_all_all <- bind_rows(adol_cea_all_all, .id="sample")
adult_cea_all_all <- bind_rows(adult_cea_all_all, .id="sample")
contacts_cea_all_all <- bind_rows(contacts_cea_all_all, .id="sample")

if(type=="i4tb") {
  child_cea_dir_all <- bind_rows(child_cea_dir_all, .id="sample")
  child_cea_cat_all <- bind_rows(child_cea_cat_all, .id="sample")
  child_cea_cat1_all <- bind_rows(child_cea_cat1_all, .id="sample")
  adol_cea_dir_all <- bind_rows(adol_cea_dir_all, .id="sample")
  adol_cea_cat_all <- bind_rows(adol_cea_cat_all, .id="sample")
  adol_cea_cat1_all <- bind_rows(adol_cea_cat1_all, .id="sample")
  adult_cea_dir_all <- bind_rows(adult_cea_dir_all, .id="sample")
  adult_cea_cat_all <- bind_rows(adult_cea_cat_all, .id="sample")
  adult_cea_cat1_all <- bind_rows(adult_cea_cat1_all, .id="sample")
  contacts_cea_dir_all <- bind_rows(contacts_cea_dir_all, .id="sample")
  contacts_cea_cat_all <- bind_rows(contacts_cea_cat_all, .id="sample")
  contacts_cea_cat1_all <- bind_rows(contacts_cea_cat1_all, .id="sample")
}

#sum across age groups
id_cols <- child_comb_all %>% ungroup() %>% select(sample, country, code, starts_with("i4tb"), year, scenario)
contacts_comb_all <- child_comb_all %>% ungroup() %>% select(-names(id_cols)) + 
  adol_comb_all %>% ungroup() %>% select(-names(id_cols)) + 
  adult_comb_all %>% ungroup() %>% select(-names(id_cols))
contacts_comb_all <- bind_cols(id_cols, contacts_comb_all)

#save results to file and run summaries and figures in separate scripts
file_lab <- as.character(array)
if(type=="i4tb") {
  path_out <- paste0("output/i4tb_", dir_end)
} else {
  path_out <- "output/latest"
}
if((analytic_horizon!=13 & type=="pub")|(type=="i4tb" & analytic_horizon!=10)) {
  path_out <- paste0(path_out, "_", as.character(analytic_horizon),
                     "yr")
}
if(tpt_covg!=0.8) {
  path_out <- paste0(path_out, "_", as.character(round(tpt_covg*100)),
                     "covg")
}
if(contact_only==1) {
  path_out <- paste0(path_out, "_contact_only")
}
if(contact_only==2) {
  path_out <- paste0(path_out, "_contact_vs_tpt")
}
if(meta_psa==1) {
  path_out <- paste0(path_out, "_meta")
}
if(meta_psa==2) {
  path_out <- paste0(path_out, "_meta_comb")
}
if(tpt_type!="3hp") {
  path_out <- paste0(path_out, "_", tpt_type)
}
if(reinfect!=0) {
  path_out <- paste0(path_out, "_", as.character(round(reinfect*100)),
                     "reinfect")
}
if(price_tpt!="base") {
  path_out <- paste0(path_out, "_price_", price_tpt)
}
if(cost_tbtx!="base") {
  path_out <- paste0(path_out, "_tbtx_", cost_tbtx)
}
if(visits_3hp!=1) {
  path_out <- paste0(path_out, "_", as.character(visits_3hp), "visits")
}
if(cxr_screen_5plus==0) {
  path_out <- paste0(path_out, "_", "noCXRscreen")
}
if(impl_costs==1) {
  path_out <- paste0(path_out, "_", "impl_costs")
}
path_out <- paste0(path_out, "/")

write.csv(contacts_comb_all, file=paste0(path_out, "contacts_out_all_", file_lab, ".csv"), row.names=F)
write.csv(contacts_cea_all_all, file=paste0(path_out, "contacts_cea_all_all_", file_lab, ".csv"), row.names=F)
write.csv(child_comb_all, file=paste0(path_out, "child_out_all_", file_lab, ".csv"), row.names=F)
write.csv(child_cea_all_all, file=paste0(path_out, "child_cea_all_all_", file_lab, ".csv"), row.names=F)
write.csv(adol_comb_all, file=paste0(path_out, "adol_out_all_", file_lab, ".csv"), row.names=F)
write.csv(adol_cea_all_all, file=paste0(path_out, "adol_cea_all_all_", file_lab, ".csv"), row.names=F)
write.csv(adult_comb_all, file=paste0(path_out, "adult_out_all_", file_lab, ".csv"), row.names=F)
write.csv(adult_cea_all_all, file=paste0(path_out, "adult_cea_all_all_", file_lab, ".csv"), row.names=F)

if(type=="i4tb") {
  write.csv(contacts_cea_dir_all, file=paste0(path_out, "contacts_cea_dir_all_", file_lab, ".csv"), row.names=F)
  write.csv(contacts_cea_cat_all, file=paste0(path_out, "contacts_cea_cat_all_", file_lab, ".csv"), row.names=F)
  write.csv(contacts_cea_cat1_all, file=paste0(path_out, "contacts_cea_cat1_all_", file_lab, ".csv"), row.names=F)
  write.csv(child_cea_dir_all, file=paste0(path_out, "child_cea_dir_all_", file_lab, ".csv"), row.names=F)
  write.csv(child_cea_cat_all, file=paste0(path_out, "child_cea_cat_all_", file_lab, ".csv"), row.names=F)
  write.csv(child_cea_cat1_all, file=paste0(path_out, "child_cea_cat1_all_", file_lab, ".csv"), row.names=F)
  write.csv(adol_cea_dir_all, file=paste0(path_out, "adol_cea_dir_all_", file_lab, ".csv"), row.names=F)
  write.csv(adol_cea_cat_all, file=paste0(path_out, "adol_cea_cat_all_", file_lab, ".csv"), row.names=F)
  write.csv(adol_cea_cat1_all, file=paste0(path_out, "adol_cea_cat1_all_", file_lab, ".csv"), row.names=F)
  write.csv(adult_cea_dir_all, file=paste0(path_out, "adult_cea_dir_all_", file_lab, ".csv"), row.names=F)
  write.csv(adult_cea_cat_all, file=paste0(path_out, "adult_cea_cat_all_", file_lab, ".csv"), row.names=F)
  write.csv(adult_cea_cat1_all, file=paste0(path_out, "adult_cea_cat1_all_", file_lab, ".csv"), row.names=F)
}
