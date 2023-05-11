

policy_horizon <- 5 #for now, implement over 5 years and calculate costs/outcomes over 5 years
analytic_horizon <- 5
start_yr <- 2024
end_yr <- start_yr + analytic_horizon - 1
policy_end_yr <- start_yr + policy_horizon - 1

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

#calculate additional screening and testing probabilities based on options
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

#load total numbers of contacts to cover by year
pop_calcs <- pivot_longer(pop_calcs %>% select(-start_yr) %>%
                            filter(country==country_name &
                                                 year>=start_yr & year<=policy_end_yr), 
                          cols=c(ends_with("tpt"), 
                                 ends_with("none")),
                          names_to=c(".value", "scenario"),
                          names_sep="_(?=[^_]+$)") #this is regex for "last underscore only"
pop_calcs <- pop_calcs %>% 
  mutate(child_ipt=if_else(regimen_child=="6H" & scenario=="tpt", covg_child/100, 0),
         adol_ipt=if_else(regimen_adol=="6H" & scenario=="tpt", covg_adol/100, 0),
         adult_ipt=if_else(regimen_adult=="6H" & scenario=="tpt", covg_adult/100, 0),
         child_3hp=if_else(regimen_child=="3HP" & scenario=="tpt", covg_child/100, 0),
         adol_3hp=if_else(regimen_adol=="3HP" & scenario=="tpt", covg_adol/100, 0),
         adult_3hp=if_else(regimen_adult=="3HP" & scenario=="tpt", covg_adult/100, 0),
         child_1hp=if_else(regimen_child=="1HP" & scenario=="tpt", covg_child/100, 0),
         adol_1hp=if_else(regimen_adol=="1HP" & scenario=="tpt", covg_adol/100, 0),
         adult_1hp=if_else(regimen_adult=="1HP" & scenario=="tpt", covg_adult/100, 0))

#separate into 3 dataframes - one for each target population group
#hh contacts - targets include initiation, so backcalculate # contacts investigated

#CHILD CONTACTS
child <- pop_calcs %>% select(-pop) %>% 
  rename("initiate_ipt_covg"="child_ipt", 
         "initiate_3hp_covg"="child_3hp",
         "initiate_1hp_covg"="child_1hp",
         "pop"="pop04", "hh"="hh_child") %>%
  mutate(total=tb_notif_new*hh,
         hh_contact_covg = initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg, #initiate_covg is now before adjusting for screening/testing negative and initiation ratios
         prop_ipt=if_else(initiate_ipt_covg==0|contact_only==1, 0,
                          initiate_ipt_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
         prop_3hp=if_else(initiate_3hp_covg==0|contact_only==1, 0, 
                          initiate_3hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
         prop_1hp=if_else(initiate_1hp_covg==0|contact_only==1, 0, 
                          initiate_1hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
         contact_invest_num = hh_contact_covg*total,
         initiate_num=contact_invest_num*p_hh_child_tpt*child_params$p_initiate,
         p_initiate=if_else(contact_only==1, 0, child_params$p_initiate)) %>%
  select(country, code, year, scenario,
         pop, hh, total, prop_ipt, prop_3hp, prop_1hp, contact_invest_num, initiate_num, p_initiate)
child <- child %>% mutate(p_notif=child_params$p_notif,
                          p_success=child_params$p_success,
                          p_die=child_params$p_die)
#run model for child contacts
child <- calc_contact_invest(child, child_params) #outcomes of contact investigation
child <- calc_tpt_outcomes_contacts(child, child_params) #TPT-related events and outcomes
#calculate out horizon years
child_all <- child %>% mutate(yrs_out=0, 
                              cum_tb_deaths=tb_deaths,
                              cum_non_tb_deaths=non_tb_deaths)
for(i in 1:analytic_horizon) {
  #children: mortality and reactivation rates vary over time, notification and mortality varies by country
  child_prev <- child_all %>% filter(yrs_out==i-1)
  if(i <=2) {
    child_cur <- model_tb_contacts(child_prev, child_params$p_notif, child_params$p_success, 
                        child_params$p_reactivate_02, child_params$p_die_tb, 
                        child_params$p_die_tb_tx, child_params$p_die,
                        child_params$p_infect, child_params$ltbi_protect,
                        child_params$prop_fast)
  } else if(i==3) {
    child_cur <- model_tb_contacts(child_prev, child_params$p_notif, child_params$p_success, 
                        child_params$p_reactivate_25, child_params$p_die_tb, 
                        child_params$p_die_tb_tx, child_params$p_die,
                        child_params$p_infect, child_params$ltbi_protect,
                        child_params$prop_fast)
  } else if(i==4|i==5) {
    child_cur <- model_tb_contacts(child_prev, mean(c(child_params$p_notif, adol_params$p_notif)),
                        child_params$p_success, 
                        child_params$p_reactivate_25, 
                        mean(c(child_params$p_die_tb, adol_params$p_die_tb)),
                        mean(c(child_params$p_die_tb_tx, adol_params$p_die_tb_tx)),
                        mean(c(child_params$p_die, adol_params$p_die)),
                        child_params$p_infect, child_params$ltbi_protect,
                        child_params$prop_fast)
  } else if(i>5 & i<=10) {
    child_cur <- model_tb_contacts(child_prev, adol_params$p_notif, adol_params$p_success,  
                        child_params$p_reactivate_510, adol_params$p_die_tb, 
                        adol_params$p_die_tb_tx, adol_params$p_die,
                        child_params$p_infect, child_params$ltbi_protect,
                        child_params$prop_fast)
  } else if(i>10) {
    child_cur <- model_tb_contacts(child_prev, adol_params$p_notif_1524, adult_params$p_success, 
                        child_params$p_reactivate_10plus, 
                        mean(c(adol_params$p_die_tb, adult_params$p_die_tb)),
                        mean(c(adol_params$p_die_tb_tx, adult_params$p_die_tb_tx)), 
                        mean(c(adol_params$p_die, adult_params$p_die)),
                        child_params$p_infect, child_params$ltbi_protect,
                        child_params$prop_fast)
  }
  child_cur <- bind_rows(child_cur)
  child_cur <- child_cur %>% mutate(yrs_out=i)
  child_all <- bind_rows(child_all, child_cur)
}
child_comb <- combine_yrs(child_all, "child", policy_end_yr, end_yr) #combine across years

#CONTACTS AGED 5-14
adol <- pop_calcs %>% select(-pop) %>% 
  rename("initiate_ipt_covg"="adol_ipt", 
         "initiate_3hp_covg"="adol_3hp",
         "initiate_1hp_covg"="adol_1hp",
         "pop"="pop514", "hh"="hh_adol") %>%
  mutate(total=tb_notif_new*hh,
         hh_contact_covg = initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg, #initiate_covg is now before adjusting for screening/testing negative and initiation ratios
         prop_ipt=if_else(initiate_ipt_covg==0|contact_only==1, 0,
                          initiate_ipt_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
         prop_3hp=if_else(initiate_3hp_covg==0|contact_only==1, 0, 
                          initiate_3hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
         prop_1hp=if_else(initiate_1hp_covg==0|contact_only==1, 0, 
                          initiate_1hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
         contact_invest_num = hh_contact_covg*total,
         initiate_num=contact_invest_num*p_hh_adol_tpt*adol_params$p_initiate,
         p_initiate=if_else(contact_only==1, 0, adol_params$p_initiate)) %>%
  select(country, code, year, scenario,
         pop, hh, total, prop_ipt, prop_3hp, prop_1hp, contact_invest_num, initiate_num, p_initiate)
adol <- adol %>% mutate(p_notif=adol_params$p_notif,
                          p_success=adol_params$p_success,
                          p_die=adol_params$p_die)
#run model for adol contacts
adol <- calc_contact_invest(adol, adol_params) #outcomes of contact investigation
adol <- calc_tpt_outcomes_contacts(adol, adol_params) #TPT-related events and outcomes
#calculate out horizon years
adol_all <- adol %>% mutate(yrs_out=0, 
                              cum_tb_deaths=tb_deaths,
                              cum_non_tb_deaths=non_tb_deaths)
for(i in 1:analytic_horizon) {
  #mortality and reactivation rates vary over time, notification and mortality varies by country
  adol_prev <- adol_all %>% filter(yrs_out==i-1)
  if(i <=2) {
    adol_cur <- model_tb_contacts(adol_prev, adol_params$p_notif, adol_params$p_success, 
                                   adol_params$p_reactivate_02, adol_params$p_die_tb, 
                                   adol_params$p_die_tb_tx, adol_params$p_die,
                                   adol_params$p_infect, adol_params$ltbi_protect,
                                   adol_params$prop_fast)
  } 
  else if(i>2 & i<=5) {
    adol_cur <- model_tb_contacts(adol_prev, adol_params$p_notif, adol_params$p_success, 
                                   adol_params$p_reactivate_25, adol_params$p_die_tb, 
                                   adol_params$p_die_tb_tx, adol_params$p_die,
                                   adol_params$p_infect, adol_params$ltbi_protect,
                                   adol_params$prop_fast)
  } else if(i>5 & i<=10) {
    adol_cur <- model_tb_contacts(adol_prev, mean(c(adol_params$p_notif, adol_params$p_notif_1524)),
                                  mean(c(adol_params$p_success, adult_params$p_success)), 
                                  adol_params$p_reactivate_510, 
                                  mean(c(adol_params$p_die_tb, adult_params$p_die_tb)),
                                  mean(c(adol_params$p_die_tb_tx, adult_params$p_die_tb_tx)), 
                                  mean(c(adol_params$p_die, adult_params$p_die)),
                                  adol_params$p_infect, adol_params$ltbi_protect,
                                  adol_params$prop_fast)
  } else if(i>10) {
    adol_cur <- model_tb_contacts(adol_prev, c(adol_params$p_notif_1524, adult_params$p_notif),
                                  adult_params$p_success, 
                                  adol_params$p_reactivate_10plus, 
                                  mean(c(adol_params$p_die_tb, adult_params$p_die_tb)),
                                  mean(c(adol_params$p_die_tb_tx, adult_params$p_die_tb_tx)), 
                                  mean(c(adol_params$p_die, adult_params$p_die)),
                                  adol_params$p_infect, adol_params$ltbi_protect,
                                  adol_params$prop_fast)
  }
  adol_cur <- bind_rows(adol_cur)
  adol_cur <- adol_cur %>% mutate(yrs_out=i)
  adol_all <- bind_rows(adol_all, adol_cur)
}
adol_comb <- combine_yrs(adol_all, "adol", policy_end_yr, end_yr) #combine across years

#ADULT CONTACTS
adult <- pop_calcs %>% select(-pop) %>% 
  rename("initiate_ipt_covg"="adult_ipt", 
         "initiate_3hp_covg"="adult_3hp",
         "initiate_1hp_covg"="adult_1hp",
         "pop"="popadult", "hh"="hh_adult") %>%
  mutate(total=tb_notif_new*hh,
         hh_contact_covg = initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg, #initiate_covg is now before adjusting for screening/testing negative and initiation ratios
         prop_ipt=if_else(initiate_ipt_covg==0|contact_only==1, 0,
                          initiate_ipt_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
         prop_3hp=if_else(initiate_3hp_covg==0|contact_only==1, 0, 
                          initiate_3hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
         prop_1hp=if_else(initiate_1hp_covg==0|contact_only==1, 0, 
                          initiate_1hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg)),
         contact_invest_num = hh_contact_covg*total,
         initiate_num=contact_invest_num*p_hh_adult_tpt*adult_params$p_initiate,
         p_initiate=if_else(contact_only==1, 0, adult_params$p_initiate)) %>%
  select(country, code, year, scenario,
         pop, hh, total, prop_ipt, prop_3hp, prop_1hp, contact_invest_num, initiate_num, p_initiate)
adult <- adult %>% mutate(p_notif=adult_params$p_notif,
                        p_success=adult_params$p_success,
                        p_die=adult_params$p_die)
#run model for adult contacts
adult <- calc_contact_invest(adult, adult_params) #outcomes of contact investigation
adult <- calc_tpt_outcomes_contacts(adult, adult_params) #TPT-related events and outcomes
#calculate out horizon years
adult_all <- adult %>% mutate(yrs_out=0, 
                            cum_tb_deaths=tb_deaths,
                            cum_non_tb_deaths=non_tb_deaths)
for(i in 1:analytic_horizon) {
  #mortality and reactivation rates vary over time, notification and mortality varies by country
  adult_prev <- adult_all %>% filter(yrs_out==i-1)
  if(i <=2) {
    adult_cur <- model_tb_contacts(adult_prev, adult_params$p_notif, adult_params$p_success,
                        adult_params$p_reactivate_02, adult_params$p_die_tb, 
                        adult_params$p_die_tb_tx, adult_params$p_die,
                        adult_params$p_infect, adult_params$ltbi_protect,
                        adult_params$prop_fast)
  } else if(i>2 & i<=5) {
    adult_cur <- model_tb_contacts(adult_prev, adult_params$p_notif, adult_params$p_success,
                                   adult_params$p_reactivate_25, adult_params$p_die_tb, 
                                   adult_params$p_die_tb_tx, adult_params$p_die,
                                   adult_params$p_infect, adult_params$ltbi_protect,
                                   adult_params$prop_fast)
  } else if(i>5 & i<=10) {
    adult_cur <- model_tb_contacts(adult_prev, adult_params$p_notif, adult_params$p_success,
                                   adult_params$p_reactivate_510, adult_params$p_die_tb, 
                                   adult_params$p_die_tb_tx, adult_params$p_die,
                                   adult_params$p_infect, adult_params$ltbi_protect,
                                   adult_params$prop_fast)
  } else if(i>10) {
    adult_cur <- model_tb_contacts(adult_prev, adult_params$p_notif, adult_params$p_success,
                                   adult_params$p_reactivate_10plus, adult_params$p_die_tb, 
                                   adult_params$p_die_tb_tx, adult_params$p_die,
                                   adult_params$p_infect, adult_params$ltbi_protect,
                                   adult_params$prop_fast)
  }
  adult_cur <- bind_rows(adult_cur)
  adult_cur <- adult_cur %>% mutate(yrs_out=i)
  adult_all <- bind_rows(adult_all, adult_cur)
}
adult_comb <- combine_yrs(adult_all, "adult", policy_end_yr, end_yr) #combine across years

#calculate DALYs and discounted DALYs
if(FALSE) {
  child_comb <-  calc_dalys(as.data.table(child_comb), "child", life_exp_child, other_params$dw_tb, 
                            other_params$dw_notb, 1, other_params$dur_tb_tx, other_params$disc_fac,
                            start_yr)
  adol_comb <-  calc_dalys(as.data.table(adol_comb), "adol", life_exp_adol, other_params$dw_tb, 
                           other_params$dw_notb, 1, other_params$dur_tb_tx, other_params$disc_fac,
                           start_yr)
  adult_comb <-  calc_dalys(as.data.table(adult_comb), "adult", life_exp_adult, other_params$dw_tb, 
                            other_params$dw_notb, 1, other_params$dur_tb_tx, other_params$disc_fac,
                            start_yr)
}


#calculate costs
child_comb <- calc_costs(child_comb, "child", child_params, 
             0, child_params, child_params$disc_fac, start_yr)
adol_comb <- calc_costs(adol_comb, "adol", adol_params, 
                         0, adol_params, adol_params$disc_fac, start_yr)
adult_comb <- calc_costs(adult_comb, "adult", adult_params, 
                         0, adult_params, adult_params$disc_fac, start_yr)
  
  