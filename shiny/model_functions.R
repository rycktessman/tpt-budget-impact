#models TB outcomes for newly enrolled PLHIV (used to initiate in start_yr)
model_tb_new_plhiv <- function(d_covg, params) {
  #calculate split of newly enrolled across TB states - nobody starts out notified or dead
  plhiv_new <- d_covg$plhiv_new
  ltbi_new <- plhiv_new*params$p_ltbi*(1-params$p_reactivate_new[[1]])
  active_tb_new <- plhiv_new*params$p_ltbi*params$p_reactivate_new[[1]]
  no_tb_new <- plhiv_new*(1-params$p_ltbi)
  
  #keep track of TB deaths and cases spearately
  cases_new <- active_tb_new
  notif_new <- active_tb_new*params$p_notif #also used to calculate costs
  tb_deaths_enroll <- active_tb_new*params$p_die_tb_art
  active_tb_new <- active_tb_new - tb_deaths_enroll
  
  #calculate which newly enrolled go on TPT
  initiate_ipt_new <- (ltbi_new + no_tb_new)*d_covg$initiate_ipt_covg_new
  initiate_3hp_new <- (ltbi_new + no_tb_new)*d_covg$initiate_3hp_covg_new
  initiate_1hp_new <- (ltbi_new + no_tb_new)*d_covg$initiate_1hp_covg_new
  initiate_ipt_ltbi_new <- ltbi_new*d_covg$initiate_ipt_covg_new
  initiate_3hp_ltbi_new <- ltbi_new*d_covg$initiate_3hp_covg_new
  initiate_1hp_ltbi_new <- ltbi_new*d_covg$initiate_1hp_covg_new
  tox_nohosp_ipt_new <- initiate_ipt_new*params$p_tox_nohosp_ipt
  tox_nohosp_3hp_new <- initiate_3hp_new*params$p_tox_nohosp_3hp
  tox_nohosp_1hp_new <- initiate_1hp_new*params$p_tox_nohosp_1hp
  tox_hosp_ipt_new <- initiate_ipt_new*params$p_tox_hosp_ipt
  tox_hosp_3hp_new <- initiate_3hp_new*params$p_tox_hosp_3hp
  tox_hosp_1hp_new <- initiate_1hp_new*params$p_tox_hosp_1hp
  complete_ipt_new <- initiate_ipt_new*params$p_complete_ipt*(1-(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt))
  complete_3hp_new <- initiate_3hp_new*params$p_complete_3hp*(1-(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp))
  complete_1hp_new <- initiate_1hp_new*params$p_complete_1hp*(1-(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp))
  part_complete_ipt_new <- initiate_ipt_new - 
    (complete_ipt_new + tox_nohosp_ipt_new + tox_hosp_ipt_new)
  part_complete_3hp_new <- initiate_3hp_new - 
    (complete_3hp_new + tox_nohosp_3hp_new + tox_hosp_3hp_new)
  part_complete_1hp_new <- initiate_1hp_new - 
    (complete_1hp_new + tox_nohosp_1hp_new + tox_hosp_1hp_new)
  
  #results of TPT/lack of TPT on TB status for newly enrolled
  ltbi_new_tpt <- initiate_ipt_ltbi_new + #LTBI and IPT initiated 
    initiate_3hp_ltbi_new +
    initiate_1hp_ltbi_new -
    initiate_ipt_ltbi_new*params$p_complete_ipt*params$eff_ipt - #subtract out full IPT completion/efficacy
    initiate_3hp_ltbi_new*params$p_complete_3hp*params$eff_3hp - #subtract out full 3HP completion/efficacy
    initiate_1hp_ltbi_new*params$p_complete_1hp*params$eff_1hp - #subtract out full 1HP completion/efficacy
    initiate_ipt_ltbi_new*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-
                             params$p_tox_nohosp_ipt)*params$eff_ipt/2 - #subtract out IPT partial completion/efficacy
    initiate_3hp_ltbi_new*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-
                             params$p_tox_nohosp_3hp)*params$eff_3hp/2 - #subtract out 3HP partial completion/efficacy
    initiate_1hp_ltbi_new*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-
                             params$p_tox_nohosp_1hp)*params$eff_1hp/2 #subtract out 1HP partial completion/efficacy
  ltbi_new_not <- ltbi_new - initiate_ipt_ltbi_new - initiate_3hp_ltbi_new - initiate_1hp_ltbi_new #LTBI and didn't initiate TPT
  no_tb_new_tpt <- initiate_ipt_new + initiate_3hp_new + initiate_1hp_new -
    (initiate_ipt_ltbi_new + initiate_3hp_ltbi_new + initiate_1hp_ltbi_new) + #no LTBI to begin with
    initiate_ipt_ltbi_new*params$p_complete_ipt*params$eff_ipt + #full IPT completion
    initiate_3hp_ltbi_new*params$p_complete_3hp*params$eff_3hp + #full 3HP completion
    initiate_1hp_ltbi_new*params$p_complete_1hp*params$eff_1hp + #full 1HP completion
    initiate_ipt_ltbi_new*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-params$p_tox_nohosp_ipt)*
    params$eff_ipt/2 + #partial IPT completion
    initiate_3hp_ltbi_new*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-params$p_tox_nohosp_3hp)*
    params$eff_3hp/2 + #partial 3HP completion
    initiate_1hp_ltbi_new*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-params$p_tox_nohosp_1hp)*
    params$eff_1hp/2 #partial 1HP completion
  no_tb_new_not <- no_tb_new*
    (1-(d_covg$initiate_ipt_covg_new + d_covg$initiate_3hp_covg_new +
          d_covg$initiate_1hp_covg_new)) #no LTBI to begin with
  #track no TPT that got TB separately (wouldn't be eligible for TPT)
  no_tb_new_not_tb <- active_tb_new*params$p_notif*params$p_success
  active_tb_new_tpt <- 0 #assume no PWH are wrongly assigned to TPT
  active_tb_new_not <- active_tb_new*(1-params$p_notif) + active_tb_new*params$p_notif*(1-params$p_success)
  
  data <- data.frame(cases_new, notif_new, tb_deaths_enroll, 
                     initiate_ipt_new, initiate_3hp_new, initiate_1hp_new,
                     tox_nohosp_ipt_new, tox_nohosp_3hp_new, tox_nohosp_1hp_new,
                     tox_hosp_ipt_new, tox_hosp_3hp_new, tox_hosp_1hp_new,
                     complete_ipt_new, complete_3hp_new, complete_1hp_new,
                     part_complete_ipt_new, part_complete_3hp_new, part_complete_1hp_new,
                     ltbi_new_tpt, ltbi_new_not, 
                     active_tb_new_tpt, active_tb_new_not,
                     no_tb_new_tpt, no_tb_new_not, no_tb_new_not_tb)
  return(data)
}

#model deaths among PWH from previous period only (used within model_tb_plhiv)
model_deaths_plhiv <- function(d, d_ltbi, d_covg, params) {
  #TB deaths happen first (also include for those that will progress this timestep - otherwise no opportunty to die if notified)
  #calculate deaths based on previous timestep - no need to stratify by new vs. previously enrolled
  tb_deaths <- (d$active_tb_new_tpt*params$p_die_tb_art +
                  d$active_tb_new_not*params$p_die_tb_art +
                  d$active_tb_est_tpt*params$p_die_tb_art +
                  d$active_tb_est_not*params$p_die_tb_art +
                  d$active_tb_ltfu_tpt*params$p_die_tb_ltfu +
                  d$active_tb_ltfu_not*params$p_die_tb_ltfu +
                  d_ltbi$ltbi_new_not%*%(params$p_reactivate_new*(1-params$p_new_die))*params$p_die_tb_art +
                  d_ltbi$ltbi_new_tpt%*%(params$p_reactivate_new*(1-params$p_new_die))*params$p_die_tb_art +
                  d_ltbi$ltbi_est_not%*%(params$p_reactivate_est*(1-params$p_est_die))*params$p_die_tb_art +
                  d_ltbi$ltbi_est_tpt%*%(params$p_reactivate_est*(1-params$p_est_die))*params$p_die_tb_art +
                  d_ltbi$ltbi_ltfu_not%*%(params$p_reactivate_ltfu*(1-params$p_ltfu_die))*params$p_die_tb_ltfu +
                  d_ltbi$ltbi_ltfu_tpt%*%(params$p_reactivate_ltfu*(1-params$p_ltfu_die))*params$p_die_tb_ltfu +
                  #reinfections and new infections (if modeling)
                  d_ltbi$ltbi_new_not%*%((1-params$p_reactivate_new)*(1-params$p_new_die))*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*params$p_die_tb_art +
                  d_ltbi$ltbi_new_tpt%*%((1-params$p_reactivate_new)*(1-params$p_new_die))*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*params$p_die_tb_art +
                  d_ltbi$ltbi_est_not%*%((1-params$p_reactivate_est)*(1-params$p_est_die))*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*params$p_die_tb_art +
                  d_ltbi$ltbi_est_tpt%*%((1-params$p_reactivate_est)*(1-params$p_est_die))*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*params$p_die_tb_art +
                  d_ltbi$ltbi_ltfu_not%*%((1-params$p_reactivate_ltfu)*(1-params$p_ltfu_die))*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*params$p_die_tb_ltfu +
                  d_ltbi$ltbi_ltfu_tpt%*%((1-params$p_reactivate_ltfu)*(1-params$p_ltfu_die))*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*params$p_die_tb_ltfu +
                  d$no_tb_new_not*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*params$p_die_tb_art +
                  d$no_tb_new_not_tb*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*params$p_die_tb_art +
                  d$no_tb_new_tpt*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*params$p_die_tb_art +
                  d$no_tb_est_not*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*params$p_die_tb_art +
                  d$no_tb_est_not_tb*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*params$p_die_tb_art +
                  d$no_tb_est_tpt*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*params$p_die_tb_art +
                  d$no_tb_ltfu_not*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*params$p_die_tb_ltfu +
                  d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*params$p_die_tb_ltfu) +
    d_covg$tb_deaths_enroll #add in active deaths upon enrollment for that year
  
  #Non-TB deaths happen second
  non_tb_deaths <- d$active_tb_new_tpt*(1-params$p_die_tb_art)*params$p_new_die[1] +
    d$active_tb_new_not*(1-params$p_die_tb_art)*params$p_new_die[1] +
    d$active_tb_est_tpt*(1-params$p_die_tb_art)*params$p_est_die +
    d$active_tb_est_not*(1-params$p_die_tb_art)*params$p_est_die +
    d$active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu)*params$p_ltfu_die +
    d$active_tb_ltfu_not*(1-params$p_die_tb_ltfu)*params$p_ltfu_die +
    d_ltbi$ltbi_new_tpt%*%params$p_new_die + #for newly enrolled, mort risk drops after Y1
    d_ltbi$ltbi_new_not%*%params$p_new_die +
    rowSums(d_ltbi$ltbi_est_tpt)*params$p_est_die + #for established, mort risk constant
    rowSums(d_ltbi$ltbi_est_not)*params$p_est_die +
    rowSums(d_ltbi$ltbi_ltfu_tpt)*params$p_ltfu_die +
    rowSums(d_ltbi$ltbi_ltfu_not)*params$p_ltfu_die +
    d$no_tb_new_tpt*params$p_new_die[1] +
    d$no_tb_new_not*params$p_new_die[1] +
    d$no_tb_est_tpt*params$p_est_die + 
    d$no_tb_est_not*params$p_est_die +
    d$no_tb_new_not_tb*params$p_new_die[1] +
    d$no_tb_est_not_tb*params$p_est_die +
    d$no_tb_ltfu_tpt*params$p_ltfu_die +
    d$no_tb_ltfu_not*params$p_ltfu_die 
  cum_tb_deaths <- tb_deaths + d$cum_tb_deaths
  cum_non_tb_deaths <- non_tb_deaths + d$cum_non_tb_deaths
  
  output <- list(tb_deaths, non_tb_deaths, cum_tb_deaths, cum_non_tb_deaths)
  return(output)
}

#model active deaths, TB progression, TPT initiation, and LTFU among established PWH (used in start_yr only)
model_outcomes_est_plhiv <- function(d, d_ltbi, params) {
  #recombine LTBI and non-LTBI states into 1 dataframe
  d <- left_join(d, d_ltbi, by=c("country", "code", "year", "scenario")) %>% ungroup()
  
  #keep track of cases and active TB deaths
  d <- d %>% mutate(cases_est=active_tb_est_not, #all cases are new in start_yr and no one has initiated TPT
                    notif_est=cases_est*params$p_notif,
                    tb_deaths_enroll=active_tb_est_not*params$p_die_tb_art,
                    active_tb_est_not=active_tb_est_not-tb_deaths_enroll)
  
  #calculate who goes on TPT
  d <- d %>% mutate(initiate_ipt_est=(ltbi_est_not + no_tb_est_not)*initiate_ipt_covg_prev,
                    initiate_3hp_est=(ltbi_est_not + no_tb_est_not)*initiate_3hp_covg_prev,
                    initiate_1hp_est=(ltbi_est_not + no_tb_est_not)*initiate_1hp_covg_prev,
                    initiate_ipt_ltbi_est=ltbi_est_not*initiate_ipt_covg_prev,
                    initiate_3hp_ltbi_est=ltbi_est_not*initiate_3hp_covg_prev,
                    initiate_1hp_ltbi_est=ltbi_est_not*initiate_1hp_covg_prev,
                    tox_nohosp_ipt_est=initiate_ipt_est*params$p_tox_nohosp_ipt,
                    tox_nohosp_3hp_est=initiate_3hp_est*params$p_tox_nohosp_3hp,
                    tox_nohosp_1hp_est=initiate_1hp_est*params$p_tox_nohosp_1hp,
                    tox_hosp_ipt_est=initiate_ipt_est*params$p_tox_hosp_ipt,
                    tox_hosp_3hp_est=initiate_3hp_est*params$p_tox_hosp_3hp,
                    tox_hosp_1hp_est=initiate_1hp_est*params$p_tox_hosp_1hp,
                    complete_ipt_est=initiate_ipt_est*params$p_complete_ipt*
                      (1-(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt)),
                    complete_3hp_est=initiate_3hp_est*params$p_complete_3hp*
                      (1-(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp)),
                    complete_1hp_est=initiate_1hp_est*params$p_complete_1hp*
                      (1-(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp)),
                    part_complete_ipt_est=initiate_ipt_est - 
                      (complete_ipt_est + tox_nohosp_ipt_est + tox_hosp_ipt_est),
                    part_complete_3hp_est=initiate_3hp_est - 
                      (complete_3hp_est + tox_nohosp_3hp_est + tox_hosp_3hp_est),
                    part_complete_1hp_est=initiate_1hp_est - 
                      (complete_1hp_est + tox_nohosp_1hp_est + tox_hosp_1hp_est))
  
  #results of TPT/lack of TPT on TB status of previously enrolled
  d <- d %>% mutate(ltbi_est_tpt=initiate_ipt_ltbi_est + #LTBI and IPT initiated 
                      initiate_3hp_ltbi_est +
                      initiate_1hp_ltbi_est -
                      initiate_ipt_ltbi_est*params$p_complete_ipt*params$eff_ipt - #subtract out full IPT completion/efficacy
                      initiate_3hp_ltbi_est*params$p_complete_3hp*params$eff_3hp - #subtract out full 3HP completion/efficacy
                      initiate_1hp_ltbi_est*params$p_complete_1hp*params$eff_1hp - #subtract out full 1HP completion/efficacy
                      initiate_ipt_ltbi_est*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-
                                               params$p_tox_nohosp_ipt)*params$eff_ipt/2 - #subtract out IPT partial completion/efficacy
                      initiate_3hp_ltbi_est*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-
                                               params$p_tox_nohosp_3hp)*params$eff_3hp/2 - #subtract out 3HP partial completion/efficacy
                      initiate_1hp_ltbi_est*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-
                                               params$p_tox_nohosp_1hp)*params$eff_1hp/2, #subtract out 1HP partial completion/efficacy
                    ltbi_est_not=ltbi_est_not - initiate_ipt_ltbi_est - 
                      initiate_3hp_ltbi_est - initiate_1hp_ltbi_est, #LTBI and didn't initiate TPT
                    no_tb_est_tpt=initiate_ipt_est + initiate_3hp_est + initiate_1hp_est -
                      (initiate_ipt_ltbi_est + initiate_3hp_ltbi_est + initiate_1hp_ltbi_est) + #no LTBI to begin with
                      initiate_ipt_ltbi_est*params$p_complete_ipt*params$eff_ipt + #full IPT completion
                      initiate_3hp_ltbi_est*params$p_complete_3hp*params$eff_3hp + #full 3HP completion
                      initiate_1hp_ltbi_est*params$p_complete_1hp*params$eff_1hp + #full 1HP completion
                      initiate_ipt_ltbi_est*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-params$p_tox_nohosp_ipt)*
                      params$eff_ipt/2 + #partial IPT completion
                      initiate_3hp_ltbi_est*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-params$p_tox_nohosp_3hp)*
                      params$eff_3hp/2 + #partial 3HP completion
                      initiate_1hp_ltbi_est*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-params$p_tox_nohosp_1hp)*
                      params$eff_1hp/2, #partial 1HP completion
                    no_tb_est_not=no_tb_est_not*(1-(initiate_ipt_covg_prev + initiate_3hp_covg_prev + initiate_1hp_covg_prev)), #no LTBI to begin with
                    #track no TPT that got TB separately (wouldn't be eligible for TPT later)
                    no_tb_est_not_tb=active_tb_est_not*params$p_notif*params$p_success,
                    active_tb_est_tpt=0,
                    active_tb_est_not=active_tb_est_not*(1-params$p_notif) +
                      active_tb_est_not*params$p_notif*(1-params$p_success))
  
  #calculate newly LTFU - keep track of TPT status since we allow return to ART
  d <- d %>% mutate(ltbi_ltfu_tpt=ltbi_est_tpt*params$p_est_ltfu,
                    ltbi_ltfu_not=ltbi_est_not*params$p_est_ltfu,
                    ltbi_est_tpt=ltbi_est_tpt-ltbi_ltfu_tpt,
                    ltbi_est_not=ltbi_est_not-ltbi_ltfu_not,
                    no_tb_ltfu_tpt=no_tb_est_tpt*params$p_est_ltfu,
                    no_tb_ltfu_not=no_tb_est_not*params$p_est_ltfu +
                      no_tb_est_not_tb*params$p_est_ltfu,
                    no_tb_est_tpt=no_tb_est_tpt-no_tb_ltfu_tpt,
                    no_tb_est_not=no_tb_est_not*(1-params$p_est_ltfu),
                    no_tb_est_not_tb=no_tb_est_not_tb*(1-params$p_est_ltfu),
                    active_tb_ltfu_tpt=active_tb_est_tpt*params$p_est_ltfu,
                    active_tb_ltfu_not=active_tb_est_not*params$p_est_ltfu,
                    active_tb_est_tpt=active_tb_est_tpt-active_tb_ltfu_tpt,
                    active_tb_est_not=active_tb_est_not-active_tb_ltfu_not)
  
  d <- d %>% select(cases_est, notif_est, tb_deaths_enroll, 
                    initiate_ipt_est, initiate_3hp_est, initiate_1hp_est,
                    tox_nohosp_ipt_est, tox_nohosp_3hp_est, tox_nohosp_1hp_est,
                    tox_hosp_ipt_est, tox_hosp_3hp_est, tox_hosp_1hp_est,
                    complete_ipt_est, complete_3hp_est, complete_1hp_est,
                    part_complete_ipt_est, part_complete_3hp_est, part_complete_1hp_est,
                    ltbi_est_tpt, ltbi_est_not, 
                    active_tb_est_tpt, active_tb_est_not,
                    no_tb_est_tpt, no_tb_est_not, no_tb_est_not_tb,
                    ltbi_ltfu_tpt, ltbi_ltfu_not,
                    active_tb_ltfu_tpt, active_tb_ltfu_not,
                    no_tb_ltfu_tpt, no_tb_ltfu_not)
  return(d)
}

#models TB outcomes for all PLHIV
model_tb_plhiv <- function(d, d_ltbi, d_covg, d_ltbi_covg, params) {
  #d_ltbi (list) is separate from d because each state is stratified by time since entering the model
  
  #TB deaths happen first (also include for those that will progress this timestep - otherwise no opportunty to die if notified)
  #calculate deaths based on previous timestep - no need to stratify by new vs. previously enrolled
  deaths_output <- model_deaths_plhiv(d, d_ltbi, d_covg, params)
  tb_deaths <- deaths_output[[1]]
  non_tb_deaths <- deaths_output[[2]]
  cum_tb_deaths <- deaths_output[[3]]
  cum_non_tb_deaths <- deaths_output[[4]]
  
  #calculate new size of previously enrolled
  #order (after deaths) is 1 ART-related transitions, 2 reactivations/reinfections, and 3 notifications/tx
  #order (after deaths) is 1 reactivations/reinfections (and resulting deaths), 2 notifications/tx, and 3 ART transitions
  #retain tracking by t (time in model) for ltbi's who remain ltbi (element-wise mult), otherwise sum across t's (matrix mult)
  #add columns of 0s to beginning of matrices to move everyone up by 1 year
  ltbi_est_tpt <- cbind(rep(0, nrow(d_covg)), #add new year 1
                        t(t(d_ltbi$ltbi_est_tpt)*(1-params$p_reactivate_est))*(1-params$p_est_die)*
                          (1-params$p_est_ltfu)*(1-params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art) +
                          t(t(d_ltbi$ltbi_ltfu_tpt)*(1-params$p_reactivate_ltfu))*(1-params$p_ltfu_die)*
                          params$p_ltfu_art*(1-params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu))
  ltbi_est_tpt[,params$yrs] <- ltbi_est_tpt[,params$yrs] + ltbi_est_tpt[,params$yrs+1] #add "yr 11" to yrs 10+
  ltbi_est_tpt <- ltbi_est_tpt[,1:params$yrs] #remove "yr 11"
  ltbi_est_not <- cbind(rep(0, nrow(d_covg)), 
                        t(t(d_ltbi$ltbi_est_not)*(1-params$p_reactivate_est))*(1-params$p_est_die)*
                          (1-params$p_est_ltfu)*(1-params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art) +
                          t(t(d_ltbi$ltbi_ltfu_not)*(1-params$p_reactivate_ltfu))*(1-params$p_ltfu_die)*
                          params$p_ltfu_art*(1-params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu))
  ltbi_est_not[,params$yrs] <- ltbi_est_not[,params$yrs] + ltbi_est_not[,params$yrs+1]
  ltbi_est_not <- ltbi_est_not[,1:params$yrs]
  #p_new_die and p_new_ltfu vary by year in new, so need different ordering/parantheses than ltbi_est above
  ltbi_new_tpt <- cbind(rep(0, nrow(d_covg)),
                        t(t(d_ltbi$ltbi_new_tpt)*(1-params$p_reactivate_new)*(1-params$p_new_die)*(1-params$p_new_ltfu))*
                          (1-params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art))[,1:params$yrs] #no one new in last yr
  ltbi_new_not <- cbind(rep(0, nrow(d_covg)),
                        t(t(d_ltbi$ltbi_new_not)*(1-params$p_reactivate_new)*(1-params$p_new_die)*(1-params$p_new_ltfu))*
                          (1-params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art))[,1:params$yrs] #no one new in last yr
  #move to established if all fully established
  ltbi_est_tpt[, (params$yrs_new+1)] <- ltbi_est_tpt[, (params$yrs_new+1)] + ltbi_new_tpt[, (params$yrs_new+1)]
  ltbi_new_tpt[, (params$yrs_new+1)] <- 0 #replace with zeros if all fully established
  ltbi_est_not[, (params$yrs_new+1)] <- ltbi_est_not[, (params$yrs_new+1)] + ltbi_new_not[, (params$yrs_new+1)]
  ltbi_new_not[, (params$yrs_new+1)] <- 0 #replace with zeros if all fully established
  
  #add new infections
  ltbi_est_tpt[, 1] <- ltbi_est_tpt[,1] + 
    d$no_tb_est_tpt*(1-params$p_est_die)*params$p_infect*(1-params$prop_fast_art)*(1-params$p_est_ltfu) +
    d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*params$p_infect*(1-params$prop_fast_ltfu)*params$p_ltfu_art +
    d$no_tb_new_tpt*(1-params$p_new_die[1])*params$p_infect*(1-params$prop_fast_art)*(1-params$p_new_ltfu[1])
  ltbi_est_not[, 1] <- ltbi_est_not[,1] + 
    (d$no_tb_est_not + d$no_tb_est_not_tb)*(1-params$p_est_die)*params$p_infect*(1-params$prop_fast_art)*(1-params$p_est_ltfu) +
    d$no_tb_ltfu_not*(1-params$p_ltfu_die)*params$p_infect*(1-params$prop_fast_ltfu)*params$p_ltfu_art +
    (d$no_tb_new_not + d$no_tb_new_not_tb)*(1-params$p_new_die[1])*params$p_infect*(1-params$prop_fast_art)*(1-params$p_new_ltfu[1])
  
  no_tb_est_tpt <- d$no_tb_est_tpt*(1-params$p_est_die)*(1-params$p_infect)*(1-params$p_est_ltfu) +
    d$no_tb_est_tpt*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success*(1-params$p_est_ltfu) + #can get infected, notified, cured in single year
    d$no_tb_new_tpt*(1-params$p_new_die[1])*(1-params$p_infect)*(1-params$p_new_ltfu[1]) +
    d$no_tb_new_tpt*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success*(1-params$p_new_ltfu[1]) +
    d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*(1-params$p_infect)*params$p_ltfu_art +
    d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*params$p_success + #if notified or if transition back on ART
    d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*(1-params$p_est_ltfu) + #if notified or if transition back on ART
    d$active_tb_est_tpt*(1-params$p_die_tb_art)*(1-params$p_est_die)*params$p_notif*params$p_success*(1-params$p_est_ltfu) +
    d$active_tb_new_tpt*(1-params$p_die_tb_art)*(1-params$p_new_die[1])*params$p_notif*params$p_success*(1-params$p_new_ltfu[1]) +
    d$active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*params$p_notif_ltfu*params$p_success*(1-params$p_est_ltfu) +
    d$active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*(1-params$p_notif_ltfu)*params$p_ltfu_art*params$p_success +
    d_ltbi$ltbi_new_tpt%*%(params$p_reactivate_new*(1-params$p_new_die)*(1-params$p_new_ltfu))*(1-params$p_die_tb_art)*params$p_notif*params$p_success +
    d_ltbi$ltbi_est_tpt%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*params$p_success*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_ltfu_tpt%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_ltfu_tpt%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*params$p_success +
    d_ltbi$ltbi_new_tpt%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*(1-params$p_new_ltfu))*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success +
    d_ltbi$ltbi_est_tpt%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*params$p_success*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_ltfu_tpt%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_ltfu_tpt%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*params$p_success
  no_tb_est_not <- d$no_tb_est_not*(1-params$p_est_die)*(1-params$p_infect)*(1-params$p_est_ltfu) +
    d$no_tb_new_not*(1-params$p_new_die[1])*(1-params$p_infect)*(1-params$p_new_ltfu[1]) +
    d$no_tb_ltfu_not*(1-params$p_ltfu_die)*(1-params$p_infect)*params$p_ltfu_art
  #track no TPT that got TB separately (wouldn't be eligible for TPT)
  no_tb_est_not_tb <- d$no_tb_est_not_tb*(1-params$p_est_die)*(1-params$p_infect)*(1-params$p_est_ltfu) +
    (d$no_tb_est_not_tb + d$no_tb_est_not)*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*
    (1-params$p_die_tb_art)*params$p_notif*params$p_success*(1-params$p_est_ltfu) +
    d$no_tb_new_not_tb*(1-params$p_new_die[1])*(1-params$p_infect)*(1-params$p_new_ltfu[1]) +
    (d$no_tb_new_not_tb + d$no_tb_new_not)*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*
    (1-params$p_die_tb_art)*params$p_notif*params$p_success*(1-params$p_new_ltfu[1]) +
    d$no_tb_ltfu_not*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*
    (1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*(1-params$p_est_ltfu) +
    d$no_tb_ltfu_not*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*
    (1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*params$p_success +
    d$active_tb_est_not*(1-params$p_die_tb_art)*(1-params$p_est_die)*params$p_notif*params$p_success*(1-params$p_est_ltfu) +
    d$active_tb_new_not*(1-params$p_die_tb_art)*(1-params$p_new_die[1])*params$p_notif*params$p_success*(1-params$p_new_ltfu[1]) +
    d$active_tb_ltfu_not*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*params$p_notif_ltfu*params$p_success*(1-params$p_est_ltfu) +
    d$active_tb_ltfu_not*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*(1-params$p_notif_ltfu)*params$p_ltfu_art*params$p_success +
    d_ltbi$ltbi_new_not%*%(params$p_reactivate_new*(1-params$p_new_die)*(1-params$p_new_ltfu))*(1-params$p_die_tb_art)*params$p_notif*params$p_success +
    d_ltbi$ltbi_est_not%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*params$p_success*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_ltfu_not%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_ltfu_not%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*params$p_success +
    d_ltbi$ltbi_new_not%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*(1-params$p_new_ltfu))*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success +
    d_ltbi$ltbi_est_not%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*params$p_success*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_ltfu_not%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_ltfu_not%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*params$p_success
  active_tb_est_tpt <- d$active_tb_est_tpt*(1-params$p_est_die)*(1-params$p_die_tb_art)*(1-params$p_notif)*(1-params$p_est_ltfu) +
    d$active_tb_new_tpt*(1-params$p_new_die[1])*(1-params$p_die_tb_art)*(1-params$p_notif)*(1-params$p_new_ltfu[1]) +
    d_ltbi$ltbi_new_tpt%*%(params$p_reactivate_new*(1-params$p_new_die)*(1-params$p_new_ltfu))*(1-params$p_die_tb_art)*(1-params$p_notif) +
    d_ltbi$ltbi_est_tpt%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*(1-params$p_notif)*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_new_tpt%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*(1-params$p_new_ltfu))*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*(1-params$p_notif) +
    d_ltbi$ltbi_est_tpt%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*(1-params$p_notif)*(1-params$p_est_ltfu) +
    d$no_tb_est_tpt*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*(1-params$p_notif)*(1-params$p_est_ltfu) +
    d$no_tb_new_tpt*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*(1-params$p_notif)*(1-params$p_new_ltfu[1]) +
    #those that get notified but have unsuccessful tx outcomes
    d$active_tb_est_tpt*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*(1-params$p_est_ltfu) +
    d$active_tb_new_tpt*(1-params$p_new_die[1])*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*(1-params$p_new_ltfu[1]) +
    d_ltbi$ltbi_new_tpt%*%(params$p_reactivate_new*(1-params$p_new_die)*(1-params$p_new_ltfu))*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success) +
    d_ltbi$ltbi_est_tpt%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_new_tpt%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*(1-params$p_new_ltfu))*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success) +
    d_ltbi$ltbi_est_tpt%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*(1-params$p_est_ltfu) +
    d$no_tb_est_tpt*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*params$p_notif*(1-params$p_success)*(1-params$p_est_ltfu) +
    d$no_tb_new_tpt*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*params$p_notif*(1-params$p_success)*(1-params$p_new_ltfu[1]) +
    #those that reactivate/get infected & fast progress, return from LTFU (thereby being notified) but have unsuccessful tx outcomes
    d$active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*(1-params$p_notif_ltfu)*params$p_ltfu_art*(1-params$p_success) +
    d_ltbi$ltbi_ltfu_tpt%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*(1-params$p_success) +
    d_ltbi$ltbi_ltfu_tpt%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*(1-params$p_success) +
    d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*(1-params$p_success) #if notified or if transition back on ART
  
  active_tb_est_not <- d$active_tb_est_not*(1-params$p_est_die)*(1-params$p_die_tb_art)*(1-params$p_notif)*(1-params$p_est_ltfu) +
    d$active_tb_new_not*(1-params$p_new_die[1])*(1-params$p_die_tb_art)*(1-params$p_notif)*(1-params$p_new_ltfu[1]) +
    d_ltbi$ltbi_new_not%*%(params$p_reactivate_new*(1-params$p_new_die)*(1-params$p_new_ltfu))*(1-params$p_die_tb_art)*(1-params$p_notif) +
    d_ltbi$ltbi_est_not%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*(1-params$p_notif)*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_new_not%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*(1-params$p_new_ltfu))*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*(1-params$p_notif) +
    d_ltbi$ltbi_est_not%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*(1-params$p_notif)*(1-params$p_est_ltfu) +
    d$no_tb_est_not*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*(1-params$p_notif)*(1-params$p_est_ltfu) +
    d$no_tb_new_not*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*(1-params$p_notif)*(1-params$p_new_ltfu[1]) +
    d$no_tb_est_not_tb*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*(1-params$p_notif)*(1-params$p_est_ltfu) +
    d$no_tb_new_not_tb*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*(1-params$p_notif)*(1-params$p_new_ltfu[1]) +
    #those that get notified but have unsuccessful tx outcomes
    d$active_tb_est_not*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*(1-params$p_est_ltfu) +
    d$active_tb_new_not*(1-params$p_new_die[1])*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*(1-params$p_new_ltfu[1]) +
    d_ltbi$ltbi_new_not%*%(params$p_reactivate_new*(1-params$p_new_die)*(1-params$p_new_ltfu))*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success) +
    d_ltbi$ltbi_est_not%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*(1-params$p_est_ltfu) +
    d_ltbi$ltbi_new_not%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*(1-params$p_new_ltfu))*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success) +
    d_ltbi$ltbi_est_not%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*(1-params$p_est_ltfu) +
    d$no_tb_est_not*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*params$p_notif*(1-params$p_success)*(1-params$p_est_ltfu) +
    d$no_tb_new_not*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*params$p_notif*(1-params$p_success)*(1-params$p_new_ltfu[1]) +
    d$no_tb_est_not_tb*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*params$p_notif*(1-params$p_success)*(1-params$p_est_ltfu) +
    d$no_tb_new_not_tb*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*params$p_notif*(1-params$p_success)*(1-params$p_new_ltfu[1]) +
    #those that return from LTFU (thereby being notified) but have unsuccessful tx outcomes
    d$active_tb_ltfu_not*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*(1-params$p_notif_ltfu)*params$p_ltfu_art*(1-params$p_success) +
    d_ltbi$ltbi_ltfu_not%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*(1-params$p_success) +
    d_ltbi$ltbi_ltfu_not%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*(1-params$p_success) +
    d$no_tb_ltfu_not*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art*(1-params$p_success) 
  
  #calculate newly LTFU - keep track of TPT status since we allow return to ART (new since I4TB version)
  #assume those that are notified automatically return to ART, those that aren't can't return to ART
  #retain tracking by t (time in model) for ltbi's who remain ltbi (element-wise mult), otherwise sum across t's (matrix mult)
  #unlike above, nobody stays in new depending on yr (because they're all now LTFU)
  ltbi_ltfu_tpt <- cbind(rep(0, nrow(d_covg)), #add new year 1
                         t(t(d_ltbi$ltbi_ltfu_tpt)*(1-params$p_reactivate_ltfu))*(1-params$p_ltfu_die)*
                           (1-params$p_ltfu_art)*(1-params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu) + #subtract out deaths, reactivations, returns to ART
                           t(t(d_ltbi$ltbi_new_tpt)*((1-params$p_reactivate_new)*(1-params$p_new_die)*params$p_new_ltfu))*
                           (1-params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art) + #add newly lost to follow up that don't die or reactivate
                           t(t(d_ltbi$ltbi_est_tpt)*(1-params$p_reactivate_est))*(1-params$p_est_die)*
                           (1-params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art)*params$p_est_ltfu)
  ltbi_ltfu_tpt[,params$yrs] <- ltbi_ltfu_tpt[,params$yrs] + ltbi_ltfu_tpt[,params$yrs+1] #add yr 11 to yr 10+
  ltbi_ltfu_tpt <- ltbi_ltfu_tpt[,1:params$yrs] #remove yr 11
  ltbi_ltfu_not <- cbind(rep(0, nrow(d_covg)), 
                         t(t(d_ltbi$ltbi_ltfu_not)*(1-params$p_reactivate_ltfu))*(1-params$p_ltfu_die)*
                           (1-params$p_ltfu_art)*(1-params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu) + #subtract out deaths and reactivations
                           t(t(d_ltbi$ltbi_new_not)*((1-params$p_reactivate_new)*(1-params$p_new_die)*params$p_new_ltfu))*
                           (1-params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art) +
                           t(t(d_ltbi$ltbi_est_not)*(1-params$p_reactivate_est))*(1-params$p_est_die)*
                           (1-params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art)*params$p_est_ltfu)
  ltbi_ltfu_not[,params$yrs] <- ltbi_ltfu_not[,params$yrs] + ltbi_ltfu_not[,params$yrs+1] #add yr 11 to yr 10+
  ltbi_ltfu_not <- ltbi_ltfu_not[,1:params$yrs] #remove yr 11
  
  #add new infections
  ltbi_ltfu_tpt[, 1] <- ltbi_ltfu_tpt[,1] + 
    d$no_tb_est_tpt*(1-params$p_est_die)*params$p_infect*(1-params$prop_fast_art)*params$p_est_ltfu +
    d$no_tb_new_tpt*(1-params$p_new_die[1])*params$p_infect*(1-params$prop_fast_art)*params$p_new_ltfu[1] +
    d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*params$p_infect*(1-params$prop_fast_ltfu)*(1-params$p_ltfu_art)
  ltbi_ltfu_not[, 1] <- ltbi_ltfu_not[,1] + 
    (d$no_tb_est_not + d$no_tb_est_not_tb)*(1-params$p_est_die)*params$p_infect*(1-params$prop_fast_art)*params$p_est_ltfu +
    (d$no_tb_new_not + d$no_tb_new_not_tb)*(1-params$p_new_die[1])*params$p_infect*(1-params$prop_fast_art)*params$p_new_ltfu[1] +
    d$no_tb_ltfu_not*(1-params$p_ltfu_die)*params$p_infect*(1-params$prop_fast_ltfu)*(1-params$p_ltfu_art)
  
  no_tb_ltfu_tpt <- d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*(1-params$p_infect)*(1-params$p_ltfu_art) + #no tb last period, subtract out deaths
    d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*params$p_est_ltfu +
    d$no_tb_new_tpt*(1-params$p_new_die[1])*(1-params$p_infect)*params$p_new_ltfu[1] + #add newly lost to follow up that don't die
    d$no_tb_new_tpt*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success*params$p_new_ltfu[1] +
    d$no_tb_est_tpt*(1-params$p_est_die)*(1-params$p_infect)*params$p_est_ltfu + 
    d$no_tb_est_tpt*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success*params$p_est_ltfu +
    d_ltbi$ltbi_new_tpt%*%(params$p_reactivate_new*(1-params$p_new_die)*params$p_new_ltfu)*(1-params$p_die_tb_art)*params$p_notif*params$p_success + #new activations that are LTFU and notified
    d_ltbi$ltbi_est_tpt%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*params$p_success*params$p_est_ltfu + #new activations that are LTFU and notified
    d_ltbi$ltbi_ltfu_tpt%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*params$p_est_ltfu +
    d_ltbi$ltbi_new_tpt%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*params$p_new_ltfu)*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success + #new activations that are LTFU and notified
    d_ltbi$ltbi_est_tpt%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*params$p_success*params$p_est_ltfu + #new activations that are LTFU and notified
    d_ltbi$ltbi_ltfu_tpt%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*params$p_est_ltfu +
    d$active_tb_new_tpt*(1-params$p_die_tb_art)*(1-params$p_new_die[1])*params$p_notif*params$p_success*params$p_new_ltfu[1] + #active TB that are LTFU and notified
    d$active_tb_est_tpt*(1-params$p_die_tb_art)*(1-params$p_est_die)*params$p_notif*params$p_success*params$p_est_ltfu + #active TB that are LTFU and notified
    d$active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*params$p_notif_ltfu*params$p_success*params$p_est_ltfu
  no_tb_ltfu_not <- d$no_tb_ltfu_not*(1-params$p_ltfu_die)*(1-params$p_infect)*(1-params$p_ltfu_art) + #no tb last period, subtract out deaths
    d$no_tb_ltfu_not*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*params$p_est_ltfu +
    (d$no_tb_new_not + d$no_tb_new_not_tb)*(1-params$p_new_die[1])*(1-params$p_infect)*params$p_new_ltfu[1] + 
    (d$no_tb_new_not + d$no_tb_new_not_tb)*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success*params$p_new_ltfu[1] +
    (d$no_tb_est_not + d$no_tb_est_not_tb)*(1-params$p_est_die)*(1-params$p_infect)*params$p_est_ltfu + 
    (d$no_tb_est_not + d$no_tb_est_not_tb)*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success*params$p_est_ltfu +
    d_ltbi$ltbi_new_not%*%(params$p_reactivate_new*(1-params$p_new_die)*params$p_new_ltfu)*(1-params$p_die_tb_art)*params$p_notif*params$p_success + #new activations that are LTFU and notified
    d_ltbi$ltbi_est_not%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*params$p_success*params$p_est_ltfu + #new activations that are LTFU and notified
    d_ltbi$ltbi_ltfu_not%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*params$p_est_ltfu +
    d_ltbi$ltbi_new_not%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*params$p_new_ltfu)*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*params$p_success + #new activations that are LTFU and notified
    d_ltbi$ltbi_est_not%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*params$p_success*params$p_est_ltfu + #new activations that are LTFU and notified
    d_ltbi$ltbi_ltfu_not%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*params$p_success*params$p_est_ltfu +
    d$active_tb_new_not*(1-params$p_die_tb_art)*(1-params$p_new_die[1])*params$p_notif*params$p_success*params$p_new_ltfu[1] + #active TB that are LTFU and notified
    d$active_tb_est_not*(1-params$p_die_tb_art)*(1-params$p_est_die)*params$p_notif*params$p_success*params$p_est_ltfu + #active TB that are LTFU and notified
    d$active_tb_ltfu_not*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*params$p_notif_ltfu*params$p_success*params$p_est_ltfu
  active_tb_ltfu_tpt <- d$active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*(1-params$p_notif_ltfu)*(1-params$p_ltfu_art) + #subtract out deaths & notifications
    d_ltbi$ltbi_ltfu_tpt%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*(1-params$p_ltfu_art) + #new activations that aren't notified and don't die
    d_ltbi$ltbi_new_tpt%*%(params$p_reactivate_new*(1-params$p_new_die)*params$p_new_ltfu)*(1-params$p_die_tb_art)*(1-params$p_notif) + #reactivations that aren't notified and are LTFU
    d_ltbi$ltbi_est_tpt%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*(1-params$p_notif)*params$p_est_ltfu + #reactivations that aren't notified and are LTFU
    d_ltbi$ltbi_new_tpt%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*params$p_new_ltfu)*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*(1-params$p_notif) + #new activations that are LTFU and notified
    d_ltbi$ltbi_est_tpt%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*(1-params$p_notif)*params$p_est_ltfu + #new activations that are LTFU and notified
    d_ltbi$ltbi_ltfu_tpt%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*(1-params$p_ltfu_art) +
    d$active_tb_new_tpt*(1-params$p_die_tb_art)*(1-params$p_new_die[1])*(1-params$p_notif)*params$p_new_ltfu[1] + #active TB that are LTFU
    d$active_tb_est_tpt*(1-params$p_die_tb_art)*(1-params$p_est_die)*(1-params$p_notif)*params$p_est_ltfu + #active TB that are LTF
    d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*(1-params$p_ltfu_art) +
    d$no_tb_new_tpt*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*(1-params$p_notif)*params$p_new_ltfu[1] +
    d$no_tb_est_tpt*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*(1-params$p_notif)*params$p_est_ltfu +
    #add in those that get notified but don't have successful tx outcomes
    d$active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*params$p_notif_ltfu*(1-params$p_success)*(1-params$p_ltfu_art) + #subtract out deaths & notifications
    d_ltbi$ltbi_ltfu_tpt%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*(1-params$p_success)*(1-params$p_ltfu_art) + #new activations that aren't notified and don't die
    d_ltbi$ltbi_new_tpt%*%(params$p_reactivate_new*(1-params$p_new_die)*params$p_new_ltfu)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success) + #reactivations that aren't notified and are LTFU
    d_ltbi$ltbi_est_tpt%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*params$p_est_ltfu + #reactivations that aren't notified and are LTFU
    d_ltbi$ltbi_new_tpt%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*params$p_new_ltfu)*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success) + #new activations that are LTFU and notified
    d_ltbi$ltbi_est_tpt%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*params$p_est_ltfu + #new activations that are LTFU and notified
    d_ltbi$ltbi_ltfu_tpt%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*(1-params$p_success)*(1-params$p_ltfu_art) +
    d$active_tb_new_tpt*(1-params$p_die_tb_art)*(1-params$p_new_die[1])*params$p_notif*(1-params$p_success)*params$p_new_ltfu[1] + #active TB that are LTFU
    d$active_tb_est_tpt*(1-params$p_die_tb_art)*(1-params$p_est_die)*params$p_notif*(1-params$p_success)*params$p_est_ltfu + #active TB that are LTF
    d$no_tb_ltfu_tpt*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*(1-params$p_success)*(1-params$p_ltfu_art) +
    d$no_tb_new_tpt*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*params$p_new_ltfu[1] +
    d$no_tb_est_tpt*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*params$p_est_ltfu 
  active_tb_ltfu_not <- d$active_tb_ltfu_not*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*params$p_notif_ltfu*(1-params$p_success)*(1-params$p_ltfu_art) + #subtract out deaths & notifications
    d_ltbi$ltbi_ltfu_not%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*(1-params$p_success)*(1-params$p_ltfu_art) + #new activations that aren't notified and don't die
    d_ltbi$ltbi_new_not%*%(params$p_reactivate_new*(1-params$p_new_die)*params$p_new_ltfu)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success) + #reactivations that aren't notified and are LTFU
    d_ltbi$ltbi_est_not%*%params$p_reactivate_est*(1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*params$p_est_ltfu + #reactivations that aren't notified and are LTFU
    d_ltbi$ltbi_new_not%*%((1-params$p_reactivate_new)*(1-params$p_new_die)*params$p_new_ltfu)*
    params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success) + #new activations that are LTFU and notified
    d_ltbi$ltbi_est_not%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*
    (1-params$p_est_die)*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*params$p_est_ltfu + #new activations that are LTFU and notified
    d_ltbi$ltbi_ltfu_not%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*
    (1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*(1-params$p_success)*(1-params$p_ltfu_art) +
    d$active_tb_new_not*(1-params$p_die_tb_art)*(1-params$p_new_die[1])*params$p_notif*(1-params$p_success)*params$p_new_ltfu[1] + #active TB that are LTFU
    d$active_tb_est_not*(1-params$p_die_tb_art)*(1-params$p_est_die)*params$p_notif*(1-params$p_success)*params$p_est_ltfu + #active TB that are LTFU
    d$no_tb_ltfu_not*(1-params$p_ltfu_die)*params$p_infect*params$prop_fast_ltfu*(1-params$p_die_tb_ltfu)*params$p_notif_ltfu*(1-params$p_success)*(1-params$p_ltfu_art) +
    (d$no_tb_new_not + d$no_tb_new_not_tb)*(1-params$p_new_die[1])*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*params$p_new_ltfu[1] +
    (d$no_tb_est_not + d$no_tb_est_not_tb)*(1-params$p_est_die)*params$p_infect*params$prop_fast_art*(1-params$p_die_tb_art)*params$p_notif*(1-params$p_success)*params$p_est_ltfu
  
  #add to cases before TPT-related adjustments are made 
  cases_est <- d_ltbi$ltbi_new_tpt%*%(params$p_reactivate_new*(1-params$p_new_die)) +
    d_ltbi$ltbi_est_tpt%*%params$p_reactivate_est*(1-params$p_est_die) +
    d_ltbi$ltbi_new_not%*%(params$p_reactivate_new*(1-params$p_new_die)) +
    d_ltbi$ltbi_est_not%*%params$p_reactivate_est*(1-params$p_est_die) +
    d_ltbi$ltbi_new_tpt%*%((1-params$p_reactivate_new)*(1-params$p_new_die))*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art +
    d_ltbi$ltbi_est_tpt%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_est_die) +
    d_ltbi$ltbi_new_not%*%((1-params$p_reactivate_new)*(1-params$p_new_die))*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art +
    d_ltbi$ltbi_est_not%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_est_die) +
    d$no_tb_est_tpt*params$p_infect*params$prop_fast_art*(1-params$p_est_die) +
    d$no_tb_est_not*params$p_infect*params$prop_fast_art*(1-params$p_est_die) +
    d$no_tb_est_not_tb*params$p_infect*params$prop_fast_art*(1-params$p_est_die) +
    d$no_tb_new_tpt*params$p_infect*params$prop_fast_art*(1-params$p_new_die[1]) +
    d$no_tb_new_not*params$p_infect*params$prop_fast_art*(1-params$p_new_die[1]) +
    d$no_tb_new_not_tb*params$p_infect*params$prop_fast_art*(1-params$p_new_die[1]) 
  notif_est <- d_ltbi$ltbi_new_tpt%*%(params$p_reactivate_new*(1-params$p_new_die))*params$p_notif +
    d_ltbi$ltbi_est_tpt%*%params$p_reactivate_est*(1-params$p_est_die)*params$p_notif +
    d_ltbi$ltbi_new_not%*%(params$p_reactivate_new*(1-params$p_new_die))*params$p_notif +
    d_ltbi$ltbi_est_not%*%params$p_reactivate_est*(1-params$p_est_die)*params$p_notif +
    d_ltbi$ltbi_new_tpt%*%((1-params$p_reactivate_new)*(1-params$p_new_die))*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*params$p_notif +
    d_ltbi$ltbi_est_tpt%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_est_die)*params$p_notif +
    d_ltbi$ltbi_new_not%*%((1-params$p_reactivate_new)*(1-params$p_new_die))*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*params$p_notif +
    d_ltbi$ltbi_est_not%*%(1-params$p_reactivate_est)*params$p_infect*(1-params$ltbi_protect_art)*params$prop_fast_art*(1-params$p_est_die)*params$p_notif +
    d$active_tb_est_tpt*(1-params$p_die_tb_art)*(1-params$p_est_die)*params$p_notif +
    d$active_tb_new_tpt*(1-params$p_die_tb_art)*(1-params$p_new_die[1])*params$p_notif +
    d$active_tb_est_not*(1-params$p_die_tb_art)*(1-params$p_est_die)*params$p_notif +
    d$active_tb_new_not*(1-params$p_die_tb_art)*(1-params$p_new_die[1])*params$p_notif +
    d$no_tb_est_tpt*params$p_infect*params$prop_fast_art*(1-params$p_est_die)*params$p_notif +
    d$no_tb_est_not*params$p_infect*params$prop_fast_art*(1-params$p_est_die)*params$p_notif +
    d$no_tb_est_not_tb*params$p_infect*params$prop_fast_art*(1-params$p_est_die)*params$p_notif +
    d$no_tb_new_tpt*params$p_infect*params$prop_fast_art*(1-params$p_new_die[1])*params$p_notif +
    d$no_tb_new_not*params$p_infect*params$prop_fast_art*(1-params$p_new_die[1])*params$p_notif +
    d$no_tb_new_not_tb*params$p_infect*params$prop_fast_art*(1-params$p_new_die[1])*params$p_notif 
  cases_ltfu <- d_ltbi$ltbi_ltfu_tpt%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die) +
    d_ltbi$ltbi_ltfu_not%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die) +
    d_ltbi$ltbi_ltfu_tpt%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*(1-params$p_ltfu_die) +
    d_ltbi$ltbi_ltfu_not%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*(1-params$p_ltfu_die) +
    d$no_tb_ltfu_tpt*params$p_infect*params$prop_fast_ltfu*(1-params$p_ltfu_die) +
    d$no_tb_ltfu_not*params$p_infect*params$prop_fast_ltfu*(1-params$p_ltfu_die)
  notif_ltfu <- d_ltbi$ltbi_ltfu_tpt%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*params$p_notif_ltfu +
    d_ltbi$ltbi_ltfu_tpt%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art +
    d_ltbi$ltbi_ltfu_not%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*params$p_notif_ltfu +
    d_ltbi$ltbi_ltfu_not%*%params$p_reactivate_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art +
    d_ltbi$ltbi_ltfu_tpt%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*(1-params$p_ltfu_die)*params$p_notif_ltfu +
    d_ltbi$ltbi_ltfu_not%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*(1-params$p_ltfu_die)*params$p_notif_ltfu +
    d_ltbi$ltbi_ltfu_tpt%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art +
    d_ltbi$ltbi_ltfu_not%*%(1-params$p_reactivate_ltfu)*params$p_infect*(1-params$ltbi_protect_ltfu)*params$prop_fast_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art +
    d$active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*params$p_notif_ltfu +
    d$active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu)*(1-params$p_ltfu_die)*(1-params$p_notif_ltfu)*params$p_ltfu_art +
    d$no_tb_ltfu_tpt*params$p_infect*params$prop_fast_ltfu*(1-params$p_ltfu_die)*params$p_notif_ltfu +
    d$no_tb_ltfu_not*params$p_infect*params$prop_fast_ltfu*(1-params$p_ltfu_die)*params$p_notif_ltfu +
    d$no_tb_ltfu_tpt*params$p_infect*params$prop_fast_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art +
    d$no_tb_ltfu_not*params$p_infect*params$prop_fast_ltfu*(1-params$p_ltfu_die)*(1-params$p_die_tb_ltfu)*(1-params$p_notif_ltfu)*params$p_ltfu_art
  
  #calculate which previously enrolled go on TPT, complete, etc.
  #only need to keep track of t (year since entering model) for the explicitly ltbi variables
  initiate_ipt_est <- (rowSums(ltbi_est_not) + no_tb_est_not)*d_covg$initiate_ipt_covg_prev #previously enrolled(prev)=established (est)
  initiate_3hp_est <- (rowSums(ltbi_est_not) + no_tb_est_not)*d_covg$initiate_3hp_covg_prev
  initiate_1hp_est <- (rowSums(ltbi_est_not) + no_tb_est_not)*d_covg$initiate_1hp_covg_prev
  initiate_ipt_ltbi_est <- ltbi_est_not*d_covg$initiate_ipt_covg_prev #keep matrix format for LTBI
  initiate_3hp_ltbi_est <- ltbi_est_not*d_covg$initiate_3hp_covg_prev
  initiate_1hp_ltbi_est <- ltbi_est_not*d_covg$initiate_1hp_covg_prev
  tox_nohosp_ipt_est <- initiate_ipt_est*params$p_tox_nohosp_ipt
  tox_nohosp_3hp_est <- initiate_3hp_est*params$p_tox_nohosp_3hp
  tox_nohosp_1hp_est <- initiate_1hp_est*params$p_tox_nohosp_1hp
  tox_hosp_ipt_est <- initiate_ipt_est*params$p_tox_hosp_ipt
  tox_hosp_3hp_est <- initiate_3hp_est*params$p_tox_hosp_3hp
  tox_hosp_1hp_est <- initiate_1hp_est*params$p_tox_hosp_1hp
  complete_ipt_est <- initiate_ipt_est*params$p_complete_ipt*(1-(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt))
  complete_3hp_est <- initiate_3hp_est*params$p_complete_3hp*(1-(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp))
  complete_1hp_est <- initiate_1hp_est*params$p_complete_1hp*(1-(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp))
  part_complete_ipt_est <- initiate_ipt_est - 
    (complete_ipt_est + tox_nohosp_ipt_est + tox_hosp_ipt_est)
  part_complete_3hp_est <- initiate_3hp_est - 
    (complete_3hp_est + tox_nohosp_3hp_est + tox_hosp_3hp_est)
  part_complete_1hp_est <- initiate_1hp_est - 
    (complete_1hp_est + tox_nohosp_1hp_est + tox_hosp_1hp_est)
  complete_ipt_ltbi_est <- initiate_ipt_ltbi_est*params$p_complete_ipt #keep matrix format
  complete_3hp_ltbi_est <- initiate_3hp_ltbi_est*params$p_complete_3hp
  complete_1hp_ltbi_est <- initiate_1hp_ltbi_est*params$p_complete_1hp
  part_complete_ipt_ltbi_est <- initiate_ipt_ltbi_est*
    (1-(params$p_complete_ipt + params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt))
  part_complete_3hp_ltbi_est <- initiate_3hp_ltbi_est*
    (1-(params$p_complete_3hp + params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp))
  part_complete_1hp_ltbi_est <- initiate_1hp_ltbi_est*
    (1-(params$p_complete_1hp + params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp))
  
  #calculate results of TPT provision on TB status and TPT status
  #those that initiate are no longer eligible for TPT again, regardless of completion
  ltbi_est_not <- ltbi_est_not - 
    (initiate_ipt_ltbi_est + initiate_3hp_ltbi_est + initiate_1hp_ltbi_est)
  no_tb_est_not <- no_tb_est_not - 
    (initiate_ipt_est - rowSums(initiate_ipt_ltbi_est)) -
    (initiate_3hp_est - rowSums(initiate_3hp_ltbi_est)) -
    (initiate_1hp_est - rowSums(initiate_1hp_ltbi_est))
  #full completion of those with LTBI
  ltbi_est_tpt <- ltbi_est_tpt + complete_ipt_ltbi_est*(1-params$eff_ipt) +
    complete_3hp_ltbi_est*(1-params$eff_3hp) +
    complete_1hp_ltbi_est*(1-params$eff_1hp)
  no_tb_est_tpt <- no_tb_est_tpt + rowSums(complete_ipt_ltbi_est)*params$eff_ipt +
    rowSums(complete_3hp_ltbi_est)*params$eff_3hp +
    rowSums(complete_1hp_ltbi_est)*params$eff_1hp
  #partial completion of those with LTBI
  ltbi_est_tpt <- ltbi_est_tpt + part_complete_ipt_ltbi_est*(1-params$eff_ipt/2) +
    part_complete_3hp_ltbi_est*(1-params$eff_3hp/2) +
    part_complete_1hp_ltbi_est*(1-params$eff_1hp/2)
  no_tb_est_tpt <- no_tb_est_tpt + rowSums(part_complete_ipt_ltbi_est)*params$eff_ipt/2 +
    rowSums(part_complete_3hp_ltbi_est)*params$eff_3hp/2 +
    rowSums(part_complete_1hp_ltbi_est)*params$eff_1hp/2
  #no completion if those with LTBI - toxicity - no efficacy so only flows to ltbi_est_tpt
  ltbi_est_tpt <- ltbi_est_tpt + 
    initiate_ipt_ltbi_est*(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt) +
    initiate_3hp_ltbi_est*(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp) +
    initiate_1hp_ltbi_est*(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp)
  #those that initiate and didn't have LTBI go to no_tb_est_tpt regardless of completion status
  no_tb_est_tpt <- no_tb_est_tpt + 
    (initiate_ipt_est - rowSums(initiate_ipt_ltbi_est)) +
    (initiate_3hp_est - rowSums(initiate_3hp_ltbi_est)) +
    (initiate_1hp_est - rowSums(initiate_1hp_ltbi_est))
  
  #add back to data and take only variables we need to track for next round
  #LTBI matrices are in a separate list
  est_data <- data.frame(tb_deaths, non_tb_deaths, 
                         cum_tb_deaths, cum_non_tb_deaths,
                         cases_est, notif_est,
                         cases_ltfu, notif_ltfu,
                         #TPT outcomes (to calculate costs)
                         initiate_ipt_est, initiate_3hp_est, initiate_1hp_est,
                         tox_nohosp_ipt_est, tox_nohosp_3hp_est, tox_nohosp_1hp_est,
                         tox_hosp_ipt_est, tox_hosp_3hp_est, tox_hosp_1hp_est,
                         complete_ipt_est, complete_3hp_est, complete_1hp_est,
                         part_complete_ipt_est, part_complete_3hp_est, part_complete_1hp_est,
                         #TB-TPT status (for next timestep)
                         active_tb_est_tpt, active_tb_est_not,
                         no_tb_est_tpt, no_tb_est_not, no_tb_est_not_tb, 
                         active_tb_ltfu_tpt, active_tb_ltfu_not,
                         no_tb_ltfu_tpt, no_tb_ltfu_not
  )
  est_data_ltbi <- list(ltbi_new_tpt, ltbi_new_not,
                        ltbi_est_tpt, ltbi_est_not, 
                        ltbi_ltfu_tpt, ltbi_ltfu_not)
  names(est_data_ltbi) <- c("ltbi_new_tpt", "ltbi_new_not",
                            "ltbi_est_tpt", "ltbi_est_not", 
                            "ltbi_ltfu_tpt", "ltbi_ltfu_not")
  
  #add newly enrolled for this year from d_covg (already modeled before loop)
  d_covg <- d_covg %>% select(-c(tb_deaths, non_tb_deaths, 
                                 cum_tb_deaths, cum_non_tb_deaths,
                                 cases_est, notif_est,
                                 cases_ltfu, notif_ltfu,
                                 #TPT outcomes (to calculate costs)
                                 initiate_ipt_est, initiate_3hp_est, initiate_1hp_est,
                                 tox_nohosp_ipt_est, tox_nohosp_3hp_est, tox_nohosp_1hp_est,
                                 tox_hosp_ipt_est, tox_hosp_3hp_est, tox_hosp_1hp_est,
                                 complete_ipt_est, complete_3hp_est, complete_1hp_est,
                                 part_complete_ipt_est, part_complete_3hp_est, part_complete_1hp_est,
                                 #TB-TPT status (for next timestep)
                                 active_tb_est_tpt, active_tb_est_not,
                                 no_tb_est_tpt, no_tb_est_not, no_tb_est_not_tb,
                                 active_tb_ltfu_tpt, active_tb_ltfu_not,
                                 no_tb_ltfu_tpt, no_tb_ltfu_not
  ))
  #add newly enrolled LTBI for this year from d_ltbi_covg (already modeled before loop)
  est_data_ltbi[["ltbi_new_tpt"]][,1] <- d_ltbi_covg$ltbi_new_tpt
  est_data_ltbi[["ltbi_new_not"]][,1] <- d_ltbi_covg$ltbi_new_not
  
  data <- bind_cols(d_covg, est_data)
  data_all <- list(data, est_data_ltbi)
  return(data_all)
}

#calculate costs
calc_costs <- function(data, pop_type, costs, cost_impl_input, 
                       params, disc_fac, start_yr) { #costs=country varying, params=child_params, adol_params, etc.
  if(pop_type %in% c("child", "adol", "adult")) {
    #INITIAL COSTS - FROM CONTACT INVESTIGATION
    #contact investigation costs - include CXR if CXR is used for screening
    #include outpatient here too - if CXR is used, everyone has to get a screening visit (and will initiate tx/TPT on that visit)
    #if CXR isn't used then everyone needs a visit to get Xpert testing (if screen+) or initiate TPT (if screen -)
    data <- data %>% mutate(cost_contact=contact_invest_num*
                              if_else(params$cxr_screen==1, 
                                      costs$contact + costs$xray + costs$outpatient, 
                                      costs$contact))
    #tests from contact investigation - costs of the test + single outpatient visit (if CXR wasn't used for screening - if it was that already requires an outpatient visit)
    data <- data %>% mutate(cost_tests_contact=contact_invest_screen*
                              case_when((pop_type=="child" & params$tb_test=="cxr")~costs$xray + costs$outpatient,
                                        (pop_type=="child" & params$tb_test=="xpert")~costs$xpert + costs$outpatient,
                                        (pop_type!="child" & params$cxr_screen==1)~costs$xpert,
                                        (pop_type!="child" & params$cxr_screen==0)~costs$xpert + costs$outpatient))
    #bundle tests with contact investigation (treatment of TPs and FPs is bundled w/ other tx)
    data <- data %>% mutate(cost_contact=cost_contact+cost_tests_contact)
  }
  #INITIAL COSTS - OF TPT
  #TPT - drugs + outpatient visits + delivery costs, assume anyone who stops early incurs 1/2 costs
  #for subclinical contacts cases, if there is no CXR screening, need to add an initiation visit (regardless of age group)
  if(length(params$c_3hp)==1 & pop_type %in% c("child", "adol", "adult")) {
    data <- data %>% 
      mutate(cost_ipt=(complete_ipt + 
                         part_complete_ipt*params$part_course_cost + 
                         tox_nohosp_ipt*params$part_course_cost + 
                         tox_hosp_ipt*params$part_course_cost)*
               params$c_ipt*(1+params$c_delivery_ipt)*(1+params$wastage) + 
               (complete_ipt + part_complete_ipt*0.5 + 
                  tox_nohosp_ipt*0.5 + tox_hosp_ipt*0.5)*
               params$n_visit_ipt*costs$outpatient +
               initiate_ipt_negscreen*costs$outpatient*(params$cxr_screen==0),
             cost_3hp=(complete_3hp + 
                         part_complete_3hp*params$part_course_cost + 
                         tox_nohosp_3hp*params$part_course_cost + 
                         tox_hosp_3hp*params$part_course_cost)*
               params$c_3hp*(1+params$c_delivery_3hp)*(1+params$wastage) +
               (complete_3hp + part_complete_3hp*0.5 + 
                  tox_nohosp_3hp*0.5 + tox_hosp_3hp*0.5)*
               params$n_visit_3hp*costs$outpatient +
               initiate_3hp_negscreen*costs$outpatient*(params$cxr_screen==0),
             cost_1hp=(complete_1hp + 
                         part_complete_1hp*params$part_course_cost + 
                         tox_nohosp_1hp*params$part_course_cost + 
                         tox_hosp_1hp*params$part_course_cost)*
               params$c_1hp*(1+params$c_delivery_1hp)*(1+params$wastage) +
               (complete_1hp + part_complete_1hp*0.5 + 
                  tox_nohosp_1hp*0.5 + tox_hosp_1hp*0.5)*
               params$n_visit_1hp*costs$outpatient +
               initiate_1hp_negscreen*costs$outpatient*(params$cxr_screen==0))
  } else if(length(params$c_3hp)>1 & pop_type %in% c("child", "adol", "adult")) {
    data <- data %>% 
      mutate(cost_ipt=(complete_ipt + 
                         part_complete_ipt*params$part_course_cost + 
                         tox_nohosp_ipt*params$part_course_cost + 
                         tox_hosp_ipt*params$part_course_cost)*
               params$c_ipt*(1+params$c_delivery_ipt)*(1+params$wastage) + 
               (complete_ipt + part_complete_ipt*0.5 + 
                  tox_nohosp_ipt*0.5 + tox_hosp_ipt*0.5)*
               params$n_visit_ipt*costs$outpatient +
               initiate_ipt_negscreen*costs$outpatient*(params$cxr_screen==0),
             cost_3hp=(complete_3hp + 
                         part_complete_3hp*params$part_course_cost + 
                         tox_nohosp_3hp*params$part_course_cost + 
                         tox_hosp_3hp*params$part_course_cost)*
               params$c_3hp[as.character(year)]*(1+params$c_delivery_3hp)*
               (1+params$wastage) +
               (complete_3hp + part_complete_3hp*0.5 + 
                  tox_nohosp_3hp*0.5 + tox_hosp_3hp*0.5)*
               params$n_visit_3hp*costs$outpatient +
               initiate_3hp_negscreen*costs$outpatient*(params$cxr_screen==0),
             cost_1hp=(complete_1hp + 
                         part_complete_1hp*params$part_course_cost + 
                         tox_nohosp_1hp*params$part_course_cost + 
                         tox_hosp_1hp*params$part_course_cost)*
               params$c_1hp[as.character(year)]*(1+params$c_delivery_1hp)*
               (1+params$wastage) +
               (complete_1hp + part_complete_1hp*0.5 + 
                  tox_nohosp_1hp*0.5 + tox_hosp_1hp*0.5)*
               params$n_visit_1hp*costs$outpatient +
               initiate_1hp_negscreen*costs$outpatient*(params$cxr_screen==0))
  } else if(length(params$c_3hp)==1 & pop_type=="plhiv") {
    data <- data %>% 
      mutate(cost_ipt=(complete_ipt + 
                         part_complete_ipt*params$part_course_cost + 
                         tox_nohosp_ipt*params$part_course_cost + 
                         tox_hosp_ipt*params$part_course_cost)*
               params$c_ipt*(1+params$c_delivery_ipt)*(1+params$wastage) + 
               (complete_ipt + part_complete_ipt*0.5 + 
                  tox_nohosp_ipt*0.5 + tox_hosp_ipt*0.5)*
               params$n_visit_ipt*costs$outpatient,
             cost_3hp=(complete_3hp + 
                         part_complete_3hp*params$part_course_cost + 
                         tox_nohosp_3hp*params$part_course_cost + 
                         tox_hosp_3hp*params$part_course_cost)*
               params$c_3hp*(1+params$c_delivery_3hp)*(1+params$wastage) +
               (complete_3hp + part_complete_3hp*0.5 + 
                  tox_nohosp_3hp*0.5 + tox_hosp_3hp*0.5)*
               params$n_visit_3hp*costs$outpatient,
             cost_1hp=(complete_1hp + 
                         part_complete_1hp*params$part_course_cost + 
                         tox_nohosp_1hp*params$part_course_cost + 
                         tox_hosp_1hp*params$part_course_cost)*
               params$c_1hp*(1+params$c_delivery_1hp)*(1+params$wastage) +
               (complete_1hp + part_complete_1hp*0.5 + 
                  tox_nohosp_1hp*0.5 + tox_hosp_1hp*0.5)*
               params$n_visit_1hp*costs$outpatient)
  } else if(length(params$c_3hp)>1 & pop_type=="plhiv") {
    data <- data %>% 
      mutate(cost_ipt=(complete_ipt + 
                         part_complete_ipt*params$part_course_cost + 
                         tox_nohosp_ipt*params$part_course_cost + 
                         tox_hosp_ipt*params$part_course_cost)*
               params$c_ipt*(1+params$c_delivery_ipt)*(1+params$wastage) + 
               (complete_ipt + part_complete_ipt*0.5 + 
                  tox_nohosp_ipt*0.5 + tox_hosp_ipt*0.5)*
               params$n_visit_ipt*costs$outpatient,
             cost_3hp=(complete_3hp + 
                         part_complete_3hp*params$part_course_cost + 
                         tox_nohosp_3hp*params$part_course_cost + 
                         tox_hosp_3hp*params$part_course_cost)*
               params$c_3hp[as.character(year)]*(1+params$c_delivery_3hp)*
               (1+params$wastage) +
               (complete_3hp + part_complete_3hp*0.5 + 
                  tox_nohosp_3hp*0.5 + tox_hosp_3hp*0.5)*
               params$n_visit_3hp*costs$outpatient,
             cost_1hp=(complete_1hp + 
                         part_complete_1hp*params$part_course_cost + 
                         tox_nohosp_1hp*params$part_course_cost + 
                         tox_hosp_1hp*params$part_course_cost)*
               params$c_1hp[as.character(year)]*(1+params$c_delivery_1hp)*
               (1+params$wastage) +
               (complete_1hp + part_complete_1hp*0.5 + 
                  tox_nohosp_1hp*0.5 + tox_hosp_1hp*0.5)*
               params$n_visit_1hp*costs$outpatient)
  }
  #TPT adverse events - nohosp = tox_labs + outpatient
  #hosp = tox_labs + outpatient + 7 days inpatient
  data <- data %>% 
    mutate(cost_tox=(tox_nohosp_ipt + tox_nohosp_3hp + tox_nohosp_1hp)*
             (costs$lab_tox + costs$outpatient) + 
             (tox_hosp_ipt + tox_hosp_3hp + tox_hosp_1hp)*
             (costs$lab_tox + costs$outpatient + costs$inpatient*params$nday_hosp_tox))
  #ONGOING COSTS - ART and TB NOTIFICATIONS
  #ART for everyone left alive that isn't LTFU (PLHIV only) - include 4 outpatient visits too
  if(pop_type=="plhiv") {
    #treatment for TB notifications
    data <- data %>% mutate(cost_tx=costs$tb_tx*notif)
    data <- data %>% mutate(cost_art=(no_tb + ltbi + active_tb - ltfu)*
                              (params$c_art_yr + params$n_visit_art*costs$outpatient))
  } 
  if(pop_type %in% c("child", "adol", "adult")) {
    data <- data %>% mutate(cost_tx=costs$tb_tx*notif_tb)
  }
  data <- data %>% mutate(cost_impl=cost_impl_input)
  if(pop_type %in% c("child", "adol", "adult")) {
    data <- data %>% 
      mutate(costs=cost_contact + cost_ipt + cost_3hp + cost_1hp + 
               cost_tox + cost_tx + cost_impl)
    data <- data %>% select(-c(cost_tests_contact, starts_with("c_imp")))
  } else if(pop_type=="plhiv") {
    data <- data %>% 
      mutate(costs=cost_ipt + cost_3hp + cost_1hp + 
               cost_tox + cost_tx + cost_art + cost_impl)
  }
  data <- data %>% mutate(disc_costs=costs/((1+disc_fac)^(year-start_yr)))
  data <- data %>% group_by(country, code, scenario) %>%
    mutate(cum_costs=cumsum(costs), cum_disc_costs=cumsum(disc_costs))
  return(data)
}

#wrapper function to run the whole model
run_model_plhiv <- function(country_name, regimen, covg, options, params) {
  policy_horizon <- 5 #for now, implement over 5 years and calculate costs/outcomes over 5 years
  analytic_horizon <- 5
  start_yr <- 2024
  end_yr <- start_yr + analytic_horizon - 1
  policy_end_yr <- start_yr + policy_horizon - 1
  
  #options to be added later
  pwh_transitions <- 1 #1=base case (infrequent transitions on/off ART); 2=more frequent transitions
  reinfect <- 0 #0=base case, or annual risk of infection
  price_tpt <- "base" #base, 3hp_reduced (3HP price only reduced by 50%), or vary (vary price widely in PSA)
  cost_tbtx <- "base" #base (main analysis, varies by country), vary (vary widely in PSA)
  visits_3hp <- 1 #1 or 2 monitoring visits (1 in main analysis - initiation and completion)
  tpt_wastage <- "full courses"  #wastage factor for drugs (e.g. 0.1), or cost out "full courses" for all initiators
  comp_scen <- "none"
  
  #implementation of options
  if(reinfect!=0) {
    params$p_infect <- reinfect
  }
  params$n_visit_3hp <- visits_3hp
  params$wastage <- ifelse(tpt_wastage=="full courses", 0, as.double(tpt_wastage))
  params$part_course_cost <- ifelse(tpt_wastage=="full courses", 1, 0.5)
  
  #load parameters 
  #total numbers of PLHIV to cover by year
  pop_calcs <- pop_calcs %>% dplyr::select(code, country, year, plhiv_art_new_lag, backlog_none) %>%
    filter(country==country_name &
             year>=start_yr & year<=policy_end_yr) %>%
    rename("plhiv_new"="plhiv_art_new_lag",
           "backlog"="backlog_none") %>%
    mutate(backlog=if_else(year==min(year), backlog, as.numeric(NA)))
  pop_calcs_none <- pop_calcs %>%
    mutate(scenario="comparison",
           initiate_ipt_covg_new=0,
           initiate_ipt_covg_prev=0,
           initiate_3hp_covg_new=0,
           initiate_3hp_covg_prev=0,
           initiate_1hp_covg_new=0,
           initiate_1hp_covg_prev=0)
  pop_calcs_tpt <- pop_calcs %>% 
    mutate(scenario="tpt",
           initiate_ipt_covg_new=if_else(regimen=="6H", covg*params$p_initiate, 0),
           initiate_ipt_covg_prev=if_else(regimen=="6H", covg*params$p_initiate, 0),
           initiate_3hp_covg_new=if_else(regimen=="3HP", covg*params$p_initiate, 0),
           initiate_3hp_covg_prev=if_else(regimen=="3HP", covg*params$p_initiate, 0),
           initiate_1hp_covg_new=if_else(regimen=="1HP", covg*params$p_initiate, 0),
           initiate_1hp_covg_prev=if_else(regimen=="1HP", covg*params$p_initiate, 0))
  pop <- bind_rows(pop_calcs_none, pop_calcs_tpt)
  
  #set up model
  #manipulate progression risk to be vector over time (since entering the model). last stratification is 10+
  p_reactivate_new <- c(params$p_reactivate_new_yr1, params$p_reactivate_new_yr2, rep(0, length(3:10)))
  p_reactivate_est <- c(params$p_reactivate_est_yr1, params$p_reactivate_est_yr2,
                        rep(params$p_reactivate_est_yr3_9, length(3:9)),
                        params$p_reactivate_est_yr10plus)
  p_reactivate_ltfu <- c(0, params$p_reactivate_ltfu_yr2,
                         rep(params$p_reactivate_ltfu_yr3_9, length(3:9)),
                         params$p_reactivate_ltfu_yr10plus)
  params$p_reactivate_new <- p_reactivate_new
  params$p_reactivate_est <- p_reactivate_est
  params$p_reactivate_ltfu <- p_reactivate_ltfu
  #need similar time-dependent vector for death/LTFU risk among newly enrolled
  p_new_ltfu <- c(params$p_new_ltfu, params$p_est_ltfu, rep(0, length(3:10)))
  p_new_die <- c(params$p_new_die, params$p_est_die, rep(0, length(3:10)))
  params$p_new_ltfu <- p_new_ltfu
  params$p_new_die <- p_new_die
  
  plhiv_new <- model_tb_new_plhiv(pop, params) #update: LTBI prev varies by country
  
  #initiate remaining columns to be updated each timestep
  plhiv <- pop %>% ungroup() %>% #deaths start out as 0 (we add tb_deaths_enroll in the loop over years)
    mutate(cases_est=0, notif_est=0, 
           cases_ltfu=0, notif_ltfu=0,
           tb_deaths=0, non_tb_deaths=0,
           cum_tb_deaths=0, cum_non_tb_deaths=0,
           #TPT outcomes start out as 0 
           initiate_ipt_est=0, initiate_3hp_est=0, initiate_1hp_est=0,
           tox_nohosp_ipt_est=0, tox_nohosp_3hp_est=0, tox_nohosp_1hp_est=0,
           tox_hosp_ipt_est=0, tox_hosp_3hp_est=0, tox_hosp_1hp_est=0,
           complete_ipt_est=0, complete_3hp_est=0, complete_1hp_est=0,
           part_complete_ipt_est=0, part_complete_3hp_est=0, part_complete_1hp_est=0,
           #TB-TPT status - est not on TPT are not 0s - need to split up by latent status
           #LTBI are in plhiv_ltbi instead
           active_tb_est_tpt=0, 
           active_tb_est_not=if_else(year==start_yr, backlog*params$p_ltbi*params$p_reactivate_est_yr1, 0),
           no_tb_est_tpt=0, 
           no_tb_est_not=if_else(year==start_yr, backlog*(1-params$p_ltbi), 0),
           no_tb_est_not_tb=0,
           #TB status - LTFU (LTBI are in plhiv_ltbi instead)
           active_tb_ltfu_tpt=0, active_tb_ltfu_not=0,
           no_tb_ltfu_tpt=0, no_tb_ltfu_not=0
    )
  plhiv <- cbind(plhiv, plhiv_new %>% select(tb_deaths_enroll)) #tb_deaths_enroll remains constant over strategies
  
  #LTBI separate dataframes so we can track yr since entering the model
  plhiv_ltbi <- cbind("t"=1,
                      pop %>% select(country, code, year, scenario, backlog),
                      plhiv_new %>% select(ltbi_new_tpt, ltbi_new_not)) %>%
    mutate(ltbi_est_tpt=0, 
           ltbi_est_not=if_else(year==start_yr, backlog*params$p_ltbi*(1-params$p_reactivate_est_yr1), 0),
           ltbi_ltfu_tpt=0, ltbi_ltfu_not=0) %>%
    select(-backlog)
  #model TB cases/notifications and TPT initiation among previously enrolled PWH
  plhiv_est <- model_outcomes_est_plhiv(plhiv %>% filter(year==start_yr), 
                                        plhiv_ltbi %>% filter(year==start_yr), 
                                        params)
  
  #merge TPT status and case updates back into plhiv and plhiv_ltbi - replace works best for conditional replacement
  plhiv_ltbi <- plhiv_ltbi %>% mutate(ltbi_est_tpt=replace(ltbi_est_tpt, year==start_yr, plhiv_est$ltbi_est_tpt),
                                      ltbi_est_not=replace(ltbi_est_not, year==start_yr, plhiv_est$ltbi_est_not),
                                      ltbi_ltfu_tpt=replace(ltbi_ltfu_tpt, year==start_yr, plhiv_est$ltbi_ltfu_tpt),
                                      ltbi_ltfu_not=replace(ltbi_ltfu_not, year==start_yr, plhiv_est$ltbi_ltfu_not))
  plhiv <- plhiv %>% ungroup() %>% mutate(cases_est=replace(cases_est, year==start_yr, plhiv_est$cases_est),
                                          notif_est=replace(notif_est, year==start_yr, plhiv_est$notif_est),
                                          tb_deaths_enroll=replace(tb_deaths_enroll, year==start_yr, tb_deaths_enroll[year==start_yr]+plhiv_est$tb_deaths_enroll),
                                          initiate_ipt_est=replace(initiate_ipt_est, year==start_yr, plhiv_est$initiate_ipt_est),
                                          initiate_3hp_est=replace(initiate_3hp_est, year==start_yr, plhiv_est$initiate_3hp_est),
                                          initiate_1hp_est=replace(initiate_1hp_est, year==start_yr, plhiv_est$initiate_1hp_est),
                                          tox_nohosp_ipt_est=replace(tox_nohosp_ipt_est, year==start_yr, plhiv_est$tox_nohosp_ipt_est),
                                          tox_nohosp_3hp_est=replace(tox_nohosp_3hp_est, year==start_yr, plhiv_est$tox_nohosp_3hp_est),
                                          tox_nohosp_1hp_est=replace(tox_nohosp_1hp_est, year==start_yr, plhiv_est$tox_nohosp_1hp_est),
                                          tox_hosp_ipt_est=replace(tox_hosp_ipt_est, year==start_yr, plhiv_est$tox_hosp_ipt_est),
                                          tox_hosp_3hp_est=replace(tox_hosp_3hp_est, year==start_yr, plhiv_est$tox_hosp_3hp_est),
                                          tox_hosp_1hp_est=replace(tox_hosp_1hp_est, year==start_yr, plhiv_est$tox_hosp_1hp_est),
                                          complete_ipt_est=replace(complete_ipt_est, year==start_yr, plhiv_est$complete_ipt_est),
                                          complete_3hp_est=replace(complete_3hp_est, year==start_yr, plhiv_est$complete_3hp_est),
                                          complete_1hp_est=replace(complete_1hp_est, year==start_yr, plhiv_est$complete_1hp_est),
                                          part_complete_ipt_est=replace(part_complete_ipt_est, year==start_yr, plhiv_est$part_complete_ipt_est),
                                          part_complete_3hp_est=replace(part_complete_3hp_est, year==start_yr, plhiv_est$part_complete_3hp_est),
                                          part_complete_1hp_est=replace(part_complete_1hp_est, year==start_yr, plhiv_est$part_complete_1hp_est),
                                          active_tb_est_tpt=replace(active_tb_est_tpt, year==start_yr, plhiv_est$active_tb_est_tpt),
                                          active_tb_est_not=replace(active_tb_est_not, year==start_yr, plhiv_est$active_tb_est_not),
                                          no_tb_est_tpt=replace(no_tb_est_tpt, year==start_yr, plhiv_est$no_tb_est_tpt),
                                          no_tb_est_not=replace(no_tb_est_not, year==start_yr, plhiv_est$no_tb_est_not),
                                          no_tb_est_not_tb=replace(no_tb_est_not_tb, year==start_yr, plhiv_est$no_tb_est_not_tb),
                                          active_tb_ltfu_tpt=replace(active_tb_ltfu_tpt, year==start_yr, plhiv_est$active_tb_ltfu_tpt),
                                          active_tb_ltfu_not=replace(active_tb_ltfu_not, year==start_yr, plhiv_est$active_tb_ltfu_not),
                                          no_tb_ltfu_tpt=replace(no_tb_ltfu_tpt, year==start_yr, plhiv_est$no_tb_ltfu_tpt),
                                          no_tb_ltfu_not=replace(no_tb_ltfu_not, year==start_yr, plhiv_est$no_tb_ltfu_not)
  )
  
  #transform LTBI states into matrix that tracks time t
  plhiv_ltbi <- do.call("rbind", replicate(10, plhiv_ltbi, simplify=F))
  ts <- unlist(lapply(1:10, function(x) rep(x, nrow(plhiv_new))))
  plhiv_ltbi <- plhiv_ltbi %>% mutate(t=ts)
  plhiv_ltbi <- plhiv_ltbi %>% mutate(ltbi_new_tpt=if_else(t==1, ltbi_new_tpt, 0),
                                      ltbi_new_not=if_else(t==1, ltbi_new_not, 0),
                                      ltbi_est_tpt=if_else(t==1, ltbi_est_tpt, 0),
                                      ltbi_est_not=if_else(t==1, ltbi_est_not, 0),
                                      ltbi_ltfu_tpt=if_else(t==1, ltbi_ltfu_tpt, 0),
                                      ltbi_ltfu_not=if_else(t==1, ltbi_ltfu_not, 0))
  plhiv_ltbi <- plhiv_ltbi %>% mutate(p_reactivate_new=p_reactivate_new[t],
                                      p_reactivate_est=p_reactivate_est[t],
                                      p_reactivate_ltfu=p_reactivate_ltfu[t])
  
  #combine plhiv with newly enrolled and add enrollment deaths for 2023 only (later years gets added in loop)
  plhiv <- bind_cols(plhiv, plhiv_new %>% select(-starts_with("ltbi"), -tb_deaths_enroll)) #LTBI goes in plhiv_ltbi instead
  plhiv <- plhiv %>% mutate(tb_deaths=if_else(year==start_yr, tb_deaths_enroll, tb_deaths),
                            cum_tb_deaths=if_else(year==start_yr, tb_deaths_enroll, cum_tb_deaths))
  
  #each year, calculate TPT initiation, events, completion
  plhiv_all <- plhiv %>% filter(year==start_yr)
  plhiv_ltbi_start <- plhiv_ltbi %>% filter(year==start_yr)
  #convert plhiv_ltbi_all to list of matrices for easier multiplication by reactivation over time
  plhiv_ltbi_start <- sapply(c('ltbi_new_tpt', 'ltbi_new_not', 'ltbi_est_tpt', 
                               'ltbi_est_not', 'ltbi_ltfu_tpt', 'ltbi_ltfu_not'), function(x)
                                 matrix(as.numeric(as.matrix(pivot_wider(plhiv_ltbi_start %>% 
                                                                           select("t", "code", "scenario", "year", !!as.symbol(x)),
                                                                         names_from="t", values_from=all_of(x)) %>%
                                                               select(-c("code", "scenario", "year")))), ncol=10), USE.NAMES=T, simplify=F)
  plhiv_ltbi_all <- list()
  plhiv_ltbi_all[[as.character(start_yr)]] <- plhiv_ltbi_start
  for(i in (start_yr+1):(end_yr)) {
    if(i<=policy_end_yr) {
      #plhiv: mortality, reactivation, notifications don't vary over time/by country
      #use TB mortality on ART for both treated and untreated (since we assume everyone gets notified)
      plhiv_prev <- plhiv_all %>% filter(year==i-1) #update status from previous timestep
      plhiv_ltbi_prev <- plhiv_ltbi_all[[as.character(i-1)]] #update status from previous timestep
      plhiv_covg <- plhiv %>% filter(year==i) #but use covg from current timestep
      plhiv_ltbi_covg <- plhiv_ltbi %>% filter(year==i & t==1)
      plhiv_cur <- model_tb_plhiv(plhiv_prev, plhiv_ltbi_prev, plhiv_covg, 
                                  plhiv_ltbi_covg, params)
      plhiv_all <- rbind(plhiv_all, plhiv_cur[[1]])
      plhiv_ltbi_all[[as.character(i)]] <- plhiv_cur[[2]]
    } else { #after end of policy horizon, just model out pops from previous years
      plhiv_prev <- plhiv_all %>% filter(year==i-1)
      plhiv_ltbi_prev <- plhiv_ltbi_all[[as.character(i-1)]] #update status from previous timestep
      plhiv_covg <- plhiv %>% filter(year==policy_end_yr) %>% mutate(year=i)
      plhiv_covg[, 5:ncol(plhiv_covg)] <- 0 #empty/0s to use in the model
      plhiv_ltbi_covg <- plhiv_ltbi %>% filter(year==policy_end_yr & t==1)
      plhiv_ltbi_covg[, 5:ncol(plhiv_ltbi_covg)] <- 0 #empty/0s to use in the model
      plhiv_cur <- model_tb_plhiv(plhiv_prev, plhiv_ltbi_prev, plhiv_covg, 
                                  plhiv_ltbi_covg, plhiv_params)
      plhiv_all <- rbind(plhiv_all, plhiv_cur[[1]])
      plhiv_ltbi_all[[as.character(i)]] <- plhiv_cur[[2]]
    }
  }
  
  plhiv_all <- plhiv_all %>% select(-tb_deaths_enroll) #now added to tb_deaths, can remove
  
  #move 2nd column of ltbi_new to prev so that "new" is 1st yr only
  for(i in 1:length(plhiv_ltbi_all)) {
    plhiv_ltbi_all[[i]][["ltbi_est_tpt"]][,2] <- plhiv_ltbi_all[[i]][["ltbi_est_tpt"]][,2] +
      plhiv_ltbi_all[[i]][["ltbi_new_tpt"]][,2]
    plhiv_ltbi_all[[i]][["ltbi_new_tpt"]][,2] <- 0
    plhiv_ltbi_all[[i]][["ltbi_est_not"]][,2] <- plhiv_ltbi_all[[i]][["ltbi_est_not"]][,2] +
      plhiv_ltbi_all[[i]][["ltbi_new_not"]][,2]
    plhiv_ltbi_all[[i]][["ltbi_new_not"]][,2] <- 0
  }
  #move LTBI sizes back into plhiv_all (no longer need tracking by t)
  plhiv_ltbi_all <- sapply(names(plhiv_ltbi_all), function(x)
    sapply(names(plhiv_ltbi_all[[1]]), function(y)
      rowSums(plhiv_ltbi_all[[x]][[y]]), USE.NAMES=T, simplify=F), USE.NAMES=T, simplify=F)
  plhiv_ltbi_all <- as.data.frame(bind_rows(plhiv_ltbi_all, .id="year"))
  plhiv_all <- cbind(plhiv_all, plhiv_ltbi_all %>% select(-year))
  
  #combine across TPT and Not TPT, Est and New groups - so same format as contacts dataframe
  plhiv_all <- plhiv_all %>% 
    mutate(ltfu=ltbi_ltfu_tpt + active_tb_ltfu_tpt + no_tb_ltfu_tpt + 
             ltbi_ltfu_not + active_tb_ltfu_not + no_tb_ltfu_not) %>% #keep track of LTFU for costs (no ART costs)
    mutate(no_tb_est_not=no_tb_est_not + no_tb_est_not_tb,
           no_tb_new_not=no_tb_new_not + no_tb_new_not_tb) %>%
    select(-c(no_tb_est_not_tb, no_tb_new_not_tb)) #remove this so consistent naming across variables - no longer need to track it
  plhiv_comb <- pivot_longer(plhiv_all, cols=c(ends_with("_tpt"), ends_with("_not")),
                             names_to=c(".value", "tpt_status"), 
                             names_sep="_(?=[^_]+$)") #this is regex for "last underscore only"
  col_sep <- which(names(plhiv_comb)=="tpt_status")
  plhiv_comb <- plhiv_comb %>% group_by_at(1:(col_sep-1)) %>%
    summarise_at(vars(-tpt_status), sum)
  plhiv_comb <- pivot_longer(plhiv_comb, cols=c(ends_with("_new"), ends_with("_est"),
                                                ends_with("_ltfu"),
                                                -ends_with("covg_new"), 
                                                -ends_with("covg_prev"),
                                                -starts_with("plhiv")),
                             names_to=c(".value", "art_status"),
                             names_sep="_(?=[^_]+$)")
  
  col_sep <- which(names(plhiv_comb)=="art_status")
  plhiv_comb <- plhiv_comb %>% group_by_at(1:(col_sep-1)) %>%
    summarise_at(vars(-art_status), ~sum(., na.rm=T)) #NAs are because no LTFU on initiation (so ipt_initiate_ltfu=NA, etc.)
  
  if(FALSE) {
    #calculate DALYs
    plhiv_comb <- calc_dalys(as.data.table(plhiv_comb), "PLHIV", params$life_exp_plhiv, 
                             params$dw_plhiv_tb, params$dw_plhiv_art, 
                             params$dw_plhiv_no_art, 
                             params$dur_tb_tx, 
                             params$disc_fac, start_yr)
  }
  
  plhiv_comb <- calc_costs(plhiv_comb, "plhiv", params, 0,
                           params, params$disc_fac, start_yr)
  
  #cost-effectiveness code would go here
  
  plhiv_comb <- plhiv_comb %>% select(-c(backlog, cum_disc_costs))
  return(plhiv_comb)
}


library(tidyverse)
library(data.table)
country_name <- "Nigeria"
regimen <- "3HP"
covg <- 0.5

params <- c(plhiv_params, unlist(cost_params[[country_code]]), 
            "p_ltbi"=ltbi_params %>% filter(iso3==country_code) %>% pull(ltbi_prev), 
            "p_child_die"=p_child_die[[country_code]], 
            "p_adol_die"=p_adol_die[[country_code]], 
            "p_adult_die"=p_adult_die[[country_code]], 
            "p_notif_child"=p_notif_child[[country_code]], 
            "p_notif_adol"=p_notif_adol[[country_code]], 
            "p_notif_1524"=p_notif_1524[[country_code]], 
            "p_notif_adult"=p_notif_adult[[country_code]],
            "p_success_child"=p_success_child[[country_code]],
            "p_success_adol"=p_success_child[[country_code]],
            "p_success_adult"=p_success_adult[[country_code]], 
            "p_success_plhiv"=p_success_plhiv[[country_code]], 
            "life_exp_child"=life_exp_child[[country_code]], 
            "life_exp_adol"=life_exp_adol[[country_code]], 
            "life_exp_adult"=life_exp_adult[[country_code]])

