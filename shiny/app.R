library(shiny)
library(bslib)
library(plotly)
library(tidyverse)
library(data.table)
library(shinyvalidate)
library(RColorBrewer)
library(DT)
library(shinyalert)
library(abind)
options(dplyr.summarise.inform = FALSE)

load(url("https://github.com/rycktessman/tpt-budget-impact/raw/main/shiny/params.Rda"))
load(url("https://github.com/rycktessman/tpt-budget-impact/raw/main/shiny/country_params.Rda"))
load(url("https://github.com/rycktessman/tpt-budget-impact/raw/main/shiny/pub_targets.Rda"))

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
  initiate_3hr_new <- (ltbi_new + no_tb_new)*d_covg$initiate_3hr_covg_new
  initiate_ipt_ltbi_new <- ltbi_new*d_covg$initiate_ipt_covg_new
  initiate_3hp_ltbi_new <- ltbi_new*d_covg$initiate_3hp_covg_new
  initiate_1hp_ltbi_new <- ltbi_new*d_covg$initiate_1hp_covg_new
  initiate_3hr_ltbi_new <- ltbi_new*d_covg$initiate_3hr_covg_new
  tox_nohosp_ipt_new <- initiate_ipt_new*params$p_tox_nohosp_ipt
  tox_nohosp_3hp_new <- initiate_3hp_new*params$p_tox_nohosp_3hp
  tox_nohosp_1hp_new <- initiate_1hp_new*params$p_tox_nohosp_1hp
  tox_nohosp_3hr_new <- initiate_3hr_new*params$p_tox_nohosp_3hr
  tox_hosp_ipt_new <- initiate_ipt_new*params$p_tox_hosp_ipt
  tox_hosp_3hp_new <- initiate_3hp_new*params$p_tox_hosp_3hp
  tox_hosp_1hp_new <- initiate_1hp_new*params$p_tox_hosp_1hp
  tox_hosp_3hr_new <- initiate_3hr_new*params$p_tox_hosp_3hr
  complete_ipt_new <- initiate_ipt_new*params$p_complete_ipt*(1-(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt))
  complete_3hp_new <- initiate_3hp_new*params$p_complete_3hp*(1-(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp))
  complete_1hp_new <- initiate_1hp_new*params$p_complete_1hp*(1-(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp))
  complete_3hr_new <- initiate_3hr_new*params$p_complete_3hr*(1-(params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr))
  part_complete_ipt_new <- initiate_ipt_new - 
    (complete_ipt_new + tox_nohosp_ipt_new + tox_hosp_ipt_new)
  part_complete_3hp_new <- initiate_3hp_new - 
    (complete_3hp_new + tox_nohosp_3hp_new + tox_hosp_3hp_new)
  part_complete_1hp_new <- initiate_1hp_new - 
    (complete_1hp_new + tox_nohosp_1hp_new + tox_hosp_1hp_new)
  part_complete_3hr_new <- initiate_3hr_new - 
    (complete_3hr_new + tox_nohosp_3hr_new + tox_hosp_3hr_new)
  
  #results of TPT/lack of TPT on TB status for newly enrolled
  ltbi_new_tpt <- initiate_ipt_ltbi_new + #LTBI and IPT initiated 
    initiate_3hp_ltbi_new +
    initiate_1hp_ltbi_new +
    initiate_3hr_ltbi_new -
    initiate_ipt_ltbi_new*params$p_complete_ipt*params$eff_ipt - #subtract out full IPT completion/efficacy
    initiate_3hp_ltbi_new*params$p_complete_3hp*params$eff_3hp - #subtract out full 3HP completion/efficacy
    initiate_1hp_ltbi_new*params$p_complete_1hp*params$eff_1hp - #subtract out full 1HP completion/efficacy
    initiate_3hr_ltbi_new*params$p_complete_3hr*params$eff_3hr - #subtract out full 3hr completion/efficacy
    initiate_ipt_ltbi_new*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-
                             params$p_tox_nohosp_ipt)*params$eff_ipt/2 - #subtract out IPT partial completion/efficacy
    initiate_3hp_ltbi_new*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-
                             params$p_tox_nohosp_3hp)*params$eff_3hp/2 - #subtract out 3HP partial completion/efficacy
    initiate_1hp_ltbi_new*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-
                             params$p_tox_nohosp_1hp)*params$eff_1hp/2 - #subtract out 1HP partial completion/efficacy
    initiate_3hr_ltbi_new*(1-params$p_complete_3hr-params$p_tox_hosp_3hr-
                             params$p_tox_nohosp_3hr)*params$eff_3hr/2 #subtract out 3hr partial completion/efficacy
  ltbi_new_not <- ltbi_new - initiate_ipt_ltbi_new - initiate_3hp_ltbi_new - 
    initiate_1hp_ltbi_new - initiate_3hr_ltbi_new #LTBI and didn't initiate TPT
  no_tb_new_tpt <- initiate_ipt_new + initiate_3hp_new + initiate_1hp_new + initiate_3hr_new -
    (initiate_ipt_ltbi_new + initiate_3hp_ltbi_new + initiate_1hp_ltbi_new + initiate_3hr_ltbi_new) + #no LTBI to begin with
    initiate_ipt_ltbi_new*params$p_complete_ipt*params$eff_ipt + #full IPT completion
    initiate_3hp_ltbi_new*params$p_complete_3hp*params$eff_3hp + #full 3HP completion
    initiate_1hp_ltbi_new*params$p_complete_1hp*params$eff_1hp + #full 1HP completion
    initiate_3hr_ltbi_new*params$p_complete_3hr*params$eff_3hr + #full 3hr completion
    initiate_ipt_ltbi_new*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-params$p_tox_nohosp_ipt)*
    params$eff_ipt/2 + #partial IPT completion
    initiate_3hp_ltbi_new*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-params$p_tox_nohosp_3hp)*
    params$eff_3hp/2 + #partial 3HP completion
    initiate_1hp_ltbi_new*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-params$p_tox_nohosp_1hp)*
    params$eff_1hp/2 + #partial 1HP completion
    initiate_3hr_ltbi_new*(1-params$p_complete_3hr-params$p_tox_hosp_3hr-params$p_tox_nohosp_3hr)*
    params$eff_3hr/2 #partial 3hr completion
  no_tb_new_not <- no_tb_new*
    (1-(d_covg$initiate_ipt_covg_new + d_covg$initiate_3hp_covg_new +
          d_covg$initiate_1hp_covg_new + d_covg$initiate_3hr_covg_new)) #no LTBI to begin with
  #track no TPT that got TB separately (wouldn't be eligible for TPT)
  no_tb_new_not_tb <- active_tb_new*params$p_notif*params$p_success
  active_tb_new_tpt <- 0 #assume no PWH are wrongly assigned to TPT
  active_tb_new_not <- active_tb_new*(1-params$p_notif) + active_tb_new*params$p_notif*(1-params$p_success)
  
  data <- data.frame(cases_new, notif_new, tb_deaths_enroll, 
                     initiate_ipt_new, initiate_3hp_new, initiate_1hp_new, initiate_3hr_new,
                     tox_nohosp_ipt_new, tox_nohosp_3hp_new, tox_nohosp_1hp_new, tox_nohosp_3hr_new,
                     tox_hosp_ipt_new, tox_hosp_3hp_new, tox_hosp_1hp_new, tox_hosp_3hr_new,
                     complete_ipt_new, complete_3hp_new, complete_1hp_new, complete_3hr_new,
                     part_complete_ipt_new, part_complete_3hp_new, part_complete_1hp_new, part_complete_3hr_new,
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
                    initiate_3hr_est=(ltbi_est_not + no_tb_est_not)*initiate_3hr_covg_prev,
                    initiate_ipt_ltbi_est=ltbi_est_not*initiate_ipt_covg_prev,
                    initiate_3hp_ltbi_est=ltbi_est_not*initiate_3hp_covg_prev,
                    initiate_1hp_ltbi_est=ltbi_est_not*initiate_1hp_covg_prev,
                    initiate_3hr_ltbi_est=ltbi_est_not*initiate_3hr_covg_prev,
                    tox_nohosp_ipt_est=initiate_ipt_est*params$p_tox_nohosp_ipt,
                    tox_nohosp_3hp_est=initiate_3hp_est*params$p_tox_nohosp_3hp,
                    tox_nohosp_1hp_est=initiate_1hp_est*params$p_tox_nohosp_1hp,
                    tox_nohosp_3hr_est=initiate_3hr_est*params$p_tox_nohosp_3hr,
                    tox_hosp_ipt_est=initiate_ipt_est*params$p_tox_hosp_ipt,
                    tox_hosp_3hp_est=initiate_3hp_est*params$p_tox_hosp_3hp,
                    tox_hosp_1hp_est=initiate_1hp_est*params$p_tox_hosp_1hp,
                    tox_hosp_3hr_est=initiate_3hr_est*params$p_tox_hosp_3hr,
                    complete_ipt_est=initiate_ipt_est*params$p_complete_ipt*
                      (1-(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt)),
                    complete_3hp_est=initiate_3hp_est*params$p_complete_3hp*
                      (1-(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp)),
                    complete_1hp_est=initiate_1hp_est*params$p_complete_1hp*
                      (1-(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp)),
                    complete_3hr_est=initiate_3hr_est*params$p_complete_3hr*
                      (1-(params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr)),
                    part_complete_ipt_est=initiate_ipt_est - 
                      (complete_ipt_est + tox_nohosp_ipt_est + tox_hosp_ipt_est),
                    part_complete_3hp_est=initiate_3hp_est - 
                      (complete_3hp_est + tox_nohosp_3hp_est + tox_hosp_3hp_est),
                    part_complete_1hp_est=initiate_1hp_est - 
                      (complete_1hp_est + tox_nohosp_1hp_est + tox_hosp_1hp_est),
                    part_complete_3hr_est=initiate_3hr_est - 
                      (complete_3hr_est + tox_nohosp_3hr_est + tox_hosp_3hr_est))
  
  #results of TPT/lack of TPT on TB status of previously enrolled
  d <- d %>% mutate(ltbi_est_tpt=initiate_ipt_ltbi_est + #LTBI and IPT initiated 
                      initiate_3hp_ltbi_est +
                      initiate_1hp_ltbi_est +
                      initiate_3hr_ltbi_est -
                      initiate_ipt_ltbi_est*params$p_complete_ipt*params$eff_ipt - #subtract out full IPT completion/efficacy
                      initiate_3hp_ltbi_est*params$p_complete_3hp*params$eff_3hp - #subtract out full 3HP completion/efficacy
                      initiate_1hp_ltbi_est*params$p_complete_1hp*params$eff_1hp - #subtract out full 1HP completion/efficacy
                      initiate_3hr_ltbi_est*params$p_complete_3hr*params$eff_3hr - #subtract out full 3hr completion/efficacy
                      initiate_ipt_ltbi_est*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-
                                               params$p_tox_nohosp_ipt)*params$eff_ipt/2 - #subtract out IPT partial completion/efficacy
                      initiate_3hp_ltbi_est*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-
                                               params$p_tox_nohosp_3hp)*params$eff_3hp/2 - #subtract out 3HP partial completion/efficacy
                      initiate_1hp_ltbi_est*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-
                                               params$p_tox_nohosp_1hp)*params$eff_1hp/2 - #subtract out 1HP partial completion/efficacy
                      initiate_3hr_ltbi_est*(1-params$p_complete_3hr-params$p_tox_hosp_3hr-
                                               params$p_tox_nohosp_3hr)*params$eff_3hr/2,  #subtract out 3hr partial completion/efficacy
                    ltbi_est_not=ltbi_est_not - initiate_ipt_ltbi_est - 
                      initiate_3hp_ltbi_est - initiate_1hp_ltbi_est - initiate_3hr_ltbi_est, #LTBI and didn't initiate TPT
                    no_tb_est_tpt=initiate_ipt_est + initiate_3hp_est + 
                      initiate_1hp_est + initiate_3hr_est -
                      (initiate_ipt_ltbi_est + initiate_3hp_ltbi_est + 
                         initiate_1hp_ltbi_est + initiate_3hr_ltbi_est) + #no LTBI to begin with
                      initiate_ipt_ltbi_est*params$p_complete_ipt*params$eff_ipt + #full IPT completion
                      initiate_3hp_ltbi_est*params$p_complete_3hp*params$eff_3hp + #full 3HP completion
                      initiate_1hp_ltbi_est*params$p_complete_1hp*params$eff_1hp + #full 1HP completion
                      initiate_3hr_ltbi_est*params$p_complete_3hr*params$eff_3hr + #full 3hr completion
                      initiate_ipt_ltbi_est*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-params$p_tox_nohosp_ipt)*
                      params$eff_ipt/2 + #partial IPT completion
                      initiate_3hp_ltbi_est*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-params$p_tox_nohosp_3hp)*
                      params$eff_3hp/2 + #partial 3HP completion
                      initiate_1hp_ltbi_est*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-params$p_tox_nohosp_1hp)*
                      params$eff_1hp/2 + #partial 1HP completion
                      initiate_3hr_ltbi_est*(1-params$p_complete_3hr-params$p_tox_hosp_3hr-params$p_tox_nohosp_3hr)*
                      params$eff_3hr/2, #partial 3hr completion
                    no_tb_est_not=no_tb_est_not*(1-(initiate_ipt_covg_prev + initiate_3hp_covg_prev + 
                                                      initiate_1hp_covg_prev + initiate_3hr_covg_prev)), #no LTBI to begin with
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
                    initiate_ipt_est, initiate_3hp_est, initiate_1hp_est, initiate_3hr_est,
                    tox_nohosp_ipt_est, tox_nohosp_3hp_est, tox_nohosp_1hp_est, tox_nohosp_3hr_est,
                    tox_hosp_ipt_est, tox_hosp_3hp_est, tox_hosp_1hp_est, tox_hosp_3hr_est,
                    complete_ipt_est, complete_3hp_est, complete_1hp_est, complete_3hr_est,
                    part_complete_ipt_est, part_complete_3hp_est, part_complete_1hp_est, part_complete_3hr_est,
                    ltbi_est_tpt, ltbi_est_not, 
                    active_tb_est_tpt, active_tb_est_not,
                    no_tb_est_tpt, no_tb_est_not, no_tb_est_not_tb,
                    ltbi_ltfu_tpt, ltbi_ltfu_not,
                    active_tb_ltfu_tpt, active_tb_ltfu_not,
                    no_tb_ltfu_tpt, no_tb_ltfu_not)
  return(d)
}

#models TB outcomes for all PLHIV
model_tb_plhiv_old <- function(d, d_ltbi, d_covg, d_ltbi_covg, params) {
  #d_ltbi (list) is separate from d because each state is stratified by time since entering the model
  
  #DONE TB deaths happen first (also include for those that will progress this timestep - otherwise no opportunty to die if notified)
  #calculate deaths based on previous timestep - no need to stratify by new vs. previously enrolled
  deaths_output <- model_deaths_plhiv(d, d_ltbi, d_covg, params)
  tb_deaths <- deaths_output[[1]]
  non_tb_deaths <- deaths_output[[2]]
  cum_tb_deaths <- deaths_output[[3]]
  cum_non_tb_deaths <- deaths_output[[4]]
  
  #PARTLY DONE - only the adding 0s part
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
  
  #DONE.calculate which previously enrolled go on TPT, complete, etc.
  #only need to keep track of t (year since entering model) for the explicitly ltbi variables
  initiate_ipt_est <- (rowSums(ltbi_est_not) + no_tb_est_not)*d_covg$initiate_ipt_covg_prev #previously enrolled(prev)=established (est)
  initiate_3hp_est <- (rowSums(ltbi_est_not) + no_tb_est_not)*d_covg$initiate_3hp_covg_prev
  initiate_1hp_est <- (rowSums(ltbi_est_not) + no_tb_est_not)*d_covg$initiate_1hp_covg_prev
  initiate_3hr_est <- (rowSums(ltbi_est_not) + no_tb_est_not)*d_covg$initiate_3hr_covg_prev
  initiate_ipt_ltbi_est <- ltbi_est_not*d_covg$initiate_ipt_covg_prev #keep matrix format for LTBI
  initiate_3hp_ltbi_est <- ltbi_est_not*d_covg$initiate_3hp_covg_prev
  initiate_1hp_ltbi_est <- ltbi_est_not*d_covg$initiate_1hp_covg_prev
  initiate_3hr_ltbi_est <- ltbi_est_not*d_covg$initiate_3hr_covg_prev
  tox_nohosp_ipt_est <- initiate_ipt_est*params$p_tox_nohosp_ipt
  tox_nohosp_3hp_est <- initiate_3hp_est*params$p_tox_nohosp_3hp
  tox_nohosp_1hp_est <- initiate_1hp_est*params$p_tox_nohosp_1hp
  tox_nohosp_3hr_est <- initiate_3hr_est*params$p_tox_nohosp_3hr
  tox_hosp_ipt_est <- initiate_ipt_est*params$p_tox_hosp_ipt
  tox_hosp_3hp_est <- initiate_3hp_est*params$p_tox_hosp_3hp
  tox_hosp_1hp_est <- initiate_1hp_est*params$p_tox_hosp_1hp
  tox_hosp_3hr_est <- initiate_3hr_est*params$p_tox_hosp_3hr
  complete_ipt_est <- initiate_ipt_est*params$p_complete_ipt*(1-(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt))
  complete_3hp_est <- initiate_3hp_est*params$p_complete_3hp*(1-(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp))
  complete_1hp_est <- initiate_1hp_est*params$p_complete_1hp*(1-(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp))
  complete_3hr_est <- initiate_3hr_est*params$p_complete_3hr*(1-(params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr))
  part_complete_ipt_est <- initiate_ipt_est - 
    (complete_ipt_est + tox_nohosp_ipt_est + tox_hosp_ipt_est)
  part_complete_3hp_est <- initiate_3hp_est - 
    (complete_3hp_est + tox_nohosp_3hp_est + tox_hosp_3hp_est)
  part_complete_1hp_est <- initiate_1hp_est - 
    (complete_1hp_est + tox_nohosp_1hp_est + tox_hosp_1hp_est)
  part_complete_3hr_est <- initiate_3hr_est - 
    (complete_3hr_est + tox_nohosp_3hr_est + tox_hosp_3hr_est)
  complete_ipt_ltbi_est <- initiate_ipt_ltbi_est*params$p_complete_ipt #keep matrix format
  complete_3hp_ltbi_est <- initiate_3hp_ltbi_est*params$p_complete_3hp
  complete_1hp_ltbi_est <- initiate_1hp_ltbi_est*params$p_complete_1hp
  complete_3hr_ltbi_est <- initiate_3hr_ltbi_est*params$p_complete_3hr
  part_complete_ipt_ltbi_est <- initiate_ipt_ltbi_est*
    (1-(params$p_complete_ipt + params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt))
  part_complete_3hp_ltbi_est <- initiate_3hp_ltbi_est*
    (1-(params$p_complete_3hp + params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp))
  part_complete_1hp_ltbi_est <- initiate_1hp_ltbi_est*
    (1-(params$p_complete_1hp + params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp))
  part_complete_3hr_ltbi_est <- initiate_3hr_ltbi_est*
    (1-(params$p_complete_3hr + params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr))
  
  #calculate results of TPT provision on TB status and TPT status
  #those that initiate are no longer eligible for TPT again, regardless of completion
  ltbi_est_not <- ltbi_est_not - 
    (initiate_ipt_ltbi_est + initiate_3hp_ltbi_est + initiate_1hp_ltbi_est + initiate_3hr_ltbi_est)
  no_tb_est_not <- no_tb_est_not - 
    (initiate_ipt_est - rowSums(initiate_ipt_ltbi_est)) -
    (initiate_3hp_est - rowSums(initiate_3hp_ltbi_est)) -
    (initiate_1hp_est - rowSums(initiate_1hp_ltbi_est)) -
    (initiate_3hr_est - rowSums(initiate_3hr_ltbi_est))
  #full completion of those with LTBI
  ltbi_est_tpt <- ltbi_est_tpt + complete_ipt_ltbi_est*(1-params$eff_ipt) +
    complete_3hp_ltbi_est*(1-params$eff_3hp) +
    complete_1hp_ltbi_est*(1-params$eff_1hp) +
    complete_3hr_ltbi_est*(1-params$eff_3hr)
  no_tb_est_tpt <- no_tb_est_tpt + rowSums(complete_ipt_ltbi_est)*params$eff_ipt +
    rowSums(complete_3hp_ltbi_est)*params$eff_3hp +
    rowSums(complete_1hp_ltbi_est)*params$eff_1hp +
    rowSums(complete_3hr_ltbi_est)*params$eff_3hr
  #partial completion of those with LTBI
  ltbi_est_tpt <- ltbi_est_tpt + part_complete_ipt_ltbi_est*(1-params$eff_ipt/2) +
    part_complete_3hp_ltbi_est*(1-params$eff_3hp/2) +
    part_complete_1hp_ltbi_est*(1-params$eff_1hp/2) +
    part_complete_3hr_ltbi_est*(1-params$eff_3hr/2)
  no_tb_est_tpt <- no_tb_est_tpt + rowSums(part_complete_ipt_ltbi_est)*params$eff_ipt/2 +
    rowSums(part_complete_3hp_ltbi_est)*params$eff_3hp/2 +
    rowSums(part_complete_1hp_ltbi_est)*params$eff_1hp/2 +
    rowSums(part_complete_3hr_ltbi_est)*params$eff_3hr/2
  #no completion if those with LTBI - toxicity - no efficacy so only flows to ltbi_est_tpt
  ltbi_est_tpt <- ltbi_est_tpt + 
    initiate_ipt_ltbi_est*(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt) +
    initiate_3hp_ltbi_est*(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp) +
    initiate_1hp_ltbi_est*(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp) +
    initiate_3hr_ltbi_est*(params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr)
  #those that initiate and didn't have LTBI go to no_tb_est_tpt regardless of completion status
  no_tb_est_tpt <- no_tb_est_tpt + 
    (initiate_ipt_est - rowSums(initiate_ipt_ltbi_est)) +
    (initiate_3hp_est - rowSums(initiate_3hp_ltbi_est)) +
    (initiate_1hp_est - rowSums(initiate_1hp_ltbi_est)) +
    (initiate_3hr_est - rowSums(initiate_3hr_ltbi_est))
  
  #add back to data and take only variables we need to track for next round
  #LTBI matrices are in a separate list
  est_data <- data.frame(tb_deaths, non_tb_deaths, 
                         cum_tb_deaths, cum_non_tb_deaths,
                         cases_est, notif_est,
                         cases_ltfu, notif_ltfu,
                         #TPT outcomes (to calculate costs)
                         initiate_ipt_est, initiate_3hp_est, initiate_1hp_est, initiate_3hr_est,
                         tox_nohosp_ipt_est, tox_nohosp_3hp_est, tox_nohosp_1hp_est, tox_nohosp_3hr_est,
                         tox_hosp_ipt_est, tox_hosp_3hp_est, tox_hosp_1hp_est, tox_hosp_3hr_est,
                         complete_ipt_est, complete_3hp_est, complete_1hp_est, complete_3hr_est,
                         part_complete_ipt_est, part_complete_3hp_est, part_complete_1hp_est, part_complete_3hr_est,
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
                                 initiate_ipt_est, initiate_3hp_est, initiate_1hp_est, initiate_3hr_est,
                                 tox_nohosp_ipt_est, tox_nohosp_3hp_est, tox_nohosp_1hp_est, tox_nohosp_3hr_est,
                                 tox_hosp_ipt_est, tox_hosp_3hp_est, tox_hosp_1hp_est, tox_hosp_3hr_est,
                                 complete_ipt_est, complete_3hp_est, complete_1hp_est, complete_3hr_est,
                                 part_complete_ipt_est, part_complete_3hp_est, part_complete_1hp_est, part_complete_3hr_est,
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

model_tb_plhiv <- function(d, d_ltbi, params) {
  #order is: (1) TPT; (2) Updates to TB status from TPT; (3) Active TB deaths; (4) Non-TB deaths, 
  # (5) TB-related transitions, (6) ART-related transitions, (7) Move LTBI up by 1 year 

  d <- d %>% ungroup() 
  #Step 1: TPT initiation
  #new
  d <- d %>% mutate(initiate_ipt_new=(as.vector(d_ltbi[,1,"ltbi_new_not"]) + no_tb_new)*initiate_ipt_covg_new,
                    initiate_3hp_new=(as.vector(d_ltbi[,1,"ltbi_new_not"]) + no_tb_new)*initiate_3hp_covg_new,
                    initiate_1hp_new=(as.vector(d_ltbi[,1,"ltbi_new_not"]) + no_tb_new)*initiate_1hp_covg_new,
                    initiate_3hr_new=(as.vector(d_ltbi[,1,"ltbi_new_not"]) + no_tb_new)*initiate_3hr_covg_new,
                    tox_nohosp_ipt_new=initiate_ipt_new*params$p_tox_nohosp_ipt,
                    tox_nohosp_3hp_new=initiate_3hp_new*params$p_tox_nohosp_3hp,
                    tox_nohosp_1hp_new=initiate_1hp_new*params$p_tox_nohosp_1hp,
                    tox_nohosp_3hr_new=initiate_3hr_new*params$p_tox_nohosp_3hr,
                    tox_hosp_ipt_new=initiate_ipt_new*params$p_tox_hosp_ipt,
                    tox_hosp_3hp_new=initiate_3hp_new*params$p_tox_hosp_3hp,
                    tox_hosp_1hp_new=initiate_1hp_new*params$p_tox_hosp_1hp,
                    tox_hosp_3hr_new=initiate_3hr_new*params$p_tox_hosp_3hr,
                    complete_ipt_new=initiate_ipt_new*params$p_complete_ipt*(1-(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt)),
                    complete_3hp_new=initiate_3hp_new*params$p_complete_3hp*(1-(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp)),
                    complete_1hp_new=initiate_1hp_new*params$p_complete_1hp*(1-(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp)),
                    complete_3hr_new=initiate_3hr_new*params$p_complete_3hr*(1-(params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr)),
                    part_complete_ipt_new=initiate_ipt_new - 
                      (complete_ipt_new + tox_nohosp_ipt_new + tox_hosp_ipt_new),
                    part_complete_3hp_new=initiate_3hp_new - 
                      (complete_3hp_new + tox_nohosp_3hp_new + tox_hosp_3hp_new),
                    part_complete_1hp_new=initiate_1hp_new - 
                      (complete_1hp_new + tox_nohosp_1hp_new + tox_hosp_1hp_new),
                    part_complete_3hr_new=initiate_3hr_new - 
                      (complete_3hr_new + tox_nohosp_3hr_new + tox_hosp_3hr_new))
  #need to keep track of t (year since entering model) for the explicitly ltbi variables - keep matrix format
  initiate_ipt_ltbi_new <- d_ltbi[,,"ltbi_new_not"]*d$initiate_ipt_covg_new 
  initiate_3hp_ltbi_new <- d_ltbi[,,"ltbi_new_not"]*d$initiate_3hp_covg_new
  initiate_1hp_ltbi_new <- d_ltbi[,,"ltbi_new_not"]*d$initiate_1hp_covg_new
  initiate_3hr_ltbi_new <- d_ltbi[,,"ltbi_new_not"]*d$initiate_3hr_covg_new
  complete_ipt_ltbi_new <- initiate_ipt_ltbi_new*params$p_complete_ipt 
  complete_3hp_ltbi_new <- initiate_3hp_ltbi_new*params$p_complete_3hp
  complete_1hp_ltbi_new <- initiate_1hp_ltbi_new*params$p_complete_1hp
  complete_3hr_ltbi_new <- initiate_3hr_ltbi_new*params$p_complete_3hr
  part_complete_ipt_ltbi_new <- initiate_ipt_ltbi_new*(1-(params$p_complete_ipt + params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt))
  part_complete_3hp_ltbi_new <- initiate_3hp_ltbi_new*(1-(params$p_complete_3hp + params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp))
  part_complete_1hp_ltbi_new <- initiate_1hp_ltbi_new*(1-(params$p_complete_1hp + params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp))
  part_complete_3hr_ltbi_new <- initiate_3hr_ltbi_new*(1-(params$p_complete_3hr + params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr))
  
  #established 
  d <- d %>% 
    mutate(initiate_ipt_est=(as.vector(rowSums(d_ltbi[,,"ltbi_est_not"])) + no_tb_est_not)*initiate_ipt_covg_est, 
           initiate_3hp_est=(as.vector(rowSums(d_ltbi[,,"ltbi_est_not"])) + no_tb_est_not)*initiate_3hp_covg_est,
           initiate_1hp_est=(as.vector(rowSums(d_ltbi[,,"ltbi_est_not"])) + no_tb_est_not)*initiate_1hp_covg_est,
           initiate_3hr_est=(as.vector(rowSums(d_ltbi[,,"ltbi_est_not"])) + no_tb_est_not)*initiate_3hr_covg_est,
           tox_nohosp_ipt_est=initiate_ipt_est*params$p_tox_nohosp_ipt,
           tox_nohosp_3hp_est=initiate_3hp_est*params$p_tox_nohosp_3hp,
           tox_nohosp_1hp_est=initiate_1hp_est*params$p_tox_nohosp_1hp,
           tox_nohosp_3hr_est=initiate_3hr_est*params$p_tox_nohosp_3hr,
           tox_hosp_ipt_est=initiate_ipt_est*params$p_tox_hosp_ipt,
           tox_hosp_3hp_est=initiate_3hp_est*params$p_tox_hosp_3hp,
           tox_hosp_1hp_est=initiate_1hp_est*params$p_tox_hosp_1hp,
           tox_hosp_3hr_est=initiate_3hr_est*params$p_tox_hosp_3hr,
           complete_ipt_est=initiate_ipt_est*params$p_complete_ipt*(1-(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt)),
           complete_3hp_est=initiate_3hp_est*params$p_complete_3hp*(1-(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp)),
           complete_1hp_est=initiate_1hp_est*params$p_complete_1hp*(1-(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp)),
           complete_3hr_est=initiate_3hr_est*params$p_complete_3hr*(1-(params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr)),
           part_complete_ipt_est=initiate_ipt_est - (complete_ipt_est + tox_nohosp_ipt_est + tox_hosp_ipt_est),
           part_complete_3hp_est=initiate_3hp_est - (complete_3hp_est + tox_nohosp_3hp_est + tox_hosp_3hp_est),
           part_complete_1hp_est=initiate_1hp_est - (complete_1hp_est + tox_nohosp_1hp_est + tox_hosp_1hp_est),
           part_complete_3hr_est=initiate_3hr_est - (complete_3hr_est + tox_nohosp_3hr_est + tox_hosp_3hr_est))
  #need to keep track of t (year since entering model) for the explicitly ltbi variables - keep matrix format
  initiate_ipt_ltbi_est <- d_ltbi[,,"ltbi_est_not"]*d$initiate_ipt_covg_est 
  initiate_3hp_ltbi_est <- d_ltbi[,,"ltbi_est_not"]*d$initiate_3hp_covg_est
  initiate_1hp_ltbi_est <- d_ltbi[,,"ltbi_est_not"]*d$initiate_1hp_covg_est
  initiate_3hr_ltbi_est <- d_ltbi[,,"ltbi_est_not"]*d$initiate_3hr_covg_est
  complete_ipt_ltbi_est <- initiate_ipt_ltbi_est*params$p_complete_ipt 
  complete_3hp_ltbi_est <- initiate_3hp_ltbi_est*params$p_complete_3hp
  complete_1hp_ltbi_est <- initiate_1hp_ltbi_est*params$p_complete_1hp
  complete_3hr_ltbi_est <- initiate_3hr_ltbi_est*params$p_complete_3hr
  part_complete_ipt_ltbi_est <- initiate_ipt_ltbi_est*(1-(params$p_complete_ipt + params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt))
  part_complete_3hp_ltbi_est <- initiate_3hp_ltbi_est*(1-(params$p_complete_3hp + params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp))
  part_complete_1hp_ltbi_est <- initiate_1hp_ltbi_est*(1-(params$p_complete_1hp + params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp))
  part_complete_3hr_ltbi_est <- initiate_3hr_ltbi_est*(1-(params$p_complete_3hr + params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr))

  
  #Step 2: update TB status based on TPT initiation
  #new
  d <- d %>% 
    mutate(no_tb_new_tpt=initiate_ipt_new + initiate_3hp_new + initiate_1hp_new + initiate_3hr_new -
             as.vector(rowSums(initiate_ipt_ltbi_new + initiate_3hp_ltbi_new + initiate_1hp_ltbi_new + initiate_3hr_ltbi_new)) + #no LTBI to begin with
             as.vector(rowSums(initiate_ipt_ltbi_new))*params$p_complete_ipt*params$eff_ipt + #full IPT completion
             as.vector(rowSums(initiate_3hp_ltbi_new))*params$p_complete_3hp*params$eff_3hp + #full 3HP completion
             as.vector(rowSums(initiate_1hp_ltbi_new))*params$p_complete_1hp*params$eff_1hp + #full 1HP completion
             as.vector(rowSums(initiate_3hr_ltbi_new))*params$p_complete_3hr*params$eff_3hr + #full 3hr completion
             as.vector(rowSums(initiate_ipt_ltbi_new))*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-params$p_tox_nohosp_ipt)*params$eff_ipt/2 + #partial IPT completion
             as.vector(rowSums(initiate_3hp_ltbi_new))*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-params$p_tox_nohosp_3hp)*params$eff_3hp/2 + #partial 3HP completion
             as.vector(rowSums(initiate_1hp_ltbi_new))*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-params$p_tox_nohosp_1hp)*params$eff_1hp/2 + #partial 1HP completion
             as.vector(rowSums(initiate_3hr_ltbi_new))*(1-params$p_complete_3hr-params$p_tox_hosp_3hr-params$p_tox_nohosp_3hr)*params$eff_3hr/2, #partial 3hr completion,
           no_tb_new_not=no_tb_new*
             (1-(initiate_ipt_covg_new + initiate_3hp_covg_new + 
                   initiate_1hp_covg_new + initiate_3hr_covg_new)), #no LTBI to begin with
           active_tb_new_tpt=0, #assume no PWH are wrongly assigned to TPT
           active_tb_new_not=active_tb_new)
  #need to keep track of t (year since entering model) for the explicitly ltbi variables - keep matrix format
  d_ltbi[,,"ltbi_new_not"] <- d_ltbi[,,"ltbi_new_not"] - 
    (initiate_ipt_ltbi_new + initiate_3hp_ltbi_new + initiate_1hp_ltbi_new + initiate_3hr_ltbi_new)
  d_ltbi[,,"ltbi_new_tpt"] <- d_ltbi[,,"ltbi_new_tpt"] + 
    complete_ipt_ltbi_new*(1-params$eff_ipt) + 
    complete_3hp_ltbi_new*(1-params$eff_3hp) +
    complete_1hp_ltbi_new*(1-params$eff_1hp) +
    complete_3hr_ltbi_new*(1-params$eff_3hr) +
    part_complete_ipt_ltbi_new*(1-params$eff_ipt/2) +
    part_complete_3hp_ltbi_new*(1-params$eff_3hp/2) +
    part_complete_1hp_ltbi_new*(1-params$eff_1hp/2) +
    part_complete_3hr_ltbi_new*(1-params$eff_3hr/2) +
    initiate_ipt_ltbi_new*(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt) +
    initiate_3hp_ltbi_new*(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp) +
    initiate_1hp_ltbi_new*(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp) +
    initiate_3hr_ltbi_new*(params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr)
  #established
  d <- d %>% 
    mutate(no_tb_est_not=no_tb_est_not - 
             (initiate_ipt_est - as.vector(rowSums(initiate_ipt_ltbi_est))) -
             (initiate_3hp_est - as.vector(rowSums(initiate_3hp_ltbi_est))) -
             (initiate_1hp_est - as.vector(rowSums(initiate_1hp_ltbi_est))) -
             (initiate_3hr_est - as.vector(rowSums(initiate_3hr_ltbi_est))),
           no_tb_est_tpt=no_tb_est_tpt + 
             as.vector(rowSums(complete_ipt_ltbi_est))*params$eff_ipt +
             as.vector(rowSums(complete_3hp_ltbi_est))*params$eff_3hp +
             as.vector(rowSums(complete_1hp_ltbi_est))*params$eff_1hp +
             as.vector(rowSums(complete_3hr_ltbi_est))*params$eff_3hr +
             as.vector(rowSums(part_complete_ipt_ltbi_est))*params$eff_ipt/2 +
             as.vector(rowSums(part_complete_3hp_ltbi_est))*params$eff_3hp/2 +
             as.vector(rowSums(part_complete_1hp_ltbi_est))*params$eff_1hp/2 +
             as.vector(rowSums(part_complete_3hr_ltbi_est))*params$eff_3hr/2 +
             (initiate_ipt_est - as.vector(rowSums(initiate_ipt_ltbi_est))) +
             (initiate_3hp_est - as.vector(rowSums(initiate_3hp_ltbi_est))) +
             (initiate_1hp_est - as.vector(rowSums(initiate_1hp_ltbi_est))) +
             (initiate_3hr_est - as.vector(rowSums(initiate_3hr_ltbi_est))))
  #need to keep track of t (year since entering model) for the explicitly ltbi variables - keep matrix format
  d_ltbi[,,"ltbi_est_not"] <- d_ltbi[,,"ltbi_est_not"] - 
    (initiate_ipt_ltbi_est + initiate_3hp_ltbi_est + initiate_1hp_ltbi_est + initiate_3hr_ltbi_est)
  d_ltbi[,,"ltbi_est_tpt"] <- d_ltbi[,,"ltbi_est_tpt"] + 
    complete_ipt_ltbi_est*(1-params$eff_ipt) + 
    complete_3hp_ltbi_est*(1-params$eff_3hp) +
    complete_1hp_ltbi_est*(1-params$eff_1hp) +
    complete_3hr_ltbi_est*(1-params$eff_3hr) +
    part_complete_ipt_ltbi_est*(1-params$eff_ipt/2) +
    part_complete_3hp_ltbi_est*(1-params$eff_3hp/2) +
    part_complete_1hp_ltbi_est*(1-params$eff_1hp/2) +
    part_complete_3hr_ltbi_est*(1-params$eff_3hr/2) +
    initiate_ipt_ltbi_est*(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt) +
    initiate_3hp_ltbi_est*(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp) +
    initiate_1hp_ltbi_est*(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp) +
    initiate_3hr_ltbi_est*(params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr)

  #Step 3: TB deaths among active
  d <- d %>% 
    mutate(tb_deaths = active_tb_new_tpt*params$p_die_tb_art +
                  active_tb_new_not*params$p_die_tb_art +
                  active_tb_est_tpt*params$p_die_tb_art +
                  active_tb_est_not*params$p_die_tb_art +
                  active_tb_ltfu_tpt*params$p_die_tb_ltfu +
                  active_tb_ltfu_not*params$p_die_tb_ltfu
                  )
  d <- d %>% mutate(active_tb_new_tpt = active_tb_new_tpt*(1-params$p_die_tb_art),
                    active_tb_new_not = active_tb_new_not*(1-params$p_die_tb_art),
                    active_tb_est_tpt = active_tb_est_tpt*(1-params$p_die_tb_art),
                    active_tb_est_not = active_tb_est_not*(1-params$p_die_tb_art),
                    active_tb_ltfu_tpt = active_tb_ltfu_tpt*(1-params$p_die_tb_ltfu),
                    active_tb_ltfu_not = active_tb_ltfu_not*(1-params$p_die_tb_ltfu))
  
  #Step 4: Non-TB Deaths
  d <- d %>% 
    mutate(non_tb_deaths = active_tb_new_tpt*params$p_new_die[1] +
             active_tb_new_not*params$p_new_die[1] +
             active_tb_est_tpt*params$p_est_die +
             active_tb_est_not*params$p_est_die +
             active_tb_ltfu_tpt*params$p_ltfu_die +
             active_tb_ltfu_not*params$p_ltfu_die +
             as.vector((d_ltbi[,,"ltbi_new_tpt"]%*%params$p_new_die)[,1]) + #for newly enrolled, mort risk drops after Y1
             as.vector((d_ltbi[,,"ltbi_new_not"]%*%params$p_new_die)[,1]) +
             as.vector(rowSums(d_ltbi[,,"ltbi_est_tpt"]))*params$p_est_die + #for established, mort risk constant
             as.vector(rowSums(d_ltbi[,,"ltbi_est_not"]))*params$p_est_die +
             as.vector(rowSums(d_ltbi[,,"ltbi_ltfu_tpt"]))*params$p_ltfu_die +
             as.vector(rowSums(d_ltbi[,,"ltbi_ltfu_not"]))*params$p_ltfu_die +
             no_tb_new_tpt*params$p_new_die[1] +
             no_tb_new_not*params$p_new_die[1] +
             no_tb_est_tpt*params$p_est_die + 
             no_tb_est_not*params$p_est_die +
             no_tb_est_not_tb*params$p_est_die +
             no_tb_ltfu_tpt*params$p_ltfu_die +
             no_tb_ltfu_not*params$p_ltfu_die)
  d <- d %>% mutate(active_tb_new_tpt = active_tb_new_tpt*(1-params$p_new_die[1]),
                      active_tb_new_not = active_tb_new_not*(1-params$p_new_die[1]),
                      active_tb_est_tpt = active_tb_est_tpt*(1-params$p_est_die),
                      active_tb_est_not = active_tb_est_not*(1-params$p_est_die),
                      active_tb_ltfu_tpt = active_tb_ltfu_tpt*(1-params$p_ltfu_die),
                      active_tb_ltfu_not = active_tb_ltfu_not*(1-params$p_ltfu_die))
  d <- d %>% mutate(no_tb_new_tpt = no_tb_new_tpt*(1-params$p_new_die[1]),
                    no_tb_new_not = no_tb_new_not*(1-params$p_new_die[1]),
                    no_tb_est_tpt = no_tb_est_tpt*(1-params$p_est_die),
                    no_tb_est_not = no_tb_est_not*(1-params$p_est_die),
                    no_tb_est_not_tb = no_tb_est_not_tb*(1-params$p_est_die),
                    no_tb_ltfu_tpt = no_tb_ltfu_tpt*(1-params$p_ltfu_die),
                    no_tb_ltfu_not = no_tb_ltfu_not*(1-params$p_ltfu_die))
  #keep LTBI as matrix
  d_ltbi[,,"ltbi_new_tpt"] <- d_ltbi[,,"ltbi_new_tpt"]*rbind((1-params$p_new_die),(1-params$p_new_die))
  d_ltbi[,,"ltbi_new_not"] <- d_ltbi[,,"ltbi_new_not"]*rbind((1-params$p_new_die),(1-params$p_new_die))
  d_ltbi[,,"ltbi_est_tpt"] <- d_ltbi[,,"ltbi_est_tpt"]*(1-params$p_est_die)
  d_ltbi[,,"ltbi_est_not"] <- d_ltbi[,,"ltbi_est_not"]*(1-params$p_est_die)
  d_ltbi[,,"ltbi_ltfu_tpt"] <- d_ltbi[,,"ltbi_ltfu_tpt"]*(1-params$p_ltfu_die)
  d_ltbi[,,"ltbi_ltfu_not"] <- d_ltbi[,,"ltbi_ltfu_not"]*(1-params$p_ltfu_die)
  
  #Step 5: Model TB transitions
  
  #reinfections (only type of new infection)
  d <- d %>% 
    mutate(no_tb_new_tpt=no_tb_new_tpt*(1-params$p_infect),
           no_tb_new_not=no_tb_new_not*(1-params$p_infect),
           no_tb_est_tpt=no_tb_est_tpt*(1-params$p_infect),
           no_tb_est_not=no_tb_est_not*(1-params$p_infect),
           no_tb_est_not_tb=no_tb_est_not_tb*(1-params$p_infect),
           no_tb_ltfu_tpt=no_tb_ltfu_tpt*(1-params$p_infect),
           no_tb_ltfu_not=no_tb_ltfu_not*(1-params$p_infect))
  d_ltbi[,1,"ltbi_new_tpt"] <- d_ltbi[,1,"ltbi_new_tpt"] + d$no_tb_new_tpt*params$p_infect
  d_ltbi[,1,"ltbi_new_not"] <- d_ltbi[,1,"ltbi_new_not"] + d$no_tb_new_not*params$p_infect
  d_ltbi[,1,"ltbi_est_tpt"] <- d_ltbi[,1,"ltbi_est_tpt"] + d$no_tb_est_tpt*params$p_infect
  d_ltbi[,1,"ltbi_est_not"] <- d_ltbi[,1,"ltbi_est_not"] + d$no_tb_est_not*params$p_infect
  d_ltbi[,1,"ltbi_est_not"] <- d_ltbi[,1,"ltbi_est_not"] + d$no_tb_est_not_tb*params$p_infect
  d_ltbi[,1,"ltbi_ltfu_tpt"] <- d_ltbi[,1,"ltbi_ltfu_tpt"] + d$no_tb_ltfu_tpt*params$p_infect
  d_ltbi[,1,"ltbi_ltfu_not"] <- d_ltbi[,1,"ltbi_ltfu_not"] + d$no_tb_ltfu_not*params$p_infect
  
  #progressions, and deaths from progressions - count cases here too
  d <- d %>% 
    mutate(active_tb_new_tpt=active_tb_new_tpt + (1-params$p_die_tb_art)*as.vector(rowSums(d_ltbi[,,"ltbi_new_tpt"]%*%params$p_reactivate_new)),
           active_tb_new_not=active_tb_new_not + (1-params$p_die_tb_art)*as.vector(rowSums(d_ltbi[,,"ltbi_new_not"]%*%params$p_reactivate_new)),
           active_tb_est_tpt=active_tb_est_tpt + (1-params$p_die_tb_art)*as.vector(rowSums(d_ltbi[,,"ltbi_est_tpt"]%*%params$p_reactivate_est)),
           active_tb_est_not=active_tb_est_not + (1-params$p_die_tb_art)*as.vector(rowSums(d_ltbi[,,"ltbi_est_not"]%*%params$p_reactivate_est)),
           active_tb_ltfu_tpt=active_tb_ltfu_tpt + (1-params$p_die_tb_ltfu)*as.vector(rowSums(d_ltbi[,,"ltbi_ltfu_tpt"]%*%params$p_reactivate_ltfu)),
           active_tb_ltfu_not=active_tb_ltfu_not + (1-params$p_die_tb_ltfu)*as.vector(rowSums(d_ltbi[,,"ltbi_ltfu_not"]%*%params$p_reactivate_ltfu)),
           tb_deaths=tb_deaths + 
             params$p_die_tb_art*as.vector(rowSums(d_ltbi[,,"ltbi_new_tpt"]%*%params$p_reactivate_new)) +
             params$p_die_tb_art*as.vector(rowSums(d_ltbi[,,"ltbi_new_not"]%*%params$p_reactivate_new)) +
             params$p_die_tb_art*as.vector(rowSums(d_ltbi[,,"ltbi_est_tpt"]%*%params$p_reactivate_est)) +
             params$p_die_tb_art*as.vector(rowSums(d_ltbi[,,"ltbi_est_not"]%*%params$p_reactivate_est)) +
             params$p_die_tb_ltfu*as.vector(rowSums(d_ltbi[,,"ltbi_ltfu_tpt"]%*%params$p_reactivate_ltfu)) +
             params$p_die_tb_ltfu*as.vector(rowSums(d_ltbi[,,"ltbi_ltfu_not"]%*%params$p_reactivate_ltfu)),
           cases=as.vector(rowSums(d_ltbi[,,"ltbi_new_tpt"]%*%params$p_reactivate_new)) +
             as.vector(rowSums(d_ltbi[,,"ltbi_new_not"]%*%params$p_reactivate_new)) +
             as.vector(rowSums(d_ltbi[,,"ltbi_est_tpt"]%*%params$p_reactivate_est)) +
             as.vector(rowSums(d_ltbi[,,"ltbi_est_not"]%*%params$p_reactivate_est)) +
             as.vector(rowSums(d_ltbi[,,"ltbi_ltfu_tpt"]%*%params$p_reactivate_ltfu)) +
             as.vector(rowSums(d_ltbi[,,"ltbi_ltfu_not"]%*%params$p_reactivate_ltfu))
    )
  d_ltbi[,,"ltbi_new_tpt"] <- d_ltbi[,,"ltbi_new_tpt"]*rbind(1-params$p_reactivate_new, 1-params$p_reactivate_new)
  d_ltbi[,,"ltbi_new_not"] <- d_ltbi[,,"ltbi_new_not"]*rbind(1-params$p_reactivate_new, 1-params$p_reactivate_new)
  d_ltbi[,,"ltbi_est_tpt"] <- d_ltbi[,,"ltbi_est_tpt"]*rbind(1-params$p_reactivate_est, 1-params$p_reactivate_est)
  d_ltbi[,,"ltbi_est_not"] <- d_ltbi[,,"ltbi_est_not"]*rbind(1-params$p_reactivate_est, 1-params$p_reactivate_est)
  d_ltbi[,,"ltbi_ltfu_tpt"] <- d_ltbi[,,"ltbi_ltfu_tpt"]*rbind(1-params$p_reactivate_ltfu, 1-params$p_reactivate_ltfu)
  d_ltbi[,,"ltbi_ltfu_not"] <- d_ltbi[,,"ltbi_ltfu_not"]*rbind(1-params$p_reactivate_ltfu, 1-params$p_reactivate_ltfu)
  
  #notifications/cures
  d <- d %>% 
    mutate(no_tb_new_tpt = no_tb_new_tpt + active_tb_new_tpt*params$p_notif*params$p_success,
           no_tb_new_not_tb = active_tb_new_not*params$p_notif*params$p_success, #track that they got TB
           no_tb_est_tpt= no_tb_est_tpt + active_tb_est_tpt*params$p_notif*params$p_success,
           no_tb_est_not_tb = no_tb_est_not_tb + active_tb_est_not*params$p_notif*params$p_success, 
           no_tb_est_tpt = no_tb_est_tpt + active_tb_ltfu_tpt*params$p_notif_ltfu*params$p_success, #if notified, LTFU -> ART
           no_tb_est_not_tb = no_tb_est_not_tb + active_tb_ltfu_not*params$p_notif_ltfu*params$p_success, 
           notif = active_tb_new_tpt*params$p_notif*params$p_success +
             active_tb_new_not*params$p_notif*params$p_success +
             active_tb_est_tpt*params$p_notif*params$p_success +
             active_tb_est_not*params$p_notif*params$p_success +
             active_tb_ltfu_tpt*params$p_notif_ltfu*params$p_success +
             active_tb_ltfu_not*params$p_notif_ltfu*params$p_success,
           active_tb_new_tpt = active_tb_new_tpt*(1-params$p_notif*params$p_success),
           active_tb_new_not = active_tb_new_not*(1-params$p_notif*params$p_success),
           active_tb_est_tpt = active_tb_est_tpt*(1-params$p_notif*params$p_success),
           active_tb_est_not = active_tb_est_not*(1-params$p_notif*params$p_success),
           active_tb_ltfu_tpt = active_tb_ltfu_tpt*(1-params$p_notif_ltfu*params$p_success),
           active_tb_ltfu_not = active_tb_ltfu_not*(1-params$p_notif_ltfu*params$p_success),
           )



  #Step 6: ART-Related Transitions
  #inflate p_LTFU since some will return the same timestep
  #new LTFU
  d <- d %>% 
    mutate(no_tb_ltfu_tpt = no_tb_ltfu_tpt + 
             params$p_new_ltfu[1]*no_tb_new_tpt/(1-params$p_ltfu_art) + 
             params$p_est_ltfu*no_tb_est_tpt/(1-params$p_ltfu_art),
           no_tb_ltfu_not = no_tb_ltfu_not + 
             params$p_new_ltfu[1]*no_tb_new_not/(1-params$p_ltfu_art) + 
             params$p_est_ltfu*no_tb_est_not/(1-params$p_ltfu_art),
           active_tb_ltfu_tpt = active_tb_ltfu_tpt + 
             params$p_new_ltfu[1]*active_tb_new_tpt/(1-params$p_ltfu_art) + 
             params$p_est_ltfu*active_tb_est_tpt/(1-params$p_ltfu_art),
           active_tb_ltfu_not = active_tb_ltfu_not + 
             params$p_new_ltfu[1]*active_tb_new_not/(1-params$p_ltfu_art) + 
             params$p_est_ltfu*active_tb_est_not/(1-params$p_ltfu_art),
           no_tb_new_tpt = no_tb_new_tpt*(1-params$p_new_ltfu[1]/(1-params$p_ltfu_art)),
           no_tb_new_not = no_tb_new_not*(1-params$p_new_ltfu[1]/(1-params$p_ltfu_art)),
           no_tb_est_tpt = no_tb_est_tpt*(1-params$p_est_ltfu/(1-params$p_ltfu_art)),
           no_tb_est_not = no_tb_est_not*(1-params$p_est_ltfu/(1-params$p_ltfu_art)),
           active_tb_new_tpt = active_tb_new_tpt*(1-params$p_new_ltfu[1]/(1-params$p_ltfu_art)),
           active_tb_new_not = active_tb_new_not*(1-params$p_new_ltfu[1]/(1-params$p_ltfu_art)),
           active_tb_est_tpt = active_tb_est_tpt*(1-params$p_est_ltfu/(1-params$p_ltfu_art)),
           active_tb_est_not = active_tb_est_not*(1-params$p_est_ltfu/(1-params$p_ltfu_art))
           )
  d_ltbi[,,"ltbi_ltfu_tpt"] <- d_ltbi[,,"ltbi_ltfu_tpt"] +
    d_ltbi[,,"ltbi_new_tpt"]*rbind(params$p_new_ltfu, params$p_new_ltfu)/(1-params$p_ltfu_art) +
    d_ltbi[,,"ltbi_est_tpt"]*params$p_est_ltfu/(1-params$p_ltfu_art)
  d_ltbi[,,"ltbi_new_tpt"] <- d_ltbi[,,"ltbi_new_tpt"]*(1-rbind(params$p_new_ltfu, params$p_new_ltfu)/(1-params$p_ltfu_art))
  d_ltbi[,,"ltbi_est_tpt"] <- d_ltbi[,,"ltbi_est_tpt"]*(1-params$p_est_ltfu/(1-params$p_ltfu_art))
  d_ltbi[,,"ltbi_ltfu_not"] <- d_ltbi[,,"ltbi_ltfu_not"] +
    d_ltbi[,,"ltbi_new_not"]*rbind(params$p_new_ltfu, params$p_new_ltfu)/(1-params$p_ltfu_art) +
    d_ltbi[,,"ltbi_est_not"]*params$p_est_ltfu/(1-params$p_ltfu_art)
  d_ltbi[,,"ltbi_new_not"] <- d_ltbi[,,"ltbi_new_not"]*(1-rbind(params$p_new_ltfu, params$p_new_ltfu)/(1-params$p_ltfu_art))
  d_ltbi[,,"ltbi_est_not"] <- d_ltbi[,,"ltbi_est_not"]*(1-params$p_est_ltfu/(1-params$p_ltfu_art))
  
  #returns from LTFU
  d <- d %>% 
    mutate(no_tb_est_tpt = no_tb_est_tpt + params$p_ltfu_art*no_tb_ltfu_tpt,
           no_tb_est_not = no_tb_est_not + params$p_ltfu_art*no_tb_ltfu_not,
           active_tb_est_tpt = active_tb_est_tpt + params$p_ltfu_art*active_tb_ltfu_tpt,
           active_tb_est_not = active_tb_est_not + params$p_ltfu_art*active_tb_ltfu_not,
           no_tb_ltfu_tpt = no_tb_ltfu_tpt*(1-params$p_ltfu_art),
           no_tb_ltfu_not = no_tb_ltfu_not*(1-params$p_ltfu_art),
           active_tb_ltfu_tpt = active_tb_ltfu_tpt*(1-params$p_ltfu_art),
           active_tb_ltfu_not = active_tb_ltfu_not*(1-params$p_ltfu_art)
           )
  d_ltbi[,,"ltbi_est_tpt"] <- d_ltbi[,,"ltbi_est_tpt"] + params$p_ltfu_art*d_ltbi[,,"ltbi_ltfu_tpt"]
  d_ltbi[,,"ltbi_est_not"] <- d_ltbi[,,"ltbi_est_not"] + params$p_ltfu_art*d_ltbi[,,"ltbi_ltfu_not"]
  d_ltbi[,,"ltbi_ltfu_tpt"] <- d_ltbi[,,"ltbi_ltfu_tpt"]*(1-params$p_ltfu_art)
  d_ltbi[,,"ltbi_ltfu_not"] <- d_ltbi[,,"ltbi_ltfu_not"]*(1-params$p_ltfu_art)
  
  #Step 7: Move all LTBI back by 1 year
  d_ltbi_new <- array(0, dim=dim(d_ltbi)+c(0,1,0))
  dimnames(d_ltbi_new)[[1]] <- dimnames(d_ltbi)[[1]] 
  dimnames(d_ltbi_new)[[3]] <- dimnames(d_ltbi)[[3]] 
  d_ltbi_new[, 2:(dim(d_ltbi_new)[2]),] <- d_ltbi
  d_ltbi_new[,params$yrs,] <- d_ltbi_new[,params$yrs,] + d_ltbi_new[,params$yrs+1,] #add "yr 11" to yrs 10+
  d_ltbi <- d_ltbi_new[,1:params$yrs,] #remove "yr 11"
  
  out <- list("plhiv"=d,
              "plhiv_ltbi"=d_ltbi)
  return(out)
 
}

#calculate TPT coverage in a given year for new/previous PLHIV
tpt_covg_plhiv <- function(d, params, option_split) {
  d <- d %>% 
    mutate(flag=(initiate_3hp_num + initiate_1hp_num + initiate_3hr_num + initiate_ipt_num)/
             ((plhiv_new + backlog - active_tb_est_not - active_tb_new - no_tb_est_not_tb)*1-params$p_initiate))
  if(option_split==2) { #prioritize newly enrolled PLHIV
    d <- d %>% #inflate by % that aren't eligible because of active disease
      mutate(initiate_tpt_covg_new=pmin(1, 
                                        (initiate_3hp_num + initiate_1hp_num + initiate_3hr_num + initiate_ipt_num)/
                                          (plhiv_new*(1-active_tb_new/plhiv_new))),
             initiate_3hp_covg_new=if_else(initiate_tpt_covg_new==0, 0, 
                                           initiate_tpt_covg_new*initiate_3hp_num/(initiate_3hp_num+initiate_1hp_num+initiate_3hr_num+initiate_ipt_num)),
             initiate_1hp_covg_new=if_else(initiate_tpt_covg_new==0, 0, 
                                           initiate_tpt_covg_new*initiate_1hp_num/(initiate_3hp_num+initiate_1hp_num+initiate_3hr_num+initiate_ipt_num)),
             initiate_3hr_covg_new=if_else(initiate_tpt_covg_new==0, 0, 
                                           initiate_tpt_covg_new*initiate_3hr_num/(initiate_3hp_num+initiate_1hp_num+initiate_3hr_num+initiate_ipt_num)),
             initiate_ipt_covg_new=if_else(initiate_tpt_covg_new==0, 0, 
                                           initiate_tpt_covg_new*initiate_ipt_num/(initiate_3hp_num+initiate_1hp_num+initiate_3hr_num+initiate_ipt_num)),
             initiate_tpt_covg_est=pmin(1, 
                                        (initiate_3hp_num + initiate_1hp_num + initiate_3hr_num + initiate_ipt_num -
                                          initiate_tpt_covg_new*plhiv_new*(1-active_tb_new/plhiv_new))/
                                          (backlog*(1-(active_tb_est_not+no_tb_est_not_tb)/backlog))),
             initiate_3hp_covg_est=if_else(initiate_tpt_covg_est==0, 0,
                                           initiate_tpt_covg_est*(initiate_3hp_num - initiate_3hp_covg_new*(plhiv_new-active_tb_new))/
                                                                    (initiate_3hp_num+initiate_1hp_num+initiate_3hr_num+initiate_ipt_num - 
                                                                       initiate_tpt_covg_new*(plhiv_new - active_tb_new))),
             initiate_1hp_covg_est=if_else(initiate_tpt_covg_est==0, 0,
                                           initiate_tpt_covg_est*(initiate_1hp_num - initiate_1hp_covg_new*(plhiv_new-active_tb_new))/
                                             (initiate_3hp_num+initiate_1hp_num+initiate_3hr_num+initiate_ipt_num - 
                                                initiate_tpt_covg_new*(plhiv_new - active_tb_new))),
             initiate_3hr_covg_est=if_else(initiate_tpt_covg_est==0, 0,
                                           initiate_tpt_covg_est*(initiate_3hr_num - initiate_3hr_covg_new*(plhiv_new-active_tb_new))/
                                             (initiate_3hp_num+initiate_1hp_num+initiate_3hr_num+initiate_ipt_num - 
                                                initiate_tpt_covg_new*(plhiv_new - active_tb_new))),
             initiate_ipt_covg_est=if_else(initiate_tpt_covg_est==0, 0,
                                           initiate_tpt_covg_est*(initiate_ipt_num - initiate_ipt_covg_new*(plhiv_new-active_tb_new))/
                                             (initiate_3hp_num+initiate_1hp_num+initiate_3hr_num+initiate_ipt_num - 
                                                initiate_tpt_covg_new*(plhiv_new - active_tb_new)))
      )
  } else if (option_split==1) {
    d <- d %>% 
      mutate(initiate_3hp_covg=if_else(initiate_3hp_num==0, 0,
                                       pmin(initiate_3hp_num/(initiate_3hp_num + initiate_1hp_num + initiate_3hr_num + initiate_ipt_num), 
                                    initiate_3hp_num/(plhiv_new + backlog - active_tb_new - active_tb_est_not - no_tb_est_not_tb))),
             initiate_3hp_covg_new=initiate_3hp_covg,
             initiate_3hp_covg_est=initiate_3hp_covg,
             initiate_1hp_covg=if_else(initiate_3hp_num==0, 0,
                                       pmin(initiate_1hp_num/(initiate_3hp_num + initiate_1hp_num + initiate_3hr_num + initiate_ipt_num), 
                                    initiate_1hp_num/(plhiv_new + backlog - active_tb_new - active_tb_est_not - no_tb_est_not_tb))),
             initiate_1hp_covg_new=initiate_1hp_covg,
             initiate_1hp_covg_est=initiate_1hp_covg,
             initiate_3hr_covg=if_else(initiate_3hp_num==0, 0,
                                       pmin(initiate_3hr_num/(initiate_3hp_num + initiate_1hp_num + initiate_3hr_num + initiate_ipt_num), 
                                    initiate_3hr_num/(plhiv_new + backlog - active_tb_new - active_tb_est_not - no_tb_est_not_tb))),
             initiate_3hr_covg_new=initiate_3hr_covg,
             initiate_3hr_covg_est=initiate_3hr_covg,
             initiate_ipt_covg=if_else(initiate_3hp_num==0, 0,
                                       pmin(initiate_ipt_num/(initiate_3hp_num + initiate_1hp_num + initiate_3hr_num + initiate_ipt_num), 
                                    initiate_ipt_num/(plhiv_new + backlog - active_tb_new - active_tb_est_not - no_tb_est_not_tb))),
             initiate_ipt_covg_new=initiate_ipt_covg,
             initiate_ipt_covg_est=initiate_ipt_covg
      )
  }
  else {
    print("Error: select a valid option to split coverage across new and established PLHIV")
  }
  return(d)
  
}
  
#used to calculate outcomes of contact investigation (not used for PLHIV)
calc_contact_invest <- function(data, params) {
  data <- data %>% 
    mutate(contact_invest_screen = contact_invest_num*params$p_screen,
           contact_invest_tb = contact_invest_num*params$p_tb,
           contact_invest_tb_tp = contact_invest_tb*params$p_tb_screen*params$sens,
           contact_invest_tb_tn = (contact_invest_screen-contact_invest_tb*params$p_tb_screen)*
             params$spec + 
             (contact_invest_num - contact_invest_screen)*(1-params$p_negscreen_tb),
           contact_invest_tb_fn = contact_invest_tb*(1-params$p_tb_screen) +
             contact_invest_tb*params$p_tb_screen*(1-params$sens),
           contact_invest_tb_fp = contact_invest_num - 
             (contact_invest_tb_tp + contact_invest_tb_tn + contact_invest_tb_fn),
           initiate_ipt = (contact_invest_tb_fn + contact_invest_tb_tn)*
             p_initiate*prop_ipt,
           initiate_3hp = (contact_invest_tb_fn + contact_invest_tb_tn)*
             p_initiate*prop_3hp,
           initiate_1hp = (contact_invest_tb_fn + contact_invest_tb_tn)*
             p_initiate*prop_1hp,
           initiate_3hr = (contact_invest_tb_fn + contact_invest_tb_tn)*
             p_initiate*prop_3hr,
           initiate_ipt_ltbi = contact_invest_tb_tn*prop_ipt*p_initiate*params$p_ltbi/(1-params$p_tb),
           initiate_3hp_ltbi = contact_invest_tb_tn*prop_3hp*p_initiate*params$p_ltbi/(1-params$p_tb),
           initiate_1hp_ltbi = contact_invest_tb_tn*prop_1hp*p_initiate*params$p_ltbi/(1-params$p_tb),
           initiate_3hr_ltbi = contact_invest_tb_tn*prop_3hr*p_initiate*params$p_ltbi/(1-params$p_tb),
           #in scenarios w/ no CXR screening, need to add an additional initiation visit for subclinical that start TPT
           #in scenarios w/ CXR screening, the CXR visit is the initiation visit
           initiate_ipt_negscreen = ((contact_invest_num - contact_invest_screen)*(1-params$p_negscreen_tb) + #TNs w/neg screen
                                       contact_invest_tb*(1-params$p_tb_screen))* #FNs (all TB cases w/neg screen)
             p_initiate*prop_ipt, #multiply by initiation and % that get IPT.
           initiate_3hp_negscreen = ((contact_invest_num - contact_invest_screen)*(1-params$p_negscreen_tb) + #TNs w/neg screen
                                       contact_invest_tb*(1-params$p_tb_screen))* #FNs (all TB cases w/neg screen)
             p_initiate*prop_3hp, #multiply by initiation and % that get 3HP.
           initiate_1hp_negscreen = ((contact_invest_num - contact_invest_screen)*(1-params$p_negscreen_tb) + #TNs w/neg screen
                                       contact_invest_tb*(1-params$p_tb_screen))* #FNs (all TB cases w/neg screen)
             p_initiate*prop_1hp, #multiply by initiation and % that get 1HP.   
           initiate_3hr_negscreen = ((contact_invest_num - contact_invest_screen)*(1-params$p_negscreen_tb) + #TNs w/neg screen
                                       contact_invest_tb*(1-params$p_tb_screen))* #FNs (all TB cases w/neg screen)
             p_initiate*prop_3hr, #multiply by initiation and % that get 3hr.    
           #also calculate those w/ active and latent TB that weren't investigated
           no_contact_tb = (total - contact_invest_num)*params$p_tb,
           no_contact_ltbi = (total - contact_invest_num)*params$p_ltbi,
           no_contact_no_tb = total - (contact_invest_num + no_contact_tb + no_contact_ltbi))
}

#calculate results of TPT (completion, impact on LTBI, toxicity) for household contacts
calc_tpt_outcomes_contacts <- function(data, params) {
  data <- data %>% 
    mutate(tox_nohosp_ipt=initiate_ipt*params$p_tox_nohosp_ipt,
           tox_nohosp_3hp=initiate_3hp*params$p_tox_nohosp_3hp,
           tox_nohosp_1hp=initiate_1hp*params$p_tox_nohosp_1hp,
           tox_nohosp_3hr=initiate_3hr*params$p_tox_nohosp_3hr,
           tox_hosp_ipt=initiate_ipt*params$p_tox_hosp_ipt,
           tox_hosp_3hp=initiate_3hp*params$p_tox_hosp_3hp,
           tox_hosp_1hp=initiate_1hp*params$p_tox_hosp_1hp,
           tox_hosp_3hr=initiate_3hr*params$p_tox_hosp_3hr,
           complete_ipt=initiate_ipt*params$p_complete_ipt*(1-(params$p_tox_nohosp_ipt + params$p_tox_hosp_ipt)),
           complete_3hp=initiate_3hp*params$p_complete_3hp*(1-(params$p_tox_nohosp_3hp + params$p_tox_hosp_3hp)),
           complete_1hp=initiate_1hp*params$p_complete_1hp*(1-(params$p_tox_nohosp_1hp + params$p_tox_hosp_1hp)),
           complete_3hr=initiate_3hr*params$p_complete_3hr*(1-(params$p_tox_nohosp_3hr + params$p_tox_hosp_3hr)),
           part_complete_ipt=initiate_ipt - (complete_ipt + tox_nohosp_ipt + tox_hosp_ipt),
           part_complete_3hp=initiate_3hp - (complete_3hp + tox_nohosp_3hp + tox_hosp_3hp),
           part_complete_1hp=initiate_1hp - (complete_1hp + tox_nohosp_1hp + tox_hosp_1hp),
           part_complete_3hr=initiate_3hr - (complete_3hr + tox_nohosp_3hr + tox_hosp_3hr)) 
  data <- data %>%
    mutate(ltbi = no_contact_ltbi +  #LTBI and no investigation
             contact_invest_tb_tn*(1-p_initiate)*params$p_ltbi/(1-params$p_tb) + #LTBI and didn't initiate
             initiate_ipt_ltbi + #LTBI and IPT initiated 
             initiate_3hp_ltbi +
             initiate_1hp_ltbi +
             initiate_3hr_ltbi -
             initiate_ipt_ltbi*params$p_complete_ipt*params$eff_ipt - #subtract out full IPT completion/efficacy
             initiate_3hp_ltbi*params$p_complete_3hp*params$eff_3hp - #subtract out full 3HP completion/efficacy
             initiate_1hp_ltbi*params$p_complete_1hp*params$eff_1hp - #subtract out full 1HP completion/efficacy
             initiate_3hr_ltbi*params$p_complete_3hr*params$eff_3hr - #subtract out full 3hr completion/efficacy
             initiate_ipt_ltbi*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-
                                  params$p_tox_nohosp_ipt)*params$eff_ipt/2 - #subtract out IPT partial completion/efficacy
             initiate_3hp_ltbi*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-
                                  params$p_tox_nohosp_3hp)*params$eff_3hp/2 - #subtract out 3HP partial completion/efficacy
             initiate_1hp_ltbi*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-
                                  params$p_tox_nohosp_1hp)*params$eff_1hp/2 - #subtract out 1HP partial completion/efficacy
             initiate_3hr_ltbi*(1-params$p_complete_3hr-params$p_tox_hosp_3hr-
                                  params$p_tox_nohosp_3hr)*params$eff_3hr/2, #subtract out 3hr partial completion/efficacy
           active_tb = (no_contact_tb + contact_invest_tb_fn)*(1-p_notif)*(1-params$p_die_tb) + #not notified
             (no_contact_tb + contact_invest_tb_fn)*p_notif*(1-params$p_die_tb_tx)*(1-p_success) + #notified but not tx success
             contact_invest_tb_tp*(1-params$p_die_tb_tx)*(1-p_success), #notified but not tx success
           no_tb = no_contact_no_tb + #no TB but not investigated
             contact_invest_tb_tn*(1-params$p_ltbi/(1-params$p_tb)) + #investigated but didn't have TB (regardless of TPT status)
             contact_invest_tb_fp + #no TB and incorrectly treated (false positives)
             initiate_ipt_ltbi*params$p_complete_ipt*params$eff_ipt + #full IPT completion
             initiate_3hp_ltbi*params$p_complete_3hp*params$eff_3hp + #full 3HP completion
             initiate_1hp_ltbi*params$p_complete_1hp*params$eff_1hp + #full 1HP completion
             initiate_3hr_ltbi*params$p_complete_3hr*params$eff_3hr + #full 3hr completion
             initiate_ipt_ltbi*(1-params$p_complete_ipt-params$p_tox_hosp_ipt-params$p_tox_nohosp_ipt)*
             params$eff_ipt/2 + #partial IPT completion
             initiate_3hp_ltbi*(1-params$p_complete_3hp-params$p_tox_hosp_3hp-params$p_tox_nohosp_3hp)*
             params$eff_3hp/2 + #partial 3HP completion
             initiate_1hp_ltbi*(1-params$p_complete_1hp-params$p_tox_hosp_1hp-params$p_tox_nohosp_1hp)*
             params$eff_1hp/2 + #partial 1HP completion
             initiate_3hr_ltbi*(1-params$p_complete_3hr-params$p_tox_hosp_3hr-params$p_tox_nohosp_3hr)*
             params$eff_3hr/2 + #partial 3hr completion
             contact_invest_tb_tp*(1-params$p_die_tb_tx)*p_success + #notified and cured
             (no_contact_tb + contact_invest_tb_fn)*p_notif*(1-params$p_die_tb_tx)*p_success, #notified and cured
           notif_tb = contact_invest_tb_tp + #ppl treated correctly from contact investigation - only used for costs
             (no_contact_tb + contact_invest_tb_fn)*p_notif + #ppl notified by background detections
             contact_invest_tb_fp, #ppl treated incorrectly from contact investigation 
           cases = no_contact_tb + contact_invest_tb_fn + contact_invest_tb_tp, #both treated and untreated cases
           #now model deaths
           tb_deaths = (no_contact_tb + contact_invest_tb_fn)*p_notif*params$p_die_tb_tx +
             (no_contact_tb + contact_invest_tb_fn)*(1-p_notif)*params$p_die_tb +
             contact_invest_tb_tp*params$p_die_tb_tx,
           non_tb_deaths = (active_tb + no_tb + ltbi)*p_die,
           #adjust other states for non-TB deaths
           active_tb=active_tb*(1-p_die),
           no_tb = no_tb*(1-p_die),
           ltbi=ltbi*(1-p_die)
    )
}

#after year of TPT provision, models TB outcomes (& non-TB deaths) over time
model_tb_contacts <- function(d, p_notif, p_success, p_reactivate, p_die_tb, p_die_tb_tx, 
                              p_die, p_infect, ltbi_protect, prop_fast) {
  #order: progressions, infections, fast progressions, notifications, TB deaths, tx success/failure, non-TB deaths
  #need to keep notifications before TB mortality so that tx can reduce risk of death
  
  #TB deaths
  tb_deaths <- d$active_tb*(1-p_notif)*p_die_tb + 
    d$active_tb*p_notif*p_die_tb_tx +
    d$ltbi*p_reactivate*(1-p_notif)*p_die_tb +
    d$ltbi*p_reactivate*p_notif*p_die_tb_tx
  cum_tb_deaths <- tb_deaths + d$cum_tb_deaths
  
  #transitions within alive states
  #non-notified cases (that don't die of TB)
  active_tb <- d$active_tb*(1-p_notif)*(1-p_die_tb) +
    d$ltbi*p_reactivate*(1-p_notif)*(1-p_die_tb) +
    d$ltbi*(1-p_reactivate)*p_infect*(1-ltbi_protect)*prop_fast*(1-p_notif)*(1-p_die_tb) + 
    d$no_tb*p_infect*prop_fast*(1-p_notif)*(1-p_die_tb) +
    #notified cases that fail tx (that don't die of TB)
    d$active_tb*p_notif*(1-p_die_tb_tx)*(1-p_success) +
    d$ltbi*p_reactivate*p_notif*(1-p_die_tb_tx)*(1-p_success) +
    d$ltbi*(1-p_reactivate)*p_infect*(1-ltbi_protect)*prop_fast*p_notif*(1-p_die_tb_tx)*(1-p_success) +
    d$no_tb*p_infect*prop_fast*p_notif*(1-p_die_tb_tx)*(1-p_success)
  ltbi <- d$ltbi*(1-p_reactivate) +
    d$no_tb*p_infect*(1-prop_fast) -
    d$ltbi*(1-p_reactivate)*p_infect*(1-ltbi_protect)*prop_fast
  no_tb <- d$active_tb*p_notif*(1-p_die_tb_tx)*p_success +
    d$ltbi*p_reactivate*p_notif*(1-p_die_tb_tx)*p_success +
    d$ltbi*(1-p_reactivate)*p_infect*(1-ltbi_protect)*prop_fast*p_notif*(1-p_die_tb_tx)*p_success +
    d$no_tb*p_infect*prop_fast*p_notif*(1-p_die_tb_tx)*p_success +
    d$no_tb*(1-p_infect)
  
  #track new cases and new notifications
  cases <- d$ltbi*(1-p_die)*p_reactivate +
    d$ltbi*(1-p_die)*(1-p_reactivate)*p_infect*(1-ltbi_protect)*prop_fast +
    d$no_tb*(1-p_die)*p_infect*prop_fast
  notif_tb <- d$active_tb*p_notif +
    d$ltbi*p_reactivate*p_notif +
    d$ltbi*(1-p_reactivate)*p_infect*(1-ltbi_protect)*prop_fast*p_notif +
    d$no_tb*p_infect*prop_fast*p_notif
  
  #non-tb deaths
  non_tb_deaths <- active_tb*p_die + ltbi*p_die + no_tb*p_die 
  active_tb <- active_tb*(1-p_die)
  no_tb <- no_tb*(1-p_die)
  ltbi <- ltbi*(1-p_die)
  cum_non_tb_deaths <- non_tb_deaths + d$cum_non_tb_deaths
  
  #add to data, replacing outcomes from previous timestep
  d <- d %>% select(-c(tb_deaths, non_tb_deaths, cum_tb_deaths, cum_non_tb_deaths,
                       active_tb, notif_tb, ltbi, no_tb, cases))
  new_data <- data.frame(tb_deaths, non_tb_deaths, cum_tb_deaths, cum_non_tb_deaths,
                         active_tb, notif_tb, ltbi, no_tb, cases)
  data <- bind_cols(d, new_data)
  return(data)
}

#combine everything that happens in a given year from model_tb output that is stratified by yr and yrs_out - used for contacts only
combine_yrs <- function(data, pop_type, policy_end_yr, end_yr) {
  data <- data %>% mutate(year=year+yrs_out)
  data_early <- data %>% filter(year<=policy_end_yr) %>% group_by(country, code, year, scenario) %>%
    summarise(contact_invest_num=unique(contact_invest_num[yrs_out==0]),
              contact_invest_screen=unique(contact_invest_screen[yrs_out==0]),
              contact_invest_tb_tp=unique(contact_invest_tb_tp[yrs_out==0]),
              contact_invest_tb_fp=unique(contact_invest_tb_fp[yrs_out==0]),
              contact_invest_tb_tn=unique(contact_invest_tb_tn[yrs_out==0]),
              contact_invest_tb_fn=unique(contact_invest_tb_fn[yrs_out==0]),
              initiate_ipt_negscreen=unique(initiate_ipt_negscreen[yrs_out==0]),
              initiate_3hp_negscreen=unique(initiate_3hp_negscreen[yrs_out==0]),
              initiate_1hp_negscreen=unique(initiate_1hp_negscreen[yrs_out==0]),
              initiate_3hr_negscreen=unique(initiate_3hr_negscreen[yrs_out==0]),
              complete_ipt=unique(complete_ipt[yrs_out==0]),
              part_complete_ipt=unique(part_complete_ipt[yrs_out==0]),
              tox_nohosp_ipt=unique(tox_nohosp_ipt[yrs_out==0]),
              tox_hosp_ipt=unique(tox_hosp_ipt[yrs_out==0]),
              complete_3hp=unique(complete_3hp[yrs_out==0]),
              part_complete_3hp=unique(part_complete_3hp[yrs_out==0]),
              tox_nohosp_3hp=unique(tox_nohosp_3hp[yrs_out==0]),
              tox_hosp_3hp=unique(tox_hosp_3hp[yrs_out==0]),
              complete_1hp=unique(complete_1hp[yrs_out==0]),
              part_complete_1hp=unique(part_complete_1hp[yrs_out==0]),
              tox_nohosp_1hp=unique(tox_nohosp_1hp[yrs_out==0]),
              tox_hosp_1hp=unique(tox_hosp_1hp[yrs_out==0]),
              complete_3hr=unique(complete_3hr[yrs_out==0]),
              part_complete_3hr=unique(part_complete_3hr[yrs_out==0]),
              tox_nohosp_3hr=unique(tox_nohosp_3hr[yrs_out==0]),
              tox_hosp_3hr=unique(tox_hosp_3hr[yrs_out==0]),
              total=sum(total), 
              ltbi=sum(ltbi), active_tb=sum(active_tb), no_tb=sum(no_tb), 
              notif_tb=sum(notif_tb), cases=sum(cases), tb_deaths=sum(tb_deaths), 
              non_tb_deaths=sum(non_tb_deaths), cum_tb_deaths=sum(cum_tb_deaths), 
              cum_non_tb_deaths=sum(cum_non_tb_deaths)) %>%
    filter(year<=(end_yr)) 
  if(end_yr > policy_end_yr) {
    data_late <- data %>% filter(year>policy_end_yr) %>% group_by(country, code, year, scenario) %>%
      summarise(contact_invest_num=0,
                contact_invest_screen=0,
                contact_invest_tb_tp=0,
                contact_invest_tb_fp=0,
                contact_invest_tb_tn=0,
                contact_invest_tb_fn=0,
                initiate_ipt_negscreen=0,
                initiate_3hp_negscreen=0,
                initiate_1hp_negscreen=0,
                initiate_3hr_negscreen=0,
                complete_ipt=0,
                part_complete_ipt=0,
                tox_nohosp_ipt=0,
                tox_hosp_ipt=0,
                complete_3hp=0,
                part_complete_3hp=0,
                tox_nohosp_3hp=0,
                tox_hosp_3hp=0,
                complete_1hp=0,
                part_complete_1hp=0,
                tox_nohosp_1hp=0,
                tox_hosp_1hp=0,
                complete_3hr=0,
                part_complete_3hr=0,
                tox_nohosp_3hr=0,
                tox_hosp_3hr=0,
                total=sum(total), ltbi=sum(ltbi), active_tb=sum(active_tb), 
                no_tb=sum(no_tb), notif_tb=sum(notif_tb), cases=sum(cases), 
                tb_deaths=sum(tb_deaths), 
                non_tb_deaths=sum(non_tb_deaths), cum_tb_deaths=sum(cum_tb_deaths), 
                cum_non_tb_deaths=sum(cum_non_tb_deaths)) %>%
      filter(year<=(end_yr)) #after policy horizon
    data <- bind_rows(data_early, data_late)
  } else {
    data <- data_early
  }
  return(data)
}

#calculate costs - used for PLHIV and contacts
calc_costs <- function(data, pop_type, costs,  
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
               initiate_1hp_negscreen*costs$outpatient*(params$cxr_screen==0),
             cost_3hr=(complete_3hr + 
                         part_complete_3hr*params$part_course_cost + 
                         tox_nohosp_3hr*params$part_course_cost + 
                         tox_hosp_3hr*params$part_course_cost)*
               params$c_3hr*(1+params$c_delivery_3hr)*(1+params$wastage) +
               (complete_3hr + part_complete_3hr*0.5 + 
                  tox_nohosp_3hr*0.5 + tox_hosp_3hr*0.5)*
               params$n_visit_3hr*costs$outpatient +
               initiate_3hr_negscreen*costs$outpatient*(params$cxr_screen==0))
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
               initiate_1hp_negscreen*costs$outpatient*(params$cxr_screen==0),
             cost_3hr=(complete_3hr + 
                         part_complete_3hr*params$part_course_cost + 
                         tox_nohosp_3hr*params$part_course_cost + 
                         tox_hosp_3hr*params$part_course_cost)*
               params$c_3hr[as.character(year)]*(1+params$c_delivery_3hr)*
               (1+params$wastage) +
               (complete_3hr + part_complete_3hr*0.5 + 
                  tox_nohosp_3hr*0.5 + tox_hosp_3hr*0.5)*
               params$n_visit_3hr*costs$outpatient +
               initiate_3hr_negscreen*costs$outpatient*(params$cxr_screen==0))
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
               params$n_visit_1hp*costs$outpatient,
             cost_3hr=(complete_3hr + 
                         part_complete_3hr*params$part_course_cost + 
                         tox_nohosp_3hr*params$part_course_cost + 
                         tox_hosp_3hr*params$part_course_cost)*
               params$c_3hr*(1+params$c_delivery_3hr)*(1+params$wastage) +
               (complete_3hr + part_complete_3hr*0.5 + 
                  tox_nohosp_3hr*0.5 + tox_hosp_3hr*0.5)*
               params$n_visit_3hr*costs$outpatient)
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
               params$n_visit_1hp*costs$outpatient,
             cost_3hr=(complete_3hr + 
                         part_complete_3hr*params$part_course_cost + 
                         tox_nohosp_3hr*params$part_course_cost + 
                         tox_hosp_3hr*params$part_course_cost)*
               params$c_3hr[as.character(year)]*(1+params$c_delivery_3hr)*
               (1+params$wastage) +
               (complete_3hr + part_complete_3hr*0.5 + 
                  tox_nohosp_3hr*0.5 + tox_hosp_3hr*0.5)*
               params$n_visit_3hr*costs$outpatient)
  }
  #TPT adverse events - nohosp = tox_labs + outpatient
  #hosp = tox_labs + outpatient + 7 days inpatient
  data <- data %>% 
    mutate(cost_tox=(tox_nohosp_ipt + tox_nohosp_3hp + tox_nohosp_1hp + tox_nohosp_3hr)*
             (costs$lab_tox + costs$outpatient) + 
             (tox_hosp_ipt + tox_hosp_3hp + tox_hosp_1hp + tox_hosp_3hr)*
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
  data <- data %>% mutate(cost_impl=costs$impl*(initiate_1hp + initiate_3hp + initiate_3hr))
  if(pop_type %in% c("child", "adol", "adult")) {
    data <- data %>% 
      mutate(costs=cost_contact + cost_ipt + cost_3hp + cost_1hp + cost_3hr +
               cost_tox + cost_tx + cost_impl)
    data <- data %>% select(-c(cost_tests_contact, starts_with("c_imp")))
  } else if(pop_type=="plhiv") {
    data <- data %>% 
      mutate(costs=cost_ipt + cost_3hp + cost_1hp + cost_3hr +
               cost_tox + cost_tx + cost_art + cost_impl)
  }
  data <- data %>% mutate(disc_costs=costs/((1+disc_fac)^(year-start_yr)))
  data <- data %>% group_by(country, code, scenario) %>%
    mutate(cum_costs=cumsum(costs), cum_disc_costs=cumsum(disc_costs))
  return(data)
}

#wrapper function to run the whole model for PLHIV - regimen option is obselete
run_model_plhiv_old <- function(country_name, regimen, covg, scenarios, options, params, pop_calcs, option_split) {
  policy_horizon <- 10 #for now, implement over 10 years and calculate costs/outcomes over 10 years
  analytic_horizon <- 10
  start_yr <- 2024
  end_yr <- start_yr + analytic_horizon - 1
  policy_end_yr <- start_yr + policy_horizon - 1
  
  #options to be added later
  pwh_transitions <- 1 #1=base case (infrequent transitions on/off ART); 2=more frequent transitions
  reinfect <- 0 #0=base case, or annual risk of infection
  price_tpt <- "base" #base, 3hp_reduced (3HP price only reduced by 50%), or vary (vary price widely in PSA)
  cost_tbtx <- "base" #base (main analysis, varies by country), vary (vary widely in PSA)
  visits_3hp <- 1 #1 or 2 monitoring visits (1 in main analysis - initiation and completion)
  visits_3hr <- visits_3hp
  tpt_wastage <- "full courses"  #wastage factor for drugs (e.g. 0.1), or cost out "full courses" for all initiators
  comp_scen <- "none"
  
  #implementation of options
  if(reinfect!=0) {
    params$p_infect <- reinfect
  }
  params$n_visit_3hp <- visits_3hp
  params$n_visit_3hr <- visits_3hr
  params$wastage <- ifelse(tpt_wastage=="full courses", 0, as.double(tpt_wastage))
  params$part_course_cost <- ifelse(tpt_wastage=="full courses", 1, 0.5)
  
  #load total numbers of PLHIV to cover by year
  pop_calcs <- pop_calcs %>% dplyr::select(code, country, year, plhiv_art_new_lag, backlog_none) %>%
    filter(year>=start_yr & year<=policy_end_yr) %>%
    rename("plhiv_new"="plhiv_art_new_lag",
           "backlog"="backlog_none") %>%
    mutate(backlog=if_else(year==min(year), backlog, as.numeric(NA)))
  pop_calcs_none <- pop_calcs %>%
    mutate(scenario=scenarios[[2]],
           initiate_ipt_covg_new=0,
           initiate_ipt_covg_prev=0,
           initiate_3hp_covg_new=0,
           initiate_3hp_covg_prev=0,
           initiate_1hp_covg_new=0,
           initiate_1hp_covg_prev=0,
           initiate_3hr_covg_new=0,
           initiate_3hr_covg_prev=0)
  pop_calcs_tpt <- pop_calcs %>% 
    mutate(scenario=scenarios[[1]],
           initiate_ipt_covg_new=pmin(covg[["6h"]]/(covg[["6h"]] + covg[["3hp"]] + covg[["1hp"]] + covg[["3hr"]]), 
                                      (covg[["6h"]]/(100*params$p_initiate))),
           initiate_ipt_covg_prev=pmin(covg[["6h"]]/(covg[["6h"]] + covg[["3hp"]] + covg[["1hp"]] + covg[["3hr"]]), 
                                       (covg[["6h"]]/(100*params$p_initiate))),
           initiate_3hp_covg_new=pmin(covg[["3hp"]]/(covg[["6h"]] + covg[["3hp"]] + covg[["1hp"]] + covg[["3hr"]]), 
                                      (covg[["3hp"]]/(100*params$p_initiate))),
           initiate_3hp_covg_prev=pmin(covg[["3hp"]]/(covg[["6h"]] + covg[["3hp"]] + covg[["1hp"]] + covg[["3hr"]]), 
                                       (covg[["3hp"]]/(100*params$p_initiate))),
           initiate_1hp_covg_new=pmin(covg[["1hp"]]/(covg[["6h"]] + covg[["3hp"]] + covg[["1hp"]] + covg[["3hr"]]), 
                                      (covg[["1hp"]]/(100*params$p_initiate))),
           initiate_1hp_covg_prev=pmin(covg[["1hp"]]/(covg[["6h"]] + covg[["3hp"]] + covg[["1hp"]] + covg[["3hr"]]), 
                                       (covg[["1hp"]]/(100*params$p_initiate))),
           initiate_3hr_covg_new=pmin(covg[["3hr"]]/(covg[["6h"]] + covg[["3hp"]] + covg[["1hp"]] + covg[["3hr"]]), 
                                      (covg[["3hr"]]/(100*params$p_initiate))),
           initiate_3hr_covg_prev=pmin(covg[["3hr"]]/(covg[["6h"]] + covg[["3hp"]] + covg[["1hp"]] + covg[["3hr"]]), 
                                       (covg[["3hr"]]/(100*params$p_initiate))))
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
           initiate_ipt_est=0, initiate_3hp_est=0, initiate_1hp_est=0, initiate_3hr_est=0,
           tox_nohosp_ipt_est=0, tox_nohosp_3hp_est=0, tox_nohosp_1hp_est=0, tox_nohosp_3hr_est=0,
           tox_hosp_ipt_est=0, tox_hosp_3hp_est=0, tox_hosp_1hp_est=0, tox_hosp_3hr_est=0,
           complete_ipt_est=0, complete_3hp_est=0, complete_1hp_est=0, complete_3hr_est=0,
           part_complete_ipt_est=0, part_complete_3hp_est=0, part_complete_1hp_est=0, part_complete_3hr_est=0,
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
                                          initiate_3hr_est=replace(initiate_3hr_est, year==start_yr, plhiv_est$initiate_3hr_est),
                                          tox_nohosp_ipt_est=replace(tox_nohosp_ipt_est, year==start_yr, plhiv_est$tox_nohosp_ipt_est),
                                          tox_nohosp_3hp_est=replace(tox_nohosp_3hp_est, year==start_yr, plhiv_est$tox_nohosp_3hp_est),
                                          tox_nohosp_1hp_est=replace(tox_nohosp_1hp_est, year==start_yr, plhiv_est$tox_nohosp_1hp_est),
                                          tox_nohosp_3hr_est=replace(tox_nohosp_3hr_est, year==start_yr, plhiv_est$tox_nohosp_3hr_est),
                                          tox_hosp_ipt_est=replace(tox_hosp_ipt_est, year==start_yr, plhiv_est$tox_hosp_ipt_est),
                                          tox_hosp_3hp_est=replace(tox_hosp_3hp_est, year==start_yr, plhiv_est$tox_hosp_3hp_est),
                                          tox_hosp_1hp_est=replace(tox_hosp_1hp_est, year==start_yr, plhiv_est$tox_hosp_1hp_est),
                                          tox_hosp_3hr_est=replace(tox_hosp_3hr_est, year==start_yr, plhiv_est$tox_hosp_3hr_est),
                                          complete_ipt_est=replace(complete_ipt_est, year==start_yr, plhiv_est$complete_ipt_est),
                                          complete_3hp_est=replace(complete_3hp_est, year==start_yr, plhiv_est$complete_3hp_est),
                                          complete_1hp_est=replace(complete_1hp_est, year==start_yr, plhiv_est$complete_1hp_est),
                                          complete_3hr_est=replace(complete_3hr_est, year==start_yr, plhiv_est$complete_3hr_est),
                                          part_complete_ipt_est=replace(part_complete_ipt_est, year==start_yr, plhiv_est$part_complete_ipt_est),
                                          part_complete_3hp_est=replace(part_complete_3hp_est, year==start_yr, plhiv_est$part_complete_3hp_est),
                                          part_complete_1hp_est=replace(part_complete_1hp_est, year==start_yr, plhiv_est$part_complete_1hp_est),
                                          part_complete_3hr_est=replace(part_complete_3hr_est, year==start_yr, plhiv_est$part_complete_3hr_est),
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
  
  plhiv_comb <- calc_costs(plhiv_comb, "plhiv", params, 
                           params, params$disc_fac, start_yr)
  
  #cost-effectiveness code would go here
  
  plhiv_comb <- plhiv_comb %>% select(-c(backlog, cum_disc_costs))
  return(plhiv_comb)
}

run_model_plhiv <- function(country_name, covg, scenarios, params, pop_calcs, option_split) {
  policy_horizon <- 10 #for now, implement over 10 years and calculate costs/outcomes over 10 years
  analytic_horizon <- 10
  start_yr <- 2024
  end_yr <- start_yr + analytic_horizon - 1
  policy_end_yr <- start_yr + policy_horizon - 1
  
  #options to be added later
  pwh_transitions <- 1 #1=base case (infrequent transitions on/off ART); 2=more frequent transitions
  reinfect <- 0 #0=base case, or annual risk of infection
  price_tpt <- "base" #base, 3hp_reduced (3HP price only reduced by 50%), or vary (vary price widely in PSA)
  cost_tbtx <- "base" #base (main analysis, varies by country), vary (vary widely in PSA)
  visits_3hp <- 1 #1 or 2 monitoring visits (1 in main analysis - initiation and completion)
  visits_3hr <- visits_3hp
  tpt_wastage <- "full courses"  #wastage factor for drugs (e.g. 0.1), or cost out "full courses" for all initiators
  comp_scen <- "none"
  
  #implementation of options
  if(reinfect!=0) {
    params$p_infect <- reinfect
  }
  params$n_visit_3hp <- visits_3hp
  params$n_visit_3hr <- visits_3hr
  params$wastage <- ifelse(tpt_wastage=="full courses", 0, as.double(tpt_wastage))
  params$part_course_cost <- ifelse(tpt_wastage=="full courses", 1, 0.5)
  
  #load total numbers of PLHIV to cover by year
  pop_calcs <- pop_calcs %>% dplyr::select(code, country, year, plhiv_art_new_lag, backlog_none) %>%
    filter(year>=start_yr & year<=policy_end_yr) %>%
    rename("plhiv_new"="plhiv_art_new_lag",
           "backlog"="backlog_none") %>%
    mutate(backlog=if_else(year==min(year), backlog, as.numeric(NA)))
  pop_calcs_none <- pop_calcs %>%
    mutate(scenario=scenarios[[2]],
           initiate_ipt_num=0,
           initiate_3hp_num=0,
           initiate_1hp_num=0,
           initiate_3hr_num=0)
  pop_calcs_tpt <- pop_calcs %>% 
    mutate(scenario=scenarios[[1]],
           initiate_ipt_num=covg[["6h"]]/100,
           initiate_3hp_num=covg[["3hp"]]/100,
           initiate_1hp_num=covg[["1hp"]]/100,
           initiate_3hr_num=covg[["3hr"]]/100)
  
  #set up time-varying parameters
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
  
  #set up initial # of people by TB status
  plhiv <- bind_rows(pop_calcs_none, pop_calcs_tpt)
  plhiv <- plhiv %>% 
    mutate(ltbi_new=plhiv_new*params$p_ltbi*(1-params$p_reactivate_new[[1]]),
           active_tb_new=plhiv_new*params$p_ltbi*params$p_reactivate_new[[1]],
           no_tb_new=plhiv_new*(1-params$p_ltbi),
           ltbi_est_tpt=0,
           active_tb_est_tpt=0,
           no_tb_est_tpt=0,
           ltbi_est_not=backlog*params$p_ltbi*(1-params$p_reactivate_est[[1]]),
           active_tb_est_not=backlog*params$p_ltbi*params$p_reactivate_est[[1]],
           no_tb_est_not=backlog*(1-params$p_ltbi),
           no_tb_est_not_tb=0, #separately track those that haven't gotten TPT and have had TB - not eligible for TPT
           ltbi_ltfu_tpt=0,
           active_tb_ltfu_tpt=0,
           no_tb_ltfu_tpt=0,
           ltbi_ltfu_not=0,
           active_tb_ltfu_not=0,
           no_tb_ltfu_not=0)
  
  #set up LTBI version that tracks time
  plhiv_ltbi <- cbind("t"=1,
                      plhiv %>% select(country, code, year, scenario,
                                     ltbi_new, ltbi_est_tpt, 
                                     ltbi_est_not, ltbi_ltfu_tpt, ltbi_ltfu_not))
  plhiv_ltbi <- do.call("rbind", replicate(10, plhiv_ltbi, simplify=F))
  ts <- unlist(lapply(1:10, function(x) rep(x, nrow(plhiv))))
  plhiv_ltbi <- plhiv_ltbi %>% mutate(t=ts)
  plhiv_ltbi <- plhiv_ltbi %>% mutate(ltbi_new_tpt=0,
                                      ltbi_new_not=if_else(t==1, ltbi_new, 0),
                                      ltbi_est_tpt=if_else(t==2, ltbi_est_tpt, 0),
                                      ltbi_est_not=if_else(t==2, ltbi_est_not, 0),
                                      ltbi_ltfu_tpt=if_else(t==2, ltbi_ltfu_tpt, 0),
                                      ltbi_ltfu_not=if_else(t==2, ltbi_ltfu_not, 0))
  plhiv_ltbi <- plhiv_ltbi %>% mutate(p_reactivate_new=p_reactivate_new[t],
                                      p_reactivate_est=p_reactivate_est[t],
                                      p_reactivate_ltfu=p_reactivate_ltfu[t])
  plhiv_ltbi <- sapply(c('ltbi_new_tpt', 'ltbi_new_not', 'ltbi_est_tpt', 
                         'ltbi_est_not', 'ltbi_ltfu_tpt', 'ltbi_ltfu_not'), function(x)
    matrix(as.numeric(as.matrix(pivot_wider(plhiv_ltbi %>% select("t", "code", "scenario", "year", !!as.symbol(x)),
                                            names_from="t", values_from=all_of(x)) %>%
                                  select(-c("code", "scenario", "year")))), ncol=10), USE.NAMES=T, simplify=F)
  plhiv_ltbi <- abind(plhiv_ltbi, along=3)
  dimnames(plhiv_ltbi)[[1]] <- c(paste0(start_yr:end_yr, "c"), paste0(start_yr:end_yr, "t"))
  
  #remove LTBI from plhiv
  plhiv <- plhiv %>% select(-c(ltbi_new, ltbi_est_tpt, ltbi_est_not, ltbi_ltfu_tpt, ltbi_ltfu_not))
  
  #loop over each year and run model
  plhiv_flag <- c()
  for(i in start_yr:end_yr) { #currently no way to have longer analytical horizon than policy horizon in the app version
    plhiv_t <- plhiv %>% filter(year==i)
    plhiv_ltbi_t <- plhiv_ltbi[c(paste0(i, "c"), paste0(i, "t")),,]
    
    #calculate split of TPT initiation by new vs. previously enrolled
    plhiv_t <- tpt_covg_plhiv(plhiv_t, params, option_split)
    plhiv_flag <- c(plhiv_flag, max(plhiv_t$flag)) #flag if imputed # to initiate is too high
    print(plhiv_flag)
    #run model
    out <- model_tb_plhiv(plhiv_t, plhiv_ltbi_t, params)
    plhiv_new <- out$plhiv
    plhiv_ltbi_new <- out$plhiv_ltbi
    
    #replace values in plhiv
    plhiv_old_keep <- plhiv %>% filter(year==i) %>% 
      select(country, code, year, scenario, no_tb_est_tpt, no_tb_est_not, no_tb_est_not_tb,
             active_tb_est_tpt, active_tb_est_not,
             no_tb_ltfu_tpt, no_tb_ltfu_not,
             active_tb_ltfu_tpt, active_tb_ltfu_not)
    plhiv_new_keep <- plhiv_new %>% select(-c(no_tb_est_tpt, no_tb_est_not, no_tb_est_not_tb,
                                              active_tb_est_tpt, active_tb_est_not,
                                              no_tb_ltfu_tpt, no_tb_ltfu_not,
                                              active_tb_ltfu_tpt, active_tb_ltfu_not))
    plhiv_keep <- left_join(plhiv_old_keep, plhiv_new_keep, by=c("country", "code", "year", "scenario"))
    plhiv <- rbind(plhiv %>% filter(year!=i), plhiv_keep) %>% arrange(scenario, year)
    
    if(i!=end_yr) {
      #move "est" and "ltfu" variables to following year, move "new" to "est" for the following year
      plhiv_new <- plhiv_new %>% mutate(no_tb_est_tpt = no_tb_est_tpt + no_tb_new_tpt,
                                        no_tb_est_not = no_tb_est_not + no_tb_new_not,
                                        no_tb_est_not_tb = no_tb_est_not_tb + no_tb_new_not_tb,
                                        active_tb_est_tpt = active_tb_est_tpt + active_tb_new_tpt,
                                        active_tb_est_not = active_tb_est_not + active_tb_new_not
      )
      plhiv <- left_join(plhiv, plhiv_new %>% mutate(year=i+1) %>%
                           select(country, code, year, scenario, no_tb_est_tpt, no_tb_est_not,
                                  no_tb_est_not_tb, active_tb_est_tpt, active_tb_est_not,
                                  no_tb_ltfu_tpt, no_tb_ltfu_not, 
                                  active_tb_ltfu_tpt, active_tb_ltfu_not),
                         by=c("country", "code", "year", "scenario"),
                         suffix=c("", "_replace"))
      plhiv <- plhiv %>%
        mutate(no_tb_est_tpt=if_else(year==i+1, no_tb_est_tpt_replace, no_tb_est_tpt),
               no_tb_est_not=if_else(year==i+1, no_tb_est_not_replace, no_tb_est_not),
               no_tb_est_not_tb=if_else(year==i+1, no_tb_est_not_tb_replace, no_tb_est_not_tb),
               active_tb_est_tpt=if_else(year==i+1, active_tb_est_tpt_replace, active_tb_est_tpt),
               active_tb_est_not=if_else(year==i+1, active_tb_est_not_replace, active_tb_est_not),
               no_tb_ltfu_tpt=if_else(year==i+1, no_tb_ltfu_tpt_replace, no_tb_ltfu_tpt),
               no_tb_ltfu_not=if_else(year==i+1, no_tb_ltfu_not_replace, no_tb_ltfu_not),
               active_tb_ltfu_tpt=if_else(year==i+1, active_tb_ltfu_tpt_replace, active_tb_ltfu_tpt),
               active_tb_ltfu_not=if_else(year==i+1, active_tb_ltfu_not_replace, active_tb_ltfu_not)
        ) %>% select(-ends_with("_replace"))
      
      #repeat for LTBI
      plhiv_ltbi[c(paste0(i+1, "c"), paste0(i+1, "t")),
                 2:dim(plhiv_ltbi)[[2]],] <- plhiv_ltbi_new[c(paste0(i, "c"), paste0(i, "t")),2:dim(plhiv_ltbi)[[2]],]
      #move new to est
      plhiv_ltbi[,params$yrs_new+1, "ltbi_est_tpt"] <- plhiv_ltbi[,params$yrs_new+1, "ltbi_est_tpt"] +
        plhiv_ltbi[,params$yrs_new+1, "ltbi_new_tpt"]
      plhiv_ltbi[,params$yrs_new+1, "ltbi_est_not"] <- plhiv_ltbi[,params$yrs_new+1, "ltbi_est_not"] +
        plhiv_ltbi[,params$yrs_new+1, "ltbi_new_not"]
      plhiv_ltbi[,params$yrs_new+1, "ltbi_new_tpt"] <- 0
      plhiv_ltbi[,params$yrs_new+1, "ltbi_new_not"] <- 0
      
      plhiv <- plhiv %>% group_by(scenario, code) %>%
        mutate(backlog=if_else(year==i+1, no_tb_est_not + no_tb_est_not_tb + active_tb_est_not +
                                 as.vector(rowSums(plhiv_ltbi[c(paste0(i+1, "c"), paste0(i+1, "t")),,"ltbi_est_not"])),
                               backlog))
      #new PLHIV don't get enrolled year after they enroll
    }
    
    #calculate cumulative cases and deaths
    #plhiv <- plhiv %>% mutate(cum_tb_deaths = tb_deaths + cum_tb_deaths,
     #                         cum_non_tb_deaths = non_tb_deaths + cum_non_tb_deaths)
  }
  
  #move 2nd column of ltbi_new to prev so that "new" is 1st yr only
  plhiv_ltbi[,2:dim(plhiv_ltbi)[2],"ltbi_est_tpt"] <- plhiv_ltbi[,2:dim(plhiv_ltbi)[2],"ltbi_est_tpt"] +
    plhiv_ltbi[,2:dim(plhiv_ltbi)[2],"ltbi_new_tpt"]
  plhiv_ltbi[,2:dim(plhiv_ltbi)[2],"ltbi_est_not"] <- plhiv_ltbi[,2:dim(plhiv_ltbi)[2],"ltbi_est_not"] +
    plhiv_ltbi[,2:dim(plhiv_ltbi)[2],"ltbi_new_not"]
  plhiv_ltbi[,2:dim(plhiv_ltbi)[2],"ltbi_new_tpt"] <- 0
  plhiv_ltbi[,2:dim(plhiv_ltbi)[2],"ltbi_new_not"] <- 0
  
  #move LTBI sizes back into plhiv_all (no longer need tracking by t)
  plhiv_ltbi <- as.data.frame(apply(plhiv_ltbi, MARGIN=c(1,3), FUN=sum))
  plhiv_ltbi <- plhiv_ltbi %>% mutate(year=as.numeric(str_sub(row.names(plhiv_ltbi), start=1, end=4)),
                                      scenario=str_sub(row.names(plhiv_ltbi), start=5),
                                      scenario=if_else(scenario=="c", scenarios[[2]], scenarios[[1]]))
  plhiv <- left_join(plhiv, plhiv_ltbi, by=c("year", "scenario"))
  
  #combine across TPT and Not TPT, Est and New groups - so same format as contacts dataframe
  plhiv <- plhiv %>% 
    mutate(ltfu=ltbi_ltfu_tpt + active_tb_ltfu_tpt + no_tb_ltfu_tpt + 
             ltbi_ltfu_not + active_tb_ltfu_not + no_tb_ltfu_not) %>% #keep track of LTFU for costs (no ART costs)
    mutate(no_tb_est_not=no_tb_est_not + no_tb_est_not_tb,
           no_tb_new_not=no_tb_new_not + no_tb_new_not_tb) %>%
    select(-c(no_tb_est_not_tb, no_tb_new_not_tb)) #remove this so consistent naming across variables - no longer need to track it
  plhiv <- plhiv %>% select(-c(active_tb_new, no_tb_new, initiate_3hp_covg, initiate_1hp_covg, initiate_3hr_covg, initiate_ipt_covg))
  plhiv_comb <- pivot_longer(plhiv, cols=c(ends_with("_tpt"), ends_with("_not")),
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
  
  plhiv_comb <- calc_costs(plhiv_comb, "plhiv", params, 
                           params, params$disc_fac, start_yr)
  
  #cost-effectiveness code would go here
  
  plhiv_comb <- plhiv_comb %>% select(-c(backlog, cum_disc_costs))
  out <- list("plhiv"=plhiv_comb,
              "plhiv_flag"=max(plhiv_flag)
              )
  print(out$plhiv_flag)
  return(out)
}


#wrapper function to run the whole model for contacts
run_model_contacts <- function(country_name, regimen_child, regimen_adol, regimen_adult,
                               covg_child, covg_adol, covg_adult, scenarios, options,
                               child_params, adol_params, adult_params, pop_calcs) {
  policy_horizon <- 10 #for now, implement over 10 years and calculate costs/outcomes over 10 years
  analytic_horizon <- 10
  start_yr <- 2024
  end_yr <- start_yr + analytic_horizon - 1
  policy_end_yr <- start_yr + policy_horizon - 1
  
  #contact_only_child <- 1*(regimen_child=="None") 
  #contact_only_adol <- 1*(regimen_adol=="None") 
  #contact_only_adult <- 1*(regimen_adult=="None") 
  
  #options to be added later
  reinfect <- 0 #0=base case, or annual risk of infection
  visits_3hp <- 1 #1 or 3 visits (1 in main analysis). initiation visit is separate (part of screening)
  visits_3hr <- visits_3hp
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
  child_params$n_visit_3hr <- visits_3hr
  adol_params$n_visit_3hr <- visits_3hr
  adult_params$n_visit_3hr <- visits_3hr
  
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
  pop_calcs <- pivot_longer(pop_calcs %>% dplyr::select(-c(start_yr, child_ipt)) %>%
                              filter(year>=start_yr & year<=policy_end_yr), 
                            cols=c(ends_with("tpt"), 
                                   ends_with("none")),
                            names_to=c(".value", "scenario"),
                            names_sep="_(?=[^_]+$)") #this is regex for "last underscore only"
  pop_calcs <- left_join(pop_calcs, covg_child, by="year")
  pop_calcs <- left_join(pop_calcs, covg_adol, by="year")
  pop_calcs <- left_join(pop_calcs, covg_adult, by="year")
  pop_calcs <- pop_calcs %>% mutate(child_ipt=if_else(scenario!="tpt", 0, child_ipt/100),
                                    child_3hp=if_else(scenario!="tpt", 0, child_3hp/100),
                                    child_1hp=if_else(scenario!="tpt", 0, child_1hp/100),
                                    child_3hr=if_else(scenario!="tpt", 0, child_3hr/100),
                                    child_none=if_else(scenario!="tpt", 0, child_none/100),
                                    adol_ipt=if_else(scenario!="tpt", 0, adol_ipt/100),
                                    adol_3hp=if_else(scenario!="tpt", 0, adol_3hp/100),
                                    adol_1hp=if_else(scenario!="tpt", 0, adol_1hp/100),
                                    adol_3hr=if_else(scenario!="tpt", 0, adol_3hr/100),
                                    adol_none=if_else(scenario!="tpt", 0, adol_none/100),
                                    adult_ipt=if_else(scenario!="tpt", 0, adult_ipt/100),
                                    adult_3hp=if_else(scenario!="tpt", 0, adult_3hp/100),
                                    adult_1hp=if_else(scenario!="tpt", 0, adult_1hp/100),
                                    adult_3hr=if_else(scenario!="tpt", 0, adult_3hr/100),
                                    adult_none=if_else(scenario!="tpt", 0, adult_none/100),
                                    scenario=if_else(scenario=="tpt", scenarios[[1]], scenarios[[2]]))

  #separate into 3 dataframes - one for each target population group
  #hh contacts - targets include initiation, so backcalculate # contacts investigated
  #CHILD CONTACTS
  child <- pop_calcs %>% select(-pop) %>% 
    rename("initiate_ipt_covg"="child_ipt", 
           "initiate_3hp_covg"="child_3hp",
           "initiate_1hp_covg"="child_1hp",
           "initiate_3hr_covg"="child_3hr",
           "pop"="pop04", "hh"="hh_child") %>%
    mutate(total=tb_notif_new*hh,
           flag=child_none + (initiate_ipt_covg + initiate_3hp_covg + 
                                             initiate_3hr_covg + initiate_1hp_covg)/
                               (child_params$p_initiate*p_hh_child_tpt),
           hh_contact_covg = pmin(1, flag), 
           prop_ipt=if_else(initiate_ipt_covg==0, 0,
                            initiate_ipt_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + child_none)),
           prop_3hp=if_else(initiate_3hp_covg==0, 0, 
                            initiate_3hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + child_none)),
           prop_1hp=if_else(initiate_1hp_covg==0, 0, 
                            initiate_1hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + child_none)),
           prop_3hr=if_else(initiate_3hr_covg==0, 0, 
                            initiate_3hr_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + child_none)),
           prop_none=if_else(child_none==0, 0,
                             child_none/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                           initiate_3hr_covg + child_none)),
           contact_invest_num = hh_contact_covg*total,
           initiate_num=contact_invest_num*p_hh_child_tpt*(prop_ipt+prop_3hp+prop_1hp+prop_3hr)*child_params$p_initiate,
           p_initiate=child_params$p_initiate) %>%
    select(country, code, year, scenario, flag,
           pop, hh, total, prop_ipt, prop_3hp, prop_1hp, prop_3hr, prop_none, 
           contact_invest_num, initiate_num, p_initiate)
  child <- child %>% mutate(p_notif=child_params$p_notif,
                            p_success=child_params$p_success,
                            p_die=child_params$p_die)
  child_flag <- max(child$flag)
  print(child_flag)
  
  #run model for child contacts
  child <- calc_contact_invest(child, child_params) #outcomes of contact investigation
  child <- calc_tpt_outcomes_contacts(child, child_params) #TPT-related events and outcomes
  #calculate out horizon years
  child_all <- list()
  child_all[[1]] <- child %>% mutate(yrs_out=0, 
                                     cum_tb_deaths=tb_deaths,
                                     cum_non_tb_deaths=non_tb_deaths)
  for(i in 1:analytic_horizon) {
    #children: mortality and reactivation rates vary over time, notification and mortality varies by country
    child_prev <- child_all[[i]]
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
    child_cur <- child_cur %>% mutate(yrs_out=i)
    child_all[[i+1]] <- child_cur
  }
  child_all <- rbindlist(child_all, use.names=T)
  child_comb <- combine_yrs(child_all, "child", policy_end_yr, end_yr) #combine across years
  
  #CONTACTS AGED 5-14
  adol <- pop_calcs %>% select(-pop) %>% 
    rename("initiate_ipt_covg"="adol_ipt", 
           "initiate_3hp_covg"="adol_3hp",
           "initiate_1hp_covg"="adol_1hp",
           "initiate_3hr_covg"="adol_3hr",
           "pop"="pop514", "hh"="hh_adol") %>%
    mutate(total=tb_notif_new*hh,
           flag=adol_none + (initiate_ipt_covg + initiate_3hp_covg + 
                                initiate_3hr_covg + initiate_1hp_covg)/
             (adol_params$p_initiate*p_hh_adol_tpt),
           hh_contact_covg = pmin(1, flag), 
           prop_ipt=if_else(initiate_ipt_covg==0, 0,
                            initiate_ipt_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + adol_none)),
           prop_3hp=if_else(initiate_3hp_covg==0, 0, 
                            initiate_3hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg +
                                                 initiate_3hr_covg + adol_none)),
           prop_1hp=if_else(initiate_1hp_covg==0, 0, 
                            initiate_1hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + adol_none)),
           prop_3hr=if_else(initiate_3hr_covg==0, 0, 
                            initiate_3hr_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + adol_none)),
           prop_none=if_else(adol_none==0, 0,
                             adol_none/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                          initiate_3hr_covg + adol_none)),
           contact_invest_num = hh_contact_covg*total,
           initiate_num=contact_invest_num*p_hh_adol_tpt*(prop_ipt+prop_3hp+prop_1hp+prop_3hr)*adol_params$p_initiate,
           p_initiate=adol_params$p_initiate) %>%
    select(country, code, year, scenario, flag,
           pop, hh, total, prop_ipt, prop_3hp, prop_1hp, prop_3hr, prop_none, 
           contact_invest_num, initiate_num, p_initiate)
  adol <- adol %>% mutate(p_notif=adol_params$p_notif,
                          p_success=adol_params$p_success,
                          p_die=adol_params$p_die)
  adol_flag <- max(adol$flag)
  print(adol_flag)
  
  #run model for adol contacts
  adol <- calc_contact_invest(adol, adol_params) #outcomes of contact investigation
  adol <- calc_tpt_outcomes_contacts(adol, adol_params) #TPT-related events and outcomes
  #calculate out horizon years
  adol_all <- list()
  adol_all[[1]] <- adol %>% mutate(yrs_out=0, 
                                     cum_tb_deaths=tb_deaths,
                                     cum_non_tb_deaths=non_tb_deaths)
  for(i in 1:analytic_horizon) {
    #mortality and reactivation rates vary over time, notification and mortality varies by country
    adol_prev <- adol_all[[i]]
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
    adol_cur <- adol_cur %>% mutate(yrs_out=i)
    adol_all[[i+1]] <- adol_cur
  }
  adol_all <- rbindlist(adol_all, use.names=T)
  adol_comb <- combine_yrs(adol_all, "adol", policy_end_yr, end_yr) #combine across years
  
  #ADULT CONTACTS
  adult <- pop_calcs %>% select(-pop) %>% 
    rename("initiate_ipt_covg"="adult_ipt", 
           "initiate_3hp_covg"="adult_3hp",
           "initiate_1hp_covg"="adult_1hp",
           "initiate_3hr_covg"="adult_3hr",
           "pop"="popadult", "hh"="hh_adult") %>%
    mutate(total=tb_notif_new*hh,
           flag=adult_none + (initiate_ipt_covg + initiate_3hp_covg + 
                                initiate_3hr_covg + initiate_1hp_covg)/
             (adult_params$p_initiate*p_hh_adult_tpt),
           hh_contact_covg = pmin(1, flag), 
           prop_ipt=if_else(initiate_ipt_covg==0, 0,
                            initiate_ipt_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + adult_none)),
           prop_3hp=if_else(initiate_3hp_covg==0, 0, 
                            initiate_3hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + adult_none)),
           prop_1hp=if_else(initiate_1hp_covg==0, 0, 
                            initiate_1hp_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + adult_none)),
           prop_3hr=if_else(initiate_3hr_covg==0, 0, 
                            initiate_3hr_covg/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                                 initiate_3hr_covg + adult_none)),
           prop_none=if_else(adult_none==0, 0,
                             adult_none/(initiate_ipt_covg + initiate_3hp_covg + initiate_1hp_covg + 
                                           initiate_3hr_covg + adult_none)),
           contact_invest_num = hh_contact_covg*total,
           initiate_num=contact_invest_num*p_hh_adult_tpt*(prop_ipt+prop_3hp+prop_1hp+prop_3hr)*adult_params$p_initiate,
           p_initiate=adult_params$p_initiate) %>%
    select(country, code, year, scenario, flag,
           pop, hh, total, prop_ipt, prop_3hp, prop_1hp, prop_3hr, prop_none, 
           contact_invest_num, initiate_num, p_initiate)
  adult <- adult %>% mutate(p_notif=adult_params$p_notif,
                            p_success=adult_params$p_success,
                            p_die=adult_params$p_die)
  adult_flag <- max(adult$flag)
  print(adult_flag)
  
  #run model for adult contacts
  adult <- calc_contact_invest(adult, adult_params) #outcomes of contact investigation
  adult <- calc_tpt_outcomes_contacts(adult, adult_params) #TPT-related events and outcomes
  #calculate out horizon years
  adult_all <- list()
  adult_all[[1]] <- adult %>% mutate(yrs_out=0, 
                                   cum_tb_deaths=tb_deaths,
                                   cum_non_tb_deaths=non_tb_deaths)
  for(i in 1:analytic_horizon) {
    #mortality and reactivation rates vary over time, notification and mortality varies by country
    adult_prev <- adult_all[[i]]
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
    adult_cur <- adult_cur %>% mutate(yrs_out=i)
    adult_all[[i+1]] <- adult_cur
  }
  adult_all <- rbindlist(adult_all, use.names=T)
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
  
  child_comb <- child_comb %>% 
    mutate(initiate_ipt=complete_ipt+part_complete_ipt+tox_nohosp_ipt+tox_hosp_ipt,
           initiate_3hp=complete_3hp+part_complete_3hp+tox_nohosp_3hp+tox_hosp_3hp,
           initiate_1hp=complete_1hp+part_complete_1hp+tox_nohosp_1hp+tox_hosp_1hp,
           initiate_3hr=complete_3hr+part_complete_3hr+tox_nohosp_3hr+tox_hosp_3hr)
  adol_comb <- adol_comb %>% 
    mutate(initiate_ipt=complete_ipt+part_complete_ipt+tox_nohosp_ipt+tox_hosp_ipt,
           initiate_3hp=complete_3hp+part_complete_3hp+tox_nohosp_3hp+tox_hosp_3hp,
           initiate_1hp=complete_1hp+part_complete_1hp+tox_nohosp_1hp+tox_hosp_1hp,
           initiate_3hr=complete_3hr+part_complete_3hr+tox_nohosp_3hr+tox_hosp_3hr)
  adult_comb <- adult_comb %>% 
    mutate(initiate_ipt=complete_ipt+part_complete_ipt+tox_nohosp_ipt+tox_hosp_ipt,
           initiate_3hp=complete_3hp+part_complete_3hp+tox_nohosp_3hp+tox_hosp_3hp,
           initiate_1hp=complete_1hp+part_complete_1hp+tox_nohosp_1hp+tox_hosp_1hp,
           initiate_3hr=complete_3hr+part_complete_3hr+tox_nohosp_3hr+tox_hosp_3hr)
  
  #calculate costs
  child_comb <- calc_costs(child_comb, "child", child_params, 
                           child_params, child_params$disc_fac, start_yr)
  adol_comb <- calc_costs(adol_comb, "adol", adol_params, 
                          adol_params, adol_params$disc_fac, start_yr)
  adult_comb <- calc_costs(adult_comb, "adult", adult_params, 
                           adult_params, adult_params$disc_fac, start_yr)
  
  out <- list("child"=child_comb, "adol"=adol_comb, "adult"=adult_comb,
              "child_flag"=child_flag, "adol_flag"=adol_flag, "adult_flag"=adult_flag)
  return(out)
}



regimens <- c("3HP", "1HP", "3HR", "6H", "None")
countries <- c("Ethiopia", "India", "Nigeria", "South Africa", "Zambia")
scenarios <- c("TPT Scaleup", "Comparator (no TPT)")
cost_colors <- data.frame(row.names=c("cost_tx", "cost_art", "cost_contact", 
                                      "cost_3hp", "cost_1hp", "cost_3hr",
                                      "cost_ipt", "cost_tox", "cost_impl"),
                          colors=c(brewer.pal(n=8, name="Set2"), brewer.pal(n=12, name="Paired")[[12]]),
                          labels=c("TB treatment", "ART",  "Contact investigation & diagnosis", 
                                   "3HP drugs & clinic visits", "1HP drugs & clinic visits",
                                   "3HR drugs & clinic visits",
                                   "6H drugs & clinic visits", "TPT toxicity management",
                                   "Implementation costs (short-course TPT)")
                          )
options_split <- c("Equal coverage across new and previously enrolled PLHIV"=1,
                   "Prioritize newly enrolled PLHIV"=2)
option_split <- options_split[[1]]

#Pt 1: User Interface (UI) - framework (structure for app's appearance)
ui <- navbarPage(
  title="TB Preventive Treatment Budget Impact Tool",
  theme=bs_theme(version=5, bootswatch="flatly"),
  tabPanel(
    title="Main",
    sidebarLayout(
      sidebarPanel(
        width=2, 
        #h3("Specify changes to inputs"),
        shiny::selectInput(
          inputId="country",
          label="Select a country",
          choices=countries
          ),
        h4("Submit changes"),
        actionButton("go", "Submit")
      ),
      mainPanel(
        h2("Results"),
        headerPanel(""),
        fluidRow(column(6, plotlyOutput("plhiv_costs_sum")),
                 column(6, plotlyOutput("plhiv_initiate"))),
        headerPanel(""),
        fluidRow(column(6, plotlyOutput("child_costs_sum")),
                 column(6, plotlyOutput("child_initiate"))),
        headerPanel(""),
        fluidRow(column(6, plotlyOutput("adol_costs_sum")),
                 column(6, plotlyOutput("adol_initiate"))),
        headerPanel(""),
        fluidRow(column(6, plotlyOutput("adult_costs_sum")),
                 column(6, plotlyOutput("adult_initiate")))
      )
    )
  ),
  tabPanel(
    title="PLHIV Results",
    h2("Additional results for PLHIV"),
    headerPanel(""),
    fluidRow(column(5, plotlyOutput("plhiv_costs_tpt_detail")),
              column(7, plotlyOutput("plhiv_costs_comp_detail"))),
    headerPanel(""),
    fluidRow(column(5, plotlyOutput("plhiv_costs_tpt_detail_art")),
              column(7, plotlyOutput("plhiv_costs_comp_detail_art")))
  ),
  tabPanel(
    title="HH Contact Results",
    h2("Additional results for Household Contacts"),
    headerPanel(""),
    fluidRow(column(5, plotlyOutput("child_costs_tpt_detail")),
             column(7, plotlyOutput("child_costs_comp_detail"))),
    headerPanel(""),
    fluidRow(column(5, plotlyOutput("adol_costs_tpt_detail")),
             column(7, plotlyOutput("adol_costs_comp_detail"))),
    headerPanel(""),
    fluidRow(column(5, plotlyOutput("adult_costs_tpt_detail")),
             column(7, plotlyOutput("adult_costs_comp_detail")))

  ),
  tabPanel(
    title="Specify Coverage",
    h2("Submit changes when finished"),
    actionButton("go_covg", "Submit"),
    h2("Specify coverage by TPT regimen, year, and population"),
    h4("Coverage for PLHIV"),
    fluidRow(tags$style("#split_plhiv_2024 {color:red;}"),
      column(1, HTML('<br> <p style="text-align:right;padding-bottom:15px;padding-top:15px;"> <b> 2024 </b> </p>')),
             column(2, numericInput(
               inputId="num_plhiv_2024",
               label=strong("Number initiating TPT"),
               value=0)),
             column(1, numericInput(
               inputId="split_plhiv_3hp_2024", 
               label=em(strong("3HP (%)")),
               value=100)),
             column(1, numericInput(
               inputId="split_plhiv_1hp_2024",
               label=em(strong("1HP (%)")),
               value=0)),
      column(1, numericInput(
        inputId="split_plhiv_3hr_2024",
        label=em(strong("3HR (%)")),
        value=0)),
      column(1, numericInput(
               inputId="split_plhiv_6h_2024",
               label=em(strong("6H (%)")),
               value=0)),
             column(2, textOutput('split_plhiv_2024'))),
    div(style="margin-top:-1em", 
        fluidRow(tags$style("#split_plhiv_2025 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2025 </b> </p>')),
             column(2, numericInput(
               inputId="num_plhiv_2025",
               label=NULL,
               value=0)),
             column(1, numericInput(
               inputId="split_plhiv_3hp_2025",
               label=NULL,
               value=100)),
             column(1, numericInput(
               inputId="split_plhiv_1hp_2025",
               label=NULL,
               value=0)),
             column(1, numericInput(
               inputId="split_plhiv_3hr_2025",
               label=NULL,
               value=0)),
             column(1, numericInput(
               inputId="split_plhiv_6h_2025",
               label=NULL,
               value=0)),
             column(2, textOutput('split_plhiv_2025')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_plhiv_2026 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2026 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_plhiv_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hp_2026",
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_plhiv_1hp_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hr_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_6h_2026", 
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_plhiv_2026')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_plhiv_2027 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2027 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_plhiv_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hp_2027",
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_plhiv_1hp_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hr_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_6h_2027",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_plhiv_2027')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_plhiv_2028 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2028 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_plhiv_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hp_2028",
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_plhiv_1hp_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hr_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_6h_2028",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_plhiv_2028')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_plhiv_2029 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2029 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_plhiv_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hp_2029",
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_plhiv_1hp_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hr_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_6h_2029",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_plhiv_2029')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_plhiv_2030 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2030 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_plhiv_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hp_2030",
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_plhiv_1hp_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hr_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_6h_2030",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_plhiv_2030')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_plhiv_2031 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2031 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_plhiv_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hp_2031",
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_plhiv_1hp_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hr_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_6h_2031",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_plhiv_2031')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_plhiv_2032 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2032 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_plhiv_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hp_2032",
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_plhiv_1hp_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hr_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_6h_2032",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_plhiv_2032')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_plhiv_2033 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2033 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_plhiv_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hp_2033",
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_plhiv_1hp_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_3hr_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_plhiv_6h_2033",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_plhiv_2033')))),
    h4("Coverage for Contacts < 5"),
    fluidRow(tags$style("#split_child_2024 {color:red;}"),
             column(1, HTML('<br> <p style="text-align:right;padding-bottom:15px;padding-top:15px;"> <b> 2024 </b> </p>')),
             column(2, numericInput(
               inputId="num_child_2024",
               label=strong("Number initiating TPT"),
               value=0)),
             column(1, numericInput(
               inputId="split_child_3hp_2024", 
               label=em(strong("3HP (%)")),
               value=100)),
             column(1, numericInput(
               inputId="split_child_1hp_2024",
               label=em(strong("1HP (%)")),
               value=0)),
             column(1, numericInput(
               inputId="split_child_3hr_2024",
               label=em(strong("3HR (%)")),
               value=0)),
             column(1, numericInput(
               inputId="split_child_6h_2024",
               label=em(strong("6H (%)")),
               value=0)),
             column(2, numericInput(
               inputId="split_child_none_2024",
               label=em(strong("Investigated only (%)")),
               value=0)),
             column(2, textOutput('split_child_2024'))),
    div(style="margin-top:-1em", 
        fluidRow(tags$style("#split_child_2025 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2025 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_child_2025",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hp_2025", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_child_1hp_2025",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hr_2025",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_6h_2025",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_child_none_2025",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_child_2025')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_child_2026 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2026 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_child_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hp_2026", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_child_1hp_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hr_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_6h_2026",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_child_none_2026",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_child_2026')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_child_2027 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2027 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_child_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hp_2027", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_child_1hp_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hr_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_6h_2027",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_child_none_2027",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_child_2027')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_child_2028 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2028 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_child_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hp_2028", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_child_1hp_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hr_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_6h_2028",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_child_none_2028",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_child_2028')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_child_2029 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2029 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_child_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hp_2029", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_child_1hp_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hr_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_6h_2029",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_child_none_2029",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_child_2029')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_child_2030 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2030 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_child_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hp_2030", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_child_1hp_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hr_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_6h_2030",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_child_none_2030",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_child_2030')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_child_2031 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2031 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_child_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hp_2031", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_child_1hp_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hr_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_6h_2031",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_child_none_2031",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_child_2031')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_child_2032 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2032 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_child_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hp_2032", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_child_1hp_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hr_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_6h_2032",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_child_none_2032",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_child_2032')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_child_2033 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2033 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_child_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hp_2033", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_child_1hp_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_3hr_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_child_6h_2033",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_child_none_2033",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_child_2033')))),
    h4("Coverage for Contacts 5-14"),
    fluidRow(tags$style("#split_adol_2024 {color:red;}"),
             column(1, HTML('<br> <p style="text-align:right;padding-bottom:15px;padding-top:15px;"> <b> 2024 </b> </p>')),
             column(2, numericInput(
               inputId="num_adol_2024",
               label=strong("Number initiating TPT"),
               value=0)),
             column(1, numericInput(
               inputId="split_adol_3hp_2024", 
               label=em(strong("3HP (%)")),
               value=100)),
             column(1, numericInput(
               inputId="split_adol_1hp_2024",
               label=em(strong("1HP (%)")),
               value=0)),
             column(1, numericInput(
               inputId="split_adol_3hr_2024",
               label=em(strong("3HR (%)")),
               value=0)),
             column(1, numericInput(
               inputId="split_adol_6h_2024",
               label=em(strong("6H (%)")),
               value=0)),
             column(2, numericInput(
               inputId="split_adol_none_2024",
               label=em(strong("Investigated only (%)")),
               value=0)),
             column(2, textOutput('split_adol_2024'))),
    div(style="margin-top:-1em", 
        fluidRow(tags$style("#split_adol_2025 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2025 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adol_2025",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hp_2025", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adol_1hp_2025",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hr_2025",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_6h_2025",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adol_none_2025",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adol_2025')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adol_2026 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2026 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adol_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hp_2026", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adol_1hp_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hr_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_6h_2026",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adol_none_2026",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adol_2026')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adol_2027 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2027 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adol_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hp_2027", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adol_1hp_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hr_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_6h_2027",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adol_none_2027",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adol_2027')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adol_2028 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2028 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adol_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hp_2028", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adol_1hp_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hr_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_6h_2028",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adol_none_2028",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adol_2028')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adol_2029 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2029 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adol_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hp_2029", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adol_1hp_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hr_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_6h_2029",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adol_none_2029",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adol_2029')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adol_2030 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2030 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adol_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hp_2030", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adol_1hp_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hr_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_6h_2030",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adol_none_2030",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adol_2030')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adol_2031 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2031 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adol_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hp_2031", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adol_1hp_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hr_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_6h_2031",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adol_none_2031",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adol_2031')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adol_2032 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2032 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adol_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hp_2032", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adol_1hp_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hr_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_6h_2032",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adol_none_2032",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adol_2032')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adol_2033 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2033 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adol_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hp_2033", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adol_1hp_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_3hr_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adol_6h_2033",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adol_none_2033",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adol_2033')))),
    h4("Coverage for Contacts 15+"),
    fluidRow(tags$style("#split_adult_2024 {color:red;}"),
             column(1, HTML('<br> <p style="text-align:right;padding-bottom:15px;padding-top:15px;"> <b> 2024 </b> </p>')),
             column(2, numericInput(
               inputId="num_adult_2024",
               label=strong("Number initiating TPT"),
               value=0)),
             column(1, numericInput(
               inputId="split_adult_3hp_2024", 
               label=em(strong("3HP (%)")),
               value=100)),
             column(1, numericInput(
               inputId="split_adult_1hp_2024",
               label=em(strong("1HP (%)")),
               value=0)),
             column(1, numericInput(
               inputId="split_adult_3hr_2024",
               label=em(strong("3HR (%)")),
               value=0)),
             column(1, numericInput(
               inputId="split_adult_6h_2024",
               label=em(strong("6H (%)")),
               value=0)),
             column(2, numericInput(
               inputId="split_adult_none_2024",
               label=em(strong("Investigated only (%)")),
               value=0)),
             column(2, textOutput('split_adult_2024'))),
    div(style="margin-top:-1em", 
        fluidRow(tags$style("#split_adult_2025 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2025 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adult_2025",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hp_2025", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adult_1hp_2025",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hr_2025",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_6h_2025",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adult_none_2025",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adult_2025')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adult_2026 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2026 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adult_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hp_2026", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adult_1hp_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hr_2026",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_6h_2026",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adult_none_2026",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adult_2026')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adult_2027 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2027 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adult_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hp_2027", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adult_1hp_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hr_2027",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_6h_2027",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adult_none_2027",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adult_2027')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adult_2028 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2028 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adult_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hp_2028", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adult_1hp_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hr_2028",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_6h_2028",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adult_none_2028",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adult_2028')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adult_2029 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2029 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adult_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hp_2029", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adult_1hp_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hr_2029",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_6h_2029",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adult_none_2029",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adult_2029')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adult_2030 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2030 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adult_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hp_2030", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adult_1hp_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hr_2030",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_6h_2030",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adult_none_2030",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adult_2030')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adult_2031 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2031 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adult_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hp_2031", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adult_1hp_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hr_2031",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_6h_2031",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adult_none_2031",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adult_2031')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adult_2032 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2032 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adult_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hp_2032", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adult_1hp_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hr_2032",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_6h_2032",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adult_none_2032",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adult_2032')))),
    div(style="margin-top:-0.5em", 
        fluidRow(tags$style("#split_adult_2033 {color:red;}"),
                 column(1, HTML('<p style="text-align:right;padding-bottom:8px;padding-top:8px;"> <b> 2033 </b> </p>')),
                 column(2, numericInput(
                   inputId="num_adult_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hp_2033", 
                   label=NULL,
                   value=100)),
                 column(1, numericInput(
                   inputId="split_adult_1hp_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_3hr_2033",
                   label=NULL,
                   value=0)),
                 column(1, numericInput(
                   inputId="split_adult_6h_2033",
                   label=NULL,
                   value=0)),
                 column(2, numericInput(
                   inputId="split_adult_none_2033",
                   label=NULL,
                   value=0)),
                 column(2, textOutput('split_adult_2033'))))
    
  ),
  tabPanel(
    title="Other Options",
    h2("Submit changes when finished"),
    actionButton("go_advanced", "Submit"),
    h2("Specify optional parameters"),
    h4("Target Population Sizes - PLHIV"),
    fluidRow(
      column(2, numericInput(
        inputId="new_plhiv_2024",
        label="Newly enrolled PLHIV eligible for TPT, 2024",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2024) %>% pull(plhiv_art_new_lag))),
      column(2, numericInput(
        inputId="backlog_plhiv_2024",
        label="PLHIV already on ART & eligible for TPT, 2024",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2024) %>% pull(backlog_none))),
      column(2, numericInput(
        inputId="new_plhiv_2025",
        label="Newly enrolled PLHIV eligible for TPT, 2025",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2025) %>% pull(plhiv_art_new_lag))),
      column(2, numericInput(
        inputId="new_plhiv_2026",
        label="Newly enrolled PLHIV eligible for TPT, 2026",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2026) %>% pull(plhiv_art_new_lag))),
      column(2, numericInput(
        inputId="new_plhiv_2027",
        label="Newly enrolled PLHIV eligible for TPT, 2027",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2027) %>% pull(plhiv_art_new_lag))),
      column(2, numericInput(
        inputId="new_plhiv_2028",
        label="Newly enrolled PLHIV eligible for TPT, 2028",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2028) %>% pull(plhiv_art_new_lag))),
    ),
    fluidRow(
      column(2, numericInput(
        inputId="new_plhiv_2029",
        label="Newly enrolled PLHIV eligible for TPT, 2029",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2029) %>% pull(plhiv_art_new_lag))),
      column(2, numericInput(
        inputId="new_plhiv_2030",
        label="Newly enrolled PLHIV eligible for TPT, 2030",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2030) %>% pull(plhiv_art_new_lag))),
      column(2, numericInput(
        inputId="new_plhiv_2031",
        label="Newly enrolled PLHIV eligible for TPT, 2031",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2031) %>% pull(plhiv_art_new_lag))),
      column(2, numericInput(
        inputId="new_plhiv_2032",
        label="Newly enrolled PLHIV eligible for TPT, 2032",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2032) %>% pull(plhiv_art_new_lag))),
      column(2, numericInput(
        inputId="new_plhiv_2033",
        label="Newly enrolled PLHIV eligible for TPT, 2033",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2033) %>% pull(plhiv_art_new_lag))),
    ),
    h4('Target Population Sizes - Household Contacts'),
    fluidRow(
      column(2, numericInput(
        inputId="notif_2024",
        label="Notified TB patients, 2024",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2024) %>% pull(tb_notif_new))),
      column(2, numericInput(
        inputId="notif_2025",
        label="Notified TB patients, 2025",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2025) %>% pull(tb_notif_new))),
      column(2, numericInput(
        inputId="notif_2026",
        label="Notified TB patients, 2026",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2026) %>% pull(tb_notif_new))),
      column(2, numericInput(
        inputId="notif_2027",
        label="Notified TB patients, 2027",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2027) %>% pull(tb_notif_new))),
      column(2, numericInput(
        inputId="notif_2028",
        label="Notified TB patients, 2028",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2028) %>% pull(tb_notif_new)))
    ),
    fluidRow(
      column(2, numericInput(
        inputId="notif_2029",
        label="Notified TB patients, 2029",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2029) %>% pull(tb_notif_new))),
      column(2, numericInput(
        inputId="notif_2030",
        label="Notified TB patients, 2030",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2030) %>% pull(tb_notif_new))),
      column(2, numericInput(
        inputId="notif_2031",
        label="Notified TB patients, 2031",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2031) %>% pull(tb_notif_new))),
      column(2, numericInput(
        inputId="notif_2032",
        label="Notified TB patients, 2032",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2032) %>% pull(tb_notif_new))),
      column(2, numericInput(
        inputId="notif_2033",
        label="Notified TB patients, 2033",
        value=pop_calcs %>% filter(country==countries[[1]] & year==2033) %>% pull(tb_notif_new)))
    ),
    h4("TPT Acceptance & Refusal"),
    fluidRow(column(3, numericInput(
      inputId="initiate_plhiv",
      label="TPT Acceptance, PLHIV (%)",
      value=73.5)),
      column(3, numericInput(
        inputId="initiate_child",
        label="TPT Acceptance, Contacts < 5 (%)",
        value=73.5)),
      column(3, numericInput(
        inputId="initiate_adol",
        label="TPT Acceptance, Contacts 5-14 (%)",
        value=73.5)),
      column(3, numericInput(
        inputId="initiate_adult",
        label="TPT Acceptance, Contacts 15+ (%)",
        value=73.5))
    ),
    h4("TPT Drug Costs (all in USD per full course)"),
    fluidRow(
      column(3, numericInput(
        inputId="c_3hp_plhiv",
        label="3HP cost per PLHIV",
        value=14.25)),
      column(3, numericInput(
        inputId="c_3hp_child",
        label="3HP cost per contact < 5",
        value=6)),
      column(3, numericInput(
        inputId="c_3hp_adol",
        label="3HP cost per contact 5-14",
        value=12)),
      column(3, numericInput(
        inputId="c_3hp_adult",
        label="3HP cost per contact 15+",
        value=14.25))),
    fluidRow(
      column(3, numericInput(
        inputId="c_1hp_plhiv",
        label="1HP cost per PLHIV",
        value=22)),
      column(3, numericInput(
        inputId="c_1hp_child",
        label="1HP cost per contact < 5",
        value=22)),
      column(3, numericInput(
        inputId="c_1hp_adol",
        label="1HP cost per contact 5-14",
        value=22)),
      column(3, numericInput(
        inputId="c_1hp_adult",
        label="1HP cost per contact 15+",
        value=22))),
    fluidRow(
      column(3, numericInput(
        inputId="c_3hr_plhiv",
        label="3HR cost per PLHIV",
        value=38)),
      column(3, numericInput(
        inputId="c_3hr_child",
        label="3HR cost per contact < 5",
        value=19)),
      column(3, numericInput(
        inputId="c_3hr_adol",
        label="3HR cost per contact 5-14",
        value=38)),
      column(3, numericInput(
        inputId="c_3hr_adult",
        label="3HR cost per contact 15+",
        value=38))),
    fluidRow(
      column(3, numericInput(
        inputId="c_6h_plhiv",
        label="6H cost per PLHIV",
        value=7.2)),
      column(3, numericInput(
        inputId="c_6h_child",
        label="6H cost per contact < 5",
        value=3.6)),
      column(3, numericInput(
        inputId="c_6h_adol",
        label="6H cost per contact 5-14",
        value=5.4)),
      column(3, numericInput(
        inputId="c_6h_adult",
        label="6H cost per contact 15+",
        value=7.2))
    ),
    h4("Other Unit Costs (all in USD)"),
    fluidRow(column(2, numericInput(
      inputId="c_contact",
      label="Cost to visit one household contact",
      value=round(cost_params[['NGA']][["contact"]], 2))),
      column(2, numericInput(
        inputId="c_tx",
        label="Cost of treating one patient for DS-TB",
        value=round(cost_params[['NGA']][["tb_tx"]], 2))),
      column(2, numericInput(
        inputId="c_xray",
        label="Cost to perform one chest X-ray",
        value=round(cost_params[['NGA']][["xray"]], 2))),
      column(2, numericInput(
        inputId="c_xpert",
        label="Cost to conduct one Xpert test",
        value=round(cost_params[['NGA']][["xpert"]], 2))),
      column(2, numericInput(
        inputId="c_outpatient",
        label="Cost of one outpatient visit",
        value=10)),
      column(2, numericInput(
        inputId="c_art",
        label="Annual cost of ART & viral load/CD4 testing for PLHIV)",
        value=94.38)),
      column(2, numericInput(
        inputId="c_impl",
        label="Implementation cost (per course of short-course TPT delivered)",
        value=0)))
  ),
  tabPanel(
    title="Download Results",
    sidebarLayout(
      sidebarPanel(
        h4("Download as CSV"),
        width=3, 
        downloadButton("downloadData", "Download")
      ),
      mainPanel(
        h2("Full Output"),
        tableOutput("table"),
        style="font-size: 75%"
      )
    )
  ),
  tabPanel(
    title="Help",
    h4("Instructions"),
    HTML("<br> This tool can be used to estimate the costs of scaling up a TB preventive treatment program for people with HIV (PLHIV) and/or household contacts of notified TB patients (contacts). Costs are calculated for a <i> TPT Scenario </i>, which the user specifies, and a <i> Comparator Scenario </i>, which includes 0% TPT coverage and 0% coverage of household contact investigation"),
    HTML("<br> <br> To get started, navigate to the <b> Main </b> tab using the toolbar on the top of your browser screen. "),
    HTML("Select a country using the dropdown menu, then select the desired coverage level (proportion of eligible PLHIV or household contacts that initiate TPT each year) and regimen (3HP, 1HP, 3HR, 6H, or None/No TPT), and then click the <b> Submit </b> button on the top left, which runs the model and updates the figures."),
    HTML("For household contacts, selecting None as the regimen type indicates coverage of contact investigation only. This option is not available for PLHIV."),
    HTML("Note that coverage is defined as the percent of the eligible target population initiating TPT. Because of < 100% TPT acceptance, coverage can be effectively capped at < 100% - but this can be adjusted in <b> Other Options </b> (see below)."),
    HTML("<br> <br> The tool will automatically populate with parameters relevant to a given country (e.g., size of target populations, unit costs) and regimen (e.g., drug costs and completion), and will assume the same coverage level over a 5-year period."),
    HTML("These automatic updates can be manually changed in the <b> Advanced Coverage Options </b> and <b> Other Options </b> tabs."),
    HTML("Be sure to click the <b> Submit </b> button after making any changes in either tab. <br>"),
    HTML("<br> The <b> Advanced Coverage Options </b> tab allows users to specify TPT coverage for each target population that varies by year and can include multiple TPT regimens. Changes to this tab apply to the <i> TPT Scenario </i> only. In this tab: 
         <ul>
         <li> For PLHIV, <b> TPT coverage </b> is defined as the percent (from 0-100%) of eligible PLHIV that initiate any type of TPT in a given year. </li>
         <li> For household contacts, <b> TPT coverage </b> is defined as the percent (from 0-100%) of eligible contacts that are investigated for TB and/or initiate any type of TPT in a given year. </li>
         <li> For both PLHIV and contacts, the remaining columns in <b> Advanced Coverage Options </b> indicate the split of TPT coverage by regimen (and for household contacts, no TPT, with contact investigation only). Note that these percentages must sum to 100%. </li>
         </ul>"),
    HTML("For example, specifying 40% total coverage for contacts aged 5-14 years, with Percent 3HP = 50%, Percent 1HP = 25%, Percent 3HR = 0%, Percent 6H = 0%, and Percent No TPT = 25%, would mean that 20% of eligible household contacts 5-14 intitiate 3HP, 10% initiate 1HP, 10% receive contact investigation only but don't initiate any TPT, and 60% are not investigated (and do not initiate TPT)."),
    HTML("<br> <br> The <b> Other Options </b> tab allows users to update the underlying sizes of the target populations, change TPT acceptance rates, and impute unit costs. Changes to this tab apply to both the <i> TPT Scenario </i> and the <i> Comparator Scenario </i>."),
    HTML("Some definitions relevant to parameters in the <b> Other Options </b> tab are included below:
         <ul>
         <li> <b> PLHIV target population sizes </b>: users can manually specify the number of PLHIV newly enrolled on ART each year that would be eligible for TPT, as well as the number of PLHIV in 2024 that had been enrolled on ART in a previous year, had not yet received TPT, and are eligible for TPT. The number of previously-enrolled PLHIV eligible for TPT in subsequent years is automatically calculated by the model as a function of the number previously enrolled in 2024, the numbers newly enrolled each year, and TPT coverage each year. </li>
         <li> <b> Household contact target population sizes </b>: users can manually specify the number of TB notifications each year. The number of notifications is combined with country-specific estimates of household size (by age) and user-provided TPT coverage for contacts, to calculate the number of contacts, by age, that receive TPT each year. </li>
         <li> <b> TPT acceptance and refusal </b>: the tool automatically assumes 73.5% TPT acceptance. That is, 26.5% of eligible individuals offered TPT are assumed to refuse it. Therefore, TPT coverage cannot effectively equal 100% unless this parameter is also set at 100%. </li>
         <li> <b> TPT drug costs </b>: specify the cost per full course of TPT (drugs only, in US Dollars) for each target population and regimen. There is no need to specify costs for regimens that are not being included in the <i> TPT Scenario </i> you have specified. </li>
         <li> <b> Other unit costs </b>: specify the cost of other components of TB prevention and treatment (in US Dollars). </li>
         </ul>"),
    HTML("The tool assumes that, under the <i> TPT Scenario </i>, active TB disease among contacts < 5 years old is detected via household visit with symptom screening only and confirmatory diagnosis using Chest Xray. For contacts aged 5 years above, active disease is assumed to be detected via household visit with symptom screening and chest Xray, with confirmatory diagnosis via Xpert. All contacts for whom active disease is not detected are assumed to be eligible to initiate TPT.  All contacts for whom active disease is detected incur costs of DS-TB treatment. The <i> Comparator Scenario </i> assumes no TPT and no contact investigation. Under this scenario, household contacts with active disease would only be diagnosed based on country-specific case detection ratios (estimated by the WHO)."),
    HTML("<br> <br> For PLHIV, the tool assumes that PLHIV on ART with active TB disease would be diagnosed through routine health center visits, and that, under the <i> TPT Scenario </i>, PLHIV not found to have active TB would be eligible for TPT.")
  )
)
  
#Pt 2: Server - runs R code, graphs
server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("initiate_plhiv", sv_between(0.1, 100))
  iv$add_rule("initiate_child", sv_between(0.1, 100))
  iv$add_rule("initiate_adol", sv_between(0.1, 100))
  iv$add_rule("initiate_adult", sv_between(0.1, 100))
  output$split_plhiv_2024 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2024 + input$split_plhiv_1hp_2024 + 
                    input$split_plhiv_3hr_2024 + input$split_plhiv_6h_2024!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_plhiv_2025 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2025 + input$split_plhiv_1hp_2025 + 
                    input$split_plhiv_3hr_2025 + input$split_plhiv_6h_2025!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_plhiv_2026 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2026 + input$split_plhiv_1hp_2026 + 
                    input$split_plhiv_3hr_2026 + input$split_plhiv_6h_2026!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_plhiv_2027 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2027 + input$split_plhiv_1hp_2027 + 
                    input$split_plhiv_3hr_2027 + input$split_plhiv_6h_2027!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_plhiv_2028 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2028 + input$split_plhiv_1hp_2028 + 
                    input$split_plhiv_3hr_2028 + input$split_plhiv_6h_2028!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_plhiv_2029 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2029 + input$split_plhiv_1hp_2029 + 
                    input$split_plhiv_3hr_2029 + input$split_plhiv_6h_2029!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_plhiv_2030 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2030 + input$split_plhiv_1hp_2030 + 
                    input$split_plhiv_3hr_2030 + input$split_plhiv_6h_2030!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_plhiv_2031 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2031 + input$split_plhiv_1hp_2031 + 
                    input$split_plhiv_3hr_2031 + input$split_plhiv_6h_2031!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_plhiv_2032 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2032 + input$split_plhiv_1hp_2032 + 
                    input$split_plhiv_3hr_2032 + input$split_plhiv_6h_2032!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_plhiv_2033 <- renderText({
    out <- ifelse(input$split_plhiv_3hp_2033 + input$split_plhiv_1hp_2033 + 
                    input$split_plhiv_3hr_2033 + input$split_plhiv_6h_2033!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2024 <- renderText({
    out <- ifelse(input$split_child_3hp_2024 + input$split_child_1hp_2024 + input$split_child_3hr_2024 +
                    input$split_child_6h_2024 + input$split_child_none_2024!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2025 <- renderText({
    out <- ifelse(input$split_child_3hp_2025 + input$split_child_1hp_2025 + input$split_child_3hr_2025 +
                    input$split_child_6h_2025 + input$split_child_none_2025!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2026 <- renderText({
    out <- ifelse(input$split_child_3hp_2026 + input$split_child_1hp_2026 + input$split_child_3hr_2026 +
                    input$split_child_6h_2026 + input$split_child_none_2026!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2027 <- renderText({
    out <- ifelse(input$split_child_3hp_2027 + input$split_child_1hp_2027 + input$split_child_3hr_2027 +
                    input$split_child_6h_2027 + input$split_child_none_2027!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2028 <- renderText({
    out <- ifelse(input$split_child_3hp_2028 + input$split_child_1hp_2028 + input$split_child_3hr_2028 +
                    input$split_child_6h_2028 + input$split_child_none_2028!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2029 <- renderText({
    out <- ifelse(input$split_child_3hp_2029 + input$split_child_1hp_2029 + 
                    input$split_child_3hr_2029 + input$split_child_6h_2029 + input$split_child_none_2029!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2030 <- renderText({
    out <- ifelse(input$split_child_3hp_2030 + input$split_child_1hp_2030 + 
                    input$split_child_3hr_2030 + input$split_child_6h_2030 + input$split_child_none_2030!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2031 <- renderText({
    out <- ifelse(input$split_child_3hp_2031 + input$split_child_1hp_2031 + 
                    input$split_child_3hr_2031 + input$split_child_6h_2031 + input$split_child_none_2031!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2032 <- renderText({
    out <- ifelse(input$split_child_3hp_2032 + input$split_child_1hp_2032 + 
                    input$split_child_3hr_2032 + input$split_child_6h_2032 + input$split_child_none_2032!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_child_2033 <- renderText({
    out <- ifelse(input$split_child_3hp_2033 + input$split_child_1hp_2033 + 
                    input$split_child_3hr_2033 + input$split_child_6h_2033 + input$split_child_none_2033!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2024 <- renderText({
    out <- ifelse(input$split_adol_3hp_2024 + input$split_adol_1hp_2024 + input$split_adol_3hr_2024 +
                    input$split_adol_6h_2024 + input$split_adol_none_2024!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2025 <- renderText({
    out <- ifelse(input$split_adol_3hp_2025 + input$split_adol_1hp_2025 + input$split_adol_3hr_2025 +
                    input$split_adol_6h_2025 + input$split_adol_none_2025!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2026 <- renderText({
    out <- ifelse(input$split_adol_3hp_2026 + input$split_adol_1hp_2026 + input$split_adol_3hr_2026 +
                    input$split_adol_6h_2026 + input$split_adol_none_2026!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2027 <- renderText({
    out <- ifelse(input$split_adol_3hp_2027 + input$split_adol_1hp_2027 + input$split_adol_3hr_2027 +
                    input$split_adol_6h_2027 + input$split_adol_none_2027!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2028 <- renderText({
    out <- ifelse(input$split_adol_3hp_2028 + input$split_adol_1hp_2028 + input$split_adol_3hr_2028 +
                    input$split_adol_6h_2028 + input$split_adol_none_2028!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2029 <- renderText({
    out <- ifelse(input$split_adol_3hp_2029 + input$split_adol_1hp_2029 + 
                    input$split_adol_3hr_2029 + input$split_adol_6h_2029 + input$split_adol_none_2029!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2030 <- renderText({
    out <- ifelse(input$split_adol_3hp_2030 + input$split_adol_1hp_2030 + 
                    input$split_adol_3hr_2030 + input$split_adol_6h_2030 + input$split_adol_none_2030!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2031 <- renderText({
    out <- ifelse(input$split_adol_3hp_2031 + input$split_adol_1hp_2031 + 
                    input$split_adol_3hr_2031 + input$split_adol_6h_2031 + input$split_adol_none_2031!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2032 <- renderText({
    out <- ifelse(input$split_adol_3hp_2032 + input$split_adol_1hp_2032 + 
                    input$split_adol_3hr_2032 + input$split_adol_6h_2032 + input$split_adol_none_2032!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adol_2033 <- renderText({
    out <- ifelse(input$split_adol_3hp_2033 + input$split_adol_1hp_2033 + 
                    input$split_adol_3hr_2033 + input$split_adol_6h_2033 + input$split_adol_none_2033!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2024 <- renderText({
    out <- ifelse(input$split_adult_3hp_2024 + input$split_adult_1hp_2024 + input$split_adult_3hr_2024 +
                    input$split_adult_6h_2024 + input$split_adult_none_2024!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2025 <- renderText({
    out <- ifelse(input$split_adult_3hp_2025 + input$split_adult_1hp_2025 + input$split_adult_3hr_2025 +
                    input$split_adult_6h_2025 + input$split_adult_none_2025!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2026 <- renderText({
    out <- ifelse(input$split_adult_3hp_2026 + input$split_adult_1hp_2026 + input$split_adult_3hr_2026 +
                    input$split_adult_6h_2026 + input$split_adult_none_2026!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2027 <- renderText({
    out <- ifelse(input$split_adult_3hp_2027 + input$split_adult_1hp_2027 + input$split_adult_3hr_2027 +
                    input$split_adult_6h_2027 + input$split_adult_none_2027!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2028 <- renderText({
    out <- ifelse(input$split_adult_3hp_2028 + input$split_adult_1hp_2028 + input$split_adult_3hr_2028 +
                    input$split_adult_6h_2028 + input$split_adult_none_2028!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2029 <- renderText({
    out <- ifelse(input$split_adult_3hp_2029 + input$split_adult_1hp_2029 + 
                    input$split_adult_3hr_2029 + input$split_adult_6h_2029 + input$split_adult_none_2029!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2030 <- renderText({
    out <- ifelse(input$split_adult_3hp_2030 + input$split_adult_1hp_2030 + 
                    input$split_adult_3hr_2030 + input$split_adult_6h_2030 + input$split_adult_none_2030!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2031 <- renderText({
    out <- ifelse(input$split_adult_3hp_2031 + input$split_adult_1hp_2031 + 
                    input$split_adult_3hr_2031 + input$split_adult_6h_2031 + input$split_adult_none_2031!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2032 <- renderText({
    out <- ifelse(input$split_adult_3hp_2032 + input$split_adult_1hp_2032 + 
                    input$split_adult_3hr_2032 + input$split_adult_6h_2032 + input$split_adult_none_2032!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  output$split_adult_2033 <- renderText({
    out <- ifelse(input$split_adult_3hp_2033 + input$split_adult_1hp_2033 + 
                    input$split_adult_3hr_2033 + input$split_adult_6h_2033 + input$split_adult_none_2033!=100,
                  "error", "")
    validate(need(out=="", "Error: Split across TPT regimens must sum to 100%"))
    out
  })
  iv$enable()
  observeEvent(input$country, {
    updateNumericInput(session, "c_contact", 
                       value=round(cost_params[[country_info %>% filter(country==input$country) %>% pull(code)]][["contact"]], 2))
    updateNumericInput(session, "c_tx", 
                       value=round(cost_params[[country_info %>% filter(country==input$country) %>% pull(code)]][["tb_tx"]], 2))
    updateNumericInput(session, "c_xray", 
                       value=round(cost_params[[country_info %>% filter(country==input$country) %>% pull(code)]][["xray"]], 2))
    updateNumericInput(session, "c_xpert", 
                       value=round(cost_params[[country_info %>% filter(country==input$country) %>% pull(code)]][["xpert"]], 2))
    updateNumericInput(session, "c_outpatient", 
                       value=round(cost_params[[country_info %>% filter(country==input$country) %>% pull(code)]][["outpatient"]], 2))
    updateNumericInput(session, "c_art", 
                       value=round(plhiv_params[["c_art_yr"]], 2))
    updateNumericInput(session, "new_plhiv_2024", value=round(pop_calcs %>% filter(country==input$country & year==2024) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "backlog_plhiv_2024", value=round(pop_calcs %>% filter(country==input$country & year==2024) %>% pull(backlog_none)))
    updateNumericInput(session, "new_plhiv_2025", value=round(pop_calcs %>% filter(country==input$country & year==2025) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "new_plhiv_2026", value=round(pop_calcs %>% filter(country==input$country & year==2026) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "new_plhiv_2027", value=round(pop_calcs %>% filter(country==input$country & year==2027) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "new_plhiv_2028", value=round(pop_calcs %>% filter(country==input$country & year==2028) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "new_plhiv_2029", value=round(pop_calcs %>% filter(country==input$country & year==2029) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "new_plhiv_2030", value=round(pop_calcs %>% filter(country==input$country & year==2030) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "new_plhiv_2031", value=round(pop_calcs %>% filter(country==input$country & year==2031) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "new_plhiv_2032", value=round(pop_calcs %>% filter(country==input$country & year==2032) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "new_plhiv_2033", value=round(pop_calcs %>% filter(country==input$country & year==2033) %>% pull(plhiv_art_new_lag)))
    updateNumericInput(session, "notif_2024", value=round(pop_calcs %>% filter(country==input$country & year==2024) %>% pull(tb_notif_new)))
    updateNumericInput(session, "notif_2025", value=round(pop_calcs %>% filter(country==input$country & year==2025) %>% pull(tb_notif_new)))
    updateNumericInput(session, "notif_2026", value=round(pop_calcs %>% filter(country==input$country & year==2026) %>% pull(tb_notif_new)))
    updateNumericInput(session, "notif_2027", value=round(pop_calcs %>% filter(country==input$country & year==2027) %>% pull(tb_notif_new)))
    updateNumericInput(session, "notif_2028", value=round(pop_calcs %>% filter(country==input$country & year==2028) %>% pull(tb_notif_new)))
    updateNumericInput(session, "notif_2029", value=round(pop_calcs %>% filter(country==input$country & year==2029) %>% pull(tb_notif_new)))
    updateNumericInput(session, "notif_2030", value=round(pop_calcs %>% filter(country==input$country & year==2030) %>% pull(tb_notif_new)))
    updateNumericInput(session, "notif_2031", value=round(pop_calcs %>% filter(country==input$country & year==2031) %>% pull(tb_notif_new)))
    updateNumericInput(session, "notif_2032", value=round(pop_calcs %>% filter(country==input$country & year==2032) %>% pull(tb_notif_new)))
    updateNumericInput(session, "notif_2033", value=round(pop_calcs %>% filter(country==input$country & year==2033) %>% pull(tb_notif_new)))
  })
  
  rv <- eventReactive(input$go|input$go_advanced|input$go_covg, {
    #update parameters
    country_code <- country_info %>% filter(country==input$country) %>% pull(code)
    cost_params <- unlist(cost_params[[country_code]])
    cost_params[["contact"]] <- input$c_contact
    cost_params[["tb_tx"]] <- input$c_tx
    cost_params[["xray"]] <- input$c_xray
    cost_params[["xpert"]] <- input$c_xpert
    cost_params[["outpatient"]] <- input$c_outpatient
    cost_params[["impl"]] <- input$c_impl
    plhiv_params[["c_art_yr"]] <- input$c_art
    plhiv_params$c_1hp <- input$c_1hp_plhiv
    plhiv_params$c_3hp <- input$c_3hp_plhiv
    plhiv_params$c_3hr <- input$c_3hr_plhiv
    plhiv_params$c_ipt <- input$c_6h_plhiv
    child_params$c_1hp <- input$c_1hp_child
    child_params$c_3hp <- input$c_3hp_child
    child_params$c_3hr <- input$c_3hr_child
    child_params$c_ipt <- input$c_6h_child
    adol_params$c_1hp <- input$c_1hp_adol
    adol_params$c_3hp <- input$c_3hp_adol
    adol_params$c_3hr <- input$c_3hr_adol
    adol_params$c_ipt <- input$c_6h_adol
    adult_params$c_1hp <- input$c_1hp_adult
    adult_params$c_3hp <- input$c_3hp_adult
    adult_params$c_3hr <- input$c_3hr_adult
    adult_params$c_ipt <- input$c_6h_adult
    plhiv_params$p_initiate <- input$initiate_plhiv/100
    child_params$p_initiate <- input$initiate_child/100
    adol_params$p_initiate <- input$initiate_adol/100
    adult_params$p_initiate <- input$initiate_adult/100
    #update population sizes
    pop_calcs <- pop_calcs %>% filter(code==country_code & year %in% 2024:2033) %>% 
      mutate(backlog_none=if_else(year==2024, as.double(input$backlog_plhiv_2024), backlog_none),
             plhiv_art_new_lag=c(input$new_plhiv_2024, input$new_plhiv_2025, input$new_plhiv_2026, 
                                 input$new_plhiv_2027, input$new_plhiv_2028, input$new_plhiv_2029,
                                 input$new_plhiv_2030, input$new_plhiv_2031, input$new_plhiv_2032,
                                 input$new_plhiv_2033),
             tb_notif_new=c(input$notif_2024, input$notif_2025, input$notif_2026, input$notif_2027, input$notif_2028,
                            input$notif_2029, input$notif_2030, input$notif_2031, input$notif_2032, input$notif_2033)) 
    #run model for PLHIV
    plhiv_params <- c(plhiv_params, other_params, cost_params, 
                      "p_ltbi"=ltbi_params %>% filter(iso3==country_code) %>% pull(ltbi_prev), 
                      "p_notif_ltfu"=p_notif_adult[[country_code]],
                      "p_success"=p_success_plhiv[[country_code]],
                      "yrs_new"=2, "yrs"=10)
    #allocation across new vs. previously enrolled will be done in the model itself
    num_plhiv_3hp <- c(input$num_plhiv_2024*input$split_plhiv_3hp_2024, 
                        input$num_plhiv_2025*input$split_plhiv_3hp_2025, 
                        input$num_plhiv_2026*input$split_plhiv_3hp_2026, 
                        input$num_plhiv_2027*input$split_plhiv_3hp_2027, 
                        input$num_plhiv_2028*input$split_plhiv_3hp_2028,
                        input$num_plhiv_2029*input$split_plhiv_3hp_2029, 
                        input$num_plhiv_2030*input$split_plhiv_3hp_2030, 
                        input$num_plhiv_2031*input$split_plhiv_3hp_2031, 
                        input$num_plhiv_2032*input$split_plhiv_3hp_2032, 
                        input$num_plhiv_2033*input$split_plhiv_3hp_2033)
    num_plhiv_1hp <- c(input$num_plhiv_2024*input$split_plhiv_1hp_2024, 
                        input$num_plhiv_2025*input$split_plhiv_1hp_2025, 
                        input$num_plhiv_2026*input$split_plhiv_1hp_2026, 
                        input$num_plhiv_2027*input$split_plhiv_1hp_2027, 
                        input$num_plhiv_2028*input$split_plhiv_1hp_2028,
                        input$num_plhiv_2029*input$split_plhiv_1hp_2029, 
                        input$num_plhiv_2030*input$split_plhiv_1hp_2030, 
                        input$num_plhiv_2031*input$split_plhiv_1hp_2031, 
                        input$num_plhiv_2032*input$split_plhiv_1hp_2032, 
                        input$num_plhiv_2033*input$split_plhiv_1hp_2033)
    num_plhiv_3hr <- c(input$num_plhiv_2024*input$split_plhiv_3hr_2024, 
                        input$num_plhiv_2025*input$split_plhiv_3hr_2025, 
                        input$num_plhiv_2026*input$split_plhiv_3hr_2026, 
                        input$num_plhiv_2027*input$split_plhiv_3hr_2027, 
                        input$num_plhiv_2028*input$split_plhiv_3hr_2028,
                        input$num_plhiv_2029*input$split_plhiv_3hr_2029, 
                        input$num_plhiv_2030*input$split_plhiv_3hr_2030, 
                        input$num_plhiv_2031*input$split_plhiv_3hr_2031, 
                        input$num_plhiv_2032*input$split_plhiv_3hr_2032, 
                        input$num_plhiv_2033*input$split_plhiv_3hr_2033)
    num_plhiv_6h <- c(input$num_plhiv_2024*input$split_plhiv_6h_2024, 
                        input$num_plhiv_2025*input$split_plhiv_6h_2025, 
                        input$num_plhiv_2026*input$split_plhiv_6h_2026, 
                        input$num_plhiv_2027*input$split_plhiv_6h_2027, 
                        input$num_plhiv_2028*input$split_plhiv_6h_2028,
                        input$num_plhiv_2029*input$split_plhiv_6h_2029, 
                        input$num_plhiv_2030*input$split_plhiv_6h_2030, 
                        input$num_plhiv_2031*input$split_plhiv_6h_2031, 
                        input$num_plhiv_2032*input$split_plhiv_6h_2032, 
                        input$num_plhiv_2033*input$split_plhiv_6h_2033)
    num_plhiv <- list("3hp"=num_plhiv_3hp,
                       "1hp"=num_plhiv_1hp,
                       "3hr"=num_plhiv_3hr,
                       "6h"=num_plhiv_6h)
    plhiv_out <- run_model_plhiv(input$country, num_plhiv, scenarios, plhiv_params, pop_calcs, option_split)
    child_params <- c(child_params, other_params, cost_params, 
                      "p_die"=p_child_die[[country_code]], 
                      "p_notif"=p_notif_child[[country_code]], 
                      "p_success"=p_success_child[[country_code]],
                      "life_exp"=life_exp_child[[country_code]], 
                      "yrs_new"=2, "yrs"=10)
    adol_params <- c(adol_params, other_params, cost_params, 
                     "p_die"=p_adol_die[[country_code]], 
                     "p_notif"=p_notif_adol[[country_code]], 
                     "p_notif_1524"=p_notif_1524[[country_code]], 
                     "p_success"=p_success_child[[country_code]],
                     "life_exp"=life_exp_adol[[country_code]], 
                     "yrs_new"=2, "yrs"=10)
    adult_params <- c(adult_params, other_params, cost_params, 
                      "p_die"=p_adult_die[[country_code]], 
                      "p_notif"=p_notif_adult[[country_code]],
                      "p_success"=p_success_adult[[country_code]], 
                      "life_exp"=life_exp_adult[[country_code]],
                      "yrs_new"=2, "yrs"=10)
    covg_child_3hp <- c(input$num_child_2024*input$split_child_3hp_2024, 
                        input$num_child_2025*input$split_child_3hp_2025, 
                        input$num_child_2026*input$split_child_3hp_2026,
                        input$num_child_2027*input$split_child_3hp_2027,
                        input$num_child_2028*input$split_child_3hp_2028,
                        input$num_child_2029*input$split_child_3hp_2029,
                        input$num_child_2030*input$split_child_3hp_2030,
                        input$num_child_2031*input$split_child_3hp_2031, 
                        input$num_child_2032*input$split_child_3hp_2032,
                        input$num_child_2033*input$split_child_3hp_2033)
    covg_child_3hp <- covg_child_3hp/(pop_calcs$tb_notif_new * pop_calcs$hh_child)
    covg_child_1hp <- c(input$num_child_2024*input$split_child_1hp_2024, 
                        input$num_child_2025*input$split_child_1hp_2025, 
                        input$num_child_2026*input$split_child_1hp_2026,
                        input$num_child_2027*input$split_child_1hp_2027,
                        input$num_child_2028*input$split_child_1hp_2028,
                        input$num_child_2029*input$split_child_1hp_2029,
                        input$num_child_2030*input$split_child_1hp_2030,
                        input$num_child_2031*input$split_child_1hp_2031, 
                        input$num_child_2032*input$split_child_1hp_2032,
                        input$num_child_2033*input$split_child_1hp_2033)
    covg_child_1hp <- covg_child_1hp/(pop_calcs$tb_notif_new * pop_calcs$hh_child)
    covg_child_3hr <- c(input$num_child_2024*input$split_child_3hr_2024, 
                        input$num_child_2025*input$split_child_3hr_2025, 
                        input$num_child_2026*input$split_child_3hr_2026,
                        input$num_child_2027*input$split_child_3hr_2027,
                        input$num_child_2028*input$split_child_3hr_2028,
                        input$num_child_2029*input$split_child_3hr_2029,
                        input$num_child_2030*input$split_child_3hr_2030,
                        input$num_child_2031*input$split_child_3hr_2031, 
                        input$num_child_2032*input$split_child_3hr_2032,
                        input$num_child_2033*input$split_child_3hr_2033)
    covg_child_3hr <- covg_child_3hr/(pop_calcs$tb_notif_new * pop_calcs$hh_child)
    covg_child_6h <- c(input$num_child_2024*input$split_child_6h_2024, 
                        input$num_child_2025*input$split_child_6h_2025, 
                        input$num_child_2026*input$split_child_6h_2026,
                        input$num_child_2027*input$split_child_6h_2027,
                        input$num_child_2028*input$split_child_6h_2028,
                        input$num_child_2029*input$split_child_6h_2029,
                        input$num_child_2030*input$split_child_6h_2030,
                        input$num_child_2031*input$split_child_6h_2031, 
                        input$num_child_2032*input$split_child_6h_2032,
                        input$num_child_2033*input$split_child_6h_2033)
    covg_child_6h <- covg_child_6h/(pop_calcs$tb_notif_new * pop_calcs$hh_child)
    covg_child_none <- c(input$num_child_2024*input$split_child_none_2024, 
                        input$num_child_2025*input$split_child_none_2025, 
                        input$num_child_2026*input$split_child_none_2026,
                        input$num_child_2027*input$split_child_none_2027,
                        input$num_child_2028*input$split_child_none_2028,
                        input$num_child_2029*input$split_child_none_2029,
                        input$num_child_2030*input$split_child_none_2030,
                        input$num_child_2031*input$split_child_none_2031, 
                        input$num_child_2032*input$split_child_none_2032,
                        input$num_child_2033*input$split_child_none_2033)
    covg_child_none <- covg_child_none/(pop_calcs$tb_notif_new * pop_calcs$hh_child)
    covg_child <- data.frame("year"=2024:2033,
                             "child_3hp"=covg_child_3hp,
                             "child_1hp"=covg_child_1hp,
                             "child_3hr"=covg_child_3hr,
                             "child_ipt"=covg_child_6h,
                             "child_none"=covg_child_none)
    covg_adol_3hp <- c(input$num_adol_2024*input$split_adol_3hp_2024, 
                        input$num_adol_2025*input$split_adol_3hp_2025, 
                        input$num_adol_2026*input$split_adol_3hp_2026,
                        input$num_adol_2027*input$split_adol_3hp_2027,
                        input$num_adol_2028*input$split_adol_3hp_2028,
                        input$num_adol_2029*input$split_adol_3hp_2029,
                        input$num_adol_2030*input$split_adol_3hp_2030,
                        input$num_adol_2031*input$split_adol_3hp_2031, 
                        input$num_adol_2032*input$split_adol_3hp_2032,
                        input$num_adol_2033*input$split_adol_3hp_2033)
    covg_adol_3hp <- covg_adol_3hp/(pop_calcs$tb_notif_new * pop_calcs$hh_adol)
    covg_adol_1hp <- c(input$num_adol_2024*input$split_adol_1hp_2024, 
                        input$num_adol_2025*input$split_adol_1hp_2025, 
                        input$num_adol_2026*input$split_adol_1hp_2026,
                        input$num_adol_2027*input$split_adol_1hp_2027,
                        input$num_adol_2028*input$split_adol_1hp_2028,
                        input$num_adol_2029*input$split_adol_1hp_2029,
                        input$num_adol_2030*input$split_adol_1hp_2030,
                        input$num_adol_2031*input$split_adol_1hp_2031, 
                        input$num_adol_2032*input$split_adol_1hp_2032,
                        input$num_adol_2033*input$split_adol_1hp_2033)
    covg_adol_1hp <- covg_adol_1hp/(pop_calcs$tb_notif_new * pop_calcs$hh_adol)
    covg_adol_3hr <- c(input$num_adol_2024*input$split_adol_3hr_2024, 
                        input$num_adol_2025*input$split_adol_3hr_2025, 
                        input$num_adol_2026*input$split_adol_3hr_2026,
                        input$num_adol_2027*input$split_adol_3hr_2027,
                        input$num_adol_2028*input$split_adol_3hr_2028,
                        input$num_adol_2029*input$split_adol_3hr_2029,
                        input$num_adol_2030*input$split_adol_3hr_2030,
                        input$num_adol_2031*input$split_adol_3hr_2031, 
                        input$num_adol_2032*input$split_adol_3hr_2032,
                        input$num_adol_2033*input$split_adol_3hr_2033)
    covg_adol_3hr <- covg_adol_3hr/(pop_calcs$tb_notif_new * pop_calcs$hh_adol)
    covg_adol_6h <- c(input$num_adol_2024*input$split_adol_6h_2024, 
                       input$num_adol_2025*input$split_adol_6h_2025, 
                       input$num_adol_2026*input$split_adol_6h_2026,
                       input$num_adol_2027*input$split_adol_6h_2027,
                       input$num_adol_2028*input$split_adol_6h_2028,
                       input$num_adol_2029*input$split_adol_6h_2029,
                       input$num_adol_2030*input$split_adol_6h_2030,
                       input$num_adol_2031*input$split_adol_6h_2031, 
                       input$num_adol_2032*input$split_adol_6h_2032,
                       input$num_adol_2033*input$split_adol_6h_2033)
    covg_adol_6h <- covg_adol_6h/(pop_calcs$tb_notif_new * pop_calcs$hh_adol)
    covg_adol_none <- c(input$num_adol_2024*input$split_adol_none_2024, 
                         input$num_adol_2025*input$split_adol_none_2025, 
                         input$num_adol_2026*input$split_adol_none_2026,
                         input$num_adol_2027*input$split_adol_none_2027,
                         input$num_adol_2028*input$split_adol_none_2028,
                         input$num_adol_2029*input$split_adol_none_2029,
                         input$num_adol_2030*input$split_adol_none_2030,
                         input$num_adol_2031*input$split_adol_none_2031, 
                         input$num_adol_2032*input$split_adol_none_2032,
                         input$num_adol_2033*input$split_adol_none_2033)
    covg_adol_none <- covg_adol_none/(pop_calcs$tb_notif_new * pop_calcs$hh_adol)
    covg_adol <- data.frame("year"=2024:2033,
                             "adol_3hp"=covg_adol_3hp,
                            "adol_3hr"=covg_adol_3hr,
                             "adol_1hp"=covg_adol_1hp,
                             "adol_ipt"=covg_adol_6h,
                             "adol_none"=covg_adol_none)
    covg_adult_3hp <- c(input$num_adult_2024*input$split_adult_3hp_2024, 
                        input$num_adult_2025*input$split_adult_3hp_2025, 
                        input$num_adult_2026*input$split_adult_3hp_2026,
                        input$num_adult_2027*input$split_adult_3hp_2027,
                        input$num_adult_2028*input$split_adult_3hp_2028,
                        input$num_adult_2029*input$split_adult_3hp_2029,
                        input$num_adult_2030*input$split_adult_3hp_2030,
                        input$num_adult_2031*input$split_adult_3hp_2031, 
                        input$num_adult_2032*input$split_adult_3hp_2032,
                        input$num_adult_2033*input$split_adult_3hp_2033)
    covg_adult_3hp <- covg_adult_3hp/(pop_calcs$tb_notif_new * pop_calcs$hh_adult)
    covg_adult_1hp <- c(input$num_adult_2024*input$split_adult_1hp_2024, 
                        input$num_adult_2025*input$split_adult_1hp_2025, 
                        input$num_adult_2026*input$split_adult_1hp_2026,
                        input$num_adult_2027*input$split_adult_1hp_2027,
                        input$num_adult_2028*input$split_adult_1hp_2028,
                        input$num_adult_2029*input$split_adult_1hp_2029,
                        input$num_adult_2030*input$split_adult_1hp_2030,
                        input$num_adult_2031*input$split_adult_1hp_2031, 
                        input$num_adult_2032*input$split_adult_1hp_2032,
                        input$num_adult_2033*input$split_adult_1hp_2033)
    covg_adult_1hp <- covg_adult_1hp/(pop_calcs$tb_notif_new * pop_calcs$hh_adult)
    covg_adult_3hr <- c(input$num_adult_2024*input$split_adult_3hr_2024, 
                        input$num_adult_2025*input$split_adult_3hr_2025, 
                        input$num_adult_2026*input$split_adult_3hr_2026,
                        input$num_adult_2027*input$split_adult_3hr_2027,
                        input$num_adult_2028*input$split_adult_3hr_2028,
                        input$num_adult_2029*input$split_adult_3hr_2029,
                        input$num_adult_2030*input$split_adult_3hr_2030,
                        input$num_adult_2031*input$split_adult_3hr_2031, 
                        input$num_adult_2032*input$split_adult_3hr_2032,
                        input$num_adult_2033*input$split_adult_3hr_2033)
    covg_adult_3hr <- covg_adult_3hr/(pop_calcs$tb_notif_new * pop_calcs$hh_adult)
    covg_adult_6h <- c(input$num_adult_2024*input$split_adult_6h_2024, 
                       input$num_adult_2025*input$split_adult_6h_2025, 
                       input$num_adult_2026*input$split_adult_6h_2026,
                       input$num_adult_2027*input$split_adult_6h_2027,
                       input$num_adult_2028*input$split_adult_6h_2028,
                       input$num_adult_2029*input$split_adult_6h_2029,
                       input$num_adult_2030*input$split_adult_6h_2030,
                       input$num_adult_2031*input$split_adult_6h_2031, 
                       input$num_adult_2032*input$split_adult_6h_2032,
                       input$num_adult_2033*input$split_adult_6h_2033)
    covg_adult_6h <- covg_adult_6h/(pop_calcs$tb_notif_new * pop_calcs$hh_adult)
    covg_adult_none <- c(input$num_adult_2024*input$split_adult_none_2024, 
                         input$num_adult_2025*input$split_adult_none_2025, 
                         input$num_adult_2026*input$split_adult_none_2026,
                         input$num_adult_2027*input$split_adult_none_2027,
                         input$num_adult_2028*input$split_adult_none_2028,
                         input$num_adult_2029*input$split_adult_none_2029,
                         input$num_adult_2030*input$split_adult_none_2030,
                         input$num_adult_2031*input$split_adult_none_2031, 
                         input$num_adult_2032*input$split_adult_none_2032,
                         input$num_adult_2033*input$split_adult_none_2033)
    covg_adult_none <- covg_adult_none/(pop_calcs$tb_notif_new * pop_calcs$hh_adult)
    covg_adult <- data.frame("year"=2024:2033,
                             "adult_3hp"=covg_adult_3hp,
                             "adult_1hp"=covg_adult_1hp,
                             "adult_3hr"=covg_adult_3hr,
                             "adult_ipt"=covg_adult_6h,
                             "adult_none"=covg_adult_none)
    contacts_output <- run_model_contacts(input$country, NULL, NULL, NULL,
                                          covg_child, covg_adol, covg_adult, scenarios, NULL,
                                          child_params, adol_params, adult_params, pop_calcs)
    child_output <- contacts_output$child
    adol_output <- contacts_output$adol
    adult_output <- contacts_output$adult
    plhiv_output <- plhiv_out$plhiv
    child_flag <- contacts_output$child_flag
    adol_flag <- contacts_output$adol_flag
    adult_flag <- contacts_output$adult_flag
    plhiv_flag <- plhiv_out$plhiv_flag
    print(names(plhiv_out))
    print(plhiv_flag)
    out_sum <- bind_rows(plhiv_output %>% ungroup() %>% 
                              mutate(pop="PLHIV",
                                     year=as.integer(year)) %>%
                              select(country, pop, year, scenario, 
                                     starts_with("cost")),
                            child_output %>% ungroup() %>% 
                              mutate(pop="Contacts < 5",
                                     year=as.integer(year)) %>%
                              select(country, pop, year, scenario, 
                                     starts_with("cost")),
                            adol_output %>% ungroup() %>% 
                              mutate(pop="Contacts 5-14",
                                     year=as.integer(year)) %>%
                              select(country, pop, year, scenario, 
                                     starts_with("cost")),
                            adult_output %>% ungroup() %>% 
                              mutate(pop="Contacts 15+",
                                     year=as.integer(year)) %>%
                              select(country, pop, year, scenario, 
                                     starts_with("cost")))
    out_sum <- out_sum %>% arrange(pop, year, scenario) %>%
      group_by(pop, scenario) %>%
      mutate(cum_costs=cumsum(costs)) %>%
      ungroup() %>% group_by(pop, year) %>%
      mutate(inc_cost=if_else(scenario==scenarios[[2]], 0,
                              costs[scenario==scenarios[[1]]] - costs[scenario==scenarios[[2]]]))
    list("plhiv_output"=plhiv_output, 
         "child_output"=child_output, 
         "adol_output"=adol_output, 
         "adult_output"=adult_output, 
         "plhiv_flag"=plhiv_flag,
         "child_flag"=child_flag,
         "adol_flag"=adol_flag,
         "adult_flag"=adult_flag,
         "out_sum"=out_sum)
    
  }, ignoreNULL=FALSE)
  observe({
    if(rv()$child_flag>1 & rv()$adol_flag<=1 & rv()$adult_flag<=1 & rv()$plhiv_flag<=1) {
      shinyalert("Warning", "Number of contacts < 5 years receiving TPT exceeds the estimated number of eligible contacts < 5 in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts < 5 years. See the <b> Other Options </b> tab to update annual notifications and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag<=1 & rv()$adol_flag>1 & rv()$adult_flag<=1 & rv()$plhiv_flag<=1) {
      shinyalert("Warning", "Number of contacts 5-14 years receiving TPT exceeds the estimated number of eligible contacts aged 5-14 in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts 5-14 years. See the <b> Other Options </b> tab to update annual notifications and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag<=1 & rv()$adol_flag<=1 & rv()$adult_flag>1 & rv()$plhiv_flag<=1) {
      shinyalert("Warning", "Number of contacts aged 15 years and above receiving TPT exceeds the estimated number of eligible contacts aged 15 and above in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts 15 and above. See the <b> Other Options </b> tab to update annual notifications and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag>1 & rv()$adol_flag>1 & rv()$adult_flag<=1 & rv()$plhiv_flag<=1) {
      shinyalert("Warning", "Number of contacts < 5 years and 5-14 years receiving TPT exceeds the estimated number of eligible contacts aged < 5 and 5-14 in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts of both age groups. See the <b> Other Options </b> tab to update annual notifications and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag>1 & rv()$adol_flag<=1 & rv()$adult_flag>1 & rv()$plhiv_flag<=1) {
      shinyalert("Warning", "Number of contacts < 5 years and 15 years and older receiving TPT exceeds the estimated number of eligible contacts aged < 5 and 15+ in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts of both age groups. See the <b> Other Options </b> tab to update annual notifications and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag<=1 & rv()$adol_flag>1 & rv()$adult_flag>1 & rv()$plhiv_flag<=1) {
      shinyalert("Warning", "Number of contacts 5-14 years and 15 years and older receiving TPT exceeds the estimated number of eligible contacts aged 5-14 and 15+ in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts of both age groups. See the <b> Other Options </b> tab to update annual notifications and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag>1 & rv()$adol_flag>1 & rv()$adult_flag>1 & rv()$plhiv_flag<=1) {
      shinyalert("Warning", "For all 3 age groups, number of contacts receiving TPT exceeds the estimated number of eligible contacts in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts of all age groups. See the <b> Other Options </b> tab to update annual notifications and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag>1 & rv()$adol_flag<=1 & rv()$adult_flag<=1 & rv()$plhiv_flag>1) {
      shinyalert("Warning", "Number of contacts < 5 years and PLHIV receiving TPT exceeds the estimated number of eligible contacts < 5 and PLHIV in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts < 5 years and PLHIV. See the <b> Other Options </b> tab to update annual notifications, number of PLHIV, and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag<=1 & rv()$adol_flag>1 & rv()$adult_flag<=1 & rv()$plhiv_flag>1) {
      shinyalert("Warning", "Number of contacts 5-14 years and PLHIV receiving TPT exceeds the estimated number of eligible contacts aged 5-14 and PLHIV in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts 5-14 years and PLHIV. See the <b> Other Options </b> tab to update annual notifications, number of PLHIV, and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag<=1 & rv()$adol_flag<=1 & rv()$adult_flag>1 & rv()$plhiv_flag>1) {
      shinyalert("Warning", "Number of contacts aged 15 years and above and number of PLHIV receiving TPT exceeds the estimated number of eligible contacts aged 15 and above and PLHIV in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts 15 and above and PLHIV. See the <b> Other Options </b> tab to update annual notifications, number of PLHIV, and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag>1 & rv()$adol_flag>1 & rv()$adult_flag<=1 & rv()$plhiv_flag>1) {
      shinyalert("Warning", "Number of contacts < 5 years and 5-14 years and number of PLHIV receiving TPT exceeds the estimated number of eligible contacts aged < 5 and 5-14 and number of eligible PLHIV in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts of both age groups and PLHIV. See the <b> Other Options </b> tab to update annual notifications, number of PLHIV, and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag>1 & rv()$adol_flag<=1 & rv()$adult_flag>1 & rv()$plhiv_flag>1) {
      shinyalert("Warning", "Number of contacts < 5 years and 15 years and older and number of PLHIV receiving TPT exceeds the estimated number of eligible contacts aged < 5 and 15+ and number of eligible PLHIV in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts of both age groups and PLHIV. See the <b> Other Options </b> tab to update annual notifications, number of PLHIV, and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag<=1 & rv()$adol_flag>1 & rv()$adult_flag>1 & rv()$plhiv_flag>1) {
      shinyalert("Warning", "Number of contacts 5-14 years and 15 years and older and number of PLHIV receiving TPT exceeds the estimated number of eligible contacts aged 5-14 and 15+ and number of eligible PLHIV in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts of both age groups and PLHIV. See the <b> Other Options </b> tab to update annual notifications, number of PLHIV, and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag>1 & rv()$adol_flag>1 & rv()$adult_flag>1 & rv()$plhiv_flag>1) {
      shinyalert("Warning", "For all 3 age groups and PLHIV, number of contacts and PLHIV receiving TPT exceeds the estimated number of eligible contacts and PLHIV in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for contacts of all age groups and PLHIV. See the <b> Other Options </b> tab to update annual notifications, number of PLHIV, and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    } else if(rv()$child_flag<=1 & rv()$adol_flag<=1 & rv()$adult_flag<=1 & rv()$plhiv_flag>1) {
      shinyalert("Warning", "Number of PLHIV receiving TPT exceeds the estimated number of eligible PLHIV in the country that would accept TPT in at least 1 year. The model has been run with 100% coverage for PLHIV. See the <b> Other Options </b> tab to update numbers of PLHIV and the TPT acceptance probability.", 
                 type="error", size="xs", html=TRUE)
    }
  })
  output$table <- renderTable({rv()[["out_sum"]]})
  output$downloadData <- downloadHandler(
    filename=function() {
      paste0("tpt_costs_", Sys.Date(), ".csv")
    }, 
    content=function(file) {
      write.csv(rv()[["out_sum"]], file, row.names=F)
    })
  output$plhiv_costs_sum <- renderPlotly({
    plot_ly(rv()$plhiv_output,
            x=~year,
            y=~round(costs-cost_art, -4),
            type='bar',
            color=~scenario,
            colors=c("#04a3bd","#f0be3d")) %>%
      layout(title=list(text="<b> Total TB-Related Costs, PLHIV </b>", x=0.1),
             yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             legend=list(x=-0.1, y=-0.25),
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$child_costs_sum <- renderPlotly({
    plot_ly(rv()$child_output,
            x=~year,
            y=~round(costs,-4),
            type='bar',
            color=~scenario,
            colors=c("#04a3bd","#f0be3d")) %>%
      layout(title=list(text="<b> Total Costs, Contacts < 5 </b>", x=0.1),
             yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             legend=list(x=-0.1, y=-0.25),
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$adol_costs_sum <- renderPlotly({
    plot_ly(rv()$adol_output,
            x=~year,
            y=~round(costs,-4),
            type='bar',
            color=~scenario,
            colors=c("#04a3bd","#f0be3d")) %>%
      layout(title=list(text="<b> Total Costs, Contacts 5-14 </b>", x=0.1),
             yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             legend=list(x=-0.1, y=-0.25),
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$adult_costs_sum <- renderPlotly({
    plot_ly(rv()$adult_output,
            x=~year,
            y=~round(costs,-4),
            type='bar',
            color=~scenario,
            colors=c("#04a3bd","#f0be3d")) %>%
      layout(title=list(text="<b> Total Costs, Contacts 15+ </b>", x=0.1),
             yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             legend=list(x=-0.1, y=-0.25),
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$plhiv_initiate <- renderPlotly({
    plot_ly(rv()$plhiv_output,
            x=~year,
            y=~round(initiate_ipt+initiate_3hp+initiate_1hp+initiate_3hr, -4), 
            type='bar',
            color=~scenario,
            colors=c("#04a3bd","#f0be3d")) %>%
      layout(title=list(text="<b> Total PLHIV Initiating TPT </b>", x=0.1),
             yaxis=list(title="Number of People"),
             xaxis=list(title="Year"),
             legend=list(x=-0.1, y=-0.25),
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$child_initiate <- renderPlotly({
    plot_ly(rv()$child_output,
            x=~year,
            y=~round(initiate_ipt+initiate_3hp+initiate_1hp+initiate_3hr, -2), 
            type='bar',
            color=~scenario,
            colors=c("#04a3bd","#f0be3d")) %>%
      layout(title=list(text="<b> Total Contacts < 5 Initiating TPT </b>", x=0.1),
             yaxis=list(title="Number of People"),
             xaxis=list(title="Year"),
             legend=list(x=-0.1, y=-0.25),
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$adol_initiate <- renderPlotly({
    plot_ly(rv()$adol_output,
            x=~year,
            y=~round(initiate_ipt+initiate_3hp+initiate_1hp+initiate_3hr, -2), 
            type='bar',
            color=~scenario,
            colors=c("#04a3bd","#f0be3d")) %>%
      layout(title=list(text="<b> Total Contacts 5-14 Initiating TPT </b>", x=0.1),
             yaxis=list(title="Number of People"),
             xaxis=list(title="Year"),
             legend=list(x=-0.1, y=-0.25),
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$adult_initiate <- renderPlotly({
    plot_ly(rv()$adult_output,
            x=~year,
            y=~round(initiate_ipt+initiate_3hp+initiate_1hp+initiate_3hr, -2), 
            type='bar',
            color=~scenario,
            colors=c("#04a3bd","#f0be3d")) %>%
      layout(title=list(text="<b> Total Contacts 15+ Initiating TPT </b>", x=0.1),
             yaxis=list(title="Number of People"),
             xaxis=list(title="Year"),
             legend=list(x=-0.1, y=-0.25),
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$plhiv_costs_tpt_detail <- renderPlotly({
    plot_ly(rv()$plhiv_output %>% filter(scenario==scenarios[[1]]),
            x=~year, y=~round(cost_tx,-4),
            type='bar', name=cost_colors["cost_tx", "labels"], 
            color=I(cost_colors["cost_tx", "colors"])) %>% 
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             title=list(text="<b> Detailed TB-Related Costs, Scenario with TPT </b>", x=0.1),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      hide_legend() %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$plhiv_costs_comp_detail <- renderPlotly({
    plot_ly(rv()$plhiv_output %>% filter(scenario==scenarios[[2]]),
            x=~year, y=~round(cost_tx,-4),
            type='bar', name=cost_colors["cost_tx", "labels"], 
            color=I(cost_colors["cost_tx", "colors"])) %>% 
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             title=list(text="<b> Detailed TB-Related Costs, Scenario without TPT </b>", x=0.1),
             legend=list(x=1, y=0.5, title=list(text="<b> Cost Categories </b>")),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$plhiv_costs_tpt_detail_art <- renderPlotly({
    plot_ly(rv()$plhiv_output %>% filter(scenario==scenarios[[1]]),
            x=~year, y=~round(cost_art,-4),
            type='bar', name=cost_colors["cost_art", "labels"], 
            color=I(cost_colors["cost_art", "colors"])) %>% 
      add_trace(y=~round(cost_tx,-4), name=cost_colors["cost_tx", "labels"], 
                color=I(cost_colors["cost_tx", "colors"])) %>%
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             title=list(text="<b> Detailed Total Costs, Scenario with TPT </b>", x=0.1),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      hide_legend() %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$plhiv_costs_comp_detail_art <- renderPlotly({
    plot_ly(rv()$plhiv_output %>% filter(scenario==scenarios[[2]]),
            x=~year, y=~round(cost_art,-4),
            type='bar', name=cost_colors["cost_art", "labels"], 
            color=I(cost_colors["cost_art", "colors"])) %>% 
      add_trace(y=~round(cost_tx,-4), name=cost_colors["cost_tx", "labels"], 
                color=I(cost_colors["cost_tx", "colors"])) %>%
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             legend=list(x=1, y=0.5, title=list(text="<b> Cost Categories </b>")),
             title=list(text="<b> Detailed Total Costs, Scenario without TPT </b>", x=0.1),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$child_costs_tpt_detail <- renderPlotly({
    plot_ly(rv()$child_output %>% filter(scenario==scenarios[[1]]),
            x=~year, y=~round(cost_tx,-4),
            type='bar', name=cost_colors["cost_tx", "labels"], 
            color=I(cost_colors["cost_tx", "colors"])) %>% 
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_contact,-4), name=cost_colors["cost_contact", "labels"], 
                color=I(cost_colors["cost_contact", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             title=list(text="<b> Contacts < 5 Costs, Scenario with TPT </b>", x=0.1),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      hide_legend() %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$child_costs_comp_detail <- renderPlotly({
    plot_ly(rv()$child_output %>% filter(scenario==scenarios[[2]]),
            x=~year, y=~round(cost_tx, -4),
            type='bar', name=cost_colors["cost_tx", "labels"], 
            color=I(cost_colors["cost_tx", "colors"])) %>% 
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_contact,-4), name=cost_colors["cost_contact", "labels"], 
                color=I(cost_colors["cost_contact", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             title=list(text="<b> Contacts < 5 Costs, Scenario without TPT </b>", x=0.1),
             legend=list(x=1, y=0.5, title=list(text="<b> Cost Categories </b>")),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$adol_costs_tpt_detail <- renderPlotly({
    plot_ly(rv()$adol_output %>% filter(scenario==scenarios[[1]]),
            x=~year, y=~round(cost_tx,-4),
            type='bar', name=cost_colors["cost_tx", "labels"], 
            color=I(cost_colors["cost_tx", "colors"])) %>% 
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_contact,-4), name=cost_colors["cost_contact", "labels"], 
                color=I(cost_colors["cost_contact", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             title=list(text="<b> Contacts 5-14 Costs, Scenario with TPT </b>", x=0.1),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      hide_legend() %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$adol_costs_comp_detail <- renderPlotly({
    plot_ly(rv()$adol_output %>% filter(scenario==scenarios[[2]]),
            x=~year, y=~round(cost_tx, -4),
            type='bar', name=cost_colors["cost_tx", "labels"], 
            color=I(cost_colors["cost_tx", "colors"])) %>% 
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_contact,-4), name=cost_colors["cost_contact", "labels"], 
                color=I(cost_colors["cost_contact", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             title=list(text="<b> Contacts 5-14 Costs, Scenario without TPT </b>", x=0.1),
             legend=list(x=1, y=0.5, title=list(text="<b> Cost Categories </b>")),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$adult_costs_tpt_detail <- renderPlotly({
    plot_ly(rv()$adult_output %>% filter(scenario==scenarios[[1]]),
            x=~year, y=~round(cost_tx,-4),
            type='bar', name=cost_colors["cost_tx", "labels"], 
            color=I(cost_colors["cost_tx", "colors"])) %>% 
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_contact,-4), name=cost_colors["cost_contact", "labels"], 
                color=I(cost_colors["cost_contact", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             title=list(text="<b> Contacts 15+ Costs, Scenario with TPT </b>", x=0.1),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      hide_legend() %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  output$adult_costs_comp_detail <- renderPlotly({
    plot_ly(rv()$adult_output %>% filter(scenario==scenarios[[2]]),
            x=~year, y=~round(cost_tx, -4),
            type='bar', name=cost_colors["cost_tx", "labels"], 
            color=I(cost_colors["cost_tx", "colors"])) %>% 
      add_trace(y=~round(cost_ipt,-4), name=cost_colors["cost_ipt", "labels"], 
                color=I(cost_colors["cost_ipt", "colors"])) %>%
      add_trace(y=~round(cost_3hp,-4), name=cost_colors["cost_3hp", "labels"], 
                color=I(cost_colors["cost_3hp", "colors"])) %>%
      add_trace(y=~round(cost_1hp,-4), name=cost_colors["cost_1hp", "labels"], 
                color=I(cost_colors["cost_1hp", "colors"])) %>%
      add_trace(y=~round(cost_3hr,-4), name=cost_colors["cost_3hr", "labels"], 
                color=I(cost_colors["cost_3hr", "colors"])) %>%
      add_trace(y=~round(cost_tox,-4), name=cost_colors["cost_tox", "labels"], 
                color=I(cost_colors["cost_tox", "colors"])) %>%
      add_trace(y=~round(cost_contact,-4), name=cost_colors["cost_contact", "labels"], 
                color=I(cost_colors["cost_contact", "colors"])) %>%
      add_trace(y=~round(cost_impl,-4), name=cost_colors["cost_impl", "labels"], 
                color=I(cost_colors["cost_impl", "colors"])) %>%
      layout(yaxis=list(title="Annual Costs (USD)"),
             xaxis=list(title="Year"),
             title=list(text="<b> Contacts 15+ Costs, Scenario without TPT </b>", x=0.1),
             legend=list(x=1, y=0.5, title=list(text="<b> Cost Categories </b>")),
             barmode="stack", bargap=0.3,
             modebar=list(orientation='v')) %>%
      config(displaylogo=F,
             modeBarButtonsToRemove=c('select2d', 'lasso2d',
                                      'autoScale2d', 'zoom2d',
                                      'zoomIn2d', 'zoomOut2d',
                                      'pan2d', 'resetScale2d'))
  })
  
}

shinyApp(ui=ui, server=server)

