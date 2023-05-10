#contact investigation-related params - children
p_hh_child_ltbi <- 0.355 #% of child contacts w/ LTBI
p_hh_child_symptom <- 0.345 #% of child contacts that have positive symptom screen and go for chest xray or Xpert (as TB tests)
p_hh_child_tb <- 0.10 #% of child contacts that have active TB
p_hh_child_tb_symptom <- 1 #100% of active TB among children is symptomatic (ie we only focus on symptomatic active TB among kids)
p_hh_child_asymptom_tb <- p_hh_child_tb*(1-p_hh_child_tb_symptom)/(1-p_hh_child_symptom) #% of asymptomatic children that have active TB
hh_child_cxr_sens <- 0.87 #CXR among child contacts is 87% sensitive
hh_child_cxr_spec <- 0.99 #CXR among child contacts is 99% specific
hh_child_xpert_sens <- 0.805 #Xpert among children (w/ pos symptom screen) is 80.5% sensitive
hh_child_xpert_spec <- 0.98 #Xpert among children (w/ pos symptom screen) is 98.2% specific
p_hh_child_cxr_tpt <- (p_hh_child_symptom - p_hh_child_tb*p_hh_child_tb_symptom)*hh_child_cxr_spec +
  (1 - p_hh_child_symptom)*(1-p_hh_child_asymptom_tb) + 
  p_hh_child_tb*(1-p_hh_child_tb_symptom*hh_child_cxr_sens) #% children eligible for TPT w/ CXR diagnosis

#contact investigation-related params - adolescents
p_hh_adol_ltbi <- p_hh_child_ltbi #% of adol contacts w/ LTBI
p_hh_adol_symptom_cxr <- 0.28 #% of adol contacts that have positive symptom screen or CXR and go for Xpert testing
p_hh_adol_tb <- 0.084 #% of adol contacts that have active TB
p_hh_adol_tb_symptom_cxr <- 1 #100% of active TB among adolescents is symptomatic or CXR+
p_hh_adol_asymptom_negcxr_tb <- p_hh_adol_tb*(1-p_hh_adol_tb_symptom_cxr)/(1-p_hh_adol_symptom_cxr) #% of asymptomatic and negative CXR adol that have active TB
hh_adol_xpert_sens <- 0.805 #Xpert among children (w/ pos symptom screen) is 80.5% sensitive
hh_adol_xpert_spec <- 0.98 #Xpert among children (w/ pos symptom screen) is 98.2% specific
p_hh_adol_symptom_cxr_tpt <- (p_hh_adol_symptom_cxr - p_hh_adol_tb*p_hh_adol_tb_symptom_cxr)*hh_adol_xpert_spec +
  (1 - p_hh_adol_symptom_cxr)*(1-p_hh_adol_asymptom_negcxr_tb) + 
  p_hh_adol_tb*(1-p_hh_adol_tb_symptom_cxr*hh_adol_xpert_sens) #% adol eligible for TPT w/ symptom & CXR as screening
#version with no CXR screen - NOT CURRENTLY USING
p_hh_adol_symptom <- 0.17 #NOT USING % of adol contacts that have positive symptom screen
p_hh_adol_tb_symptom <- 0.60 #NOT USING % of active TB among adolescents is symptomatic
p_hh_adol_asymptom_tb <- p_hh_adol_tb*(1-p_hh_adol_tb_symptom)/(1-p_hh_adol_symptom) #NOT USING % of asymptomatic adol that have active TB
p_hh_adol_symptom_tpt <- (p_hh_adol_symptom - p_hh_adol_tb*p_hh_adol_tb_symptom)*hh_adol_xpert_spec +
  (1 - p_hh_adol_symptom)*(1-p_hh_adol_asymptom_tb) + 
  p_hh_adol_tb*(1-p_hh_adol_tb_symptom*hh_adol_xpert_sens) #% adol eligible for TPT w/ symptom screening only

#contact investigation-related params - adults
p_hh_adult_ltbi <- p_hh_child_ltbi #% of adult contacts w/ LTBI
p_hh_adult_symptom_cxr <- 0.181 #% of adult contacts have positive symptom screen or CXR (and go for Xpert testing)
p_hh_adult_tb <- 0.032 #% of adult contacts that have active TB
p_hh_adult_tb_symptom_cxr <- 1 #100% of active TB among adults is symptomatic or CXR+
p_hh_adult_asymptom_negcxr_tb <- p_hh_adult_tb*(1-p_hh_adult_tb_symptom_cxr)/(1-p_hh_adult_symptom_cxr) #% of asymptomatic adults that have active TB
hh_adult_xpert_sens <- 0.91 #Xpert among suspected adults (w/ pos symptom screen) is 89% sensitive
hh_adult_xpert_spec <- 0.98 #Xpert among suspected adults (w/ pos symptom screen) is 99% specific
p_hh_adult_symptom_cxr_tpt <- (p_hh_adult_symptom_cxr - p_hh_adult_tb*p_hh_adult_tb_symptom_cxr)*hh_adult_xpert_spec +
  (1 - p_hh_adult_symptom_cxr)*(1-p_hh_adult_asymptom_negcxr_tb) + 
  p_hh_adult_tb*(1-p_hh_adult_tb_symptom_cxr*hh_adult_xpert_sens) #% adult eligible for TPT w/ symptom & CXR as screening
#version with no CXR screen - NOT CURRENTLY USING
p_hh_adult_symptom <- 0.064 #% of adult contacts that have positive symptom screen (and go for Xpert testing)
p_hh_adult_tb_symptom <- 0.5 #% of active TB among adults that is symptomatic
p_hh_adult_asymptom_tb <- p_hh_adult_tb*(1-p_hh_adult_tb_symptom)/(1-p_hh_adult_symptom) #% of asymptomatic adults that have active TB
p_hh_adult_symptom_tpt <- (p_hh_adult_symptom - p_hh_adult_tb*p_hh_adult_tb_symptom)*hh_adult_xpert_spec +
  (1 - p_hh_adult_symptom)*(1-p_hh_adult_asymptom_tb) + 
  p_hh_adult_tb*(1-p_hh_adult_tb_symptom*hh_adult_xpert_sens) #% adult eligible for TPT w/ symptom screening only

#TPT-related params - TPT initiation
p_hh_child_initiate <- 0.735 #% of < 5 contacts investigated that initiate TPT
p_hh_adol_initiate <- p_hh_child_initiate #% of 5-14 contacts investigated that initiate TPT
p_hh_adult_initiate <- p_hh_child_initiate #% of 15+ contacts investigated that initiate TPT
p_plhiv_initiate <- p_hh_child_initiate #doesn't matter for CEA, but just have similar scales of coverage

#TPT completion
complete_diff_1hp <- 0.1 #assume 10 % pts higher completion on 1HP than 3HP
complete_diff_ipt <- -0.05 #assume 5 % pts lower completion on IPT than 3HP
p_child_complete_3hp <- 0.85 #probability of completing 3HP, conditional on initiating
p_child_complete_ipt <- p_child_complete_3hp + complete_diff_ipt
p_child_complete_1hp <- p_child_complete_3hp + complete_diff_1hp
p_adol_complete_3hp <- p_child_complete_3hp
p_adol_complete_ipt <- p_adol_complete_3hp + complete_diff_ipt
p_adol_complete_1hp <- p_adol_complete_3hp + complete_diff_1hp
p_adult_complete_3hp <- p_child_complete_3hp 
p_adult_complete_ipt <- p_adult_complete_3hp + complete_diff_ipt
p_adult_complete_1hp <- p_adult_complete_3hp + complete_diff_1hp
p_plhiv_complete_3hp <- p_child_complete_3hp
p_plhiv_complete_ipt <- p_plhiv_complete_3hp + complete_diff_ipt 
p_plhiv_complete_1hp <- p_plhiv_complete_3hp + complete_diff_1hp

#TPT efficacy
#set equal across regimens and target population groups
eff_child_ipt <- 0.87
eff_child_3hp <- eff_child_ipt
eff_child_1hp <- eff_child_3hp
eff_adol_ipt <- eff_child_ipt
eff_adol_3hp <- eff_child_3hp
eff_adol_1hp <- eff_adol_3hp
eff_adult_ipt <- eff_child_ipt
eff_adult_3hp <- eff_child_3hp
eff_adult_1hp <- eff_adult_3hp
eff_plhiv_ipt <- eff_child_ipt
eff_plhiv_3hp <- eff_child_3hp
eff_plhiv_1hp <- eff_plhiv_3hp

#hepatotoxicity-related params - children
p_child_tox_nohosp_ipt <- 0.002 #% experiencing adverse events that don't require hospitalization
p_child_tox_hosp_ipt <- 0.002 #% initiating TPT that experience hepatotoxicity and require hospitalization
p_child_tox_nohosp_3hp <- 0.015  #% experiencing adverse events that don't require hospitalization
p_child_tox_hosp_3hp <- 0.002 #% initiating TPT that experience hepatotoxicity and require hospitalization
p_child_tox_nohosp_1hp <- p_child_tox_nohosp_3hp #% experiencing adverse events that don't require hospitalization
p_child_tox_hosp_1hp <- p_child_tox_hosp_3hp #% initiating TPT that experience hepatotoxicity and require hospitalization

#hepatotoxicity-related params - adolescents
p_adol_tox_nohosp_ipt <- p_child_tox_nohosp_ipt #% experiencing hepatotoxicity that don't require hospitalization
p_adol_tox_hosp_ipt <- p_child_tox_hosp_ipt #% initiating TPT that experience hepatotoxicity and require hospitalization
p_adol_tox_nohosp_3hp <- p_child_tox_nohosp_3hp #% experiencing hepatotoxicity that don't require hospitalization
p_adol_tox_hosp_3hp <- p_child_tox_hosp_3hp #% initiating TPT that experience hepatotoxicity and require hospitalization
p_adol_tox_nohosp_1hp <- p_child_tox_nohosp_1hp #% experiencing hepatotoxicity that don't require hospitalization
p_adol_tox_hosp_1hp <- p_child_tox_hosp_1hp #% initiating TPT that experience hepatotoxicity and require hospitalization

#hepatotoxicity-related params - adults
p_adult_tox_nohosp_ipt <- 0.035 #% experiencing hepatotoxicity that don't require hospitalization
p_adult_tox_hosp_ipt <- 0.019 #% initiating TPT that experience hepatotoxicity and require hospitalization
p_adult_tox_nohosp_3hp <- 0.051 #% experiencing hepatotoxicity that don't require hospitalization
p_adult_tox_hosp_3hp <- 0.005 #% initiating TPT that experience hepatotoxicity and require hospitalization
p_adult_tox_nohosp_1hp <- p_adult_tox_nohosp_3hp #% experiencing hepatotoxicity that don't require hospitalization
p_adult_tox_hosp_1hp <- p_adult_tox_hosp_3hp #% initiating TPT that experience hepatotoxicity and require hospitalization

#hepatotoxicity-related params - PLHIV
p_plhiv_tox_nohosp_ipt <- 0.043 #% of PLHIV initiating TPT that experience hepatotoxicity but don't require hospitalization
p_plhiv_tox_hosp_ipt <- 0.043 #% of PLHIV initiating TPT that require hospitalization due to hepatotoxicity
p_plhiv_tox_nohosp_3hp <- p_adult_tox_nohosp_3hp #% of PLHIV initiating TPT that experience hepatotoxicity but don't require hospitalization
p_plhiv_tox_hosp_3hp <- 0.02 #% of PLHIV initiating TPT that require hospitalization due to hepatotoxicity
p_plhiv_tox_nohosp_1hp <- p_plhiv_tox_nohosp_3hp #% of PLHIV initiating TPT that experience hepatotoxicity but don't require hospitalization
p_plhiv_tox_hosp_1hp <- p_plhiv_tox_hosp_3hp #% of PLHIV initiating TPT that require hospitalization due to hepatotoxicity

#reactivation risk over time - child contacts
p_child_reactivate_02 <- 0.1 #cumulative 19% probability (within 2 years - e.g. year 0 and year 1) converted to annual probability
p_child_reactivate_25 <- 0.002 #0.2% annual probability during 2nd, 3rd, 4th years
p_child_reactivate_510 <- 0.001 #0.1% annual probability from year 5-year 10
p_child_reactivate_10plus <- 0.0008 #0.08% probability from year 10 on

#reactivation risk over time - adolescent contacts
p_adol_reactivate_02 <- 0.05 #cumulative 9.9% probability (within 2 years - e.g. year 0 and year 1) converted to annual probability
p_adol_reactivate_25 <- p_child_reactivate_25
p_adol_reactivate_510 <- p_child_reactivate_510
p_adol_reactivate_10plus <- p_child_reactivate_10plus

#reactivation risk over time - adult contacts
p_adult_reactivate_02 <- p_adol_reactivate_02
p_adult_reactivate_25 <- p_adol_reactivate_25
p_adult_reactivate_510 <- p_adol_reactivate_510
p_adult_reactivate_10plus <- p_adol_reactivate_10plus

#reactivation risk over time - PLHIV
p_plhiv_reactivate_new01 <- 0.074
p_plhiv_reactivate_new12 <- 0.033
p_plhiv_reactivate_new39 <- 0.007
p_plhiv_reactivate_new10plus <- 0.0033
p_plhiv_reactivate_est01 <- 0.040
p_plhiv_reactivate_est12 <- 0.018
p_plhiv_reactivate_est39 <- p_plhiv_reactivate_new39
p_plhiv_reactivate_est10plus <- p_plhiv_reactivate_new10plus
p_plhiv_reactivate_ltfu12 <- 0.083
p_plhiv_reactivate_ltfu39 <- 0.017
p_plhiv_reactivate_ltfu10plus <- 0.0088

#notification/detection probability for PLHIV is 100% for all countries
p_plhiv_notif <- 1
#hh contact detection probability varies by country

#parameters related to DALY calcs NOT USING IN SHINY
life_exp_plhiv <- 27
dw_plhiv_art <- 0.08 #disability weight for PLHIV on ART (without TB)
dw_plhiv_tb <- 0.408 #disability weight for PLHIV with TB
dw_plhiv_no_art <- 0.274 #disability weight for PLHIV not on ART
dw_tb <- 0.333 #disabilty weight for TB (used for hh contacts)
dw_notb <- 0 #for household contacts w/out active TB, no morbidity/0 disability weight
dur_tb_tx <- 0.5 #treated TB lasts 6 months from when tx starts

#TB mortality
p_child_die_tb_tx <- 0.02
p_child_die_tb_notx <- 0.436
p_adol_die_tb_tx <- 0.008
p_adol_die_tb_notx <- 0.149
p_adult_die_tb_tx <- p_adol_die_tb_tx
p_adult_die_tb_notx <- p_adol_die_tb_notx
p_plhiv_ltfu_die_tb <- 0.25 
p_plhiv_art_die_tb <- 0.10

#NON-TB mortality (for PLHIV)
p_plhiv_new_die <- 0.058
p_plhiv_est_die <- 0.022
p_plhiv_ltfu_die <- 0.065

#TRANSITION ON/OFF OF ART (for PLHIV)
#LTFU probabilities from literature, return calibrated so that 90% of newly enrolled remain on ART [85-95]
p_plhiv_new_ltfu <- 0.15
p_plhiv_est_ltfu <- 0.08
p_plhiv_ltfu_art <- 0.72

#costs that don't vary by country: TPT drugs, # TPT visits, delivery costs
#drug prices
dur_child_ipt <- 6*30 #6 months IPT 
dur_adol_ipt <- dur_child_ipt
dur_adult_ipt <- dur_child_ipt
dur_plhiv_ipt <- 6*30 #6 months IPT 
c_ipt100 <- 0.02 
c_ipt300 <- 0.04
c_child_ipt <- c_ipt100*dur_child_ipt
c_adol_ipt <- mean(c(c_ipt300, c_ipt100))*dur_adol_ipt
c_adult_ipt <- c_ipt300*dur_adult_ipt
c_plhiv_ipt <- c_ipt300*dur_plhiv_ipt
c_child_3hp <- 6 
c_adol_3hp <- 12 
c_adult_3hp <- 14.25
c_plhiv_3hp <- c_adult_3hp
c_child_1hp <- 22
c_adol_1hp <- c_child_1hp
c_adult_1hp <- c_child_1hp
c_plhiv_1hp <- c_child_1hp

#treatment visits
n_visit_ipt_contacts <- 6 #6 monitoring visits (initiation=same days as testing visit)
n_visit_ipt_plhiv <- 4 #4 monitoring visits (initiation, 3rd & 6th monitoring visit occur during quarterly ART visits)
n_visit_3hp_contacts <- 3 #3 monitoring visits (initiation visit=same day as testing visit). 1 in SA
n_visit_3hp_plhiv <- 2 #2 monitoring visits (initiation and last monitoring visit occur during quarterly ART visits). 1 in SA
n_visit_1hp_contacts <- 1 #initial visit after 1-2 weeks
n_visit_1hp_plhiv <- 1 #initial visit after 1-2 weeks

#TPT delivery costs
c_delivery_ipt <- 0.1
c_delivery_3hp <- c_delivery_ipt 
c_delivery_1hp <- c_delivery_3hp 

#ART (also include 4 outpatient visits)
c_delivery_art <- 0.1 #10% customs/insurance/freight/regulatory
c_art_yr <- (63+12+12)*(1+c_delivery_art) #63=drugs, 12=2 VL tests, 0-20% overheads
n_visit_art <- 4

#number of inpatient hospitalization days for severe toxicity event
nday_hosp_tox <- 3

#discount factor - NOT USING
disc_fac <- 0.03

#save params that vary by target population for each population
child_params <- list("p_symptom_cxr"=NA, #no CXR screening for children < 5
                     "p_symptom"=p_hh_child_symptom, 
                     "p_tb"=p_hh_child_tb,
                     "p_tb_symptom_cxr"=NA, #no CXR screening for children < 5
                     "p_tb_symptom"=p_hh_child_tb_symptom,
                     "cxr_sens"=hh_child_cxr_sens,
                     "cxr_spec"=hh_child_cxr_spec,
                     "p_asymptom_negcxr_tb"=NA, #no CXR screening for children < 5
                     "p_asymptom_tb"=p_hh_child_asymptom_tb,
                     "p_initiate"=p_hh_child_initiate,
                     "p_ltbi"=p_hh_child_ltbi,
                     "p_tox_nohosp_ipt"=p_child_tox_nohosp_ipt,
                     "p_tox_hosp_ipt"=p_child_tox_hosp_ipt,
                     "p_tox_nohosp_3hp"=p_child_tox_nohosp_3hp,
                     "p_tox_hosp_3hp"=p_child_tox_hosp_3hp,
                     "p_tox_nohosp_1hp"=p_child_tox_nohosp_1hp,
                     "p_tox_hosp_1hp"=p_child_tox_hosp_1hp,
                     "p_complete_ipt"=p_child_complete_ipt,
                     "p_complete_3hp"=p_child_complete_3hp,
                     "p_complete_1hp"=p_child_complete_1hp,
                     "eff_ipt"=eff_child_ipt,
                     "eff_3hp"=eff_child_3hp,
                     "eff_1hp"=eff_child_1hp,
                     "p_reactivate_02"=p_child_reactivate_02,
                     "p_reactivate_25"=p_child_reactivate_25,
                     "p_reactivate_510"=p_child_reactivate_510,
                     "p_reactivate_10plus"=p_child_reactivate_10plus,
                     "p_infect"=0, #no reinfections in main analysis
                     "ltbi_protect"=0.8, #80% protection from reinfection - only used in sensitivity analysis when reinfect!=0
                     "prop_fast"=0.1, #10% of infections/reinfections progress immediately after being infected
                     "p_die_tb_tx"=p_child_die_tb_tx,
                     "p_die_tb"=p_child_die_tb_notx,
                     "c_ipt"=c_child_ipt,
                     "c_3hp"=c_child_3hp,
                     "c_1hp"=c_child_1hp,
                     "c_delivery_ipt"=c_delivery_ipt,
                     "c_delivery_3hp"=c_delivery_3hp,
                     "c_delivery_1hp"=c_delivery_1hp,
                     "n_visit_ipt"=n_visit_ipt_contacts,
                     "n_visit_3hp"=n_visit_3hp_contacts,
                     "n_visit_1hp"=n_visit_1hp_contacts,
                     "nday_hosp_tox"=nday_hosp_tox)

adol_params <- list("p_symptom_cxr"=p_hh_adol_symptom_cxr,
                    "p_symptom"=p_hh_adol_symptom,
                    "p_tb"=p_hh_adol_tb,
                    "p_tb_symptom_cxr"=p_hh_adol_tb_symptom_cxr,
                    "p_tb_symptom"=p_hh_adol_tb_symptom,
                    "xpert_sens"=hh_adol_xpert_sens,
                    "xpert_spec"=hh_adol_xpert_spec,
                    "p_asymptom_negcxr_tb"=p_hh_adol_asymptom_negcxr_tb,
                    "p_asymptom_tb"=p_hh_adol_asymptom_tb,
                    "p_initiate"=p_hh_adol_initiate,
                    "p_ltbi"=p_hh_adol_ltbi,
                    "p_tox_nohosp_ipt"=p_adol_tox_nohosp_ipt,
                    "p_tox_hosp_ipt"=p_adol_tox_hosp_ipt,
                    "p_tox_nohosp_3hp"=p_adol_tox_nohosp_3hp,
                    "p_tox_hosp_3hp"=p_adol_tox_hosp_3hp,
                    "p_tox_nohosp_1hp"=p_adol_tox_nohosp_1hp,
                    "p_tox_hosp_1hp"=p_adol_tox_hosp_1hp,
                    "p_complete_ipt"=p_adol_complete_ipt,
                    "p_complete_3hp"=p_adol_complete_3hp,
                    "p_complete_1hp"=p_adol_complete_1hp,
                    "eff_ipt"=eff_adol_ipt,
                    "eff_3hp"=eff_adol_3hp,
                    "eff_1hp"=eff_adol_1hp,
                    "p_reactivate_02"=p_adol_reactivate_02,
                    "p_reactivate_25"=p_adol_reactivate_25,
                    "p_reactivate_510"=p_adol_reactivate_510,
                    "p_reactivate_10plus"=p_adol_reactivate_10plus,
                    "p_infect"=0, #no infections/reinfections in main analysis
                    "ltbi_protect"=0.8, #80% protection from reinfection - only used in sensitivity analysis when reinfect!=0
                    "prop_fast"=0.1, #10% of infections/reinfections progress immediately after being infected
                    "p_die_tb_tx"=p_adol_die_tb_tx,
                    "p_die_tb"=p_adol_die_tb_notx,
                    "c_ipt"=c_adol_ipt,
                    "c_3hp"=c_adol_3hp,
                    "c_1hp"=c_adol_1hp,
                    "c_delivery_ipt"=c_delivery_ipt,
                    "c_delivery_3hp"=c_delivery_3hp,
                    "c_delivery_1hp"=c_delivery_1hp,
                    "n_visit_ipt"=n_visit_ipt_contacts,
                    "n_visit_3hp"=n_visit_3hp_contacts,
                    "n_visit_1hp"=n_visit_1hp_contacts,
                    "nday_hosp_tox"=nday_hosp_tox)

adult_params <- list("p_symptom_cxr"=p_hh_adult_symptom_cxr,
                     "p_symptom"=p_hh_adult_symptom,
                     "p_tb"=p_hh_adult_tb,
                     "p_tb_symptom_cxr"=p_hh_adult_tb_symptom_cxr,
                     "p_tb_symptom"=p_hh_adult_tb_symptom,
                     "xpert_sens"=hh_adult_xpert_sens,
                     "xpert_spec"=hh_adult_xpert_spec,
                     "p_asymptom_negcxr_tb"=p_hh_adult_asymptom_negcxr_tb,
                     "p_asymptom_tb"=p_hh_adult_asymptom_tb,
                     "p_initiate"=p_hh_adult_initiate,
                     "p_ltbi"=p_hh_adult_ltbi,
                     "p_tox_nohosp_ipt"=p_adult_tox_nohosp_ipt,
                     "p_tox_hosp_ipt"=p_adult_tox_hosp_ipt,
                     "p_tox_nohosp_3hp"=p_adult_tox_nohosp_3hp,
                     "p_tox_hosp_3hp"=p_adult_tox_hosp_3hp,
                     "p_tox_nohosp_1hp"=p_adult_tox_nohosp_1hp,
                     "p_tox_hosp_1hp"=p_adult_tox_hosp_1hp,
                     "p_complete_ipt"=p_adult_complete_ipt,
                     "p_complete_3hp"=p_adult_complete_3hp,
                     "p_complete_1hp"=p_adult_complete_1hp,
                     "eff_ipt"=eff_adult_ipt,
                     "eff_3hp"=eff_adult_3hp,
                     "eff_1hp"=eff_adult_1hp,
                     "p_reactivate_02"=p_adult_reactivate_02,
                     "p_reactivate_25"=p_adult_reactivate_25,
                     "p_reactivate_510"=p_adult_reactivate_510,
                     "p_reactivate_10plus"=p_adult_reactivate_10plus,
                     "p_infect"=0, #no reinfections in main analysis
                     "ltbi_protect"=0.8, #80% protection from reinfection - only used in sensitivity analysis when reinfect!=0
                     "prop_fast"=0.1, #10% of infections/reinfections progress immediately after being infected
                     "p_die_tb_tx"=p_adult_die_tb_tx,
                     "p_die_tb"=p_adult_die_tb_notx,
                     "c_ipt"=c_adult_ipt,
                     "c_3hp"=c_adult_3hp,
                     "c_1hp"=c_adult_1hp,
                     "c_delivery_ipt"=c_delivery_ipt,
                     "c_delivery_3hp"=c_delivery_3hp,
                     "c_delivery_1hp"=c_delivery_1hp,
                     "n_visit_ipt"=n_visit_ipt_contacts,
                     "n_visit_3hp"=n_visit_3hp_contacts,
                     "n_visit_1hp"=n_visit_1hp_contacts,
                     "nday_hosp_tox"=nday_hosp_tox)

plhiv_params <- list("p_tox_nohosp_ipt"=p_plhiv_tox_nohosp_ipt,
                     "p_tox_hosp_ipt"=p_plhiv_tox_hosp_ipt,
                     "p_tox_nohosp_3hp"=p_plhiv_tox_nohosp_3hp,
                     "p_tox_hosp_3hp"=p_plhiv_tox_hosp_3hp,
                     "p_tox_nohosp_1hp"=p_plhiv_tox_nohosp_1hp,
                     "p_tox_hosp_1hp"=p_plhiv_tox_hosp_1hp,
                     "p_initiate"=p_plhiv_initiate,
                     "p_complete_ipt"=p_plhiv_complete_ipt,
                     "p_complete_3hp"=p_plhiv_complete_3hp,
                     "p_complete_1hp"=p_plhiv_complete_1hp,
                     "eff_ipt"=eff_plhiv_ipt,
                     "eff_3hp"=eff_plhiv_3hp,
                     "eff_1hp"=eff_plhiv_1hp,
                     "p_reactivate_new_yr1"=p_plhiv_reactivate_new01,
                     "p_reactivate_new_yr2"=p_plhiv_reactivate_new12,
                     "p_reactivate_est_yr1"=p_plhiv_reactivate_est01,
                     "p_reactivate_est_yr2"=p_plhiv_reactivate_est12,
                     "p_reactivate_est_yr3_9"=p_plhiv_reactivate_est39,
                     "p_reactivate_est_yr10plus"=p_plhiv_reactivate_est10plus,
                     "p_reactivate_ltfu_yr2"=p_plhiv_reactivate_ltfu12,
                     "p_reactivate_ltfu_yr3_9"=p_plhiv_reactivate_ltfu39,
                     "p_reactivate_ltfu_yr10plus"=p_plhiv_reactivate_ltfu10plus,
                     "p_infect"=0, #no reinfections in main analysis
                     "ltbi_protect_art"=0.8, #80% protection from reinfection - only used in sensitivity analysis when reinfect!=0
                     "ltbi_protect_ltfu"=0, #0% protection if not on ART - only used in sensitivity analysis
                     "prop_fast_art"=0.1, #10% of infections/reinfections progress immediately after being infected
                     "prop_fast_ltfu"=1, #100% of infections/reinfections progres immediately after infection if not on ART
                     "p_die_tb_art"=p_plhiv_art_die_tb,
                     "p_die_tb_ltfu"=p_plhiv_ltfu_die_tb,
                     "p_notif"=p_plhiv_notif,
                     "p_new_die"=p_plhiv_new_die,
                     "p_est_die"=p_plhiv_est_die,
                     "p_ltfu_die"=p_plhiv_ltfu_die,
                     "p_new_ltfu"=p_plhiv_new_ltfu, #base case (more frequent transitions, lit-based)
                     "p_est_ltfu"=p_plhiv_est_ltfu, 
                     "p_ltfu_art"=p_plhiv_ltfu_art,
                     "c_ipt"=c_plhiv_ipt,
                     "c_3hp"=c_plhiv_3hp,
                     "c_1hp"=c_plhiv_1hp,
                     "c_delivery_ipt"=c_delivery_ipt,
                     "c_delivery_3hp"=c_delivery_3hp,
                     "c_delivery_1hp"=c_delivery_1hp,
                     "n_visit_ipt"=n_visit_ipt_plhiv,
                     "n_visit_3hp"=n_visit_3hp_plhiv,
                     "n_visit_1hp"=n_visit_1hp_plhiv,
                     "c_art_yr"=c_art_yr,
                     "n_visit_art"=n_visit_art,
                     "nday_hosp_tox"=nday_hosp_tox)

other_params <- list("disc_fac"=disc_fac,
                      "life_exp_plhiv"=life_exp_plhiv,
                      "dw_plhiv_art"=dw_plhiv_art,
                      "dw_plhiv_tb"=dw_plhiv_tb,
                      "dw_plhiv_no_art "=dw_plhiv_no_art,
                      "dw_tb"=dw_tb,
                      "dur_tb_tx"=dur_tb_tx,
                     "p_hh_child_cxr_tpt"=p_hh_child_cxr_tpt,
                     "p_hh_adol_symptom_cxr_tpt"=p_hh_adol_symptom_cxr_tpt,
                     "p_hh_adol_symptom_tpt"=p_hh_adol_symptom_tpt,
                     "p_hh_adult_symptom_cxr_tpt"=p_hh_adult_symptom_cxr_tpt,
                     "p_hh_adult_symptom_tpt"=p_hh_adult_symptom_tpt,
                     "dw_notb"=dw_notb)

save(child_params, adol_params, adult_params, plhiv_params, other_params,
     file="shiny/params.rda", compress="gzip")

