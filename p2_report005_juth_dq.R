rm(list = ls())

library(plyr)
library(labelled)
library(sqldf)
library(magrittr)
library(dplyr)

myurl <- scan("config/redcap_url", what="")
mytoken <- scan("config/juth_p2_clinical_token", what="")

mydata = REDCapR::redcap_read(
  redcap_uri = myurl, 
  token = mytoken
  
)$data

mymetadata = REDCapR::redcap_metadata_read(
  redcap_uri = myurl, token = mytoken
)$data

rm(list = c("mytoken"))


juth_data <- mydata
rec_oth_id <- juth_data %>% filter((redcap_event_name == 'enrollment_arm_1')|(redcap_event_name == 'enrollment_arm_2')) %>% select(record_id, enr_otherid)

####Functions
# To sum number of fields with NA values 
sumna<-function(x){sum(is.na(x))}
# Function to concatenate list of fields with NA values
test_proc <- function(x) {
  fields <-c()
  if(nrow(x) > 0)  {
    for ( i in c(1:nrow(x))) {
      vec <- x[i,]
      var <- which(vec %in% c('NA', NA))
      col <- names(x)[var]
     
      pcol <- do.call(paste, c(as.list(col), sep = " <br/> "))
      fields <- append(fields,pcol)
    }}else{
      fields<-character()
    }
  return (fields)
}

#fields <- test_proc(x)
######################cohorts###############
dat <- mydata
dat$enr_hiv_y <- ifelse(!is.na(dat$hiv_year) | dat$visit_hiv_y == 1, 1, 0)
dat$enr_hiv_y <- ifelse(is.na(dat$enr_hiv_y), 0, dat$enr_hiv_y)

dat$enr_icc_y <- ifelse(!is.na(dat$icc_diag_d) | dat$enr_icc_diag == 1, 1, 0)
dat$enr_icc_y <- ifelse(is.na(dat$enr_icc_y), 0, dat$enr_icc_y)
dat$group <- interaction(dat$enr_hiv_y, dat$enr_icc_y) # or paste()

dat.arm1 <- dat %>%
  filter(redcap_event_name == "enrollment_arm_1")
dat2.CIN <- dat %>%
  filter(redcap_event_name == "enrollment_arm_2") %>%
  mutate(group = "HIV+/CIN")
dat3 <- rbind.data.frame(dat.arm1, dat2.CIN)
dat3$cohort <- factor(dat3$group,
                      levels = c("1.1", "0.1", "1.0", "0.0", "HIV+/CIN", "Total"),
                      labels = c("HIV+/ICC+", "HIV-/ICC+", "HIV+/ICC-", "HIV-/ICC-", "HIV+/CIN", "Total"))

dat3 <- dat3 %>% select(-redcap_repeat_instance,-redcap_repeat_instrument)

cohorts <- dat3 %>% 
  select(record_id, redcap_event_name, enr_otherid, hiv_year, visit_hiv_y, icc_diag_d, enr_icc_diag, cohort, enr_icc_diag_hist,icc_diag_histopath_typ, icc_diag_histopath_typ_oth, visit_cervix_biopsy_y, visit_cervix_biopsy_d, visit_biopsy_result) %>%
  arrange(record_id)

cohort_hivpos <- cohorts %>% filter((cohort == 'HIV+/ICC+') | (cohort == 'HIV+/ICC-') | (cohort == 'HIV+/CIN')) %>% select(record_id, enr_otherid)
cohort_iccpos <- cohorts %>% filter((cohort == 'HIV+/ICC+') | (cohort == 'HIV-/ICC+')) %>% select(record_id, enr_otherid)
cohort_cin <- cohorts %>% filter((cohort == 'HIV+/CIN')) %>% select(record_id, enr_otherid)
#########################################################
###Enrollment form - Missing fields
enrform_otherid_missing <- sqldf(" select record_id,
                                         redcap_event_name,
                                         enr_otherid,
                                         enr_site, 
                                         enr_enroll_d,
                                         enr_dob_d,
                                         enr_marital,
                                         enr_occupation,
                                         enr_edu,
                                         enr_income_y,
                                         enr_weight,
                                         enr_height				 
                                 from juth_data
                                 where enrollment_complete = '2' and 
                                       (redcap_event_name = 'enrollment_arm_1' or redcap_event_name = 'enrollment_arm_2') 
                                       and 
                                       ((enr_otherid is null) or
                                       (enr_site is null) or
                                       (enr_enroll_d is null) or
                                       (enr_dob_d is null) or
                                       (enr_marital is null) or
                                       (enr_occupation is null) or
                                       (enr_edu is null) or
                                       (enr_weight is null) or
                                       (enr_height is null)) ")
                                       
#####rename
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_otherid"]="Other ID"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_site"]="Site"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_enroll_d"]="Date of Enrollment"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_dob_d"]="Date of Birth"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_gender"]="Sex"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_marital"]="Marital Status"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_occupation"]="Occupation"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_edu"]="Highest Level of Education Completed"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_income_y"]="Would you be able to provide your income for the last 12 months"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_weight"]="Weight"
names(enrform_otherid_missing)[names(enrform_otherid_missing) =="enr_height"]="Height"

###Function to list fields with 'NA' value
x = enrform_otherid_missing
fields <- test_proc(x)

enr_oth_form_miss <- data.frame(enrform_otherid_missing, missing =apply(enrform_otherid_missing,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enr_oth_form_miss <- cbind(enr_oth_form_miss, fields)

#####Enrollment - reproductive history missing
enrform_rep_hx1_missing <- sqldf(" select record_id, 
                                         redcap_event_name,  
                                         enr_rep_preg, 
                                         enr_rep_age,
                                         enr_rep_preg_ct,
                                         enr_rep_preg_live,
                                         enr_rep_preg_live_age
                                 from juth_data
                                 where redcap_event_name in ('enrollment_arm_1', 'enrollment_arm_2') and 
                                       ((enr_rep_preg in (1)) and 
                                       ((enr_rep_age is null) or
                                       (enr_rep_preg_ct is null )or
                                       (enr_rep_preg_live is null) or
                                       (enr_rep_preg_live_age is null))) ")
                                       
enrform_rep_hx2_missing <- sqldf(" select record_id, 
                                         redcap_event_name,  
                                         enr_rep_preg, 
                                         enr_rep_age,
                                         enr_rep_preg_ct,
                                         enr_rep_preg_live,
                                         enr_rep_preg_live_age
                                 from juth_data
                                 where redcap_event_name in ('enrollment_arm_1', 'enrollment_arm_2') and 
                                       ((enr_rep_preg is null) and
                                       ((enr_rep_age is null) or
                                       (enr_rep_preg_ct is null )or
                                       (enr_rep_preg_live is null) or
                                       (enr_rep_preg_live_age is null))) ")
                                       

enrform_rep_hx_missing <- rbind(enrform_rep_hx1_missing, enrform_rep_hx2_missing)


names(enrform_rep_hx_missing)[names(enrform_rep_hx_missing) =="enr_rep_preg"] = "Have you ever been pregnant?"
names(enrform_rep_hx_missing)[names(enrform_rep_hx_missing) =="enr_rep_age"] = "How old were you when you first became pregnant?"
names(enrform_rep_hx_missing)[names(enrform_rep_hx_missing) =="enr_rep_preg_ct"] = "How many times have you been pregnant?" #Please include stillbirths, miscarriages, abortions, tubal or ectopic pregnancies, and live births.
names(enrform_rep_hx_missing)[names(enrform_rep_hx_missing) =="enr_rep_preg_live"] = "How many of your pregnancies resulted in live birth(s)?"
names(enrform_rep_hx_missing)[names(enrform_rep_hx_missing) =="enr_rep_preg_live_age"] = "What was your age at the birth of your first live born child?"
x = enrform_rep_hx_missing
fields <- test_proc(x)

enr_rep_form_miss <- data.frame(enrform_rep_hx_missing, missing =apply(enrform_rep_hx_missing,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enr_rep_form_miss <- cbind(enr_rep_form_miss, fields)

#####Enrollment - birth control missing
enrform_birth_y_missing <- sqldf(" select record_id, 
                                         redcap_event_name,  
                                         enr_brth_ctrl_y,
                                         enr_brth_ctrl_age,
                                         enr_brth_ctrl_age_last,
                                         enr_brth_ctrl_yrs
                                 from juth_data 
                                 where ((redcap_event_name ='enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                                       ((enr_brth_ctrl_y = 1) or (enr_brth_ctrl_y is null)) and 
                                       (( enr_brth_ctrl_age is null) or
                                       (enr_brth_ctrl_age_last is null) or
                                       (enr_brth_ctrl_yrs is null)) ")
####rename
names(enrform_birth_y_missing)[names(enrform_birth_y_missing) =="enr_brth_ctrl_y"] = "Have you ever used birth control pills for birth control or to regulate menstrual periods?"
names(enrform_birth_y_missing)[names(enrform_birth_y_missing) =="enr_brth_ctrl_age"] = "How old were you when you first started taking birth control pills?"
names(enrform_birth_y_missing)[names(enrform_birth_y_missing) =="enr_brth_ctrl_age_last"] = "How old were you when you last took birth control pills?"
names(enrform_birth_y_missing)[names(enrform_birth_y_missing) =="enr_brth_ctrl_yrs"] = "How many years in total have you taken birth control pills?"

x = enrform_birth_y_missing
fields <- test_proc(x)

enrform_birth_y_missing_miss <- data.frame(enrform_birth_y_missing, missing =apply(enrform_birth_y_missing,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enrform_birth_y_missing_miss <- cbind(enrform_birth_y_missing_miss, fields)

####iud missing
enrform_iud_y_missing <- sqldf(" select record_id, 
                                        redcap_event_name,  
                                        enr_brth_ctrl_iud_y, 
                                        enr_brth_ctrl_iud_yrs
                               from juth_data 
                               where ((redcap_event_name ='enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                                     (((enr_brth_ctrl_iud_y = 1) or (enr_brth_ctrl_iud_y is null)) and enr_brth_ctrl_iud_yrs is null) ")

####rename
names(enrform_iud_y_missing)[names(enrform_iud_y_missing) =="enr_brth_ctrl_iud_y"] = "Have you ever used IUDs?"
names(enrform_iud_y_missing)[names(enrform_iud_y_missing) =="enr_brth_ctrl_iud_yrs"] = "How many years in total have you used an IUD?"

x = enrform_iud_y_missing
fields <- test_proc(x)

enrform_iud_y_missing_miss <- data.frame(enrform_iud_y_missing, missing =apply(enrform_iud_y_missing,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enrform_iud_y_missing_miss <- cbind(enrform_iud_y_missing_miss, fields)

####birth control - condoms missing
enrform_cdms_y_missing <- sqldf(" select record_id, 
                                        redcap_event_name,  
                                        enr_brth_ctrl_cdms_y, 
                                        enr_brth_ctrl_cdms_use
                                from juth_data 
                                where ((redcap_event_name ='enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                                      (((enr_brth_ctrl_cdms_y = 1) or (enr_brth_ctrl_cdms_y is null)) and enr_brth_ctrl_cdms_use is null) ")

####rename
names(enrform_cdms_y_missing)[names(enrform_cdms_y_missing) =="enr_brth_ctrl_cdms_y"] = "Have you ever used condoms during sex?"
names(enrform_cdms_y_missing)[names(enrform_cdms_y_missing) =="enr_brth_ctrl_cdms_use"] = "How often did you use it?"

x = enrform_cdms_y_missing
fields <- test_proc(x)

enrform_cdms_y_missing_miss <- data.frame(enrform_cdms_y_missing, missing =apply(enrform_cdms_y_missing,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enrform_cdms_y_missing_miss <- cbind(enrform_cdms_y_missing_miss, fields)
######Birth control section missing
enrform_bc_miss <- rbind(enrform_birth_y_missing_miss, enrform_iud_y_missing_miss, enrform_cdms_y_missing_miss)
########Sexual history
enrform_sx_hx <- juth_data %>% filter((redcap_event_name == "enrollment_arm_1")|(redcap_event_name == "enrollment_arm_2")) %>% select(record_id, redcap_event_name, enr_sexhx_age, enr_sexhx_num_partner, enr_sexhx_y)
enrform_sx_hx <- enrform_sx_hx[rowSums(is.na(enrform_sx_hx)) > 0,]

####rename
names(enrform_sx_hx)[names(enrform_sx_hx) =="enr_sexhx_age"] = "At about what age did you have your first sexual experience?"
names(enrform_sx_hx)[names(enrform_sx_hx) =="enr_sexhx_num_partner"] = "How many sexual partners have you had in your life time?"
names(enrform_sx_hx)[names(enrform_sx_hx) =="enr_sexhx_y"] = "Have you ever exchanged sex for money or other incentives?"


x = enrform_sx_hx
fields <- test_proc(x)

enrform_sx_hx_miss <- data.frame(enrform_sx_hx, missing =apply(enrform_sx_hx,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enrform_sx_hx_miss <- cbind(enrform_sx_hx_miss, fields)
enrform_sx_hx_std <- sqldf(" select record_id, 
                                   redcap_event_name,  
                                   enr_sexhx_std,
                                   enr_sexhx_std_type
                           from juth_data 
                           where ((redcap_event_name ='enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                                 (((enr_sexhx_std = 1) or (enr_sexhx_std is null)) and enr_sexhx_std_type is null) ")

####rename
names(enrform_sx_hx_std)[names(enrform_sx_hx_std) =="enr_sexhx_std"] = "Have you ever been treated for any sexually transmissible infection (STI/STD)?"
names(enrform_sx_hx_std)[names(enrform_sx_hx_std) =="enr_sexhx_std_type"] = "If yes, please specify type of STI/STD(if known)"

x = enrform_sx_hx_std
fields <- test_proc(x)

enrform_sx_hx_std_miss <- data.frame(enrform_sx_hx_std, missing =apply(enrform_sx_hx_std,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enrform_sx_hx_std_miss <- cbind(enrform_sx_hx_std_miss, fields)


#######Cervical cancer screening
enrform_cx_scrn <- juth_data %>%  filter((redcap_event_name == "enrollment_arm_1")|(redcap_event_name == "enrollment_arm_2")) %>% select(record_id, redcap_event_name,enr_icc_scrhx_y,enr_icc_scrhx_test_y,enr_icc_scrhx_test_type,enr_icc_scrhx_test_age)
enrform_cx_scrn_y <- sqldf("select * from enrform_cx_scrn where ((enr_icc_scrhx_y = 1) or (enr_icc_scrhx_y is null))")
enrform_cx_scrn_y <- enrform_cx_scrn_y[rowSums(is.na(enrform_cx_scrn_y)) > 0,]

####rename
names(enrform_cx_scrn_y)[names(enrform_cx_scrn_y) =="enr_icc_scrhx_test_y"] = "If yes, have you ever had a cervical cancer screening test?"
names(enrform_cx_scrn_y)[names(enrform_cx_scrn_y) =="enr_icc_scrhx_y"] = "Are you aware of cervical cancer screening or the Pap test?"
names(enrform_cx_scrn_y)[names(enrform_cx_scrn_y) =="enr_icc_scrhx_test_type"] = "If Yes, which type of screening test did you have?"
names(enrform_cx_scrn_y)[names(enrform_cx_scrn_y) =="enr_icc_scrhx_test_age"] = "If yes, at about what age did you have your first cervical cancer screening test?"

x = enrform_cx_scrn_y
fields <- test_proc(x)

enrform_cx_scrn_y_miss <- data.frame(enrform_cx_scrn_y, missing =apply(enrform_cx_scrn_y,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enrform_cx_scrn_y_miss <- cbind(enrform_cx_scrn_y_miss, fields)

enrform_cx_scrn_pap <- juth_data %>%  filter((redcap_event_name == "enrollment_arm_1")|(redcap_event_name == "enrollment_arm_2")) %>% select(record_id, redcap_event_name,enr_icc_scrhx_test_type, enr_icc_pap_hx_y, enr_icc_pap_hx_test_y, enr_icc_pap_hx_tmt) 
enrform_cx_scrn_pap_y <- sqldf(" select record_id,
                                        redcap_event_name,
                                        enr_icc_pap_hx_y,
                                        enr_icc_pap_hx_test_y,
                                        enr_icc_pap_hx_tmt 
                               from enrform_cx_scrn_pap 
                               where (((enr_icc_scrhx_test_type = 1) and (enr_icc_pap_hx_y = 1)) or
                                     ((enr_icc_scrhx_test_type = 0) and (enr_icc_pap_hx_y = 1)) or
                                     ((enr_icc_scrhx_test_type is null) and (enr_icc_pap_hx_y = 1)) or
                                     ((enr_icc_scrhx_test_type = 1) and (enr_icc_pap_hx_y is null)) or
                                     ((enr_icc_scrhx_test_type is null) and (enr_icc_pap_hx_y is null))) ")

enrform_cx_scrn_pap_y <- enrform_cx_scrn_pap_y[rowSums(is.na(enrform_cx_scrn_pap_y)) > 0,]
####rename
names(enrform_cx_scrn_pap_y)[names(enrform_cx_scrn_pap_y) =="enr_icc_pap_hx_y"] = "Have you ever been diagnosed with an abnormal pap test?"
names(enrform_cx_scrn_pap_y)[names(enrform_cx_scrn_pap_y) =="enr_icc_pap_hx_test_y"] = "If yes, have you ever had a biopsy of your cervix or a LEEP procedure?"
names(enrform_cx_scrn_pap_y)[names(enrform_cx_scrn_pap_y) =="enr_icc_pap_hx_tmt"] = "What other treatment have you ever received for abnormal Pap?"

x = enrform_cx_scrn_pap_y 
fields <- test_proc(x)

enrform_cx_scrn_pap_y_miss <- data.frame(enrform_cx_scrn_pap_y, missing =apply(enrform_cx_scrn_pap_y,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enrform_cx_scrn_pap_y_miss <- cbind(enrform_cx_scrn_pap_y_miss, fields)

#####Risk factors
enr_risk_med <- juth_data %>% filter((redcap_event_name == "enrollment_arm_1")|(redcap_event_name == "enrollment_arm_2")) %>% select(record_id, redcap_event_name,enr_icc_hpv_y, enr_icc_chly_y, enr_icc_tobacco_life,enr_icc_tobacco_prtnr,enr_icc_iccfhx_y)
enr_risk_med <- enr_risk_med[rowSums(is.na(enr_risk_med)) > 0,]

####rename
names(enr_risk_med)[names(enr_risk_med) =="enr_icc_hpv_y"] = "Human Papilloma Virus(HPV)?"
names(enr_risk_med)[names(enr_risk_med) =="enr_icc_chly_y"] = "Chlamydia?"
names(enr_risk_med)[names(enr_risk_med) =="enr_icc_tobacco_life"] = "In your entire life, have you smoked 100 or more cigarettes?"
names(enr_risk_med)[names(enr_risk_med) =="enr_icc_tobacco_prtnr"] = "Do you live with a partner who smoke cigarettes?"
names(enr_risk_med)[names(enr_risk_med) =="enr_icc_iccfhx_y"] = "Family history of ICC?"

x = enr_risk_med
fields <- test_proc(x)

enr_risk_med_miss <- data.frame(enr_risk_med, missing = apply(enr_risk_med,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enr_risk_med_miss <- cbind(enr_risk_med_miss, fields)
###Risk factor - tobacco -yes
enr_risk_tob_y <- sqldf(" select record_id, 
                                 redcap_event_name,  
                                 enr_icc_tobacco_y,
                                 enr_icc_tobacco_avg_day,
                                 enr_icc_tobacco_reg,
                                 enr_icc_tobacco_yr,
                                 enr_packyr
                        from juth_data 
                        where ((redcap_event_name ='enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                              ((enr_icc_tobacco_y = 1) or (enr_icc_tobacco_y is null)) and (enr_icc_tobacco_avg_day is null or 
                              enr_icc_tobacco_reg is null or
                              enr_icc_tobacco_yr is null or
                              enr_packyr is null) ")
####rename
names(enr_risk_tob_y)[names(enr_risk_tob_y) =="enr_icc_tobacco_y"] = "Current tobacco use? (Choose yes even if you only smoke occassionally)"
names(enr_risk_tob_y)[names(enr_risk_tob_y) =="enr_icc_tobacco_avg_day"] = "On average, about how many cigarettes a day do you smoke?  (If less than 1 cigarette per day, put 0.5)"
names(enr_risk_tob_y)[names(enr_risk_tob_y) =="enr_icc_tobacco_reg"] = "At what age did you first start smoking regularly?"
names(enr_risk_tob_y)[names(enr_risk_tob_y) =="enr_icc_tobacco_yr"] = "How many years have you smoked, not counting time periods when you had quit?"
names(enr_risk_tob_y)[names(enr_risk_tob_y) =="enr_packyr"] = "Pack-year history?"

x = enr_risk_tob_y
fields <- test_proc(x)

enr_risk_tob_y_miss <- data.frame(enr_risk_tob_y, missing = apply(enr_risk_tob_y,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enr_risk_tob_y_miss <- cbind(enr_risk_tob_y_miss, fields)
enr_risk_tob_n <- sqldf(" select record_id,
                                 redcap_event_name,
                                 enr_icc_tobacco_q_age 
                        from juth_data
                        where ((redcap_event_name ='enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                        ((enr_icc_tobacco_y = 0) or (enr_icc_tobacco_y is null)) and enr_icc_tobacco_q_age is null ")

####rename
names(enr_risk_tob_n)[names(enr_risk_tob_n) =="enr_icc_tobacco_q_age"] = "How old were you the last time when you quit smoking cigarettes?"

x = enr_risk_tob_n
fields <- test_proc(x)

enr_risk_tob_n_miss <- data.frame(enr_risk_tob_n, missing = apply(enr_risk_tob_n,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enr_risk_tob_n_miss <- cbind(enr_risk_tob_n_miss, fields)                                                                                                                                 
#####Risk factor - alcohol
enr_risk_alc <- sqldf(" select record_id, 
                               redcap_event_name,
                               enr_hcc_alcohol_y,
                               enr_alcohol_yrs 
                      from juth_data
                      where ((redcap_event_name ='enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                      ((enr_hcc_alcohol_y = 1) or (enr_hcc_alcohol_y is null)) and enr_alcohol_yrs is null ")

####rename
names(enr_risk_alc)[names(enr_risk_alc) =="enr_hcc_alcohol_y"] = "Current alcohol use?"
names(enr_risk_alc)[names(enr_risk_alc) =="enr_alcohol_yrs"] = "Alcohol duration in years"

x = enr_risk_alc
fields <- test_proc(x)

enr_risk_alc_miss <- data.frame(enr_risk_alc, missing = apply(enr_risk_alc,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enr_risk_alc_miss <- cbind(enr_risk_alc_miss, fields) 

enr_risk_fac_miss <- rbind(enr_risk_med_miss,enr_risk_tob_y_miss,enr_risk_tob_n_miss,enr_risk_alc_miss)                                                                                                                                
#####HPV immunization and other medication
enrform_comorb <- sqldf(" select record_id, 
                                redcap_event_name,  
                                enr_comorbid_malig_y,
                                enr_comorbid_malig_type
                        from juth_data
                        where ((redcap_event_name ='enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and 
                              ((enr_comorbid_malig_y = 1 and enr_comorbid_malig_type is null) or
                              (enr_comorbid_malig_y = 0 and enr_comorbid_malig_type is not null) or
                              (enr_comorbid_malig_y is null and enr_comorbid_malig_type is null)) ")

####rename
names(enrform_comorb)[names(enrform_comorb) =="enr_comorbid_malig_y"] = "Malignancy other than ICC?"
names(enrform_comorb)[names(enrform_comorb) =="enr_comorbid_malig_type"] = "Malignancy type?"


x = enrform_comorb
fields <- test_proc(x)

enrform_comorb_miss <- data.frame(enrform_comorb, missing =apply(enrform_comorb,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enrform_comorb_miss <- cbind(enrform_comorb_miss, fields) 
enrform_hpv_conditions <- juth_data %>% filter((redcap_event_name == "enrollment_arm_1")|(redcap_event_name == "enrollment_arm_2")) %>% select(record_id,redcap_event_name,enr_hpv_hpvimm, enr_oth_diab, enr_oth_htn, enr_oth_kid_dis)
enrform_hpv_conditions <- enrform_hpv_conditions[rowSums(is.na(enrform_hpv_conditions)) > 0,]

####rename
names(enrform_hpv_conditions)[names(enrform_hpv_conditions) =="enr_hpv_hpvimm"] = "HPV Immunization series administered?"
names(enrform_hpv_conditions)[names(enrform_hpv_conditions) =="enr_oth_diab"] = "Diabetes Mellitus"
names(enrform_hpv_conditions)[names(enrform_hpv_conditions) =="enr_oth_htn"] = "Hypertension"
names(enrform_hpv_conditions)[names(enrform_hpv_conditions) =="enr_oth_kid_dis"] = "Kidney Disease"

x = enrform_hpv_conditions
fields <- test_proc(x)

enrform_hpv_conditions_miss <- data.frame(enrform_hpv_conditions, missing =apply(enrform_hpv_conditions,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
enrform_hpv_conditions_miss <- cbind(enrform_hpv_conditions_miss, fields)  

miss_enr_form <- rbind(enr_oth_form_miss,enr_rep_form_miss, enrform_bc_miss, enrform_sx_hx_miss, enrform_sx_hx_std_miss, enrform_cx_scrn_y_miss, enrform_cx_scrn_pap_y_miss, enr_risk_fac_miss, enrform_comorb_miss, enrform_hpv_conditions_miss)
miss_enr_form<- ddply(miss_enr_form, .(record_id, redcap_event_name, form), summarize,fields = paste(fields, collapse="<br/> "), missing = sum(missing))


#######################################################
###HIV Diagnosis form
###HIV Diagnosis form - Enrollment arm 1 and Enrollment arm 2
juth_data_hiv_pre <- sqldf(" select j.record_id,
                                    redcap_event_name,
                                    j.enr_otherid,
                                    hiv_year,
                                    hiv_cd4_initial,
                                    hiv_who
                          from cohort_hivpos h
                          left join
                          juth_data j on h.record_id = j.record_id
                           where ((redcap_event_name = 'enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                                 (hiv_year is not null and (hiv_cd4_initial is null or hiv_who is null)) or 
                                 (hiv_year is null and (hiv_cd4_initial is not null or hiv_who is not null)) ")


###rename
names(juth_data_hiv_pre)[names(juth_data_hiv_pre) =="hiv_cd4_initial"]="Pre-ART CD4 T-Cell Count"
names(juth_data_hiv_pre)[names(juth_data_hiv_pre) =="hiv_who"]="Pre-ART WHO Stage"

x = juth_data_hiv_pre
fields <- test_proc(x)

juth_data_hiv_pre_miss<- data.frame(juth_data_hiv_pre, missing =apply(juth_data_hiv_pre,1,sumna)) %>% mutate (form ='HIV Diagnosis') %>% select(record_id,redcap_event_name, form, missing) #, Var)


juth_data_hiv_pre_miss <- cbind(juth_data_hiv_pre_miss,fields)

juth_data_hiv_enr  <- sqldf(" select j.record_id,
                                    redcap_event_name,
                                    j.enr_otherid,
                                    hiv_aids_y,
                                    hiv_prioroi_y,
                                    hiv_pjp_y,                    
                                    hiv_mac_y,
                                    hiv_cmv_y,  
                                    hiv_pml_y,
                                    hiv_candida_y,
                                    hiv_crypto_y,
                                    hiv_nhl_y,
                                    hiv_ks_y,
                                    hiv_ade_other
                            from cohort_hivpos h
                            left join
                            juth_data j on h.record_id = j.record_id
                            where ((redcap_event_name = 'enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                                  hiv_aids_y = 1 and 
                                  ((hiv_prioroi_y = 0 or hiv_prioroi_y = 2 or hiv_prioroi_y is null) and 
                                  (hiv_pjp_y = 0 or hiv_pjp_y = 2 or hiv_pjp_y is null) and 
                                  (hiv_mac_y = 0 or hiv_mac_y = 2 or hiv_mac_y is null) and
                                  (hiv_cmv_y = 0 or hiv_cmv_y =2 or hiv_cmv_y is null ) and
                                  (hiv_pml_y = 0 or hiv_pml_y =2 or hiv_pml_y is null) and
                                  (hiv_candida_y = 0 or hiv_candida_y =2 or hiv_candida_y is null) and
                                  (hiv_crypto_y = 0 or hiv_crypto_y =2 or hiv_crypto_y is null) and
                                  (hiv_nhl_y = 0 or hiv_nhl_y =2 or hiv_nhl_y is null) and
                                  (hiv_ks_y = 0 or hiv_ks_y =2 or hiv_ks_y is null) and
                                  (hiv_ade_other is null)) ")

juth_data_hiv_enr$hiv_prioroi_y = NA
juth_data_hiv2_enr <- sqldf(" select j.record_id,
                                    redcap_event_name,
                                    j.enr_otherid,
                                    hiv_aids_y,
                                    hiv_prioroi_y,
                                    hiv_pjp_y,
                                    hiv_mac_y,
                                    hiv_cmv_y,
                                    hiv_pml_y,
                                    hiv_candida_y,
                                    hiv_crypto_y,
                                    hiv_nhl_y,
                                    hiv_ks_y,
                                    hiv_ade_other
                            from cohort_hivpos h
                            left join
                            juth_data j on h.record_id = j.record_id 
                            where ((redcap_event_name = 'enrollment_arm_1') or (redcap_event_name = 'enrollment_arm_2')) and
                                  hiv_aids_y = 0 and 
                                  (hiv_prioroi_y = 1 or  
                                  hiv_pjp_y = 1 or
                                  hiv_mac_y = 1 or
                                  hiv_cmv_y = 1 or 
                                  hiv_pml_y = 1 or 
                                  hiv_pml_y = 1 or
                                  hiv_candida_y = 1 or
                                  hiv_crypto_y = 1 or
                                  hiv_nhl_y = 1 or
                                  hiv_ks_y = 1) ")
juth_data_hiv2_enr$hiv_aids_y = NA
hiv_diagnosis_miss_field <- rbind(juth_data_hiv2_enr,juth_data_hiv_enr) %>% select(record_id, redcap_event_name,hiv_aids_y, hiv_prioroi_y)

x = hiv_diagnosis_miss_field
fields <- test_proc(x)

hiv_diagnosis_miss1 <- hiv_diagnosis_miss_field %>% mutate(form ="HIV Diagnosis", missing = apply(hiv_diagnosis_miss_field,1,sumna)) %>% select(record_id,redcap_event_name,form,missing)

###rename
names(hiv_diagnosis_miss_field)[names(hiv_diagnosis_miss_field) =="hiv_prioroi_y"]="Prior Opportunistic Infections"
names(hiv_diagnosis_miss_field)[names(hiv_diagnosis_miss_field) =="hiv_aids_y"]="AIDS Diagnosis"

#####Function to list fields with 'NA' value
x = hiv_diagnosis_miss_field
fields <- test_proc(x)

hiv_diagnosis_miss1 <- cbind(hiv_diagnosis_miss1,fields)
hiv_diagnosis_miss1$missing = as.integer(hiv_diagnosis_miss1$missing)
miss_hiv_diagnosis <- rbind(hiv_diagnosis_miss1,juth_data_hiv_pre_miss)
miss_hiv_diagnosis<- ddply(miss_hiv_diagnosis, .(record_id, redcap_event_name, form), summarize,fields = paste(fields, collapse="; "), missing = sum(missing))

#############################################
####ICC Diagnosis form

icc_diag <- sqldf(" select j.record_id,
                          rec.enr_otherid,
                          redcap_event_name,
                          icc_diag_d,
                          icc_diag_figo_version,
                          icc_diag_t_stg,
                          icc_n_lymph_node,
                          icc_diag_metastasis,
                          icc_diag_tumor_size_y,
                          icc_diag_histopath_typ,
                          icc_diag_tumor_grade
                  from cohort_iccpos i
                  left join
                  juth_data j on i.record_id = j.record_id  
                  left join rec_oth_id rec on j.record_id = rec.record_id
                  where ((redcap_event_name = 'enrollment_arm_1') or (redcap_event_name = 'icc_diagnosis_arm_2'))  and 
                        (icc_diag_d is null or icc_diag_figo_version is null or
                        icc_diag_t_stg is null or icc_n_lymph_node is null or 
                        icc_diag_metastasis is null or icc_diag_tumor_size_y is null or icc_diag_histopath_typ is null or
                        icc_diag_tumor_grade is null) and 
                        icc_diag_d is not null and
                        icc_diag_figo_version is not null
                        order by icc_diag_d ")
icc_diag_d_figo_arm1 <- sqldf(" select j.record_id,
                                      rec.enr_otherid,
                                      redcap_event_name,
                                      icc_diag_d,
                                      icc_diag_figo_version,
                                      icc_diag_t_stg,
                                      icc_n_lymph_node,
                                      icc_diag_metastasis,
                                      icc_diag_tumor_size_y,
                                      icc_diag_histopath_typ,
                                      icc_diag_tumor_grade
                              from cohort_iccpos i
                              left join
                              juth_data j on i.record_id = j.record_id    
                              left join rec_oth_id rec on j.record_id = rec.record_id
                              where redcap_event_name in ('enrollment_arm_1', 'icc_diagnosis_arm_2')   and 
                                    (icc_diag_d is null and icc_diag_figo_version is not null) or
                                    (icc_diag_d is not null and icc_diag_figo_version is null) ")
icc_diag_arms <- rbind(icc_diag,icc_diag_d_figo_arm1)

icc_diag_miss1 <- icc_diag_arms %>% mutate(form ="ICC Diagnosis form", missing = apply(icc_diag_arms,1,sumna)) %>% select(record_id,redcap_event_name,form,missing)

###rename
names(icc_diag_arms)[names(icc_diag_arms) =="icc_diag_d"]="Date of ICC diagnosis"
names(icc_diag_arms)[names(icc_diag_arms) =="icc_diag_figo_version"]="FiGO version"
names(icc_diag_arms)[names(icc_diag_arms) =="icc_diag_t_stg"]="T - Primary tumour"                     
names(icc_diag_arms)[names(icc_diag_arms) =="icc_n_lymph_node"]="N - Regional Lymph Nodes"                     
names(icc_diag_arms)[names(icc_diag_arms) =="icc_diag_metastasis"]="M - Distant Metastasis"                     
names(icc_diag_arms)[names(icc_diag_arms) =="icc_diag_tumor_size_y"]="Tumor Size"                     
names(icc_diag_arms)[names(icc_diag_arms) =="icc_diag_histopath_typ"]="Histopathological type of ICC"                     
names(icc_diag_arms)[names(icc_diag_arms) =="icc_diag_tumor_grade"]="Tumor Grade"                     

#####Function to list fields with 'NA' value
x = icc_diag_arms
fields <- test_proc(x)

icc_diag_miss1 <- cbind(icc_diag_miss1,fields)

icc_diag_figo <- sqldf(" select j.record_id,
                               rec.enr_otherid,
                               redcap_event_name,
                               'ICC Diagnosis form' as form, 
                               icc_diag_figo_version,
                               icc_diag_figo_stg_2009,
                               icc_diag_figo_stg_2018
                       from cohort_iccpos i
                       left join
                       juth_data j on i.record_id = j.record_id  
                       left join rec_oth_id rec on j.record_id = rec.record_id
                       where redcap_event_name in ('enrollment_arm_1', 'icc_diagnosis_arm_2')   and 
                             (icc_diag_figo_version = '1' and icc_diag_figo_stg_2009 is null) or
                             (icc_diag_figo_version = '2' and icc_diag_figo_stg_2018 is null) ")
icc_diag_figo_miss1 <- icc_diag_figo %>% mutate(missing = apply(icc_diag_figo,1,sumna)) %>% select(record_id,redcap_event_name,form,missing)

###rename
names(icc_diag_figo)[names(icc_diag_figo) =="icc_diag_figo_version"]="FiGO version"
names(icc_diag_figo)[names(icc_diag_figo) =="icc_diag_figo_stg_2009"]="FIGO Staging (2009) at the time of Diagnosis"
names(icc_diag_figo)[names(icc_diag_figo) =="icc_diag_figo_stg_2018"]="FIGO Staging (2018) at the time of Diagnosis"                     

#####Function to list fields with 'NA' value
x = icc_diag_figo
fields <- test_proc(x)

icc_diag_figo_miss1 <- cbind(icc_diag_figo_miss1,fields)


icc_diag_tumor <- sqldf(" select j.record_id,
                                 rec.enr_otherid,
                                 redcap_event_name,
                                 'ICC Diagnosis form' as form, 
                                 icc_diag_tumor_size_y,
                                 icc_diag_tumor_size
                        from cohort_iccpos i
                        left join
                        juth_data j on i.record_id = j.record_id  
                        left join rec_oth_id rec on j.record_id = rec.record_id
                        where redcap_event_name in ('enrollment_arm_1', 'icc_diagnosis_arm_2')   and 
                              (icc_diag_tumor_size_y = '1' and icc_diag_tumor_size is null) ")
icc_diag_tumor_miss1 <- icc_diag_tumor %>% mutate(missing = apply(icc_diag_tumor,1,sumna)) %>% select(record_id,redcap_event_name,form,missing)

###rename
names(icc_diag_tumor)[names(icc_diag_tumor) =="icc_diag_tumor_size_y"]="Tumor Size"
names(icc_diag_tumor)[names(icc_diag_tumor) =="icc_diag_tumor_size"]="Tumor size (in centimeters)"

#####Function to list fields with 'NA' value
x = icc_diag_tumor
fields <- test_proc(x)

icc_diag_tumor_miss1 <- cbind(icc_diag_tumor_miss1,fields)
miss_icc_diag <- rbind(icc_diag_miss1, icc_diag_figo_miss1, icc_diag_tumor_miss1)
miss_icc_diagnosis <- ddply(miss_icc_diag, .(record_id, redcap_event_name, form), summarize,fields = paste(fields, collapse="; "), missing = sum(missing))
#############################################
#####Visit form
### Height missing
visit_ht <- juth_data %>% filter(redcap_event_name == "enrollment_arm_1" | redcap_event_name == "visit_arm_2") %>% 
                          mutate(form = "Visit form") %>% select(record_id, visit_ex_height, redcap_event_name, form)

###rename
names(visit_ht)[names(visit_ht) =="visit_ex_height"]="Height"

#####Function to list fields with 'NA' value
x=visit_ht
fields <- test_proc(x)
visit_ht_miss<- data.frame(visit_ht, missing = apply(visit_ht,1,sumna))  %>% 
                select(record_id,redcap_event_name, form, missing) %>% 
                filter(missing >'0')
visit_ht_miss <- cbind(visit_ht_miss,fields)

### Weight missing
visit_wt <- juth_data %>% filter(redcap_event_name == "enrollment_arm_1" | redcap_event_name == "visit_arm_2") %>% 
                          mutate(form = "Visit form") %>% select(record_id, visit_ex_weight, redcap_event_name, form)

###rename
names(visit_wt)[names(visit_wt) =="visit_ex_weight"]="Weight"

#####Function to list fields with 'NA' value
x=visit_wt
fields <- test_proc(x)
visit_wt_miss<- data.frame(visit_wt, missing = apply(visit_wt,1,sumna))  %>% 
                select(record_id,redcap_event_name, form, missing) %>% 
                filter(missing >'0')
visit_wt_miss <- cbind(visit_wt_miss,fields)

###hiv and icc missing
visit_hivy <- juth_data %>% filter(redcap_event_name == "enrollment_arm_1" | redcap_event_name == "visit_arm_2") %>% 
                            mutate(form = "Visit form") %>% select(record_id, visit_hiv_y, redcap_event_name, form)
###rename
names(visit_hivy)[names(visit_hivy) =="visit_hiv_y"]="Is the patient HIV positive at time of Visit "

#####Function to list fields with 'NA' value
x=visit_hivy
fields <- test_proc(x)
visit_hivy_miss<- data.frame(visit_hivy, missing = apply(visit_hivy,1,sumna))  %>% select(record_id,redcap_event_name, form, missing) %>% filter(missing >'0')
visit_hivy_miss <- cbind(visit_hivy_miss,fields)



###HIV Diagnosis is Yes - Either HIV Treatment start date is missing or HIV Treatment Regimen is missing
hiv_diagnosis_y <- sqldf(" select h.record_id,
                                  redcap_event_name,
                                  r.enr_otherid,
                                  hiv_year
                         from cohort_hivpos h
                         left join
                         juth_data j on h.record_id = j.record_id
                         left join 
                         rec_oth_id r  on r.record_id = j.record_id ")

hiv_treatment_y <- juth_data %>% filter(visit_hiv_rx_y == "1")  %>% 
                                 select (record_id,redcap_event_name,visit_hiv_rx_y, visit_hiv_rx_sd,visit_hiv_rx_reg)

visit_hiv_treatment_missing <- sqldf(" select distinct hivty.record_id,
                                               hivty.redcap_event_name,
                                               hy.enr_otherid, 
                                               hivty.visit_hiv_rx_y, 
                                               hivty.visit_hiv_rx_sd,
                                               hivty.visit_hiv_rx_reg 
                                     
                                     from cohort_hivpos h
                                     left join
                                     hiv_diagnosis_y hy on h.record_id = hy.record_id
                                     left join
                                     hiv_treatment_y hivty on hy.record_id = hivty.record_id
                                     where visit_hiv_rx_y = '1' and ((visit_hiv_rx_sd = '') or (visit_hiv_rx_reg is null)) ")

#introduce NA for blank in empty date field
visit_hiv_treatment_missing$visit_hiv_rx_sd <- as.numeric(visit_hiv_treatment_missing$visit_hiv_rx_sd)

###rename
names(visit_hiv_treatment_missing)[names(visit_hiv_treatment_missing) =="visit_hiv_rx_y"]="HIV Treatment"
names(visit_hiv_treatment_missing)[names(visit_hiv_treatment_missing) =="visit_hiv_rx_sd"]="Start date(HIV)"
names(visit_hiv_treatment_missing)[names(visit_hiv_treatment_missing) =="visit_hiv_rx_reg"]="Most recent Regimen(HIV)"
#####Function to list fields with 'NA' value
x=visit_hiv_treatment_missing
fields <- test_proc(x)

visit_hiv_tmt_miss <- data.frame(visit_hiv_treatment_missing, missing =apply(visit_hiv_treatment_missing,1,sumna)) %>% 
                      mutate(form = "Visit form") %>% select(record_id,redcap_event_name, form, missing)
visit_hiv_tmt_miss <- cbind(visit_hiv_tmt_miss,fields)

### Date of ICC Diagnosis is entered - 
###arm1
icc_diag_y_arm1 <- juth_data %>% filter(redcap_event_name == 'enrollment_arm_1')  %>% 
                                 select (record_id,redcap_event_name,enr_otherid, enr_icc_diag, icc_diag_d)

###arm2
juth_data_select_arm2 <- sqldf(" select record_id,
                                        redcap_event_name,
                                        enr_otherid,
                                        enr_icc_diag, 
                                        icc_diag_d,
                                        visit_cervix_biopsy_y,
                                        visit_cervix_biopsy_d,  
                                        visit_biopsy_result
                              from juth_data
                               where redcap_event_name in ('enrollment_arm_2', 'icc_diagnosis_arm_2','visit_arm_2','hiv_diagnosis_arm_2') ")


#selecting variables data for different events
juth_data_select_arm2_enr <- sqldf("select record_id,
                                           redcap_event_name,
                                           enr_otherid,
                                           enr_icc_diag
                                   from juth_data_select_arm2
                                   where redcap_event_name = 'enrollment_arm_2'")

juth_data_select_arm2_icc <- sqldf("select record_id,
                                           redcap_event_name,
                                           icc_diag_d
                                   from juth_data_select_arm2
                                   where redcap_event_name = 'icc_diagnosis_arm_2'")


juth_data_select_arm2_gp <- sqldf(" select enr.record_id,
                                           enr.redcap_event_name,
                                           enr_otherid,
                                           enr_icc_diag,
                                           icc_diag_d
                                  from juth_data_select_arm2_enr enr
                                  left join 
                                  juth_data_select_arm2_icc icc on enr.record_id = icc.record_id ")

##grouping ICC variables

icc_icc_diag_d_miss_arm2 <- juth_data_select_arm2_gp  %>% select(record_id,redcap_event_name,enr_otherid,enr_icc_diag,icc_diag_d)
#icc_y_arm1_arm2 <- rbind(enr_icc_diag_d_miss_arm1,icc_icc_diag_d_miss_arm2)
icc_y_arm1_arm2 <- rbind(icc_diag_y_arm1,icc_icc_diag_d_miss_arm2)
icc_y_arm1_arm2$enr_icc_diag[(icc_y_arm1_arm2$enr_icc_diag == '0') & (!is.na(icc_y_arm1_arm2$icc_diag_d))] <- NA
#####icc treatment missing
icc_tmt_arm1 <-  juth_data %>% filter(redcap_event_name == 'enrollment_arm_1' ) %>% 
                               select(record_id,enr_otherid,visit_tmt_icc_type)
icc_tmt_arm1 <- icc_tmt_arm1 %>% filter(icc_tmt_arm1$record_id %in% cohort_iccpos$record_id)
icc_tmt_arm2 <- juth_data %>% filter(redcap_event_name == 'visit_arm_2') %>% 
                              select(record_id,visit_tmt_icc_type)
icc_tmt_arm2 <-  sqldf(" select a2.record_id, 
                                r.enr_otherid,
                                a2.visit_tmt_icc_type
                         from icc_tmt_arm2 a2 left join rec_oth_id r on a2.record_id = r.record_id ")
#Subjects in arm2 will not have ICC treatment entered in Visit form
icc_tmt_arm2 <- icc_tmt_arm2 %>% filter(icc_tmt_arm2$record_id %in% cohort_iccpos$record_id)

icc_tmt_arm1_arm2 <- rbind(icc_tmt_arm1, icc_tmt_arm2)

icc_tmt_miss <- sqldf(" select icc.*,
                               tmt.visit_tmt_icc_type,
                               'Visit form' as form
                      from cohort_iccpos iccp
                      left join 
                      icc_y_arm1_arm2 icc on iccp.record_id = icc.record_id
                      left join 
                      icc_tmt_arm1_arm2 tmt on icc.record_id = tmt.record_id
                      where redcap_event_name = 'enrollment_arm_1' and (enr_icc_diag is null or
                            icc_diag_d is null or
                            visit_tmt_icc_type is null) ")

names(icc_tmt_miss)[names(icc_tmt_miss) =="visit_tmt_icc_type"]="Type of treatment for ICC received"
names(icc_tmt_miss)[names(icc_tmt_miss) =="enr_icc_diag"]="Have you ever been diagnosed with Cervical Cancer?"
names(icc_tmt_miss)[names(icc_tmt_miss) =="icc_diag_d"]="Date of ICC diagnosis"

x=icc_tmt_miss
fields <- test_proc(x)

visit_icc_tmt_miss <- data.frame(icc_tmt_miss) %>% mutate(form = 'Visit form', missing =apply(icc_tmt_miss,1,sumna))  %>% select(record_id,redcap_event_name, form, missing)
visit_icc_tmt_miss <- cbind(visit_icc_tmt_miss,fields)

#####lab specimen
visit_lab_miss <- sqldf(" select data.record_id, 
                                 data.redcap_event_name,
                                 r.enr_otherid,
                                 data.visit_lab_genomicspec_y 
                        from juth_data data
                        left join
                        rec_oth_id r on data.record_id = r.record_id 
                        where redcap_event_name in ('enrollment_arm_1', 'visit_arm_2') and
                              data.visit_lab_genomicspec_y is null ")
####rename
names(visit_lab_miss)[names(visit_lab_miss) =="visit_lab_genomicspec_y"] = "Specimens for Genomic analyses"

x=visit_lab_miss
fields <- test_proc(x)

visit_form_lab_miss <- data.frame(visit_lab_miss) %>% mutate(form = 'Visit form', missing =apply(visit_lab_miss,1,sumna))  %>% select(record_id,redcap_event_name, form, missing)
visit_form_lab_miss <- cbind(visit_form_lab_miss,fields)

####colposcopy arm1
cpspy_arm1 <- sqldf(" select data.record_id,
                             data.redcap_event_name,
                             r.enr_otherid, 
                             data.visit_cpspy_d, 
                             data.visit_cpspy_img,
                             data.visit_cpspy_dys_grade, 
                             data.visit_dys_class
                    from juth_data data
                    left join
                    rec_oth_id r on data.record_id = r.record_id
                    where (redcap_event_name ='enrollment_arm_1') and
                          (visit_cpspy_d is null or 
                          visit_cpspy_img is null or 
                          visit_cpspy_dys_grade is null or
                          visit_dys_class is null) ")
cpspy_arm2 <- sqldf(" select data.record_id,
                             data.redcap_event_name,
                             r.enr_otherid, 
                             data.visit_cpspy_d,
                             data.visit_cpspy_img,
                             data.visit_cpspy_dys_grade, 
                             data.visit_dys_class
                    from juth_data data
                    left join
                    rec_oth_id r on data.record_id = r.record_id
                    where (redcap_event_name ='visit_arm_2') and
                          (visit_cpspy_d is null or 
                          visit_cpspy_img is null or 
                          visit_cpspy_dys_grade is null or
                          visit_dys_class is null) ")
cpspy_arm1_arm2 <- rbind(cpspy_arm1,cpspy_arm2)
####rename
names(cpspy_arm1_arm2)[names(cpspy_arm1_arm2) =="visit_cpspy_d"]="Date of Colposcopy"
names(cpspy_arm1_arm2)[names(cpspy_arm1_arm2) =="visit_cpspy_img"]="Colposcopic image"
names(cpspy_arm1_arm2)[names(cpspy_arm1_arm2) =="visit_cpspy_dys_grade"]="Colposcopy based dysplasia grade"
names(cpspy_arm1_arm2)[names(cpspy_arm1_arm2) =="visit_dys_class"]="Classification of Dysplasia"

x=cpspy_arm1_arm2
fields <- test_proc(x)

visit_form_cpspy_miss <- data.frame(cpspy_arm1_arm2) %>% mutate(form = 'Visit form', missing =apply(cpspy_arm1_arm2,1,sumna))  %>% select(record_id,redcap_event_name, form, missing)
visit_form_cpspy_miss <- cbind(visit_form_cpspy_miss,fields)

#####cervical biopsy 
######have to check if we need to filter this based on cohort identifying variables
###arm1
biopsy_arm1 <- sqldf(" select record_id,
                              redcap_event_name,
                              visit_cervix_biopsy_y,
                              visit_cervix_biopsy_d,
                              visit_biopsy_result,
                              visit_icc_y
                     from 
                     juth_data 
                     where (redcap_event_name = 'enrollment_arm_1') and
                           (((visit_cervix_biopsy_y = 0) and (visit_cervix_biopsy_d is not null)) or
                           ((visit_cervix_biopsy_y = 1) and (visit_cervix_biopsy_d is null)) or
                           ((visit_cervix_biopsy_y = 1) and (visit_cervix_biopsy_d is not null)) or
                           ((visit_cervix_biopsy_y is null) and (visit_cervix_biopsy_d is not null)) or
                           ((visit_cervix_biopsy_y is null) and (visit_cervix_biopsy_d is null))) ")
biopsy_arm1 <- biopsy_arm1[rowSums(is.na(biopsy_arm1)) > 0,]

##The following lines have to be added only if we restrict biopsy to icc+ cohort
# select iccp.record_id,
# redcap_event_name,
# visit_cervix_biopsy_y,
# visit_cervix_biopsy_d,
# visit_biopsy_result,
# visit_icc_y
# from cohort_iccpos iccp
# left join 
# juth_data j on iccp.record_id = j.record_id

###arm2
biopsy_arm2 <- sqldf(" select record_id,
                              redcap_event_name,
                              visit_cervix_biopsy_y,
                              visit_cervix_biopsy_d,
                              visit_biopsy_result,
                              visit_icc_y
                     from juth_data 
                     where (redcap_event_name = 'visit_arm_2') and 
                           (((visit_cervix_biopsy_y = 0) and (visit_cervix_biopsy_d is not null)) or
                           ((visit_cervix_biopsy_y = 1) and (visit_cervix_biopsy_d is null)) or
                           ((visit_cervix_biopsy_y = 1) and (visit_cervix_biopsy_d is not null)) or    
                           ((visit_cervix_biopsy_y is null) and (visit_cervix_biopsy_d is not null)) or
                           ((visit_cervix_biopsy_y is null) and (visit_cervix_biopsy_d is null))) ")
biopsy_arm2 <- biopsy_arm2[rowSums(is.na(biopsy_arm2)) > 0,]

biopsy_arm1_arm2 <- rbind(biopsy_arm1,biopsy_arm2)
####rename
names(biopsy_arm1_arm2)[names(biopsy_arm1_arm2) =="visit_cervix_biopsy_y"]="Was a Cervical Biopsy done?"
names(biopsy_arm1_arm2)[names(biopsy_arm1_arm2) =="visit_cervix_biopsy_d"]="Date of Biopsy"
names(biopsy_arm1_arm2)[names(biopsy_arm1_arm2) =="visit_biopsy_result"]="Biopsy results"
names(biopsy_arm1_arm2)[names(biopsy_arm1_arm2) =="visit_icc_y"]="Does the patient have ICC or CIN 2+ at time of visit?"

x=biopsy_arm1_arm2
fields <- test_proc(x)

visit_form_biopsy_miss <- data.frame(biopsy_arm1_arm2) %>% 
                          mutate(form = 'Visit form', missing =apply(biopsy_arm1_arm2,1,sumna))  %>% 
                          select(record_id,redcap_event_name, form, missing)
visit_form_biopsy_miss <- cbind(visit_form_biopsy_miss,fields)

miss_visit_form <- rbind(visit_ht_miss, visit_wt_miss, visit_hivy_miss, visit_hiv_tmt_miss, visit_icc_tmt_miss, visit_form_lab_miss,visit_form_cpspy_miss,visit_form_biopsy_miss)
miss_visit_form <- ddply(miss_visit_form, .(record_id, redcap_event_name, form), summarize,fields = paste(fields, collapse="<br/>"), missing = sum(missing))

#############################################
####study_end - outcome form
#####At the end of script; should create a dataframe with record_id and enr_otherid and combine with following dataframes
outcome <- juth_data %>% filter((redcap_event_name == 'study_end_arm_1')|(redcap_event_name == 'study_end_arm_2')) %>% 
                         select(record_id, redcap_event_name, final_completed_d, final_death_y, final_death_d, final_death_causes,  final_withdraw_y, final_withdraw_d, final_withdraw_reason,outcome_tmt)
#####Missing outcome form entry
outcome_d <- sqldf(" select rec_oth_id.record_id,
                            rec_oth_id.enr_otherid,
                            outcome.redcap_event_name,
                            final_completed_d, 
                            final_death_y, 
                            final_death_d, 
                            final_death_causes,  
                            final_withdraw_y, 
                            final_withdraw_d, 
                            final_withdraw_reason,
                            outcome_tmt
                   from rec_oth_id
                   left join outcome on outcome.record_id = rec_oth_id.record_id ")
outcome_missing <- outcome_d %>% filter(is.na(final_completed_d)) %>% select(record_id,redcap_event_name,final_completed_d,final_death_y, final_death_d,final_death_causes,final_withdraw_y, final_withdraw_d,final_withdraw_reason) 

outcome_form_miss <- outcome_missing %>% mutate(form ="outcome",  missing = apply(outcome_missing,1,sumna)) %>% select(record_id,redcap_event_name,form,missing)                                         
###rename
names(outcome_missing)[names(outcome_missing) =="final_completed_d"]="Please enter the date this form was completed"
names(outcome_missing)[names(outcome_missing) =="final_death_y"]="Has the patient died?"
names(outcome_missing)[names(outcome_missing) =="final_death_d"]="Date of Death"
names(outcome_missing)[names(outcome_missing) =="final_death_causes"]="Causes of Death"
names(outcome_missing)[names(outcome_missing) =="final_withdraw_y"]="Did the patient withdraw their consent from the study?"
names(outcome_missing)[names(outcome_missing) =="final_withdraw_d"]="Date consent withdrawn"
names(outcome_missing)[names(outcome_missing) =="final_withdraw_reason"]="Reason for consent withdrawal"
names(outcome_missing)[names(outcome_missing) =="outcome_tmt"]="What type of treatment did the patient receive?"
###print names of the fields with missing values
x = outcome_missing
fields <- test_proc(x)
outcome_form_miss <- cbind(outcome_form_miss,fields)

#####Death records
outcome_death <- sqldf(" select record_id, 
                                redcap_event_name,
                                final_death_d,
                                final_death_causes,
                                'outcome' as form
                       from outcome_d where (final_death_y = 1 and ((final_death_d is null)  or (final_death_causes is null))) ")

outcome_death_miss <- outcome_death %>% mutate(missing = apply(outcome_death,1,sumna)) %>% 
                                        select(record_id,redcap_event_name,form,missing)
###rename
names(outcome_death)[names(outcome_death) =="final_death_d"]="Date of Death"
names(outcome_death)[names(outcome_death) =="final_death_causes"]="Causes of Death"
###print names of the fields with missing values
x = outcome_death
fields <- test_proc(x)
outcome_death_miss <- cbind(outcome_death_miss,fields)


#####Withdrawn records
outcome_withdraw <- sqldf(" select record_id, 
                                  redcap_event_name,
                                  final_withdraw_d,
                                  final_withdraw_reason,
                                  'outcome' as form
                          from outcome_d where (final_withdraw_y = 1 and ((final_withdraw_d is null)  or (final_withdraw_reason is null))) ")

outcome_withdraw_miss <- outcome_withdraw %>% mutate(missing = apply(outcome_withdraw,1,sumna)) %>% select(record_id,redcap_event_name,form,missing)
###rename
names(outcome_withdraw)[names(outcome_withdraw) =="final_withdraw_y"]= "Did the patient withdraw their consent from the study?"
names(outcome_withdraw)[names(outcome_withdraw) =="final_withdraw_d"]= "Date consent withdrawn"
names(outcome_withdraw)[names(outcome_withdraw) =="final_withdraw_reason"]= "Reason for consent withdrawal"
###print names of the fields with missing values
x = outcome_withdraw
fields <- test_proc(x)
outcome_withdraw_miss <- cbind(outcome_withdraw_miss,fields)

#####Outcome treatment missing in records
outcome_trmt <- outcome_d %>% filter ((!is.na(final_completed_d)) & (is.na(outcome_tmt))) %>% select(record_id, redcap_event_name, final_completed_d, outcome_tmt)
outcome_tmt_miss <- outcome_trmt %>% mutate(form ="outcome",  missing = apply(outcome_trmt,1,sumna)) %>% select(record_id,redcap_event_name,form,missing)
###rename
names(outcome_trmt)[names(outcome_trmt) =="outcome_tmt"]="What type of treatment did the patient receive?"
x = outcome_trmt
fields <- test_proc(x)
outcome_tmt_miss <- cbind(outcome_tmt_miss,fields)

miss_outcome <- rbind(outcome_form_miss, outcome_death_miss, outcome_withdraw_miss, outcome_tmt_miss )

#### Combine all forms into one table
item_missing <- bind_rows(miss_enr_form,
                          miss_hiv_diagnosis,
                          miss_icc_diagnosis,
                          miss_visit_form,
                          miss_outcome) 
#write_csv(item_missing, '~/Desktop/juth_item_missing.csv')

### Add u54 subject ID and prep the data for R markdown
u54_id <- mydata %>% select(record_id, enr_otherid)

agg_missing <- sqldf(" select i.record_id as Record_id, 
                              u.enr_otherid as Other_ID,
                              c.cohort as Cohort_group,
                              i.redcap_event_name as Event,
                              form as Form, 
                              fields as Fields,
                              missing as Missing
                     from item_missing i
                     left join cohorts c on i.record_id = c.record_id
                     left join u54_id u on i.record_id =u.record_id
                     group by i.record_id, form
                     order by i.record_id ")
#write_csv(agg_missing, '~/Desktop/juth_agg_missing.csv')

# agg_missing$Form_fields <- paste("-", agg_missing$Event," :: ",agg_missing$Form,":",agg_missing$Fields)
# agg_missing <- ddply(agg_missing, .(Record_id, Other_ID), summarize, Form_fields = paste(Form_fields, collapse=" <br/> ", sep = "<br/>"), missing = sum(Missing))
# # agg_missing <- ddply(agg_missing, .(Record_id, Other_ID,Cohort_group), 
# #                      summarize, 
# #                      Form_fields = paste(Form_fields, collapse=" <br/>"), 
# #                      missing = sum(Missing))
agg_missing$Fields <- paste(agg_missing$Fields,  sep = "*")
wide_agg_missing<- agg_missing

#create markdown report
rmarkdown::render('juth_dq_p2.Rmd', output_dir = 'output/juth', run_pandoc = TRUE)
new <- paste("output/juth/juth_dq_p2_",Sys.Date() ,".html", sep = "")
file.rename('output/juth/juth_dq_p2.html', new)
