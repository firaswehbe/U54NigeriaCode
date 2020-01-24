rm(list = ls())

library(plyr)
source('fields_j.R')
myurl <- scan("config/redcap_url", what="")
mytoken <- scan("config/luthclinical_token", what="")

mydata = REDCapR::redcap_read(
  redcap_uri = myurl, 
  token = mytoken,
  fields =my_fields_j
)$data

mymetadata = REDCapR::redcap_metadata_read(
  redcap_uri = myurl, token = mytoken
)$data

rm(list = c("mytoken"))


luth_data <- mydata

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
      pcol <- do.call(paste, c(as.list(col), sep = " ; "))
      fields <- append(fields,pcol)
    }}else{
      fields<-character()
    }
  return (fields)
}

#fields <- test_proc(x)

###Enrollment form - Missing fields
enrform_otherid_missing <- sqldf("select record_id,
                                 redcap_event_name,
                                 enr_otherid, 
                                 enr_site, 
                                 enr_enroll_d,
                                 enr_dob_d,
                                 enr_gender,
                                 enr_marital,
                                 enr_occupation,
                                 enr_edu,
                                 enr_income_y
                                 from luth_data
                                 where enrollment_complete = '2' and 
                                 (redcap_event_name = 'enrollment_arm_1' or redcap_event_name = 'enrollment_arm_2') 
                                 and 
                                 ((enr_otherid is null) or
                                 (enr_site is null) or
                                 (enr_enroll_d is null) or
                                 (enr_dob_d is null) or
                                 (enr_gender is null) or
                                 (enr_marital is null) or
                                 (enr_occupation is null) or
                                 (enr_edu is null)) ")

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

###Function to list fields with 'NA' value
x = enrform_otherid_missing
fields <- test_proc(x)

miss_enr_form <- data.frame(enrform_otherid_missing, missing =apply(enrform_otherid_missing,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)
miss_enr_oth_form <- cbind(miss_enr_form, fields)

#####Enrollment - serology missing
enrform_hiv_y_missing <- sqldf("select record_id, redcap_event_name, enr_hiv_serology_y, enr_hcc_serology_y
                               from luth_data where enr_hiv_serology_y is null and redcap_event_name = ('enrollment_arm_1' or  'enrollment_arm_2')")


enrform_hcc_y_missing <- sqldf("select record_id, redcap_event_name, enr_hiv_serology_y, enr_hcc_serology_y
                               from luth_data where enr_hcc_serology_y is null and redcap_event_name = ('enrollment_arm_1' or  'enrollment_arm_2')")




enrform_hiv_hcc_missing <- rbind(enrform_hiv_y_missing, enrform_hcc_y_missing)

#enrform_hiv_hcc_missing
miss_ser_missing <- data.frame(enrform_hiv_hcc_missing, missing =apply(enrform_hiv_hcc_missing,1,sumna))%>%
  mutate(form ="Enrollment Form")%>% 
  select(record_id,redcap_event_name, form, missing)

######rename
names(enrform_hiv_hcc_missing)[names(enrform_hiv_hcc_missing) =="enr_hiv_serology_y"]="Is the patient HIV positive"
names(enrform_hiv_hcc_missing)[names(enrform_hiv_hcc_missing) =="enr_hcc_serology_y"]="Does the patient have HCC"


#####Function to list fields with 'NA' value
x = enrform_hiv_hcc_missing
fields <- test_proc(x)

miss_ser_missing <- cbind(miss_ser_missing, fields) 



#####Enrollment form - Risk Factors and comorbid medical conditions

enrform_risk_factors <- sqldf("select record_id, redcap_event_name, enr_hcc_hepb_y,
                              enr_hcc_hepc_y,
                              enr_hcc_herbaldrug_y,
                              enr_hcc_hccfhx_y,
                              enr_hcc_alcliver_y,
                              enr_hcc_fattyliver_y,
                              enr_liverdis_other,
                              enr_hiv_msm_y,
                              enr_hiv_idu_y,
                              enr_hiv_nmdu_y,
                              enr_hiv_hemophilia_y,
                              enr_hiv_transfusion_y,
                              enr_hiv_highrisksex_y,
                              enr_hiv_iatrogenic_y,
                              enr_hiv_prenatal_y,
                              enr_hiv_hcexp_y,
                              enr_hiv_other,
                              enr_liver_hepbimm,
                              enr_liver_hepaimm,
                              enr_comorbid_ckd_y,
                              enr_comorbid_renaltx_y,
                              enr_comorbid_cad_y,
                              enr_comorbid_hyperlipid_y,
                              enr_comorbid_obese_y,
                              enr_comorbid_lung_y,
                              enr_comorbid_dm_y
                              from luth_data  
                              where redcap_event_name in ('enrollment_arm_1','enrollment_arm_2') and ((enr_hcc_hepb_y is null) or
                              (enr_hcc_hepc_y is null) or
                              (enr_hcc_herbaldrug_y is null) or
                              (enr_hcc_hccfhx_y is null) or
                              (enr_hcc_alcliver_y is null) or
                              (enr_hcc_fattyliver_y is null) or
                              (enr_liverdis_other is null) or
                              (enr_hiv_msm_y is null) or
                              (enr_hiv_idu_y is null) or
                              (enr_hiv_nmdu_y is null) or
                              (enr_hiv_hemophilia_y is null) or
                              (enr_hiv_transfusion_y is null) or
                              (enr_hiv_highrisksex_y is null) or
                              (enr_hiv_iatrogenic_y is null) or
                              (enr_hiv_prenatal_y is null) or
                              (enr_hiv_hcexp_y is null) or
                              (enr_hiv_other is null) or
                              (enr_liver_hepbimm is null) or
                              (enr_liver_hepaimm is null) or
                              (enr_comorbid_ckd_y is null) or
                              (enr_comorbid_renaltx_y is null) or
                              (enr_comorbid_cad_y is null) or
                              (enr_comorbid_hyperlipid_y is null) or
                              (enr_comorbid_obese_y is null) or
                              (enr_comorbid_lung_y is null) or
                              (enr_comorbid_dm_y is null))")
names(enrform_risk_factors)[names(enrform_risk_factors) == "enr_liverdis_other"] = "Other liver disease"
miss_enr_risk_form <- data.frame(enrform_risk_factors, missing =apply(enrform_risk_factors,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing) #, Var)


#####rename
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hcc_hepb_y"]="Viral Hepatitis B co-infection"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hcc_hepc_y"]="Viral Hepatitis C co-infection"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hcc_herbaldrug_y"]="Current Herbal Drug Use"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hcc_hccfhx_y"]="Family history of HCC?"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hcc_alcliver_y"]="Alcoholic liver disease"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hcc_fattyliver_y"]="Non-alcoholic fatty liver disease"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_liverdis_other"]="Other liver disease"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_msm_y"]="MSM?"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_idu_y"]="Current or past IV Drug Use"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_nmdu_y"]="Current or past non-medical drug use"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_hemophilia_y"]="Hemophilia"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_transfusion_y"]="Blood transfusion"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_highrisksex_y"]="High-risk sexual behavior (non MSM)"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_iatrogenic_y"]="Iatrogenic"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_prenatal_y"]="Prenatally acquired"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_hcexp_y"]="Health care exposure"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_hiv_other"]="Other HIV, HBV, HCV risks"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_liver_hepbimm"]="Hepatitis B Immunization series administered"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_liver_hepaimm"]="Hepatitis A Immunization series administered"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_comorbid_ckd_y"]="Chronic kidney disease"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_comorbid_renaltx_y"]="Renal replacement therapy"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_comorbid_cad_y"]="Coronary artery disease"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_comorbid_hyperlipid_y"]="Dyslipidemia"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_comorbid_obese_y"]="Obesity"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_comorbid_lung_y"]="Chronic lung disease/condition"
names(enrform_risk_factors)[names(enrform_risk_factors) =="enr_comorbid_dm_y"]="Diabetes mellitus"

########Function to list fields with 'NA' value
x = enrform_risk_factors
fields <- test_proc(x)

miss_enr_risk_form <- cbind(miss_enr_risk_form, fields)

########Risk factor - tobacco
enrform_risk_tob <- sqldf("select record_id, redcap_event_name,
                          enr_hcc_tobacco_y,
                          enr_packyr
                          from luth_data
                          where redcap_event_name in ('enrollment_arm_1','enrollment_arm_2') and (
                          (enr_hcc_tobacco_y is null) or
                          (enr_hcc_tobacco_y = '1' and enr_packyr is null))")
miss_enr_risk_tob <- data.frame(enrform_risk_tob, missing =apply(enrform_risk_tob,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing)                               


####rename
names(enrform_risk_tob)[names(enrform_risk_tob) =="enr_hcc_tobacco_y"]="Current tobacco use"
names(enrform_risk_tob)[names(enrform_risk_tob) =="enr_packyr"]="Pack-year history"
#####Function to list fields with 'NA' value
x = enrform_risk_tob
fields <- test_proc(x)
miss_enr_risk_tob <- cbind(miss_enr_risk_tob, fields)

########Risk factor - Alcohol
enrform_risk_alc <- sqldf("select record_id, redcap_event_name,
                          enr_hcc_alcohol_y,
                          enr_alcohol_qty,
                          enr_alcohol_yrs
                          from luth_data
                          where redcap_event_name in ('enrollment_arm_1','enrollment_arm_2') and (
                          (enr_hcc_alcohol_y is null) or
                          (enr_hcc_alcohol_y = '1' and enr_alcohol_qty is null) or
                          (enr_hcc_alcohol_y = '1' and enr_alcohol_yrs is null))")
miss_enr_risk_alc <- data.frame(enrform_risk_alc, missing =apply(enrform_risk_alc,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing)                               


####rename
names(enrform_risk_alc)[names(enrform_risk_alc) =="enr_hcc_alcohol_y"]="Current alcohol use"
names(enrform_risk_alc)[names(enrform_risk_alc) =="enr_alcohol_qty"]="Alcohol quantity in grams"
names(enrform_risk_alc)[names(enrform_risk_alc) =="enr_alcohol_yrs"]="Alcohol duration in years"
#####Function to list fields with 'NA' value
x = enrform_risk_alc
fields <- test_proc(x)

miss_enr_risk_alc <- cbind(miss_enr_risk_alc, fields)

########Risk factor - Co-Morbidity
enr_risk_comorbid <- sqldf("select record_id, redcap_event_name,
                           enr_comorbid_malig_y,
                           enr_comorbid_malig_type
                           from luth_data  
                           where redcap_event_name in ('enrollment_arm_1','enrollment_arm_2') and (
                           (enr_comorbid_malig_y is null) or
                           (enr_comorbid_malig_y = '1' and enr_comorbid_malig_type is null))")
miss_enr_risk_comorbid <- data.frame(enr_risk_comorbid, missing =apply(enr_risk_comorbid,1,sumna)) %>% mutate (form ='Enrollment Form') %>% select(record_id,redcap_event_name, form, missing)                               
###rename
names(enr_risk_comorbid)[names(enr_risk_comorbid) =="enr_comorbid_malig_y"]="Malignancy other than HCC"
names(enr_risk_comorbid)[names(enr_risk_comorbid) =="enr_comorbid_malig_type"]="Malignancy type"
#####Function to list fields with 'NA' value
x = enr_risk_comorbid
fields <- test_proc(x)

miss_enr_risk_comorbid <- cbind(miss_enr_risk_comorbid, fields)
miss_enr_form <- rbind(miss_enr_oth_form, miss_ser_missing,
                       miss_enr_risk_form,miss_enr_risk_tob, miss_enr_risk_alc, miss_enr_risk_comorbid)

miss_enr_form = miss_enr_form %>% group_by(record_id,redcap_event_name,form,fields) %>% summarise(missing = sum(missing))

miss_enr_form <- ddply(miss_enr_form, .(record_id, redcap_event_name, form), summarize,fields = paste(fields, collapse="; "), missing = sum(missing))
miss_enr_form <- sqldf("select * from miss_enr_form order by record_id")
miss_enr_form <- miss_enr_form %>% select(record_id,redcap_event_name,form,missing,fields)

#######################################################

####HCC Diagnosis form
####HCC Diagnosis form - Missing CT findings
miss_ct_findings <- sqldf("select record_id, 
                          redcap_event_name,
                          hcc_ct_cirr_y,
                          hcc_ct_stea_y,
                          hcc_ct_asc_y,
                          hcc_ct_spl_y,
                          hcc_ct_var_y, 
                          hcc_pvinvasion_y,
                          hcc_pvthrombosis_y from luth_data 
                          where redcap_event_name in ('enrollment_arm_1','enrollment_arm_2') and 
                          (hcc_ct_cirr_y is null or
                          hcc_ct_stea_y is null or
                          hcc_ct_asc_y is null or
                          hcc_ct_spl_y is null or
                          hcc_ct_var_y is null or 
                          hcc_pvinvasion_y is null or
                          hcc_pvthrombosis_y is null)")

###rename
names(miss_ct_findings)[names(miss_ct_findings) =="hcc_ct_cirr_y"]="Cirrhosis"
names(miss_ct_findings)[names(miss_ct_findings) =="hcc_ct_stea_y"]="Steatosis"
names(miss_ct_findings)[names(miss_ct_findings) =="hcc_ct_asc_y"]="Ascites"
names(miss_ct_findings)[names(miss_ct_findings) =="hcc_ct_spl_y"]="Splenomegaly"
names(miss_ct_findings)[names(miss_ct_findings) =="hcc_ct_var_y"]="Varicies"
names(miss_ct_findings)[names(miss_ct_findings) =="hcc_pvinvasion_y"]="Portal vein invasion"
names(miss_ct_findings)[names(miss_ct_findings) =="hcc_pvthrombosis_y"]="Portal Vein thrombosis"
#####Function to list fields with 'NA' value
x = miss_ct_findings
fields <- test_proc(x)

miss_ct_findings_form <- data.frame(miss_ct_findings, missing =apply(miss_ct_findings,1,sumna)) %>% mutate(form ="HCC_diagnosis") %>% select(record_id,redcap_event_name, form, missing)
miss_ct_findings_form <- cbind(miss_ct_findings_form,fields)
####HCC Diagnosis form - Missing/Discrepancy in arterial_washout and bclc staging
enrform_hcc_ae_missing <- luth_data %>% filter(luth_data$enr_hcc_serology_y == 1 & (!hcc_artwashout_y %in% c(1,2,0) )) %>% select(record_id, redcap_event_name, hcc_artwashout_y )
#other way ####enrform_hcc_ae_missing <- luth_data %>% filter(luth_data$enr_hcc_serology_y == 1 & (hcc_artwashout_y %in% (NA) )) %>% select(record_id, redcap_event_name, hcc_artwashout_y )

###rename
names(enrform_hcc_ae_missing)[names(enrform_hcc_ae_missing) =="hcc_artwashout_y"]="Arterial enhancement with washout"
#####Function to list fields with 'NA' value
x = enrform_hcc_ae_missing
fields <- test_proc(x)

miss_hcc_ae_form <- data.frame(enrform_hcc_ae_missing, missing =apply(enrform_hcc_ae_missing,1,sumna)) %>% mutate(form ="HCC_diagnosis") %>% select(record_id,redcap_event_name, form, missing)
miss_hcc_ae_form$missing <- ifelse(miss_hcc_ae_form$missing == '0', '1', miss_hcc_ae_form$missing)
miss_hcc_ae_form$missing <- as.integer(miss_hcc_ae_form$missing)

miss_hcc_ae_form = cbind(miss_hcc_ae_form,fields)

####HCC Diagnosis form - Missing/Discrepancy in bclc staging
enrform_hcc_bclc_missing <- luth_data %>% filter(luth_data$enr_hcc_serology_y == 1 & !(luth_data$hcc_bclc %in% c(0,1,2,3,4 )))  %>% select(record_id, redcap_event_name, hcc_bclc )

###rename
names(enrform_hcc_bclc_missing)[names(enrform_hcc_bclc_missing) =="hcc_bclc"]="BCLC Stage"
#####Function to list fields with 'NA' value
x = enrform_hcc_bclc_missing
fields <- test_proc(x)

miss_hcc_bclc_form <- data.frame(enrform_hcc_bclc_missing, missing =apply(enrform_hcc_bclc_missing,1,sumna)) %>% mutate(form ="HCC_diagnosis") %>% select(record_id,redcap_event_name, form, missing)
miss_hcc_bclc_form$missing <- ifelse(miss_hcc_bclc_form$missing == '0', '1', miss_hcc_bclc_form$missing)
miss_hcc_bclc_form$missing <- as.integer(miss_hcc_bclc_form$missing)

miss_hcc_bclc_form = cbind(miss_hcc_bclc_form,fields)

#Sum two required fields(arterial washout and bclc staging) if missing in HCC Diagnosis form - 
miss_enr_ae_bclc <- rbind(miss_ct_findings_form,miss_hcc_ae_form,miss_hcc_bclc_form)
miss_hcc_enr_ae_bclc_agg = miss_enr_ae_bclc %>% group_by(record_id,redcap_event_name,form,fields) %>% summarise(missing = sum(missing)) %>% select(record_id,redcap_event_name,form,fields,missing)
miss_hcc_enr_ae_bclc_agg <- ddply(miss_hcc_enr_ae_bclc_agg, .(record_id, redcap_event_name, form), summarize,fields = paste(fields, collapse="; "), missing = sum(missing))

miss_hcc_enr_ae_bclc_agg <- miss_hcc_enr_ae_bclc_agg %>% select(record_id,redcap_event_name,form,missing,fields)

#######################################################
###HIV Diagnosis form
###HIV Diagnosis form - Enrollment arm 1
#luth_data_hiv_enr <- luth_data %>% filter(luth_data$redcap_event_name == "enrollment_arm_1"  & luth_data$hiv_aids_y == 1 & (hiv_prioroi_y == 0|hiv_prioroi_y == 2))  %>%  mutate(aids_group="Yes") %>% select(record_id,redcap_event_name,enr_otherid,hiv_aids_y, hiv_prioroi_y,hiv_pjp_y,hiv_mac_y,hiv_cmv_y,hiv_pml_y,hiv_candida_y,hiv_crypto_y,hiv_nhl_y,hiv_ks_y, hiv_ade_other)
luth_data_hiv_enr  <- sqldf("select record_id,redcap_event_name,enr_otherid,hiv_aids_y, hiv_prioroi_y,hiv_pjp_y,hiv_mac_y,hiv_cmv_y,hiv_pml_y,hiv_candida_y,hiv_crypto_y,hiv_nhl_y,hiv_ks_y, hiv_ade_other
                            from luth_data 
                            where redcap_event_name = 'enrollment_arm_1' and
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
                            (hiv_ade_other is null))")
#luth_data_hiv2_enr <- luth_data %>% filter(luth_data$redcap_event_name == "enrollment_arm_1"  & luth_data$hiv_aids_y == 0 & luth_data$hiv_prioroi_y == 1 & (hiv_pjp_y == 1 | hiv_mac_y == 1 | hiv_cmv_y == 1 | hiv_pml_y == 1 | hiv_candida_y == 1 | hiv_crypto_y == 1 | hiv_nhl_y == 1 | hiv_ks_y == 1 | hiv_ade_other == 1)) %>% select(record_id,redcap_event_name,enr_otherid,hiv_aids_y, hiv_prioroi_y,hiv_pjp_y,hiv_mac_y,hiv_cmv_y,hiv_pml_y,hiv_candida_y,hiv_crypto_y,hiv_nhl_y,hiv_ks_y, hiv_ade_other)
luth_data_hiv_enr$hiv_prioroi_y = NA
luth_data_hiv2_enr <- sqldf("select record_id,redcap_event_name,enr_otherid,hiv_aids_y, hiv_prioroi_y,hiv_pjp_y,hiv_mac_y,hiv_cmv_y,hiv_pml_y,hiv_candida_y,hiv_crypto_y,hiv_nhl_y,hiv_ks_y, hiv_ade_other
                            from luth_data 
                            where redcap_event_name = 'enrollment_arm_1' and
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
                            hiv_ks_y = 1)")
luth_data_hiv2_enr$hiv_aids_y = NA
hiv_diagnosis_miss_field <- rbind(luth_data_hiv2_enr,luth_data_hiv_enr) %>% select(record_id, redcap_event_name,hiv_aids_y, hiv_prioroi_y)

#hiv_diagnosis_miss_field[order(hiv_diagnosis_miss_field$record_id),]
hiv_diagnosis_miss1 <- hiv_diagnosis_miss_field %>% mutate(form ="HIV_diagnosis", missing = apply(hiv_diagnosis_miss_field,1,sumna)) %>% select(record_id,redcap_event_name,form,missing)

###rename
names(hiv_diagnosis_miss_field)[names(hiv_diagnosis_miss_field) =="hiv_prioroi_y"]="Prior Opportunistic Infections"
names(hiv_diagnosis_miss_field)[names(hiv_diagnosis_miss_field) =="hiv_aids_y"]="AIDS Diagnosis"

#####Function to list fields with 'NA' value
x = hiv_diagnosis_miss_field
fields <- test_proc(x)

hiv_diagnosis_miss1 <- cbind(hiv_diagnosis_miss1,fields)

#HIV Diagnosis form - Enrollment arm 2
luth_data_hiv_enr2 <- sqldf("select record_id,redcap_event_name,enr_otherid,hiv_aids_y, hiv_prioroi_y,hiv_pjp_y,hiv_mac_y,hiv_cmv_y,hiv_pml_y,hiv_candida_y,hiv_crypto_y,hiv_nhl_y,hiv_ks_y, hiv_ade_other
                            from luth_data 
                            where redcap_event_name = 'enrollment_arm_2' and
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
                            (hiv_ade_other is null))")
luth_data_hiv_enr2$hiv_prioroi_y = NA

luth_data_hiv2_enr2 <- sqldf("select record_id,redcap_event_name,enr_otherid,hiv_aids_y, hiv_prioroi_y,hiv_pjp_y,hiv_mac_y,hiv_cmv_y,hiv_pml_y,hiv_candida_y,hiv_crypto_y,hiv_nhl_y,hiv_ks_y, hiv_ade_other
                             from luth_data 
                             where redcap_event_name = 'enrollment_arm_2' and
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
                             hiv_ks_y = 1)")
luth_data_hiv2_enr2$hiv_aids_y = NA

hiv_diagnosis_miss_field2 <- rbind(luth_data_hiv2_enr2,luth_data_hiv_enr2) %>% select(record_id, redcap_event_name,hiv_aids_y, hiv_prioroi_y)
hiv_diagnosis_miss2 <- hiv_diagnosis_miss_field2 %>% mutate(form ="HIV_diagnosis",  missing = apply(hiv_diagnosis_miss_field2,1,sumna)) %>% select(record_id,redcap_event_name,form,missing)

###rename
names(hiv_diagnosis_miss_field2)[names(hiv_diagnosis_miss_field2) =="hiv_prioroi_y"]="Prior Opportunistic Infections"
names(hiv_diagnosis_miss_field2)[names(hiv_diagnosis_miss_field2) =="hiv_aids_y"]="AIDS Diagnosis"

#####Function to list fields with 'NA' value
x = hiv_diagnosis_miss_field2
fields <- test_proc(x)

hiv_diagnosis_miss2 <- cbind(hiv_diagnosis_miss2,fields)
hiv_diagnosis_miss <- rbind(hiv_diagnosis_miss1,hiv_diagnosis_miss2)
hiv_diagnosis_miss$missing = as.integer(hiv_diagnosis_miss$missing)

#######################################################

###Visit HCC form
###Visit HCC form - CPS/MELD score
visithcc_y_cps_meld_missing <- sqldf("select record_id, enr_otherid,visit_hcc_child_pugh, visit_hcc_meld
                                     from luth_data
                                     where enr_hcc_serology_y = '1' and( visit_hcc_child_pugh is null or visit_hcc_meld is null)
                                     ")
###rename
names(visithcc_y_cps_meld_missing)[names(visithcc_y_cps_meld_missing) =="visit_hcc_child_pugh"]="Child-Pugh Score"
names(visithcc_y_cps_meld_missing)[names(visithcc_y_cps_meld_missing) =="visit_hcc_meld"]="MELD Score"

visithcc_cps_meld_missing<- data.frame(visithcc_y_cps_meld_missing, missing =apply(visithcc_y_cps_meld_missing,1,sumna)) %>% mutate(form = "Visit HCC form", redcap_event_name = "visit_arm_1") %>% select(record_id,redcap_event_name,form, missing)

x = visithcc_y_cps_meld_missing
fields <- test_proc(x)

visithcc_cps_meld_missing <- cbind(visithcc_cps_meld_missing, fields)

#######################################################

###Visit HIV

#### confirm if it is on VIsit HIV or Visit HCC form????? It is confirmed that missing Liver Ultrasound findings are from Visit HIV form
###Visit HIV - HIV +ve - missing Liver Ultrasound findings
hiv_y <- luth_data %>% filter(enr_hiv_serology_y == "1") %>% select ( record_id,redcap_event_name, enr_otherid)
hiv_us <- luth_data %>% filter(visit_hiv_us___0 == 0 & visit_hiv_us___1 == 0 & visit_hiv_us___2 == 0 & visit_hiv_us___3 == 0 & visit_hiv_us___4 == 0 & visit_hiv_us___5 == 0 & visit_hiv_us___6 == 0 & visit_hiv_us___99 == 0) %>% select(record_id,visit_hiv_us___0,visit_hiv_us___1,visit_hiv_us___2,visit_hiv_us___3,visit_hiv_us___4,visit_hiv_us___5,visit_hiv_us___6,visit_hiv_us___99)
hiv_us <- hiv_us  %>% mutate(redcap_event_name = "visit_arm_2", visit_hiv_us ="NA")
hiv_us_missing <- sqldf("select hivy.record_id as record_id, us.redcap_event_name, us.visit_hiv_us
                        from
                        hiv_us us                      
                        left join
                        hiv_y hivy on hivy.record_id = us.record_id")
###rename
names(hiv_us_missing)[names(hiv_us_missing) =="visit_hiv_us"]="Liver Ultrasound Findings"
#####Function to list fields with 'NA' value
x=hiv_us_missing
fields <- test_proc(x)

hiv_us_missing <- cbind(hiv_us_missing,fields)  
hiv_us_miss <- hiv_us_missing %>% mutate(form ="Visit HIV form", missing = "1") %>% select(record_id,redcap_event_name, form, missing,fields)
hiv_us_miss$missing = as.integer(hiv_us_miss$missing)
visit_hiv_miss_ct_findings <- sqldf("select record_id, 
                                    redcap_event_name,
                                    visit_hiv_ct_cirr_y,
                                    visit_hiv_ct_stea_y,
                                    visit_hiv_ct_asc_y,
                                    visit_hiv_ct_spl_y,
                                    visit_hiv_ct_var_y,
                                    visit_hiv_artwashout_y,
                                    visit_hiv_pvinvasion_y,
                                    visit_hiv_pvthrombosis_y from luth_data 
                                    where redcap_event_name in ('visit_arm_2') and 
                                    (visit_hiv_ct_cirr_y is null or
                                    visit_hiv_ct_stea_y is null or
                                    visit_hiv_ct_asc_y is null or
                                    visit_hiv_ct_spl_y is null or
                                    visit_hiv_ct_var_y is null or 
                                    visit_hiv_artwashout_y is null or
                                    visit_hiv_pvinvasion_y is null or
                                    visit_hiv_pvthrombosis_y is null)")

###rename
names(visit_hiv_miss_ct_findings)[names(visit_hiv_miss_ct_findings) =="visit_hiv_ct_cirr_y"]="Cirrhosis?"
names(visit_hiv_miss_ct_findings)[names(visit_hiv_miss_ct_findings) =="visit_hiv_ct_stea_y"]="Steatosis?"
names(visit_hiv_miss_ct_findings)[names(visit_hiv_miss_ct_findings) =="visit_hiv_ct_asc_y"]="Ascites?"
names(visit_hiv_miss_ct_findings)[names(visit_hiv_miss_ct_findings) =="visit_hiv_ct_spl_y"]="Splenomegaly?"
names(visit_hiv_miss_ct_findings)[names(visit_hiv_miss_ct_findings) =="visit_hiv_ct_var_y"]="Varicies?"
names(visit_hiv_miss_ct_findings)[names(visit_hiv_miss_ct_findings) =="visit_hiv_artwashout_y"]="Arterial enhancement with washout"
names(visit_hiv_miss_ct_findings)[names(visit_hiv_miss_ct_findings) =="visit_hiv_pvinvasion_y"]="Portal vein invasion?"
names(visit_hiv_miss_ct_findings)[names(visit_hiv_miss_ct_findings) =="visit_hiv_pvthrombosis_y"]="Portal Vein thrombosis"
#####Function to list fields with 'NA' value
x=visit_hiv_miss_ct_findings
fields <- test_proc(x)

visit_hiv_miss_ct_findings <- cbind(visit_hiv_miss_ct_findings,fields)

visit_hiv_miss_ct_findings_form <- data.frame(visit_hiv_miss_ct_findings, missing =apply(visit_hiv_miss_ct_findings,1,sumna)) %>% mutate(form ="Visit HIV form") %>% select(record_id,redcap_event_name, form, missing,fields)

miss_visit_hiv_form <- rbind(hiv_us_miss,visit_hiv_miss_ct_findings_form)
miss_visit_hiv_form_agg <- ddply(miss_visit_hiv_form, .(record_id, redcap_event_name, form), summarize,fields = paste(fields, collapse="; "), missing = sum(missing))



#####Visit form
###height and weight missing
visit_ht <- luth_data %>% filter(redcap_event_name == "visit_arm_1" | redcap_event_name == "visit_arm_2") %>% mutate(form = "Visit form") %>% select(record_id, visit_ex_height, redcap_event_name, form)

###rename
names(visit_ht)[names(visit_ht) =="visit_ex_height"]="Height"
#####Function to list fields with 'NA' value
x=visit_ht
fields <- test_proc(x)

visit_ht_miss<- data.frame(visit_ht, missing = apply(visit_ht,1,sumna))  %>% select(record_id,redcap_event_name, form, missing) %>% filter(missing >'0')
visit_ht_miss <- cbind(visit_ht_miss,fields)
visit_wt <- luth_data %>% filter(redcap_event_name == "visit_arm_1" | redcap_event_name == "visit_arm_2") %>% mutate(form = "Visit form") %>% select(record_id, visit_ex_weight, redcap_event_name, form)

###rename
names(visit_wt)[names(visit_wt) =="visit_ex_weight"]="Weight"
#####Function to list fields with 'NA' value
x=visit_wt
fields <- test_proc(x)

visit_wt_miss<- data.frame(visit_wt, missing = apply(visit_wt,1,sumna))  %>% select(record_id,redcap_event_name, form, missing) %>% filter(missing >'0')
visit_wt_miss <- cbind(visit_wt_miss,fields)
###hiv and hcc missing
visit_hivy <- luth_data %>% filter(redcap_event_name == "visit_arm_1" | redcap_event_name == "visit_arm_2") %>% mutate(form = "Visit form") %>% select(record_id, visit_hiv_y, redcap_event_name, form)

###rename
names(visit_hivy)[names(visit_hivy) =="visit_hiv_y"]="Is the patient HIV positive at time of Visit "
#####Function to list fields with 'NA' value
x=visit_hivy
fields <- test_proc(x)

visit_hivy_miss<- data.frame(visit_hivy, missing = apply(visit_hivy,1,sumna))  %>% select(record_id,redcap_event_name, form, missing) %>% filter(missing >'0')
visit_hivy_miss <- cbind(visit_hivy_miss,fields)
visit_hccy <- luth_data %>% filter(redcap_event_name == "visit_arm_1" | redcap_event_name == "visit_arm_2") %>% mutate(form = "Visit form") %>% select(record_id, visit_hcc_y, redcap_event_name, form) 

###rename
names(visit_hccy)[names(visit_hccy) =="visit_hcc_y"]="Does the patient have HCC at time of visit?"
#####Function to list fields with 'NA' value
x=visit_hccy
fields <- test_proc(x)

visit_hccy_miss<- data.frame(visit_hccy, missing = apply(visit_hccy,1,sumna))  %>% select(record_id,redcap_event_name, form, missing) %>% filter(missing >'0')
visit_hccy_miss <- cbind(visit_hccy_miss,fields)
###ecog missing
hcc_diagnosis_y <- luth_data %>% filter(enr_hcc_serology_y == "1" & (redcap_event_name == "enrollment_arm_1" | redcap_event_name == "enrollment_arm_2")) %>% mutate(form = "Visit form") %>% select(record_id, form)
ecog_missing <- luth_data %>% filter(!(luth_data$visit_hcc_ecog %in% c(0,1,2,3,4 )) &((redcap_event_name == "visit_arm_1") | (redcap_event_name == "visit_arm_2"))) %>% select(record_id,redcap_event_name,visit_hcc_ecog)
visit_ecog_missing <- sqldf("select em.record_id,em.redcap_event_name,hdy.form, em.visit_hcc_ecog from hcc_diagnosis_y hdy join ecog_missing em on em.record_id = hdy.record_id where em.visit_hcc_ecog is null")

###rename
names(visit_ecog_missing)[names(visit_ecog_missing) =="visit_hcc_ecog"]="ECOG performance status"
#####Function to list fields with 'NA' value
x=visit_ecog_missing
fields <- test_proc(x)

visit_ecog_miss<- data.frame(visit_ecog_missing, missing =apply(visit_ecog_missing,1,sumna))  %>% select(record_id,redcap_event_name, form, missing)
visit_ecog_miss <- cbind(visit_ecog_miss,fields)
###fibroscan
fibroscan <-luth_data %>% filter(redcap_event_name == "visit_arm_1" | redcap_event_name == "visit_arm_2") %>% select (record_id, redcap_event_name, visit_inv_fibro)
visit_hcc_inv_missing <- sqldf("select hccdy.record_id,f.redcap_event_name, f.visit_inv_fibro from hcc_diagnosis_y hccdy join fibroscan f on hccdy.record_id = f.record_id ")

###rename
names(visit_hcc_inv_missing)[names(visit_hcc_inv_missing) =="visit_inv_fibro"]="Fibroscan score"
#####Function to list fields with 'NA' value
x=visit_hcc_inv_missing
fields <- test_proc(x)

visit_inv_miss <- data.frame(visit_hcc_inv_missing, missing =apply(visit_hcc_inv_missing,1,sumna)) %>% mutate(form = "Visit form") %>% select(record_id,redcap_event_name, form, missing) %>% filter(missing > '0')
visit_inv_miss <- cbind(visit_inv_miss,fields)
###HIV Diagnosis is Yes - Either HIV Treatment start date is missing or HIV Treatment Regimen is missing
hiv_diagnosis_y <- luth_data %>% filter(enr_hiv_serology_y == "1" & (redcap_event_name == "enrollment_arm_1" | redcap_event_name == "enrollment_arm_2")) %>% mutate(hiv_diagnosis = "Yes") %>% select(record_id, enr_otherid, hiv_diagnosis)
hiv_treatment_y <- luth_data %>% filter(visit_hiv_rx_y == "1")  %>% select (record_id,redcap_event_name,visit_hiv_rx_y, visit_hiv_rx_sd,visit_hiv_rx_reg)
visit_hiv_treatment_missing <- sqldf("select hivty.record_id,redcap_event_name, hy.enr_otherid, 
                                     hivty.visit_hiv_rx_y, 
                                     hivty.visit_hiv_rx_sd,
                                     hivty.visit_hiv_rx_reg from
                                     hiv_diagnosis_y hy
                                     left join
                                     hiv_treatment_y hivty on hy.record_id = hivty.record_id
                                     where visit_hiv_rx_y = '1' and ((visit_hiv_rx_sd = '') or (visit_hiv_rx_reg is null))")
#introduce NA for blank in empty date field
visit_hiv_treatment_missing$visit_hiv_rx_sd <- as.numeric(visit_hiv_treatment_missing$visit_hiv_rx_sd)

###rename
names(visit_hiv_treatment_missing)[names(visit_hiv_treatment_missing) =="visit_hiv_rx_y"]="HIV Treatment"
names(visit_hiv_treatment_missing)[names(visit_hiv_treatment_missing) =="visit_hiv_rx_sd"]="Start date"
names(visit_hiv_treatment_missing)[names(visit_hiv_treatment_missing) =="visit_hiv_rx_reg"]="Most recent Regimen"
#####Function to list fields with 'NA' value
x=visit_hiv_treatment_missing
fields <- test_proc(x)

visit_hiv_tmt_miss <- data.frame(visit_hiv_treatment_missing, missing =apply(visit_hiv_treatment_missing,1,sumna)) %>% mutate(form = "Visit form") %>% select(record_id,redcap_event_name, form, missing)
visit_hiv_tmt_miss <- cbind(visit_hiv_tmt_miss,fields)

### HBV Diagnosis is Yes - Either HBV Treatment start date is missing or HBV Treatment Regimen is missing
hbv_treatment_y <- luth_data %>% filter(visit_hbv_rx_y == "1")  %>% select (record_id,redcap_event_name,visit_hbv_rx_y, visit_hbv_rx_sd,visit_hbv_rx_reg)
hbv_record <- luth_data %>% filter(redcap_event_name == "enrollment_arm_1" | redcap_event_name == "enrollment_arm_2") %>% select(record_id, enr_otherid)
visit_hbv_treatment_missing <- sqldf("select hbvty.record_id,redcap_event_name, hbvrec.enr_otherid, hbvty.visit_hbv_rx_y as HBV_Treatment, hbvty.visit_hbv_rx_sd,visit_hbv_rx_reg from
                                     hbv_treatment_y hbvty 
                                     left join
                                     hbv_record hbvrec on hbvrec.record_id = hbvty.record_id 
                                     where visit_hbv_rx_y = '1' and ((visit_hbv_rx_sd = '') or (visit_hbv_rx_reg is null))")
#introduce NA for blank in empty date field
visit_hbv_treatment_missing$visit_hbv_rx_sd <- as.numeric(visit_hbv_treatment_missing$visit_hbv_rx_sd)

###rename
names(visit_hbv_treatment_missing)[names(visit_hbv_treatment_missing) =="visit_hbv_rx_y"]="HBV Treatment"
names(visit_hbv_treatment_missing)[names(visit_hbv_treatment_missing) =="visit_hbv_rx_sd"]="Start date"
names(visit_hbv_treatment_missing)[names(visit_hbv_treatment_missing) =="visit_hbv_rx_reg"]="Most recent Regimen"
#####Function to list fields with 'NA' value
x=visit_hbv_treatment_missing
fields <- test_proc(x)

visit_hbv_tmt_miss <- data.frame(visit_hbv_treatment_missing, missing =apply(visit_hbv_treatment_missing,1,sumna)) %>% mutate(form = "Visit form") %>% select(record_id,redcap_event_name, form, missing)
visit_hbv_tmt_miss <- cbind(visit_hbv_tmt_miss,fields)

###Jaundince and Encephaolopathy missing
visit_j <- luth_data %>% filter(redcap_event_name == "visit_arm_1" | redcap_event_name == "visit_arm_2") %>% mutate(form = "Visit form") %>% select(record_id, visit_sx_jaun_y, redcap_event_name, form) %>% filter(is.na(visit_sx_jaun_y))

###rename
names(visit_j)[names(visit_j) =="visit_sx_jaun_y"]="Jaundice"
#####Function to list fields with 'NA' value
x=visit_j
fields <- test_proc(x)

visit_j_miss<- data.frame(visit_j, missing = apply(visit_j,1,sumna))  %>% select(record_id,redcap_event_name, form, missing)
visit_j_miss <- cbind(visit_j_miss,fields)

visit_enc <- luth_data %>% filter(redcap_event_name == "visit_arm_1" | redcap_event_name == "visit_arm_2") %>% mutate(form = "Visit form") %>% select(record_id, visit_ex_liver_enc_y, redcap_event_name, form)

###rename
names(visit_enc)[names(visit_enc) =="visit_ex_liver_enc_y"]="Encephalopathy"
#####Function to list fields with 'NA' value
x=visit_enc
fields <- test_proc(x)

visit_enc_miss<- data.frame(visit_enc, missing = apply(visit_enc,1,sumna))  %>% select(record_id,redcap_event_name, form, missing) %>% filter(missing > 0)
visit_enc_miss <- cbind(visit_enc_miss,fields)


##Combining all dataframes in Visit form

miss_visit_form <- rbind(visit_ht_miss,visit_wt_miss,visit_hivy_miss, visit_hccy_miss, visit_j_miss, visit_enc_miss, visit_ecog_miss,visit_inv_miss,visit_hiv_tmt_miss,visit_hbv_tmt_miss)
miss_visit_form <- ddply(miss_visit_form, .(record_id, redcap_event_name, form), summarize,fields = paste(fields, collapse="; "), missing = sum(missing))

miss_visit_form_agg <- sqldf("select * from miss_visit_form where missing != 0")

#######################################################

#####Laboratory testing results form
#####(Laboratory testing results) # with CD4 and HIV RNA but HIV negative
hiv_diagnosis_n <- luth_data %>% filter((enr_hiv_serology_y == "1") & (redcap_event_name == "enrollment_arm_1" | redcap_event_name == "enrollment_arm_2")) %>% select(record_id, enr_otherid, enr_hiv_serology_y)
lab_result <- luth_data %>% filter(redcap_event_name == "visit_arm_1" | redcap_event_name == "visit_arm_2") %>% select(record_id,redcap_event_name, visit_lab_cd4cnt_res, visit_lab_hivrna_res)
hiv_n_cd4hiv <- sqldf("select lab.record_id , redcap_event_name, hivn.enr_otherid as U54_SubjectID, lab.visit_lab_cd4cnt_res as CD4cnt_Result, lab.visit_lab_hivrna_res as HIVRNA_Result from
                      hiv_diagnosis_n hivn 
                      left join
                      lab_result lab on hivn.record_id = lab.record_id
                      where lab.visit_lab_cd4cnt_res is null or
                      lab.visit_lab_hivrna_res is null")

###rename
names(hiv_n_cd4hiv)[names(hiv_n_cd4hiv) =="CD4cnt_Result"]="CD4 + Cell Count"
names(hiv_n_cd4hiv)[names(hiv_n_cd4hiv) =="HIVRNA_Result"]="HIV RNA"
#####Function to list fields with 'NA' value
x=hiv_n_cd4hiv
fields <- test_proc(x)

visit_hiv_n_cd4hiv_miss <- data.frame(hiv_n_cd4hiv, missing =apply(hiv_n_cd4hiv,1,sumna)) %>% mutate(form = "Laboratory Testing Results form") %>% select(record_id,redcap_event_name, form, missing)
visit_hiv_n_cd4hiv_miss <- cbind(visit_hiv_n_cd4hiv_miss,fields)

#####(Laboratory testing results) # All missing variables
lab1 <- sqldf("select record_id, redcap_event_name,
              visit_lab_hbsag_res,
              visit_lab_antihcv_res,
              visit_lab_inr_res,
              visit_lab_creatinine_res,
              visit_lab_hemoglobin_res,
              visit_lab_alt_res,
              visit_lab_pt_res,
              visit_lab_platelets_res,
              visit_lab_ast_res,
              visit_lab_albumin_res,
              visit_lab_tot_bil_res,
              visit_lab_tot_pro_res,
              visit_lab_afp_res
              from luth_data where redcap_event_name in ('visit_arm_1','visit_arm_2') and 
              (visit_lab_hbsag_res is null or
              visit_lab_antihcv_res is null or
              visit_lab_inr_res is null or
              visit_lab_creatinine_res is null or
              visit_lab_hemoglobin_res is null or
              visit_lab_alt_res is null or
              visit_lab_pt_res is null or
              visit_lab_platelets_res is null or
              visit_lab_ast_res is null or
              visit_lab_albumin_res is null or
              visit_lab_tot_bil_res is null or
              visit_lab_tot_pro_res is null or
              visit_lab_afp_res is null)")

###rename
names(lab1)[names(lab1) =="visit_lab_hbsag_res"]="HBsAg"
names(lab1)[names(lab1) =="visit_lab_antihcv_res"]="Anti-HCV"
names(lab1)[names(lab1) =="visit_lab_inr_res"]="INR"
names(lab1)[names(lab1) =="visit_lab_pt_res"]="PT"
names(lab1)[names(lab1) =="visit_lab_creatinine_res"]="Creatinine"
names(lab1)[names(lab1) =="visit_lab_hemoglobin_res"]="Hemoglobin"
names(lab1)[names(lab1) =="visit_lab_platelets_res"]="Platelets"
names(lab1)[names(lab1) =="visit_lab_alt_res"]="ALT"
names(lab1)[names(lab1) =="visit_lab_ast_res"]="AST"
names(lab1)[names(lab1) =="visit_lab_albumin_res"]="Albumin"
names(lab1)[names(lab1) =="visit_lab_tot_bil_res"]="Total Bilrubin"
names(lab1)[names(lab1) =="visit_lab_tot_pro_res"]="Total Protein"
names(lab1)[names(lab1) =="visit_lab_afp_res"]="AFP"
#####Function to list fields with 'NA' value
x=lab1
fields <- test_proc(x)

lab1 <- data.frame(lab1, missing =apply(lab1,1,sumna)) %>% mutate(form = "Laboratory Testing Results form") %>% select(record_id,redcap_event_name, form, missing)
lab1 <- cbind(lab1,fields)

lab2 <- sqldf("select record_id, redcap_event_name,
              
              visit_lab_hbeag_res,
              visit_lab_antihbe_res,
              visit_lab_hbvdna_res,
              visit_lab_antihdv_res 
              from luth_data where redcap_event_name in ('visit_arm_1','visit_arm_2') and 
              (visit_lab_hbsag_res ='1' or visit_lab_hbsag_res is null)")

###rename
names(lab2)[names(lab2) =="visit_lab_hbeag_res"]="HBeAg"
names(lab2)[names(lab2) =="visit_lab_antihbe_res"]="Anti-HBe"
names(lab2)[names(lab2) =="visit_lab_hbvdna_res"]="HBV DNA"
names(lab2)[names(lab2) =="visit_lab_antihdv_res"]="Anti-HDV"
#####Function to list fields with 'NA' value
x=lab2
fields <- test_proc(x)

lab2 <- data.frame(lab2, missing =apply(lab2,1,sumna)) %>% mutate(form = "Laboratory Testing Results form") %>% select(record_id,redcap_event_name, form, missing)
lab2 <- cbind(lab2,fields)

##Combine all dataframes from Laboratory Testing Results form
miss_lab_form <- rbind(visit_hiv_n_cd4hiv_miss, lab1, lab2)
miss_lab_form <- ddply(miss_lab_form, .(record_id, redcap_event_name, form), summarize,fields = paste(fields, collapse="; "), missing = sum(missing))
miss_lab_form_agg <- sqldf("select * from miss_lab_form where missing != 0")

#######################################################

###Outcome form

oc_missing1 <- luth_data %>% filter(final_death_y == "1") %>% mutate(death = "yes") %>% select (record_id, death, final_death_d, final_death_causes)
oc_missing2 <- luth_data %>% filter(redcap_event_name == "enrollment_arm_1" | redcap_event_name == "enrollment_arm_2") %>% select (record_id,redcap_event_name, enr_otherid)
oc <- left_join(oc_missing1,oc_missing2)
outcome_filled <- oc[,c("record_id","enr_otherid","death", "final_death_d", "final_death_causes")]
oc_rec <- length(outcome_filled$Record_id)
# There are `r oc_rec` records filled for outcome form.
outcome_missing <- sqldf("select record_id,redcap_event_name, enr_otherid, final_death_y, final_death_d, final_death_causes
                         from luth_data
                         where final_death_y = '1' and (final_death_d is null or final_death_causes is null)")
outcome_var_missing<- data.frame(outcome_missing, missing =apply(outcome_missing,1,sumna)) %>% mutate(form = "Outcome form") %>% select(record_id,redcap_event_name, form, missing)

###rename
names(outcome_missing)[names(outcome_missing) =="final_death_y"]="Has the patient died?"
names(outcome_missing)[names(outcome_missing) =="final_death_d"]="Date of death"
names(outcome_missing)[names(outcome_missing) =="final_death_causes"]="Causes of Death"
#####Function to list fields with 'NA' value
x = outcome_missing
fields <- test_proc(x)

outcome_var_missing <- cbind(outcome_var_missing,fields)


#### Combine all forms into one table
item_missing <- bind_rows(miss_enr_form,
                          miss_hcc_enr_ae_bclc_agg,
                          hiv_diagnosis_miss,
                          visithcc_cps_meld_missing,
                          miss_visit_hiv_form_agg,
                          miss_visit_form_agg,
                          miss_lab_form_agg,
                          outcome_var_missing) 
#write_csv(item_missing, '~/Desktop/luth_item_missing.csv')
### Add u54 subject ID and prep the data for R markdown
u54_id <- mydata %>% select(record_id, enr_otherid)
agg_missing <- sqldf("select i.record_id as Record_id, u.enr_otherid as Other_ID, redcap_event_name as Event, form as Form, fields as Fields, missing as Missing
                     from item_missing i
                     left join u54_id u on i.record_id =u.record_id
                     group by i.record_id, form
                     order by i.record_id")
#write_csv(agg_missing, '~/Desktop/luth_agg_missing.csv')

agg_missing$Form_fields <- paste(agg_missing$Event," :: ",agg_missing$Form,":",agg_missing$Fields)
agg_missing <- ddply(agg_missing, .(Record_id, Other_ID), summarize, Form_fields = paste(Form_fields, collapse=" || "), missing = sum(Missing))
wide_agg_missing<- agg_missing

#create markdown report
#rmarkdown::render('luth_dq_version2.Rmd', output_dir = 'output/luth', run_pandoc = TRUE)
#rmarkdown::render('luth_dq_invalid.Rmd', output_dir = 'output/luth', run_pandoc = TRUE)

rmarkdown::render('R:\PrevMed\Projects\QDSC\Projects\07 Nigeria U54\project1_version2_dataquality_reports\luth_dq_version2.Rmd', output_dir = 'R:\PrevMed\Projects\QDSC\Projects\07 Nigeria U54\project1_version2_dataquality_reports', run_pandoc = TRUE)

