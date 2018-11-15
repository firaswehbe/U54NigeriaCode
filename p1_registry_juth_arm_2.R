# Script to run the Global Registry Report (Report-001)
#rm(list = ls())

# Set the variables you need
myfields_arm2 = c(
  'record_id',
  'case_id',
  'enr_site',
  #'disease_site',
  'enr_dob_d',
  'enr_gender',
  #'race',
  #'ethnicity',
  #'consented_y',
  'enr_enroll_d')

# Set the events you need
myevents_arm2 = (
  'enrollment_arm_2'
)


#Load redcap data 'arm_2' for Project1- HCC
source('p1_rcload_juth_arm2.R')

# Applying functions for date format and factors
mydata_arm2$enr_siteasfactor <- makeColAsRCFactor('enr_site') # Affiliate - factor
mydata_arm2$enr_enroll_dasPOSIXct <- makeColAsPOSIXct('enr_enroll_d') # Enrolled Date- POSIXct
mydata_arm2$enr_genderasfactor <- makeColAsRCFactor('enr_gender') # Gender - factor
mydata_arm2$enr_dob_dasPOSIXct <- makeColAsPOSIXct('enr_dob_d') # Data of Birth - POSIXct
mydata_arm2$yobasinteger <- as.integer(strftime(mydata_arm2$enr_dob_dasPOSIXct,format = '%Y')) # Year of Birth - integer


#script for plotting 'arm_2' enrollment
source('p1_fig002_juth_arm2.R')
