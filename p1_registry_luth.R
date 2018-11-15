# Script to run the "Project 1 - LUTH Report" (p1_report001_luth)
rm(list = ls())

# Set the variables you need
myfields = c(
  'record_id',
  'case_id',
  'enr_site',
  'enr_dob_d',
  'enr_gender',
  #'race',
  #'ethnicity',
  #'consented_y',
  'enr_enroll_d')

# Set the events you need
myevents = c(
  'enrollment_arm_1'
)

#Load redcap data 'arm_1' for Project1- HCC
source('p1_rcload_arm1.R')


# Applying functions for date format and factors
mydata$enr_siteasfactor <- makeColAsRCFactor('enr_site') # Affiliate - factor
mydata$enr_enroll_dasPOSIXct <- makeColAsPOSIXct('enr_enroll_d') # Enrolled Date- POSIXct
mydata$enr_genderasfactor <- makeColAsRCFactor('enr_gender') # Gender - factor
mydata$enr_dob_dasPOSIXct <- makeColAsPOSIXct('enr_dob_d') # Data of Birth - POSIXct
mydata$yobasinteger <- as.integer(strftime(mydata$enr_dob_dasPOSIXct,format = '%Y')) # Year of Birth - integer

#script for plotting 'arm_1' enrollment
source('p1_fig002_arm1.R')

#script for Project1 - luth arm2
source('p1_registry_luth_arm_2.R')

### script for RMD document with LUTH - 'arm_1' and 'arm_2' plots and table with total enrollment by gender

library(knitr)
rmarkdown::render('p1_report002_luth.Rmd', output_dir = 'output', run_pandoc = TRUE)

