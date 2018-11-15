## LUTH API
library(REDCapR)
rc_credentials <- read.csv(
  file='config/main2.token',header = FALSE,
  stringsAsFactors = FALSE,col.names = c("var","val"))

rownames(rc_credentials)<-rc_credentials$var



mydata = REDCapR::redcap_read(
  redcap_uri = rc_credentials[ 'url','val'], 
  token = rc_credentials[ 'token','val'],
  fields = myfields,
  events = myevents
)$data

mymetadata = REDCapR::redcap_metadata_read(
  redcap_uri = rc_credentials[ 'url','val'], token = rc_credentials[ 'token','val']
)$data

rm(list = c("rc_credentials"))

makeColAsPOSIXct <- function(columnname) {
  as.POSIXct( mydata[,columnname], format = '%Y-%m-%d' )
}

makeColAsRCFactor <- function(columnname) {
  consent_choices <- REDCapR::checkbox_choices(
    mymetadata[mymetadata$field_name==columnname,'select_choices_or_calculations']
  )
  factor(
    mydata[,columnname], levels = consent_choices$id,
    ordered = TRUE, labels = consent_choices$label
  )
  
}