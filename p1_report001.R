mydata <- read.csv('input/p1export.csv')

mydata_arm1 <- mydata[mydata$redcap_event_name=='enrollment_arm_1',]
mydata_arm3 <- mydata[mydata$redcap_event_name=='enrollment_arm_2',]

mydata_arm1$enr_hiv_serology_y <- factor(mydata_arm1$enr_hiv_serology_y, levels = c(0,1), labels = c('HIV -', 'HIV +'))
mydata_arm1$enr_hcc_serology_y <- factor(mydata_arm1$enr_hcc_serology_y, levels = c(0,1), labels = c('No HCC', 'HCC'))

tabulate_serology <- table(mydata_arm1$enr_hiv_serology_y,mydata_arm1$enr_hcc_serology_y)

print(tabulate_serology)
