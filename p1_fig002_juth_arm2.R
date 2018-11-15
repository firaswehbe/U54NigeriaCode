mycumdata_arm2 <- mydata_arm2[mydata_arm2$redcap_event_name=='enrollment_arm_2',]

# Only consented
#mycumdata <- mycumdata[mycumdata$consented_y=='1',]

# Sort by consent_data
mycumdata_arm2 <- mycumdata_arm2[sort.list(mycumdata_arm2$enr_enroll_dasPOSIXct),]

# Plot cumsum
mycumdata_arm2$count = 1
png('output/fig001_juth_arm2.png',width =800,height = 900, res=150)
plot(mycumdata_arm2$enr_enroll_dasPOSIXct,cumsum(mycumdata_arm2$count),
     type = 's', lty = 1, lwd = 4, col = '#7a97b7', ann = FALSE, bty = 'L',
     xlim = c(as.POSIXct('2018-06-01'),as.POSIXct(Sys.Date()+7)), xaxs = 'i',
     ylim = c(0,50), yaxs = 'i'
)
#abline(h=seq(0,10,100),lty=1,lwd=1)
#abline(h=seq(0,2000,50),lty=2,lwd=0.5)
dev.off()

rm(list = c('mycumdata_arm2'))