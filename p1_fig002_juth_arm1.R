
####script for Plot starts here

mycumdata <- mydata[mydata$redcap_event_name=='enrollment_arm_1',]

# Only consented
#mycumdata <- mycumdata[mycumdata$consented_y=='1',]

# Sort by consent_data
mycumdata <- mycumdata[sort.list(mycumdata$enr_enroll_dasPOSIXct),]

# Plot cumsum
mycumdata$count = 1
png('output/fig001_juth_arm1.png',width =800,height = 900, res=150)
plot(mycumdata$enr_enroll_dasPOSIXct,cumsum(mycumdata$count),
     type = 's', lty = 1, lwd = 4, col = '#7a97b7', ann = FALSE, bty = 'L',
     xlim = c(as.POSIXct('2018-06-01'),as.POSIXct(Sys.Date()+7)), xaxs = 'i',
     ylim = c(0,50), yaxs = 'i'
)
#abline(h=seq(0,10,100),lty=1,lwd=1)
#abline(h=seq(0,2000,50),lty=2,lwd=0.5)
dev.off()

rm(list = c('mycumdata'))