# This program uses the preprocess fucntion, and plots the third requried plot.

source("preprocess.R")
subdf<- preprocess()
png(filename = "plot3.png", width = 480, height = 480)
plot(x= subdf$DateTime, y= subdf$Sub_metering_1, type = "l", xlab = "", 
     ylab = "Energy Sub metering")
lines(x= subdf$DateTime, y= subdf$Sub_metering_2, col= "RED")
lines(x= subdf$DateTime, y= subdf$Sub_metering_3, col= "BLUE")
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty = c(1,1,1),
       lwd = c(1.5,1.5,1.5),col = c("black","red","blue"),cex = 0.75)
dev.off()
