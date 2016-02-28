# This program uses the preprocess fucntion, and plots the first requried plot.

source("preprocess.R")
subdf<- preprocess()
png(filename = "plot1.png", width = 480, height = 480)
plot(x=(subdf$DateTime),y=subdf$Global_active_power,type="l",ylab="Global Active Power (kilowatts)",xlab="")
dev.off()
