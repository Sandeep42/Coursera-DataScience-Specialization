# This program uses the preprocess fucntion, and plots the second requried plot.

source("preprocess.R")
subdf<- preprocess()
png(filename = "plot2.png", width = 480, height = 480)
hist(subdf$Global_active_power,xlab = "Global Active Power(kilowatts)", col="RED", main ="Global active power")
dev.off()
title("")
plot(x=(subdf$DateTime),y=subdf$Global_active_power,type="l",ylab="Global Active Power (kilowatts)",xlab="")
hist(subdf$Global_active_power,xlab = "Global Active Power(kilowatts", col="RED")
