# This function takes nothing, but returns the data frame containg the data of two dates from
# the dataset. It fetches the data from the internet, converts into the required form.


preprocess <- function() {

fileUrl <-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile = "./data.zip")

fileUrl <-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile = "./data.zip")
unzip("data.zip")
df <- read.csv("household_power_consumption.txt", sep = ";", header = TRUE, colClasses = c(rep("character",2),rep("numeric",7)), na.strings = "?")
df$DateTime <- paste(df$Date, df$Time, sep = " ")
df$Date <- NULL
df$Time <- NULL
df$DateTime = as.POSIXlt(df$DateTime,format="%d/%m/%Y %H:%M:%S")
subdf <- subset(df,DateTime$year==107 & DateTime$mon==1 & (DateTime$mday==1 | DateTime$mday==2))
rm(df)
subdf
}
}
