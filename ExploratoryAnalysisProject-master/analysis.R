# Downloading data
# I have added a read me document with the code in the same repository.

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl, destfile = "./data/data.zip")
unzip("data.zip")

# Reading them into files

clscode <- readRDS("./data/Source_Classification_Code.rds")
summarypm2.5 <- readRDS("./data/summarySCC_PM25.rds")


## Question-1

summary.by.year <- aggregate(Emissions ~ year, data = summarypm2.5, FUN = sum)
plot(summary.by.year)

# Loading libraries
library(ggplot2)
library(dplyr)


# Question 2

baltimore <- filter(summarypm2.5, fips == "24510") %>%  
                                    group_by(year) %>%
                                      summarise(sum(Emissions))
plot(baltimore)

# ggplot baltimore

baltimore.data <- filter(summarypm2.5, fips == "24510") %>% 
                                  group_by(year, type) %>%
                                  summarise(sum(Emissions))

qplot(x= baltimore.data$year, y= baltimore.data$`sum(Emissions)`,
      data = baltimore.data, color= type, geom_line(baltimore.data),
      geom = c("line","point"), xlab = "Year", ylab = "Total emission")

# Question 4




# Question 5

baltimore.road <- filter(summarypm2.5, fips == "24510", type== "ON-ROAD") %>% 
  group_by(year) %>%
  summarise(sum(Emissions))
# Plotting data

qplot(x= baltimore.road$year, y=baltimore.road$`sum(Emissions)`, geom = c("line","point"),
          xlab= "Year", ylab= "Total emission from motor vehicles")

# Question 6

la.road <- filter(summarypm2.5, fips == "06037", type == "ON-ROAD") %>%
              group_by(year)  %>%
                      summarise(sum(Emissions))
qplot(x= la.road$year, y=la.road$`sum(Emissions)`, geom = c("line","point"),
      xlab= "Year", ylab= "Total emission from motor vehicles")

# Plotting both graphs in the same windows

la.road$city <- "LA"
baltimore.road$city <- "baltimore"
complete <- rbind(la.road, baltimore.road)
qplot(x= complete$year, y= complete$`sum(Emissions)`, geom = c("line","point"),
      xlab = "Year", ylab= "Total emission from motor vehicles", color= city, 
      data = complete)