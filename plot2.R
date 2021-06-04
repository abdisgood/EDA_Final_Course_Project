# Exploratory Data Analysis - Final Course Project Submission

## Plot No. 2 - Have total emissions from PM2.5 decreased in the 
## Baltimore City, Maryland (fips == "24510" from 1999 to 2008? 
## Use the base plotting system to make a plot answering this 
## question.

### Download data files, read data and remove temporary folders
library(dplyr)
URL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(URL, "./data.zip")
unzip ("./data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
unlink("./data.zip"); unlink ("summarySCC_PM25.rds")
unlink("Source_Classification_Code.rds")

### Plot the exploratory charts
btm.em <- NEI %>% filter (fips == "24510")
btm.em <- with(btm.em, tapply(Emissions, year, sum))
btm.em <- data.frame ("Year" = row.names(btm.em),
                        "Emissions" = btm.em)
row.names(btm.em) = NULL
btm.em <- filter(btm.em,Year == c("1999","2008"))

with (btm.em, plot (Year, Emissions,
                      xlab = "Year", ylab = "PM2.5",
                      pch = 20,
                      type = "h",
                      lwd = 15,
                      col = "SteelBlue",
                      main = "Baltimore - Total emissions of PM2.5, in tons"))


dev.copy (png, "plot2.png", width = 1024, height = 768)
dev.off()
