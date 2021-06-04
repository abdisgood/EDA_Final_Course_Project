# Exploratory Data Analysis - Final Course Project Submission

## Plot No. 1 - Have total emissions from PM2.5 decreased 
## in the United States from 1999 to 2008? Using the base 
## plotting system, make a plot showing the total PM2.5 
## emission from all sources for each of the years 1999, 
## 2002, 2005, and 2008.

### Download data files, read data and remove temporary folders
URL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(URL, "./data.zip")
unzip ("./data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
unlink("./data.zip"); unlink ("summarySCC_PM25.rds")
unlink("Source_Classification_Code.rds")

### Plot the exploratory charts
total.em <- with(NEI, tapply(Emissions, year, sum))
total.em <- data.frame ("Year" = row.names(total.em),
                        "Emissions" = total.em)
row.names(total.em) = NULL

with (total.em, plot (Year, Emissions,
                 xlab = "Year", ylab = "PM2.5",
                 pch = 20,
                 type = "h",
                 lwd = 15,
                 col = "SteelBlue",
                 main = "Total emissions of PM2.5, in tons"))

dev.copy (png, "plot1.png", width= 1024, height = 768)
dev.off()

### Check data structure
dim(NEI); dim(SCC)
str (NEI); str (SCC)
scc.codes<- unique(NEI$SCC)
dim(scc.codes)
length (scc.codes)
unique(NEI$Pollutant)
unique(NEI$type)
mean(is.na(NEI$Pollutant))
summary(NEI)
total.em
