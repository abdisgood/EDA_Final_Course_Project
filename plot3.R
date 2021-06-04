# Exploratory Data Analysis - Final Course Project Submission

## Plot No. 3 - Of the four types of sources indicated by the
## type (point, nonpoint, onroad, nonroad) variable, which of
## these four sources have seen decreases in emissions from 
## 1999–2008 for Baltimore City? Which have seen increases 
## in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

### Download data files, read data and remove temporary folders
library(dplyr)
library (ggplot2)
library (RColorBrewer)
library (grDevices)
URL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(URL, "./data.zip")
unzip ("./data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
unlink("./data.zip"); unlink ("summarySCC_PM25.rds")
unlink("Source_Classification_Code.rds")

### Plot the exploratory charts
btm.em <- NEI %>% filter (fips == "24510")
btm.em <- group_by(btm.em, type, year) %>%
                       summarize("Emissions" = sum(Emissions))
row.names(btm.em) = NULL

cols <- brewer.pal(11, "RdBu")
pal <- colorRampPalette(cols)

My_Theme = theme(
        axis.title.x = element_text(size = 16, 
                                    face="bold"),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16, 
                                    angle = 360, 
                                    face="bold"),
        title = element_text (size=24, face="bold"))

g <- ggplot(btm.em, aes(year,Emissions)) + My_Theme
g <- g + facet_grid(.~type)
g <- g + geom_line(size=1, linetype=3, show.legend=F)
g <- g + geom_point (aes(col = -Emissions, size=3), show.legend=F)
g <- g + labs (x = "Year")
g <- g + labs (title = "Total Emissions in Baltimore, by type")
print (g)

dev.copy (png, "plot3.png", width = 1024, height = 768)
dev.off()
