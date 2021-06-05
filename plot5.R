# Exploratory Data Analysis - Final Course Project Submission

## Plot No. 5 - How have emissions from motor vehicle sources changed 
## from 1999–2008 in Baltimore City?

### Download data files, read data and remove temporary folders
library (dplyr)
library (ggplot2)
library (RColorBrewer)
library (grDevices)
library (scales)
library (ggpubr)
library (lubridate)

URL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(URL, "./data.zip")
unzip ("./data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
unlink("./data.zip"); unlink ("summarySCC_PM25.rds")
unlink("Source_Classification_Code.rds")

### Plot the exploratory charts
mv_cat <- grep ("Mobile - On-Road",SCC[,4])
mv_code <- SCC[mv_cat,1]
mv_name<- SCC[mv_cat,c(1,4)]
data_mv <- data.frame(filter(NEI,SCC %in% mv_code,
                               fips == "24510"))
data_mv <- merge(data_mv,mv_name, by = "SCC")
data_mv <- data.frame(data_mv%>%
                                group_by(year)%>%
                                mutate("Annual_Emissions" = sum(Emissions)
                                ))
data_mv$year <- ymd(paste(data_mv$year,"01","01",sep="-"))

My_Theme = theme_bw() + theme(
        axis.title.x = element_text(size = 12, 
                                    face="bold"),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 360,
                                    face="bold"),
        title = element_text (size=16, face="bold"),
        legend.position = c(.85,.85))

g1 <- ggplot(data_mv, aes(x = year,
                          y = Emissions,
                          fill = EI.Sector)) + 
        My_Theme +
        geom_bar (position = "stack",
                  stat = 'identity') +
        labs (y = "Emissions", x = "Year") +
        labs (title = "Total motor vehicle emissions in Baltimore (TPA)",
              fill = "Sector")

windows ()        
print (g1)

dev.copy (png, "plot5.png", width = 1024, height = 768)
dev.off()
