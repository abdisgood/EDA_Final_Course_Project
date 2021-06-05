# Exploratory Data Analysis - Final Course Project Submission

## Plot No. 6 - Compare emissions from motor vehicle sources in 
## Baltimore City with emissions from motor vehicle sources in 
## Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?

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
                             fips %in% c("24510","06037")))
data_mv <- merge(data_mv,mv_name, by = "SCC")
data_mv$fips <- factor(data_mv$fips, labels = c("Los Angeles", "Baltimore"))

cols <- brewer.pal(4,"Pastel1")

My_Theme = theme_bw() + theme(
        axis.title.x = element_text(size = 12, 
                                    face="bold"),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 360,
                                    face="bold"),
        title = element_text (size=16, face="bold"),
        legend.position = "right")

g1 <- ggplot(data_mv, aes(x = year, 
                          y = Emissions, 
                          fill = EI.Sector)) + 
        My_Theme +
        facet_wrap(~fips, ncol = 2, scales = "free") +
        geom_bar (position="stack", 
                  stat = "identity")+
        scale_fill_manual (values = cols)+
        labs (y = "Emissions", x = "Year", fill = "Sector" ) +
        theme (axis.text.x = element_blank(),
               axis.title.x = element_blank())+
        labs (title = "Total motor vehicle emissions across 
              Los Angeles County and Baltimore City (TPA)")

windows ()        
print (g1)

dev.copy (png, "plot6.png", width = 1024, height = 768)
dev.off()
