# Exploratory Data Analysis - Final Course Project Submission

## Plot No. 4 - Across the United States, how have emissions from coal 
## combustion-related sources changed from 1999–2008?

### Download data files, read data and remove temporary folders
library (dplyr)
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
coal_cat <- grep ("Coal",SCC[,4])
coal_code <- SCC[coal_cat,1]
data_coal <- data.frame(filter(NEI,SCC %in% coal_code))
data_coal <- data.frame(data_coal%>%
                        group_by(year)%>%
                        mutate("Annual_Emissions" = sum(Emissions)
                               ))
                        
cols <- brewer.pal(11, "RdBu")
pal <- colorRampPalette(cols)

My_Theme = theme_bw() + theme(
        axis.title.x = element_text(size = 12, 
                                    face="bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 14, angle = 360,
                                    face="bold"),
        title = element_text (size=16, face="bold"))

g <- ggplot(data_coal, aes(x =fips)) + 
        My_Theme +
        facet_wrap(~year, ncol = 4) +
        geom_hline (aes(yintercept=Annual_Emissions,
                        col="red",
                        lwd = 2),
                        show.legend = F)+
        geom_point (aes(y=Emissions, 
                        col= "steelblue",
                        size=1,
                        alpha = 1/2), show.legend=F) +
        labs (x = "US County Code") +
        labs (title = "Total coal combusion emissions across United States")
        

windows ()
print (g)

dev.copy (png, "plot4.png", width = 1024, height = 768)
dev.off()
