## read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Plot1

# # Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# # Using the base plotting system, make a plot showing the total PM2.5 emission from all 
# sources for each of the years 1999, 2002, 2005, and 2008.
## read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
#create year summary df
yearsum <- NEI %>% group_by(year) %>% summarise(totemissions = sum(Emissions))
#open file
png(file = "plot1.png")
#create barplot
barplot(yearsum$totemissions, names.arg = yearsum$year, main = "Total PM 2.5 Emissions in the United States",
        ylab = "Amount of PM2.5 in Tons", col = "blue")
dev.off() #close file
#Yes, emissions have decreased

#Plot2
# # Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

## read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#subset to Baltimore
bmore <- NEI[NEI$fips == "24510",]
library(dplyr)
#get year summaries for Baltimore
bmoreyearsum <- bmore %>% group_by(year) %>% summarise(totemissions = sum(Emissions))
#open file
png(file = "plot2.png")
#create a barplot
barplot(bmoreyearsum$totemissions, names.arg = bmoreyearsum$year, main = "Total PM 2.5 Emissions in Baltimore",
        ylab = "Amount of PM2.5 in Tons", col = "wheat")
dev.off() #close file
#Emissions have declined, though not consistently. They are lower in 2008 than 1999

#Plot3
# # Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) varia
# ble, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a
# plot answer this question.
## read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(ggplot2)
#use dply to aggregate
library(dplyr)
#subset to Baltimore
bmore <- NEI[NEI$fips == "24510",]
#aggregate by year and type w sum of emissions
bmoretype <- bmore %>% group_by(year, type) %>% summarize(total = sum(Emissions))
png(file = 'plot3.png') #open file
#use ggplot to plot line plot by type
ggplot(data = bmoretype, aes(x= year, y = total, color = type)) + geom_line() + 
  labs(x = "Year", y = "Total PM2.5 in Tons", title = "Baltimore City Emissions by Type", color = "Type")
dev.off() #close file
#All but point have shown declines.

#Plot 4
# # Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
## read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#Merge datasets NEI and SCC on SCC
merged <- merge(NEI, SCC, by = "SCC" , all.x = TRUE)
#subset to coal emissions only
coal <- merged[grep("Coal", merged$EI.Sector),]
#calculate total by year
coaltotal <- coal %>% group_by(year) %>% summarise(total = sum(Emissions))
#opem file 
png(file = 'plot4.png')
#create line chart
ggplot(data = coaltotal, aes(x= year, y = total)) + geom_line() + 
  labs(x = "Year", y = "Total PM2.5 in Tons", title = "Coal Emissions in the United States", color = "Type")
dev.off() #close file
#Coal emissions declined sharply from 2005 to 2008, after holding fairly contant from 1999- 2005

#Plot 5
# # How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
## read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
#merge NEI and SCC on SCC
merged <- merge(NEI, SCC, by = "SCC" , all.x = TRUE)
#subset to baltimore
bmore <- merged[merged$fips == '24510',]
#subset to motor vehicles only
vehicles <- bmore[grep("Vehicle", bmore$EI.Sector),]
#get yearly totals
vehiclestotal <- vehicles %>% group_by(year) %>% summarise(total = sum(Emissions))
#open file
png(file = "plot5.png")
#create a line plt
ggplot(data = vehiclestotal, aes(x= year, y = total)) + geom_line() +
  labs(x = "Year", y = "Total PM2.5 in Tons", title = "Baltimore City Motor Vehicle Emissions 1999-2008")
dev.off() #close file
#emissions have dropped sharply between 1999-2002, and fallen slightly since then



#Plot 6
# # Compare emissions from motor vehicle sources in Baltimore City with emissions from motor
# vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city has see
# n greater changes over time in motor vehicle emissions?
## read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(dplyr)
#merge NEI and SCC on SCC
merged <- merge(NEI, SCC, by = "SCC" , all.x = TRUE)
#subset to baltimore and LA
cities <- merged[merged$fips == '24510' | merged$fips == '06037',]
#subset to motor vehicles only
vehicles <- cities[grep("Vehicle", cities$EI.Sector),]
#get yearly totals by city
vehiclestotal <- vehicles %>% group_by(year, fips) %>% summarise(total = sum(Emissions))
png(file="plot6.png") #open file
#plot a line graph by city
ggplot(data = vehiclestotal, aes(x= year, y = total, color = fips)) + geom_line() +
  scale_color_discrete(labels = c("LA", "Baltimore")) +
  labs(x = "Year", y = "Total PM2.5 in Tons", title = "Motor Vehicle Emissions 1999-2008", color = "City")
dev.off() #close file
#LA has seen greater variance than Baltimore, but a smaller difference from 1999-2008 than Baltimore








