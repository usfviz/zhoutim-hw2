library(RColorBrewer)
library(ggplot2)
library(magrittr)
library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)

setwd("~/documents/study/MSAN/coursework/spring/module-2/MSAN 622/hw2")

le_data <- read.csv("data/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv")
fr_data <- read.csv("data/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv")
pp_data <- read.csv("data/API_SP.POP.TOTL_DS2_en_csv_v2.csv")
cr_data <- read.csv("data/Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", header = F)

le_data <- le_data[c(-2, -3, -4, -ncol(le_data))]
names(le_data) <- gsub('\\.', ' ', gsub('X([0-9]+?)', '\\1', colnames(le_data)))

fr_data <- fr_data[c(-2, -3, -4, -ncol(fr_data))]
names(fr_data) <- gsub('\\.', ' ', gsub('X([0-9]+?)', '\\1', colnames(fr_data)))

pp_data <- pp_data[c(-2, -3, -4, -ncol(pp_data))]
names(pp_data) <- gsub('\\.', ' ', gsub('X([0-9]+?)', '\\1', colnames(pp_data)))

cr_data <- cr_data[c('V5', 'V2')]
names(cr_data) <- c('Country Name', 'Region')
cr_data <- cr_data[cr_data$Region != '', ]

le_plotdata <- melt(le_data,
					id.vars = c('Country Name'),
					variable.name = 'Year',
					value.name = 'Life Expectancy')

fr_plotdata <- melt(fr_data,
					id.vars = c('Country Name'),
					variable.name = 'Year',
					value.name = 'Fertility Rate')

pp_plotdata <- melt(pp_data,
					id.vars = c('Country Name'),
					variable.name = 'Year',
					value.name = 'Population')

plotdata <- pp_plotdata

plotdata$`Life Expectancy` <- le_plotdata$`Life Expectancy`
plotdata$`Fertility Rate` <- fr_plotdata$`Fertility Rate`
plotdata$Population <- plotdata$Population

## so the high populations are plotted near the bottom
plotdata <- plotdata %>% arrange(-Population)
plotdata <- merge(plotdata, cr_data, by = 'Country Name', all = F)

plotdata$Opacity <- 1
# plotdata[plotdata$Region == 'East Asia & Pacific', ]$Opacity <- 1

THEME <- theme(
	panel.background = element_rect(fill = 'white'),
	panel.grid.major = element_line(color = 'grey80', size = 0.25),
	panel.grid.minor = element_line(color = 'grey80', size = 0.25)
)

baseScale <- max(plotdata[plotdata$Year == '1960', ]$Population, na.rm = T) -
			 min(plotdata[plotdata$Year == '1960', ]$Population, na.rm = T)

newScale <- max(plotdata[plotdata$Year == '2014', ]$Population, na.rm = T) -
			min(plotdata[plotdata$Year == '2014', ]$Population, na.rm = T)

p <- ggplot(data = plotdata[plotdata$Year == '2014', ],
			aes(x = `Life Expectancy`,
				y = `Fertility Rate`)) +
		
		geom_point(aes(size = `Population`,
					   fill = `Region`,
					   alpha = I(`Opacity`)),
				   pch = 21,
				   color = 'black') +
	
		scale_size(range = c(1, 30*newScale/baseScale)) +
	
		scale_x_continuous(limits = c(10, 90),
						   breaks = seq(10, 90, 5),
						   labels = sprintf('%.2g', seq(10, 90, 5))) +
	
		scale_y_continuous(limits = c(0.5, 9),
						   breaks = seq(0.5, 9, 0.5),
						   labels = sprintf('%.2g', seq(0.5, 9, 0.5))) +
	
		scale_fill_manual(values = c("#377EB8", "#E41A1C", "#4DAF4A", "#984EA3",
									 "#FF7F00", "#3C5CA6", "#E05DB0")) +
	
		guides(size = F, fill = F, alpha = F) +

		THEME

print(p)

