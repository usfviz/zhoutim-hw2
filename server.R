library(shiny)
library(scales)
library(ggplot2)
library(magrittr)
library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)

# setwd("~/documents/study/MSAN/coursework/spring/module-2/MSAN 622/hw2")
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

data <- pp_plotdata
data$`Life Expectancy` <- le_plotdata$`Life Expectancy`
data$`Fertility Rate` <- fr_plotdata$`Fertility Rate`

data <- merge(data, cr_data, by = 'Country Name', all = F)
data$Region <- droplevels(data$Region)

regions <- sort(unique(data$Region))
countries <- sort(unique(data$`Country Name`))

## stores the clicked countries
selected_countries <- character(0)

## controls opacity, stroke size and color of points
data$opacity <- 1
data$strokeSize <- 0.5
data$strokeColor <- 'UNSELECTED'

data <- as.data.table(data)
data <- data[order(Year, -Population)]

THEME <- theme(
	panel.background = element_rect(fill = 'white'),
	panel.grid.major = element_line(color = 'grey80', size = 0.25),
	panel.grid.minor = element_blank(),

	axis.title.x = element_text(size = 18, vjust = -5),
	axis.title.y = element_text(size = 18, vjust = 5),

	axis.text.x = element_text(size = 14, margin = margin(t = 10)),
	axis.text.y = element_text(size = 14, margin = margin(r = 10)),

	axis.line = element_line(size = 1, color = 'grey40'),

	legend.background = element_rect(fill = "white", color = "grey"),
	legend.margin = margin(0.5, 1, 0.5, 1, 'cm'),
	legend.position = c(0.91, 0.8),
	legend.title = element_blank(),
	legend.text = element_text(size = 14),
	legend.key = element_rect(fill = "white", colour = NULL),
	legend.key.width = unit(1, 'line'),

	plot.margin = unit(c(1, 1, 1, 1), 'cm')
)

server <- function(input, output) {

	## Set up buffer, to keep the hovers/click
	click_pos <- reactiveValues(coords = NULL)
	hover_pos <- reactiveValues(coords = NULL)

	## Save the hovers/click, once it occurs.
	observeEvent(eventExpr = input$plot_click,
				 handlerExpr = {
					 	click_pos$coords <- input$plot_click
				 }
	)

	observeEvent(eventExpr = input$plot_hover,
				 handlerExpr = {
					 	hover_pos$coords <- input$plot_hover
				 }
	)

	getPlotData <- reactive({
		if (!is.null(input$regionSel)) {
			data[!data$Region %in% input$regionSel, ]$opacity <- 0.15
			data[data$Region %in% input$regionSel, ]$opacity <- 1
		}

		## limit to the year
		data <- data[data$Year == input$yearSlider, ]

		## clean NAs
		data <- data[!is.na(data$Population), ]
		data <- data[!is.na(data$`Life Expectancy`), ]
		data <- data[!is.na(data$`Fertility Rate`), ]

		return(data)
	})

	getMaxSize <- reactive({
		baseScale <- max(data[data$Year == '1960', ]$Population, na.rm = T) -
					 min(data[data$Year == '1960', ]$Population, na.rm = T)

		newScale <- max(data[data$Year == input$yearSlider, ]$Population, na.rm = T) -
					min(data[data$Year == input$yearSlider, ]$Population, na.rm = T)

		return(32*sqrt(newScale/baseScale))
	})

	output$countrySel <- renderUI({
		selectizeInput(inputId = 'countrySel',
					   label = 'Select countries to highlight',
					   choices = c("Choose a country" = "", as.character(countries)),
					   multiple = T,
					   selected = NULL)
	})

	output$regionSel <- renderUI({
		checkboxGroupInput(inputId = 'regionSel',
						   label = 'Select regions to highlight',
						   choices = regions,
						   selected = NULL)
	})

	output$dataPlot <- renderPlot({
		plotdata <- getPlotData()
		popScale <- getMaxSize()

		hovered_country <- click_plot <- hover_plot <- NULL

		## CLICK
		if (!is.null(click_pos$coords)) {
			np <- nearPoints(plotdata, click_pos$coords,
							 threshold = 20,
							 xvar = 'Life Expectancy', yvar = 'Fertility Rate')

			click_pos$coords <- NULL

			if (nrow(np)) {
				selected_country <- np$`Country Name`[1]

				## we have to assign globally to store it permanently
				if (selected_country %in% selected_countries) {
					selected_countries <<- selected_countries[-which(selected_countries == as.character(selected_country))]
				} else {
					selected_countries <<- unique(c(selected_countries, as.character(selected_country)))
				}
			}
		}

		## HOVER
		if (!is.null(hover_pos$coords)) {
			np <- nearPoints(plotdata, hover_pos$coords,
							 threshold = 20,
							 xvar = 'Life Expectancy', yvar = 'Fertility Rate')

			if (nrow(np)) {
				hovered_country <- as.character(np$`Country Name`[1])
			}
		}

		## highlight the selected points
		plotdata[plotdata$`Country Name` %in% c(hovered_country, selected_countries, input$countrySel), 'strokeSize'] <- 3
		plotdata[plotdata$`Country Name` %in% c(hovered_country, selected_countries, input$countrySel), 'strokeColor'] <- 'SELECTED'

		labeldata_sel <- plotdata[plotdata$`Country Name` %in% input$countrySel, ]
		labeldata_click <- plotdata[plotdata$`Country Name` %in% selected_countries, ]
		labeldata_hover <- plotdata[plotdata$`Country Name` == hovered_country, ]

		country_sel_label <- geom_label(data = labeldata_sel,
									   aes(label = `Country Name`),
									   size = 5,
									   nudge_x = 1, nudge_y = 0.5)

		click_label <- geom_label(data = labeldata_click,
								 aes(label = `Country Name`),
								 size = 5,
								 nudge_x = 1, nudge_y = 0.5)

		hover_label <- geom_label(data = labeldata_hover,
								 aes(label = `Country Name`),
								 size = 5,
								 nudge_x = 1, nudge_y = 0.5)

		p <- ggplot(data = plotdata,
					aes(x = `Life Expectancy`,
						y = `Fertility Rate`)) +

			geom_point(aes(size = Population,
						   fill = Region,
						   stroke = strokeSize,
						   color = strokeColor,
						   alpha = I(opacity)),

					   pch = 21) +

			country_sel_label + click_label + hover_label +

			scale_size(range = c(2, popScale)) +
			scale_x_continuous(limits = c(10, 90),
							   breaks = seq(10, 90, 5),
							   labels = sprintf('%.2g', seq(10, 90, 5))) +

			scale_y_continuous(limits = c(0.5, 9),
							   breaks = seq(0.5, 9, 0.5),
							   labels = sprintf('%.2g', seq(0.5, 9, 0.5))) +

			scale_fill_manual(values = c("#3C5CA6", "#E41A1C", "#FF7F00", "#4DAF4A",
										 "#990099", "#0099C6", "#E05DB0"),
							  labels = regions,
							  guide = guide_legend(
							  			name = 'top',
							  			override.aes = list(
							  							shape = rep(22, 7),
							  							size = rep(7, 7)
							  						   )
							  		  )
							  ) +

			scale_color_manual(values = c('#000000', '#191919')) +

			guides(size = F, color = F, alpha = F) +
			THEME

		return(p)
	})
}
