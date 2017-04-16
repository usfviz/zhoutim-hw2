library(shiny)
library(ggplot2)
library(magrittr)
library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)

ui <- fluidPage(
	 
	titlePanel(strong('Gapminder Interactive Visualization')),
	
	br(),	
	br(),	
	
	sidebarLayout(
		sidebarPanel(
			tags$style(" .well { background-color: white;
					   			 border: 0px;
					   			 webkit-box-shadow: none;
					   			 box-shadow: none; } "),

			
			uiOutput(outputId = 'regionSel'),
			br(),
			br(),
			uiOutput(outputId = 'countrySel'),			
			width = 2
		),
		
		mainPanel(
			column(12, align = 'center',
				plotOutput(outputId = 'dataPlot',
						   height = '768px',
						   width = '90%',
						   click = clickOpts(id = 'plot_click'),
						   hover = hoverOpts(id = 'plot_hover',
						   				  	 delay = 100,
						   				  	 delayType = 'debounce')),
				
				sliderInput(inputId = 'yearSlider',
							label = NULL, sep = "", ticks = F,
							width = '85%',
							min = 1960,
							max = 2014,
							value = 1960,
							step = 1,
							animate = animationOptions(interval = 300, loop = F))
			), width = 10
		)
	)	
)
