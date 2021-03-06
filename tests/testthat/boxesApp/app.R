library(shiny)
library(shinyBS)
library(easyshiny)
library(ggplot2)

es_init()
es_renderText({'This is a simple test if elements are inserted into the app.'}, box = 'Text')
es_add_input(textInput, inputId = 'title', label = 'Title:', box = 'Plot')
es_renderPlot({qplot(mtcars$mpg) + ggtitle(label = input$title)}, box = 'Plot')
es_start(title = 'tabsApp')
