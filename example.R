# simple test script

library(easyshiny)
library(ggplot2)

es_init()

es_read('statistics.csv')

es_add_plot({
  # data <- inputdata()
  ggplot(mpg, aes(drv, cty)) +
    geom_point()
})

es_start()

