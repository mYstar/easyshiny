# simple test script

library(easyshiny)
library(ggplot2)

es_init()

es_read('statistic.csv')

es_add_plot({
  data <- statistic()
  ggplot(data, aes(Simulation.Time, Test)) +
    geom_line()
})

es_add_plot({
  ggplot(statistic(), aes(Test)) +
    geom_histogram()
})

es_start()

