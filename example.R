# simple test script

library(easyshiny)
library(ggplot2)

es_init()

es_read('bike_buyers.csv')

es_add_plot({
  ggplot(bike_buyers(), aes(x=Commute.Distance, fill=Purchased.Bike)) +
    geom_bar(position = 'fill')
})

es_add_plot({
  ggplot(bike_buyers(), aes(x=Age, y=Income)) +
    geom_point() +
    geom_smooth()
})

es_start(title = 'Bikes')

