# simple test script

library(easyshiny)
library(ggplot2)

es_init()

es_read('bike_buyers.csv')

es_add_plot({
  ggplot(bike_buyers(), aes(x=Commute.Distance, fill=Purchased.Bike)) +
    geom_bar(position = 'fill')
}, tab = 'Bikes', box = 'Visualization')

es_add_plot({
  ggplot(bike_buyers(), aes(x=Age, y=Income)) +
    geom_point() +
    geom_smooth()
}, tab = 'Bikes', box = 'Visualization')

visuals <- get('visuals', envir = easyshiny:::appData)
# easyshiny:::es_build_ui('Bikes', visuals)
es_start(title = 'Bikes')
