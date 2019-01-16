library(easyshiny)

context('test adding elements to the shiny app')

test_that('list of visuals is created correctly', {
  es_init()
  expect_equal(length(get('visuals', easyshiny:::appData)), 0)
  expect_equal(class(get('visuals', easyshiny:::appData)), 'matrix')
  expect_equal(get('vis_counter', easyshiny:::appData), 0)

  es_add_plot({})
  visuals <- get('visuals', easyshiny:::appData)
  expect_equal(nrow(visuals), 1)
  expect_equal(visuals[1,]$id, 'plot1')
  expect_equal(class(visuals[1,]$expr), 'call')

  es_add_plot({})
  visuals <- get('visuals', easyshiny:::appData)
  expect_equal(nrow(visuals), 2)
  expect_equal(visuals[1,]$id, 'plot1')
  expect_equal(class(visuals[1,]$expr), 'call')
  expect_equal(visuals[2,]$id, 'plot2')
  expect_equal(class(visuals[2,]$expr), 'call')
})
