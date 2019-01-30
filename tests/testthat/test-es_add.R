library(easyshiny)
library(ggplot2)
library(vdiffr)


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

  testplot <- es_add_plot({
    ggplot(mpg, aes(cty, hwy)) +
      geom_point()
  })
  expect_doppelganger('simple plot', testplot())

})

test_that('input objects are created correctly', {

  es_init()
  es_add_object(
    textInput,
    inputId = 'test',
    value = 42
  )
  input <- get('input', envir = globalenv())
  expect_equal(input$test, 42)
})
