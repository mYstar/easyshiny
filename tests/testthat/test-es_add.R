library(shiny)
library(vdiffr)
library(ggplot2)

context('test adding elements to the shiny app')

test_that('objects are created correctly', {
  es_init()
  visuals <- get('visuals', easyshiny:::appData)
  expect_equal(nrow(visuals), 1)
  expect_equal(class(visuals), 'matrix')
  expect_equal(get('vis_counter', easyshiny:::appData), 1)

  es_add(quote(renderPlot({})), quote(plotOutput(outputId = 'output_')), tab = 'Output', box = 'Result')
  visuals <- get('visuals', easyshiny:::appData)
  expect_equal(nrow(visuals), 2)
  expect_equal(class(visuals[2,]$render_func), 'call')
  expect_equal(class(visuals[2,]$ui_func), 'call')
  expect_equal(visuals[2,]$tab, 'Output')
  expect_equal(visuals[2,]$box, 'Result')

  es_add(quote(renderDataTable({})), quote(plotOutput(outputId = 'output_')), tab = 'Output2', box = 'Result2')
  visuals <- get('visuals', easyshiny:::appData)
  expect_equal(nrow(visuals), 3)

  expect_equal(visuals[2,]$tab, 'Output')
  expect_equal(visuals[2,]$box, 'Result')
  expect_equal(class(visuals[2,]$render_func), 'call')
  expect_equal(class(visuals[2,]$ui_func), 'call')
  expect_false(visuals[2,]$resize)

  expect_false(visuals[2,]$id == visuals[3,]$id)
  expect_equal(class(visuals[3,]$render_func), 'call')
  expect_equal(class(visuals[3,]$ui_func), 'call')
  expect_equal(visuals[3,]$tab, 'Output2')
  expect_equal(visuals[3,]$box, 'Result2')
  expect_false(visuals[3,]$resize)
} )

test_that('output objects are created correctly', {
  testplot <- es_renderPlot({
    ggplot(mpg, aes(cty, hwy)) +
      geom_point()
  })
  expect_doppelganger('simple plot', testplot())
})

test_that('input objects are created correctly', {

  es_init()
  es_add_input(
    textInput,
    inputId = 'test',
    value = 42
  )
  input <- get('input', envir = globalenv())
  expect_equal(input$test, 42)
})

test_that('static elements are created correctly', {

  es_init()
  es_add_static(tags$p('This is a paragraph', strong('with a bold element in it')), tab = 'static test', box = 'static box')

  visuals <- get('visuals', easyshiny:::appData)
  expect_equal(nrow(visuals), 2)

  expect_equal(visuals[2,]$tab, 'static test')
  expect_equal(visuals[2,]$box, 'static box')
  expect_null(visuals[2,]$render_func)
  expect_equal(class(visuals[2,]$ui_func), 'call')
  expect_false(visuals[2,]$resize)
} )
