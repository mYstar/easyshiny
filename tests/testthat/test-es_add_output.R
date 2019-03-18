library(shiny)
library(tibble)
library(checkmate)
library(ggplot2)
library(vdiffr)

context('test adding output elements to the easyshiny app')

test_that('output objects are inserted correctly', {
  es_init()

  # render Data Table
  test_func <- es_renderDataTable({as.tibble(mtcars)}, tab = 'testDT', box = 'resDT')
  visuals <- get('visuals', envir = easyshiny:::appData)
  last_elem <- visuals[length(visuals[,1]),]

  expect_string(last_elem$id)
  expect_equal(last_elem$tab, 'testDT')
  expect_equal(last_elem$box, 'resDT')
  expect_tibble(test_func())

  # render Image
  test_func <- es_renderImage({list(src='figs/Rlogo.svg')}, tab = 'testIMG', box = 'resIMG')
  visuals <- get('visuals', envir = easyshiny:::appData)
  last_elem <- visuals[length(visuals[,1]),]

  expect_string(last_elem$id)
  expect_equal(last_elem$tab, 'testIMG')
  expect_equal(last_elem$box, 'resIMG')
  expect_list(test_func(), min.len = 1)
  expect_subset( 'src', names(test_func()) )

  # render Plot
  test_func <- es_renderPlot({qplot(mtcars$mpg)}, tab = 'testPlot', box = 'resPlot')
  visuals <- get('visuals', envir = easyshiny:::appData)
  last_elem <- visuals[length(visuals[,1]),]

  expect_string(last_elem$id)
  expect_equal(last_elem$tab, 'testPlot')
  expect_equal(last_elem$box, 'resPlot')
  expect_class(test_func(), classes = c('gg', 'ggplot'))
  expect_doppelganger(title = 'mpg test', test_func())

  # render Print
  test_func <- es_renderPrint({'This is a printable text'}, tab = 'testPrint', box = 'resPrint')
  visuals <- get('visuals', envir = easyshiny:::appData)
  last_elem <- visuals[length(visuals[,1]),]

  expect_string(last_elem$id)
  expect_equal(last_elem$tab, 'testPrint')
  expect_equal(last_elem$box, 'resPrint')
  expect_equal('This is a printable text', test_func())

  # render Table
  test_func <- es_renderTable({as.tibble(mtcars)}, tab = 'testTable', box = 'resTable')
  visuals <- get('visuals', envir = easyshiny:::appData)
  last_elem <- visuals[length(visuals[,1]),]

  expect_string(last_elem$id)
  expect_equal(last_elem$tab, 'testTable')
  expect_equal(last_elem$box, 'resTable')
  expect_tibble(test_func())

  # render Text
  text_example <- 'This is a printable text!'
  test_func <- es_renderText({text_example}, tab = 'testText', box = 'resText')
  visuals <- get('visuals', envir = easyshiny:::appData)
  last_elem <- visuals[length(visuals[,1]),]

  expect_string(last_elem$id)
  expect_equal(last_elem$tab, 'testText')
  expect_equal(last_elem$box, 'resText')
  expect_equal(text_example, test_func())

  # render UI
  html_example <- tags$h1('This is HTML')
  test_func <- es_renderUI({html_example}, tab = 'testUI', box = 'resUI')
  visuals <- get('visuals', envir = easyshiny:::appData)
  last_elem <- visuals[length(visuals[,1]),]

  expect_string(last_elem$id)
  expect_equal(last_elem$tab, 'testUI')
  expect_equal(last_elem$box, 'resUI')
  expect_class(test_func(), classes = 'shiny.tag')
  expect_equal(html_example, test_func())
} )
