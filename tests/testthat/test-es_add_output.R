library(shiny)
library(tibble)
library(checkmate)

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
} )
