library(easyshiny)
library(shinytest)

context('test UI generation')

test_that('barebone UI', {
  run <- testApp('bareboneApp/', 'bareboneApp', quiet = TRUE)
  expect_true(run$results[[1]]$pass)
})

test_that('elements are added', {
  expect_warning(run <- testApp('elementsApp/', 'elementsApp', quiet = TRUE))
  expect_true(run$results[[1]]$pass)
})

test_that('tabs are added', {
  es_init()
  es_renderPlot({}, tab = 'new tab')
  es_renderPlot({}, tab = 'new tab 2')
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  # expect_known_hash(ui, 'cbedc2a2e7')
})

test_that('boxes are added', {
  es_init()
  es_renderPlot({}, box = 'new box')
  es_renderPlot({}, box = 'new box 2')
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  # expect_known_hash(ui, 'b5e5df8704')
})

test_that('combination of tabs and boxes', {
  es_init()
  es_renderPlot({}, tab = 'tab 1', box = 'new box')
  es_renderPlot({}, tab = 'tab 2', box = 'new box 2')
  es_renderPlot({}, tab = 'tab 2', box = 'box 3')
  es_renderPlot({}, tab = 'tab 3', box = 'box 3')
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  # expect_known_hash(ui, '4a0e54847c')
})
