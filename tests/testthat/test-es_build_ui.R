library(easyshiny)

context('test UI generation')

test_that('barebone UI', {
  es_init()
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, '27870904ff')
})

test_that('plots are added', {
  es_init()
  es_add_output(renderPlot, {})
  es_add_output(renderPlot, {})
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, 'f1d170ca59')
})

test_that('tabs are added', {
  es_init()
  es_add_output(renderPlot, {}, tab = 'new tab')
  es_add_output(renderPlot, {}, tab = 'new tab 2')
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, 'cbedc2a2e7')
})

test_that('boxes are added', {
  es_init()
  es_add_output(renderPlot, {}, box = 'new box')
  es_add_output(renderPlot, {}, box = 'new box 2')
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, 'b5e5df8704')
})

test_that('combination of tabs and boxes', {
  es_init()
  es_add_output(renderPlot, {}, tab = 'tab 1', box = 'new box')
  es_add_output(renderPlot, {}, tab = 'tab 2', box = 'new box 2')
  es_add_output(renderPlot, {}, tab = 'tab 2', box = 'box 3')
  es_add_output(renderPlot, {}, tab = 'tab 3', box = 'box 3')
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, '4a0e54847c')
})
