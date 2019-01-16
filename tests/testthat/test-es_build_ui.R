library(easyshiny)

context('test UI generation')

test_that('barebone UI', {
  es_init()
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, 'beb6c10bee')
})

test_that('plots are added', {
  es_init()
  es_add_plot({})
  es_add_plot({})
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, '5bf65e8894')
})

test_that('tabs are added', {
  es_init()
  es_add_plot({}, tab = 'new tab')
  es_add_plot({}, tab = 'new tab 2')
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, '272727770b')
})

test_that('boxes are added', {
  es_init()
  es_add_plot({}, box = 'new box')
  es_add_plot({}, box = 'new box 2')
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, '3246716f22')
})

test_that('combination of tabs and boxes', {
  es_init()
  es_add_plot({}, tab = 'tab 1', box = 'new box')
  es_add_plot({}, tab = 'tab 2', box = 'new box 2')
  es_add_plot({}, tab = 'tab 2', box = 'box 3')
  es_add_plot({}, tab = 'tab 3', box = 'box 3')
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, '34e072728e')
})
