library(easyshiny)

context('test UI generation')

test_that('UI html is correct', {
  es_init()
  es_add_plot({ })
  ui <- easyshiny:::es_build_ui(title = 'test', visuals = get('visuals', easyshiny:::appData))
  expect_known_hash(ui, hash = '0b1f89813e')
})
