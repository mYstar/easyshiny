library(easyshiny)
library(checkmate)

context('test init of global vars')

test_that('global vars are created and deleted', {

  es_init()

  es_read('test.csv')
  es_renderPlot({})
  es_add_input(textInput, inputId = 'id', label = 'label')

  es_init()

  visuals <- get('visuals', easyshiny:::appData)
  expect_matrix(visuals, nrows = 1)
  expect_set_equal(colnames(visuals), c('id', 'render_func', 'tab', 'box', 'ui_func', 'resize'))
  expect_list(get('files', easyshiny:::appData), len = 0)
  expect_equal(get('vis_counter', easyshiny:::appData), 1)
  expect_null(get('console_fileset', easyshiny:::appData))
  expect_list(input)

  es_init(local_folder = 'data/Exp_14_09_standard')

  expect_tibble(
    get('console_fileset', easyshiny:::appData),
    types = c('character', 'integer'),
    any.missing = F,
    min.rows = 1,
    ncols = 4,
    null.ok = F,
    col.names = 'named'
  )
  expect_subset(
    c('datapath', 'folder', 'name', 'n'),
    get('console_fileset', easyshiny:::appData) %>% colnames()
  )
})

test_that('values are checked', {
  es_init(local_folder = './data/tmp')
  es_init(local_folder = NULL)
  expect_error(es_init(local_folder = '/unknown'))
  expect_error(es_init(local_folder = NA))
} )
