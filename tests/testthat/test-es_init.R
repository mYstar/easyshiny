library(easyshiny)

context('test init of global vars')

test_that('global vars are created and deleted', {

  es_init()

  es_read('test.csv')
  es_add_plot({})

  es_init()

  expect_equal(length(get('visuals', easyshiny:::appData)), 0)
  expect_equal(class(get('visuals', easyshiny:::appData)), 'matrix')

  expect_equal(length(get('files', easyshiny:::appData)), 0)
  expect_equal(class(get('files', easyshiny:::appData)), 'list')

  expect_equal(get('vis_counter', easyshiny:::appData), 0)
})

test_that('values are checked', {
  es_init(local_folder = '/tmp')
  es_init(local_folder = NULL)
  expect_error(es_init(local_folder = '/unknown'))
  expect_error(es_init(local_folder = NA))
} )
