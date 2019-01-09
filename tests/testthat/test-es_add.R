library(easyshiny)

context('test adding elements to the shiny app')

test_that('list of visuals is created correctly', {
  es_init()
  expect_equal(length(get('visuals', easyshiny:::appData)), 0)
  expect_equal(class(get('visuals', easyshiny:::appData)), 'list')
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
})

test_that('summaries are converted to html format', {

  fileset <- base.data.read.filesets('data/Exp_14_09_standard_rds/')
  machines_boxes <- base.data.read.files(fileset, file.machines_boxes, NULL) %>%
    base.data.add.setname('machinetest')
  sumboxes <- machines_boxes %>%
    group_by( setname, machine ) %>%
    do( summary = summary(.$TubesProduced))

  sumhtml <- summaries_to_html(sumboxes)
  expect_known_hash(sumhtml, hash = '5a461d2048')
})
