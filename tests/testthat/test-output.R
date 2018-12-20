library(shinystat)

context('test output helper functions')

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
