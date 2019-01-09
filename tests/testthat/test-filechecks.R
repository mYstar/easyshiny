library(easyshiny)

context('File checking abilities')

test_that('fileset checking works', {
  fileset <- base.data.read.filesets('data/Exp_14_09_standard_rds/')
  expect_equal( base.data.fileset.present(fileset, '1'), T )
  expect_equal( base.data.fileset.present(fileset, '2'), F )

  fileset2 <- base.data.read.filesets( c('data/Exp_14_09_standard_rds/', 'data/Exp_14_09_seed2_rds') )
  expect_equal( base.data.fileset.present(fileset2, '1'), T )
  expect_equal( base.data.fileset.present(fileset2, '2'), T )
  expect_equal( base.data.fileset.present(fileset2, '3'), F )
} )

test_that('file discovery works', {
  fileset <- base.data.read.filesets( c('./data/Exp_14_09_standard_rds/', './data/Exp_14_09_seed2_rds/') )
  expect_equal( base.data.file.present(fileset, '1', shinystat::file.box_stacker), T )
  expect_equal( base.data.file.present(fileset, '1', shinystat::file.machines_boxes), T )
  expect_equal( base.data.file.present(fileset, '1', shinystat::file.loop), T )
  expect_equal( base.data.file.present(fileset, '2', shinystat::file.caps), T )
  expect_equal( base.data.file.present(fileset, '2', shinystat::file.warehouse_pallets), T )
  expect_equal( base.data.file.present(fileset, '2', shinystat::file.lifter), T )

  expect_equal(base.data.file.present(fileset, '3', shinystat::file.caps), F )
  expect_equal(base.data.file.present(fileset, '3', shinystat::file.warehouse_pallets), F )
  expect_equal(base.data.file.present(fileset, '3', shinystat::file.lifter), F )
  expect_equal(base.data.file.present(fileset, '1', 'imaginary_file.csv'), F )
  expect_equal(base.data.file.present(fileset, '1', 'imaginary_file.rds'), F )
  expect_equal(base.data.file.present(fileset, '1', 'imaginary_file.xls'), F )
  expect_equal(
    base.data.file.present( fileset, '1', paste0(shinystat::file.box_stacker, '.old') ),
    F
  )
} )
