library(easyshiny)
library(feather)
library(dplyr)
library(tidyr)

context('file reading and fileset handling')

test_that('list of input files is created correctly', {
  es_init(local_folder = './data/Exp_14_09_standard')
  expect_equal(length(get('files', easyshiny:::appData)), 0)
  expect_equal(class(get('files', easyshiny:::appData)), 'list')

  es_read('test1.csv')
  files <- get('files', easyshiny:::appData)
  expect_equal(nrow(files), 1)
  expect_equal(files[1,]$name, 'test1.csv')
  expect_equal(files[1,]$id, 'test1')

  es_read('test2.csv')
  files <- get('files', easyshiny:::appData)
  expect_equal(nrow(files), 2)
  expect_equal(files[1,]$name, 'test1.csv')
  expect_equal(files[1,]$id, 'test1')
  expect_equal(files[2,]$name, 'test2.csv')
  expect_equal(files[2,]$id, 'test2')

  es_read(filename = 'statistic_machines.csv', readerId = 'testreader')
  test_machines <- testreader()
  expect_true('tbl_df' %in% class(test_machines))
})

test_that('filesets are read correctly', {
  test_fs <- es_read_filesets(NULL)
  expect_null(test_fs)

  expect_error(es_read_filesets('~/non_existing_folder'))

  test_fs <- es_read_filesets('./data/Exp_14_09_standard')
  expect_equal(length(test_fs), 4)
  expect_equal(nrow(test_fs), 12)
  expect_equal( unique(test_fs$n), 1)
  expect_true('statistic.csv' %in% test_fs$name)
  expect_true('statistic_machines.csv' %in% test_fs$name)

  test_fs <- es_read_filesets( c('./data/Exp_14_09_standard', './data/Exp_14_09_testrds') )
  expect_equal(length(test_fs), 4)
  expect_equal(nrow(test_fs), 24)
  expect_equal( unique(test_fs$n), c(1, 2) )
  expect_equal(test_fs %>% filter(name == 'statistic.csv') %>% nrow(), 2)
  expect_equal(test_fs %>% filter(name == 'statistic_machines.csv') %>% nrow(), 2)
})

test_that('filereader reads correctly', {

  test_fs <- easyshiny:::es_read_filesets('./data/Exp_14_09_standard')
  expect_error(easyshiny:::es_read_files(test_fs, 'nonexistent'))
  expect_warning(expect_null(easyshiny:::es_read_files(test_fs, 'nonexistent.csv')))
  expect_error(easyshiny:::es_read_files(test_fs, 'statistic_stacker.xyza'))

  test_tbl <- easyshiny:::es_read_files(test_fs, 'statistic_qc_buffer.csv', function(data){data})
  expect_true('tbl_df' %in% class(test_tbl))
})
