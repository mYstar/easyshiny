library(easyshiny)
library(feather)
library(dplyr)
library(tidyr)

context('file reading and fileset handling')

test_that('list of input files is created correctly', {
  es_init()
  expect_equal(length(get('files', easyshiny:::appData)), 0)
  expect_equal(class(get('files', easyshiny:::appData)), 'list')

  es_read('test1.csv')
  files <- get('files', easyshiny:::appData)
  expect_equal(nrow(files), 1)
  expect_equal(files[1,]$name, 'test1')

  es_read('test2.csv')
  files <- get('files', easyshiny:::appData)
  expect_equal(nrow(files), 2)
  expect_equal(files[1,]$name, 'test1')
  expect_equal(files[2,]$name, 'test2')
})
