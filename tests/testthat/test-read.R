library(shinystat)
library(feather)
library(dplyr)
library(tidyr)

context('file reading and fileset handling')

test_that('creating fileset from folder', {
  fileset <- base.data.read.filesets('data/Exp_14_09_standard_rds/')
  expect_equal( fileset %>% nrow(), 12 )
  expect_equal( fileset %>% ncol(), 4 )
  expect_false( fileset$datapath %>% is.null() )
  expect_false( fileset$folder %>% is.null() )
  expect_false( fileset$name %>% is.null() )
  expect_false( fileset$n %>% is.null() )
  expect_equal( fileset$n %>% min(), 1 )
  expect_equal( fileset$n %>% unique() %>% length() , 1 )

  fileset2 <- base.data.read.filesets( c('./data/Exp_14_09_standard_rds/', './data/Exp_14_09_seed2_rds/') )
  expect_equal( fileset2 %>% nrow(), 24 )
  expect_equal( fileset2 %>% ncol(), 4 )
  expect_false( fileset2$datapath %>% is.null() )
  expect_false( fileset2$folder %>% is.null() )
  expect_false( fileset2$name %>% is.null() )
  expect_false( fileset2$n %>% is.null() )
  expect_equal( fileset2$n %>% min(), 1 )
  expect_equal( fileset2$n %>% max(), 2 )
  expect_equal( fileset2$n %>% unique() %>% length() , 2 )
} )

test_that('setnames are applied', {
  fileset <- base.data.read.filesets('data/Exp_14_09_standard_rds/')
  pallets <- base.data.read.files(fileset, shinystat::file.warehouse_pallets, NULL)
  expect_warning(
    expect_true( pallets$setname %>% is.null() )
    )
  pallets_test <- base.data.add.setname(pallets, c('test1'))
  expect_false( pallets_test$setname %>% is.null() )
  expect_equal( pallets_test$setname %>% unique(), 'test1' %>% as.factor() )
  pallets_test <- base.data.add.setname(pallets, c('test1', 'test2', 'test3'))
  expect_false( pallets_test$setname %>% is.null() )
  expect_equal( pallets_test$setname %>% unique() %>% as.character(), 'test1' )

  fileset <- base.data.read.filesets(c('data/Exp_14_09_standard_rds/', 'data/Exp_14_09_seed2_rds/') )
  pallets <- base.data.read.files(fileset, shinystat::file.warehouse_pallets, NULL)
  expect_warning(
    expect_true( pallets$setname %>% is.null() )
    )
  pallets_test <- base.data.add.setname(pallets, c('test1', 'test2'))
  expect_false( pallets_test$setname %>% is.null() )
  expect_equal( pallets_test$setname %>% unique(), c('test1', 'test2') %>% as.factor() )
  pallets_test <- base.data.add.setname(pallets, c('test1', 'test2', 'test3'))
  expect_false( pallets_test$setname %>% is.null() )
  expect_equal( pallets_test$setname %>% unique() %>% as.character(), c('test1', 'test2') )
})

test_that('timeframe is limited', {
  fileset <- base.data.read.filesets('data/Exp_14_09_standard_rds/')
  pallets <- base.data.read.files(fileset, shinystat::file.warehouse_pallets, NULL)
  expect_equal(pallets$Simulation.Time %>% min(), 0)
  expect_equal(pallets$Simulation.Time %>% max(), 172740)

  pallets_limited <- base.data.select(pallets, 180, 18000)
  expect_equal(pallets_limited$Simulation.Time %>% min(), 180)
  expect_equal(pallets_limited$Simulation.Time %>% max(), 18000)

  pallets_limited <- base.data.select(pallets, 2500, 10000)
  expect_equal(pallets_limited$Simulation.Time %>% min(), 2520) # exact values are not available
  expect_equal(pallets_limited$Simulation.Time %>% max(), 9960)

  pallets_limited <- base.data.select(pallets, -1000, 18000)
  expect_equal(pallets_limited$Simulation.Time %>% min(), 0)
  expect_equal(pallets_limited$Simulation.Time %>% max(), 18000)

  pallets_limited <- base.data.select(pallets, 180, 99999999)
  expect_equal(pallets_limited$Simulation.Time %>% min(), 180)
  expect_equal(pallets_limited$Simulation.Time %>% max(), 172740)
})

test_that('data is read in correctly', {
  fileset <- base.data.read.filesets('data/Exp_14_09_standard_rds/')
  pallets <- base.data.read.files(fileset, shinystat::file.warehouse_pallets, NULL)

  write_feather(pallets, 'data/tmp/Exp_14_09_standard_rds_pallets.feather')
  checkreturn <- system(
    'tdda verify data/tmp/Exp_14_09_standard_rds_pallets.feather data/constraints/Exp_14_09_standard_rds_pallets.tdda',
    intern = T)

  expect_equal( sub('Constraints passing: ', '',checkreturn[length(checkreturn)-1] ) %>% as.numeric(), 20, info = checkreturn)
  expect_equal( sub('Constraints failing: ', '',checkreturn[length(checkreturn)] ) %>% as.numeric(), 0, info = checkreturn)

  cycles <- base.data.read.files(fileset, shinystat::file.qc_cycles, NULL)
  write_feather(cycles, 'data/tmp/Exp_14_09_standard_rds_qccycles.feather')
  checkreturn <- system(
    'tdda verify data/tmp/Exp_14_09_standard_rds_qccycles.feather data/constraints/Exp_14_09_standard_rds_qccycles.tdda',
    intern = T)

  expect_equal( sub('Constraints passing: ', '',checkreturn[length(checkreturn)-1] ) %>% as.numeric(), 15, info = checkreturn)
  expect_equal( sub('Constraints failing: ', '',checkreturn[length(checkreturn)] ) %>% as.numeric(), 0, info = checkreturn)

  fileset <- base.data.read.filesets(c('data/Exp_14_09_standard', 'data/Exp_14_09_seed2_rds/') )
  pallets <- base.data.read.files( fileset, shinystat::file.warehouse_pallets, callback = function(raw_data) {
    raw_data %>%
      gather( "pallettype", "pallets", starts_with('Warehouse') ) %>%
      mutate( pallettype = substring(pallettype, 11))
  } )

  write_feather(pallets, 'data/tmp/Exp_14_09_standard_seed2_pallets.feather')
  checkreturn <- system(
    'tdda verify data/tmp/Exp_14_09_standard_seed2_pallets.feather data/constraints/Exp_14_09_standard_seed2_pallets.tdda',
    intern = T)

  expect_equal( sub('Constraints passing: ', '',checkreturn[length(checkreturn)-1] ) %>% as.numeric(), 19, info = checkreturn)
  expect_equal( sub('Constraints failing: ', '',checkreturn[length(checkreturn)] ) %>% as.numeric(), 0, info = checkreturn)

  cycles <- base.data.read.files(fileset, shinystat::file.qc_cycles, callback = function(raw_data) {raw_data} )
  write_feather(cycles, 'data/tmp/Exp_14_09_standard_seed2_qccycles.feather')
  checkreturn <- system(
    'tdda verify data/tmp/Exp_14_09_standard_seed2_qccycles.feather data/constraints/Exp_14_09_standard_seed2_qccycles.tdda',
    intern = T)

  expect_equal( sub('Constraints passing: ', '',checkreturn[length(checkreturn)-1] ) %>% as.numeric(), 14, info = checkreturn)
  expect_equal( sub('Constraints failing: ', '',checkreturn[length(checkreturn)] ) %>% as.numeric(), 0, info = checkreturn)
})
