library(easyshiny)
library(magrittr)

context('test RDS generation script')

test_that('files are generated at the correct location', {

  convert_rds('data/Exp_14_09_testrds/')
  expect_true(dir.exists('data/Exp_14_09_testrds_rds/'))
  expect_equal(list.files('data/Exp_14_09_testrds_rds/') %>% length(), 12)
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_caps.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_conveyor_boxes.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_lifter_states.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_machines.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_montage_auto.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_montage_manual.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_qc_buffer.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_qc_cycles.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_stacker.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_transferlids.rds'))
  expect_true(file.exists('data/Exp_14_09_testrds_rds/statistic_warehouse_pallets.rds'))

  unlink('data/Exp_14_09_testrds_rds/', recursive = T)
})
