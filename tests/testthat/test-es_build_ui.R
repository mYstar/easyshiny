library(easyshiny)
library(shinytest)

context('test UI generation')

test_that('barebone UI', {
  run <- testApp('bareboneApp/', 'bareboneApp', quiet = TRUE)
  expect_true(run$results[[1]]$pass)
})

test_that('elements are added', {
  run <- testApp('elementsApp/', 'elementsApp', quiet = TRUE)
  expect_true(run$results[[1]]$pass)
})

# test_that('zoom in works', {
#   run <- testApp('elementsApp/', 'zoomIn', quiet = TRUE)
#   expect_true(run$results[[1]]$pass)
# })

test_that('tabs are added', {
  run <- testApp('tabsApp/', 'tabsApp', quiet = TRUE)
  expect_true(run$results[[1]]$pass)
})

test_that('boxes are added', {
  run <- testApp('boxesApp/', 'boxesApp', quiet = TRUE)
  expect_true(run$results[[1]]$pass)
})

test_that('combination of tabs and boxes', {
  run <- testApp('combinedApp/', 'combinedApp', quiet = TRUE)
  expect_true(run$results[[1]]$pass)
})
