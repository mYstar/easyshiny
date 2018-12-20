library(shinystat)

context('test UI generation')

test_that('UI html is correct', {
  ui <- ui.build()
  expect_known_hash(ui, hash = '6b532441ab')
})
