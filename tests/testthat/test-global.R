test_that("preloader returns a list of HTML tags",{
  test_tags = tagList(p("hello"),br(),div())
  
  expect_error(preloader(test_tags))
  expect_type(preloader("hello"), typeof(test_tags))
})
