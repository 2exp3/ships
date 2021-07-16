test_that("get_names_ids_by_type works", {
  data = expand.grid(ship_type = LETTERS, 
                     ID = factor(1:10),
                     fixed_name = sample(LETTERS,size = 5))
  type = sample(LETTERS,1)
  type_x = 123
  expect_error(get_names_ids_by_type(1:5, type))
  expect_error(get_names_ids_by_type(data, type_x))
  
  expect_type(get_names_ids_by_type(data, type), typeof(data))
  expect_equal(ncol(get_names_ids_by_type(data,type)), 2)
})
 

test_that("map_name_id works", {
  data = expand.grid(type = LETTERS, 
                     ID = 1:10,
                     fixed_name = sample(LETTERS, size = 5))
  filter = sample(LETTERS,1)
  filter_var = names(data)[3]
  counter_var = names(data)[2]
  filter_x = 123
  
  expect_error(map_name_id(1:5, filter_var, filter, counter_var))
  expect_error(gmap_name_id(data, filter_var, filter_x, counter_var))
  
  expect_type(map_name_id(data, filter_var, filter, counter_var),typeof(1:10))
  expect_gte(sum(map_name_id(data, filter_var, filter, counter_var)),0)
})

