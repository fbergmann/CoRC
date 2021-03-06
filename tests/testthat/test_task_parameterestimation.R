context("Task Parameter Estimation")

loadExamples(4)
setParameterEstimationSettings(update_model = FALSE, method = "LevenbergMarquardt")

test_that("runParameterEstimation()", {
  PE <- runParameterEstimation()
  expect_length(PE, 13)
  
  expect_is(PE$settings, "list")
  
  expect_is(PE$main, "list")
  expect_gt(PE$main$objective_value, 0)
  expect_gt(PE$main$root_mean_square, 0)
  expect_gt(PE$main$standard_deviation, 0)
  expect_gte(PE$main$validation_objective_value, 0)
  expect_identical(PE$main$validation_root_mean_square, NaN)
  expect_identical(PE$main$validation_standard_deviation, NaN)
  expect_gt(PE$main$function_evaluations, 0)
  expect_gt(PE$main$cpu_time_s, 0)
  expect_gt(PE$main$evaluations_second_1_s, 0)
  
  param_count <- 3L
  
  expect_length(PE$parameters, 8)
  expect_equal(nrow(PE$parameters), param_count)
  
  expect_length(PE$experiments, 5)
  expect_equal(nrow(PE$experiments), 1L)
  
  expect_length(PE$fitted_values, 5)
  expect_equal(nrow(PE$fitted_values), 2L)
  
  expect_equal(ncol(PE$correlation), param_count)
  expect_equal(nrow(PE$correlation), param_count)
  
  expect_equal(ncol(PE$fim), param_count)
  expect_equal(nrow(PE$fim), param_count)
  
  expect_equal(ncol(PE$fim_eigenvalues), 1L)
  expect_equal(nrow(PE$fim_eigenvalues), param_count)
  
  expect_equal(ncol(PE$fim_eigenvectors), param_count)
  expect_equal(nrow(PE$fim_eigenvectors), param_count)
  
  expect_equal(ncol(PE$fim_scaled), param_count)
  expect_equal(nrow(PE$fim_scaled), param_count)
  
  expect_equal(ncol(PE$fim_scaled_eigenvalues), 1L)
  expect_equal(nrow(PE$fim_scaled_eigenvalues), param_count)
  
  expect_equal(ncol(PE$fim_scaled_eigenvectors), param_count)
  expect_equal(nrow(PE$fim_scaled_eigenvectors), param_count)
  
  expect_is(PE$protocol, "character")
})

test_that("setOptimizationSettings() method", {
  expect_error(setParameterEstimationSettings(method = "..."))
  setParameterEstimationSettings(method = list())
  expect_error(setParameterEstimationSettings(method = list(".")))
})

unloadAllModels()
clearCustomKineticFunctions()
