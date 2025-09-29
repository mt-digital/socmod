test_that("Well-mixed with self-sampling works as expected", {
  
  # init ABM and well-mixed selection function with self-selection
  abm <- make_opinion_abm(n_agents = 2, init_mean = 0.0, init_sd = 1.0)
  wm_sel <- make_well_mixed_selection(self_selection = TRUE)
  
  # check that 1 is in the selected agent ids at least once in 50 samples
  selected_agent_ids <- replicate(50, wm_sel(abm$get_agent(1), abm)$get_id())
  
  expect_true(1 %in% selected_agent_ids)
})

test_that("Well-mixed without self-sampling works as expected", {
  
  # init ABM and well-mixed selection function with self-selection
  abm <- make_opinion_abm(n_agents = 3, init_mean = 0.0, init_sd = 1.0)
  wm_sel <- make_well_mixed_selection(self_selection = FALSE)

  # check that 1 is in the selected agent ids at least once in 50 samples
  selected_agent_ids <- replicate(50, wm_sel(abm$get_agent(1), abm)$get_id())
  expect_false(1 %in% selected_agent_ids)
})
