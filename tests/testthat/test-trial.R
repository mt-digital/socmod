test_that("Trial records observations and outcomes correctly", {

  # Minimal model with adaptive initialization.
  g <- igraph::make_ring(3)
  model <- AgentBasedModel$new(make_model_parameters(graph = g))

  for (agent in model$agents) {
    agent$set_behavior("Adaptive")
    agent$set_fitness(1.0)
  }



  # trial <- Trial$new(model, )
  # trial$run(stop = fixated)
  trial <- run_trial(model, stop = fixated)

  obs <- trial$get_observations()
  out <- trial$get_outcomes()

  expect_s3_class(obs, "tbl_df")
  expect_true(nrow(obs) >= 1)
  expect_true("Behavior" %in% names(obs))
  expect_true("Fitness" %in% names(obs))
  expect_true(out$adaptation_success)
  expect_equal(out$fixation_steps, 1)
})

test_that("Trial stops after max steps and adapts outcomes", {
 
  lstrat <- ModelDynamics$new(
    function(learner, model) NULL,
    function(learner, partner, model) NULL,
    function(model) NULL,
    "null strategy"
  )
  
  model <- 
    AgentBasedModel$new(
      make_model_parameters(
        lstrat, 
        n_agents = 4,
        graph = igraph::make_full_graph(4)
      )
    )

  model$agents[[1]]$set_behavior("Adaptive")
  model$agents[[2]]$set_behavior("Legacy")
  model$agents[[3]]$set_behavior("Legacy")
  model$agents[[4]]$set_behavior("Adaptive")

  for (i in 1:4) {
    model$agents[[i]]$set_fitness(1.0)
  }

  trial <- run_trial(model, stop = 3)

  out <- trial$get_outcomes()
  
  expect_false(out$adaptation_success)
  expect_equal(out$fixation_steps, 3)
})

test_that("run_trials() returns expected number of Trial objects", {
  
  gen <- function(param_row) {
    
    agents <- list(
      Agent$new(1, name = "1", behavior = "Legacy", fitness = 1),
      Agent$new(2, name = "2", behavior = "Adaptive", fitness = 4)
    )
    
    net <- igraph::make_graph(~ 1-2)
    
    AgentBasedModel$new(make_model_parameters(
                          graph = net, 
                          adoption_rate = param_row$adoption_rate
                        ), 
                        agents = agents)
  }

  # Check that there are five trials and all are Trial instances for the following.
  trials <- run_trials(gen, n_trials_per_param = 5)
  expect_length(trials, 5)
  expect_true(all(purrr::map_lgl(trials, ~ inherits(.x, "Trial"))))

  # Check that all observations are tibbles.
  obs_list <- purrr::map(trials, ~ .x$get_observations())
  expect_true(all(purrr::map_lgl(obs_list, tibble::is_tibble)))
})


test_that("summarise_outcomes correctly summarizes grouped trial outcomes", {
  
  mock_run_one_trial <- function(ii) {
    # Create model based on parameters...
    trial <- make_model_parameters(
        n_agents = 10, 
        graph = igraph::make_full_graph(10),
        seed_set = ifelse(ii <= 2, "A", "B"),
        adaptive_fitness = ifelse(ii %% 2 == 0, 1.2, 1.0), 
      ) %>%
      # ...make an agent-based model with these settings...
      make_abm() %>%
      # ...and simulate model dynamics for three time steps.
      run_trial(observer = new_behavior_observer(), stop = 3)
    
    trial$outcomes$adaptation_success <- (ii %% 2 == 0) # Even trials "succeed".
    trial$outcomes$fixation_steps <- ii + 1
    
    return (trial)
  }
  
  # Create synthetic trial outcomes.
  trials <- purrr::map(1:4, mock_run_one_trial)

  # Create summary over specified outcome measures.
  summary <- summarise_outcomes(
    trials, input_parameters = c("adaptive_fitness", "seed_set"),
    outcome_measures = c("success_rate", "mean_fixation_steps")
  )

  # Check columns
  expect_true(
    all(
      c("adaptive_fitness", "seed_set", 
        "Measure", "Value") %in% colnames(summary)
    )
  )
  
  # Check group count and shape
  # 2 seed_sets × 2 fitness levels x 2 outcome measures measures = 8 rows
  expect_equal(nrow(summary), 8)  
  
  # Check that the success rates are all 0.0 when the adaptive_fitness is 1.0.
  result <- summary %>% 
    dplyr::filter(adaptive_fitness == 1.0 & Measure == "success_rate")
  
  expect_true(all(result$Value == 0.0))
  
  # Check that the success rates are all 1.0 when the adaptive_fitness is 1.2.
  result <- summary %>% 
    dplyr::filter(adaptive_fitness == 1.2 & Measure == "success_rate")
  
  expect_true(all(result$Value == 1.0))  # Successful even trials
})


test_that("Opinion dynamics trials work as expected", {
  expect_true(F)
})
