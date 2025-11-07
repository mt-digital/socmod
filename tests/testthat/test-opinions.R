# tests/testthat/test-opinions.R


test_that("receptivity function works correctly", {
  expect_equal(calc_receptivity(0, 1), 1)          # at 0 → 1
  expect_equal(calc_receptivity(1, 1), 0.5)        # at ±1 → 0
  expect_lt(calc_receptivity(10, 1), 0.1)          # large opinions ≈ 0
})


test_that("opinion updates work correctly", {
  # identical agents → no change
  a <- OpinionAgent$new(id = 1, name = "yo", init_opinions = c(0))
  b <- OpinionAgent$new(id = 2, name = "hey", init_opinions = c(0))
  graph <- igraph::make_empty_graph(n = 2, directed = FALSE)
  m <- socmod::make_abm(agents = c(a, b), alpha = 1.0)

  social_influence(a, b, m)
  expect_equal(a$next_opinions, c(0))

  # similar opinions → small attraction
  a <- OpinionAgent$new(id = 1, init_opinions = c(0.2))
  b <- OpinionAgent$new(id = 2, init_opinions = c(0.4))
  social_influence(a, b, m)
  expect_gt(a$next_opinions, 0.2)
  expect_lt(a$next_opinions, 0.4)

  # distant opinions → repulsion
  a <- OpinionAgent$new(id = 1, init_opinions = c(-0.9))
  b <- OpinionAgent$new(id = 2, init_opinions = c(0.9))
  social_influence(a, b, m)
  expect_gt(abs(a$next_opinions), abs(a$opinions))

  # 3D opinions → updates each dimension
  a <- OpinionAgent$new(id = 1, init_opinions = c(-0.5, 0.0, 0.8))
  b <- OpinionAgent$new(id = 2, init_opinions = c(0.5, -0.2, 0.9))
  social_influence(a, b, m)

  expect_length(a$next_opinions, 3)        # dimension preserved
  expect_length(a$receptivity, 3)         # receptivity matches opinions
  expect_false(all(a$next_opinions == a$opinions))  # opinions updated

  # --- manual calculation check for the 3D case ---
  opinions_a <- c(-0.5, 0.0, 0.8)
  opinions_b <- c( 0.5, -0.2, 0.9)

  dij <- mean(abs(opinions_a - opinions_b))    # ≈ 0.4333
  wij <- 1 - dij                               # ≈ 0.5667
  delta_ok <- 0.5 * wij * (opinions_b - opinions_a)
  recept <- calc_receptivity(opinions_a, alpha = 1)
  expected <- opinions_a + delta_ok * recept
  # ≈ (-0.3583, -0.0567, 0.8057)

  expect_equal(as.numeric(a$next_opinions), expected, tolerance = 1e-6)
})


test_that("Trial with opinion ABMs runs as expected (via run_trial)", {
  
  
  agents <- c(OpinionAgent$new(id = 1, init_opinions = c(0.0)),
              OpinionAgent$new(id = 2, init_opinions = c(1.0)),
              OpinionAgent$new(id = 3, init_opinions = c(-1.0)))
  
  abm <- make_opinion_abm(agents = agents)
  abm_agents <- abm$agents
  a1 <- abm$agents[[1]]
  a2 <- abm$agents[[2]]
  a3 <- abm$agents[[3]]
  
  expect_equal(a1$opinions, c(0.0))
  expect_equal(a2$opinions, c(1.0))
  expect_equal(a3$opinions, c(-1.0))
  
  expect_equal(length(abm$agents), 3)
  
  trial <- run_trial(abm, observer = new_opinion_observer(), 
                     stop = 10)
})


test_that("run_trials with opinion ABMs run as expected", {
  
  abm <- make_opinion_abm(n_agents = 3, init_mean = 0.0, init_sd = 0.25)
  
  expect_equal(length(abm$agents), 3)
  
  
})
