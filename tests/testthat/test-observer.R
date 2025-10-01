test_that("Behavior observer reports as expected", {
  abm <- make_abm(n_agents = 10) |> initialize_agents(0.5)

  # check that five agents currently reported doing the behavior
  obs_row <- observe_behavior(abm, 0)
  expect_equal(table(obs_row$Behavior)[["Adaptive"]], 5)
})


test_that("Opinion observer reports as expected", {
  
  agents <- c(OpinionAgent$new(id = 1, init_opinions = c(0.0)),
              OpinionAgent$new(id = 2, init_opinions = c(1.0)),
              OpinionAgent$new(id = 3, init_opinions = c(-1.0)),
              OpinionAgent$new(id = 4, init_opinions = -2.0, alpha = 2.0))
  abm <- make_opinion_abm(agents = agents)
  
  # check that each agent has its expected opinions and stubbornness
  obs_tib <- observe_opinion(abm, 0)
  # expect the opinions and stubbornness be listed in order equal expected
  obs_tib$agent
  expect_equal(
    obs_tib$agent, c("a1", "a2", "a3", "a4")
  )
  expect_equal(
    obs_tib$Opinions, c(0, 1, -1, -2)
  )
  # when alpha = 2, opinion = 2, receptivity = 1 / (1 + | -2|^2) = 1/(1 + 4) = 0.2, 
  # then stubbornness = 1 - receptivity = 0.8
  expect_equal(
    obs_tib$Stubbornness, c(0, 0.5, 0.5, 0.8)
  )
})
