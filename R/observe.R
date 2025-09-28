#' Observation functions for agent-based models
#'
#' These functions provide a standardized way to record model state
#' during trials. The dispatcher selects the appropriate function
#' based on the `observe` argument.
#'
#' @param model An AgentBasedModel instance
#' @param step Current simulation step (integer)
#' @param label Optional label string for the trial
#' @param ... Additional arguments for future observation types
#'
#' @export
observe_behavior <- function(model, step, label = NULL, ...) {
  tibble::tibble(
    Step = step,
    agent = vapply(model$agents, \(a) a$name, character(1)),
    Behavior = vapply(model$agents, \(a) as.character(a$behavior_current), character(1)),
    Fitness  = vapply(model$agents, \(a) a$fitness_current, numeric(1)),
    label = label
  )
}

# Stub for latent opinions (future expansion in v0.3.0)
#' @export
observe_latent <- function(model, step, label = NULL, ...) {
  stop("Observation type 'latent' not implemented in v0.2.5")
}

# Dispatcher
#' @export
observe_dispatch <- function(model, type = "behavior", step, label = NULL, ...) {
  switch(
    type,
    behavior = observe_behavior(model, step = step, label = label, ...),
    latent   = observe_latent(model, step = step, label = label, ...),
    stop(sprintf("Unknown observation type: %s", type))
  )
}

