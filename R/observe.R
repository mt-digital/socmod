#' Function to gather behavior adoption data at a single model `step`
#'
#' @param model An AgentBasedModel instance
#' @param step Current simulation step (integer)
#' @param label Optional label string for the trial
#' @param ... Additional arguments for future observation types
#'
#' @export
observe_behavior <- function(model, step, label = NULL) {
  observation_row <- tibble::tibble(
    Step = step,
    agent = vapply(model$agents, \(a) a$name, character(1)),
    Behavior = vapply(model$agents, \(a) as.character(a$behavior_current), character(1)),
    Fitness  = vapply(model$agents, \(a) a$fitness_current, numeric(1)),
    label = label
  )
  
  return(observation_row)
}


#' Function to gather opinion and stubbornness data at a single model `step`
#'
#' @param model An AgentBasedModel instance
#' @param step Current simulation step (integer)
#' @param label Optional label string for the trial
#' @param ... Additional arguments for future observation types
#'
#' @export
observe_opinion <- function(model, step, label = NULL, ...) {
  observation_row <- tibble::tibble(
    Step = step,
    agent = as.character(vapply(model$agents, \(a) a$name, character(1))),
    Opinions = as.double(map_vec(model$agents, \(a) as.numeric(a$opinions))),
    Stubbornness = as.double(map_vec(model$agents, \(a) 1 - as.numeric(a$receptivity))),
    label = label
  )
  
  return(observation_row)
}


#' Wrap observer function with a label
#'
#' @export
Observer <- R6::R6Class(
  "Observer",
  public = list(
    observe_fn = NULL,
    label = NULL,
    
    initialize = function(observe_fn, label = NULL) {
      self$observe_fn <- observe_fn
      self$label <- label
      return(invisible(self))
    },
    
    observe = function(model, step, obs_label = NULL,...) {
      return(self$observe_fn(model, step, label = obs_label, ...))
    }
  )
)


#' Call this to get a new Observer for behaviors.
#'
#' @export
new_behavior_observer <- function(observer_label = "behavior") {
  return(Observer$new(observe_behavior, observer_label))
}


#' Call this to get a new Observer for opinions.
#'
#' @export
new_opinion_observer <- function(observer_label = "opinion") {
  return(Observer$new(observe_opinion, observer_label))
}
