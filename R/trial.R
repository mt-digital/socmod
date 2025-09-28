#' Trial class for running a single simulation
#'
#' Represents a single run of an AgentBasedModel over time, with customizable
#' interaction and update logic. Tracks time-series observations and outcome
#' measures such as adaptation success and steps to fixation.
#'
#' @export
Trial <- R6::R6Class(
  "Trial",
  public = list(
    
    model = NULL,
    observations = NULL,
    outcomes = NULL,
    metadata = list(),
    label = NULL,
    
    #' @description Initialize a Trial with a model and functions
    #' @param model An AgentBasedModel instance
    #' @param metadata Label-value metadata store for trial information
    initialize = function(model, metadata = list(), label = NULL) {

      # Sync internal variables with user-provided.
      self$model <- model
      self$metadata <- metadata

      # Initialize outputs: observations and outcomes.
      self$observations <- tibble::tibble()
      self$outcomes <- list()

      invisible (self)
    },
    
    #' @description Run the model and collect results
    #' @param stop Either integer for max steps, or predicate function
    #' @param legacy_behavior The maladaptive behavior treated as "adaptation failure"
    #' @param adaptive_behavior The behavior treated as "adaptation success"
    run = function(
        stop = 50, legacy_behavior = "Legacy", 
        adaptive_behavior = "Adaptive", observer = new_behavior_observer()) {
      
      step <- 0

      self$model$set_parameter("legacy_behavior", legacy_behavior)
      self$model$set_parameter("adaptive_behavior", adaptive_behavior)
      step <- 0

      obs_list <- list()
      
      # Get learning and iteration functions from the model's learning strategy.
      lstrat <- self$model$get_parameter("model_dynamics")
      partner_selection <- lstrat$get_partner_selection()
      interaction <- lstrat$get_interaction()
      model_step <- lstrat$get_model_step()
      
      repeat {
        # --- run one model step ---
        # Partner selection and interaction with selected partner.
        for (agent in self$model$agents) {
          partner <- NULL
          if (!is.null(partner_selection)) {
            partner <- partner_selection(agent, self$model)
          }
          interaction(agent, partner, self$model)
        }
        
        # Run model step function if provided.
        if (!is.null(model_step)) {
          model_step(self$model)
        }

        # --- `observer` `observes`behaviors (default) or opinions ---
        obs_list[[step + 1]] <- observer$observe(
          self$model,
          step = step
        )

        # --- stopping condition ---
        step <- step + 1
        if (is.numeric(stop) && step >= stop) break
        if (is.function(stop) && stop(self$model)) break
      }

      # bind all observations
      self$observations <- dplyr::bind_rows(obs_list)

      # if we observe behavior, identify whether this is adaptive success or not
      if (observer$label == "behavior") {
        behaviors <- unlist(
          purrr::map(
            self$model$agents, \(a) as.character(a$get_behavior())
          ), 
          use.names = FALSE
        )
        self$outcomes$adaptation_success <- 
          length(unique(behaviors)) == 1 && 
          unique(behaviors) == adaptive_behavior
        
        self$outcomes$fixation_steps <- step
      }
      
      return(invisible(self))
    },
    
    #' Add or update metadata in a Trial object
    #'
    #' @param self The Trial object
    #' @param new_metadata A named list to merge into existing metadata
    add_metadata = function(new_metadata) {
      self$metadata <- modifyList(self$metadata, new_metadata)
      invisible(self)
    },
    
    #' @description Return the trial's metadata as a named list.
    #' @return A named list containing metadata values for this trial, including
    #' any scalar or function-valued inputs specified during setup.
    get_metadata = function() {
      return (self$metadata)
    },
    
    #' @description Return the observation data
    get_observations = function() {
      return (self$observations)
    },
    
    #' @description Return outcome measures
    get_outcomes = function() {
      return (self$outcomes)
    },
    
    #' @description Return the label for this trial (if set)
    get_label = function() {
      return (self$label)
    }
  )
)


#' Predicate function: has the model fixated on a single behavior?
#'
#' @param model An AgentBasedModel
#' @return TRUE if all agents have the same behavior
#' @export
#' @examples
#' net <- igraph::make_graph(~ 1-2)
#' model <- make_abm(graph = net)
#' trial <- run_trial(model, stop = fixated) # <- "stop trial when fixated"
fixated <- function(model) {
  behaviors <- unlist(
    purrr::map(model$agents, 
               \(a) as.character(a$get_behavior())), 
               use.names = FALSE)
  
  length(unique(behaviors)) == 1
}


#' Trial runner helper function
#'
#' @param model An AgentBasedModel
#' @param stop Stopping condition: max steps (int) or predicate function
#' @param adaptive_behavior The behavior treated as "adaptation success". Default is "Adaptive".
#' @param adaptive_behavior The behavior treated as "adaptation success". Default is "Adaptive".
#' @return A Trial object
#' @examples
#' agents <- c(
#'   Agent$new(id = 1, name = "1", behavior = "Legacy", fitness = 1),
#'   Agent$new(id = 2, name = "2", behavior = "Adaptive", fitness = 4)
#' )
#' net <- igraph::make_graph(~ 1-2)
#' model <- make_abm(agents = agents, graph = net)
#' trial <- run_trial(model, stop = 10)
#' @export
run_trial <- function(model,
                      observer = new_behavior_observer(),
                      stop = socmod::fixated,
                      legacy_behavior = "Legacy",
                      adaptive_behavior = "Adaptive",
                      metadata = list()) {

  # Initialize, run, and return a new Trial object.
  return (
    Trial$new(model = model, metadata = metadata)$run( 
      observer = observer, stop = stop, 
      legacy_behavior = legacy_behavior, adaptive_behavior = adaptive_behavior
      )
  )
}


#' Run a grid of trial ensembles with parameter metadata
#'
#' Runs trial ensembles across a parameter grid. All scalar and function-valued parameters
#' used in model construction or trial dynamics are included in metadata for transparency.
#' @param model_generator Function that returns a new AgentBasedModel instance according to model_parameters, a named list of parameter label-value pairs.
#' @param n_trials_per_param Number of trials per parameter combination.
#' @param stop Stopping condition (number or function).
#' @param .progress Whether to show progressbar when running the trials.
#' @param ... List of parameter label-value pairs; vector or singleton values.
#'
#' @return A list of Trial objects 
#' @examples
#' abm_gen <- function(params) {
#'   params$graph <- make_small_world(params$n_agents, 6, 0.5)
#'   return (do.call(make_abm, params) |>
#'             initialize_agents(
#'               initial_prevalence = params$initial_prevalence,
#'               adaptive_fitness = params$adaptive_fitness
#'             )
#'   )
#' }
#' trials <-
#'   run_trials(
#'     abm_gen,
#'     n_trials_per_param = 2,
#'     stop = socmod::fixated,
#'     n_agents = 20,
#'     initial_prevalence = 0.1,
#'     adaptive_fitness = c(0.9, 1.1, 1.3)
#' )
#' @export
run_trials <- function(model_generator, n_trials_per_param = 10,
                       stop = 10, .progress = TRUE, 
                       observer = new_behavior_observer(),
                       syncfile = NULL, overwrite = FALSE, ...) {
  
  # Check if syncfile is given...
  if (!is.null(syncfile)) {
    # ...and load it if it exists and we aren't overwriting existing
    if (file.exists(syncfile) && !overwrite) {
      cat("\nLoading trials from syncfile:", syncfile, "\n\n")
      load(syncfile)
      return (trials)
    }
  }

  # Initialize dataframe where each row is a set of model parameters
  model_parameters <- c(list(...), 
                        list(replication_id = 1:(n_trials_per_param)))
  
  parameter_grid <- tidyr::crossing(!!!model_parameters)
  
  legacy_behavior <- "Legacy"
  adaptive_behavior <- "Adaptive"
  if ("legacy_behavior" %in% model_parameters) {
    legacy_behavior <- model_parameters$legacy_behavior
  }
  if ("adaptive_behavior" %in% model_parameters) {
    adaptive_behavior <- model_parameters$adaptive_behavior
  }

  # list of trials returned, each initialized from the grid
  trials <- purrr::pmap(
    parameter_grid, function(...) {
      param_list <- list(...)
      model <- model_generator(param_list)
      
      run_trial( 
        model, stop, legacy_behavior, adaptive_behavior, 
        metadata = list(replication_id = param_list$replication_id),
        observer = observer
      )
    }, 
    .progress = .progress
  )

  # Check if syncfile is given...
  if (!is.null(syncfile)) {
    # ...and write if it hasn't been synced or overite is TRUE
    if (!file.exists(syncfile) || overwrite) {
      save(trials, file = syncfile)
    }
  }

  return (trials)
}


