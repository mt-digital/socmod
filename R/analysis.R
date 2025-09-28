
#' Plot behavior adoption on a network
#'
#' Visualizes agent behaviors in a network using ggnetwork. Accepts a Trial
#' or AgentBasedModel and colors nodes by behavior.
#'
#' @param x A `Trial` or `AgentBasedModel`.
#' @param behaviors Behavior levels. Default: `c("Adaptive", "Legacy")`.
#' @param behavior_colors Color palette. Default: first 2 of `SOCMOD_PALETTE`.
#' @param node_size Single number or named list (e.g. `list(Degree = igraph::degree)`).
#' @param label Whether to show node labels. Default: TRUE.
#' @param plot_mod A function to modify the ggplot object. Default: `identity`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' sw_net <- socmod::make_small_world(N = 20, k = 6, p = 0.5)
#' abm <- make_abm(graph = sw_net) |> initialize_agents(initial_prevalence = 0.2)
#' plot_network_adoption(abm)
#'
#' # Use degree centrality to size nodes
#' plot_network_adoption(abm, node_size = list(Degree = igraph::degree))
#'
#' # Add a title and modify legend position
#' plot_network_adoption(
#'   abm,
#'   plot_mod = \(x) {
#'     x |>
#'     ggplot2::ggtitle("Adoption at t = 0") |>
#'     ggplot2::theme(legend.position = "bottom")
#'   }
#' )
#' @export
plot_network_adoption <- function(
    x, layout = NULL, behaviors = c("Adaptive", "Legacy"),
    behavior_colors = SOCMOD_PALETTE[c(2,1)], node_size = 6,
    label = FALSE, plot_mod = identity, edgewidth = 1
  ) {
  
  if (inherits(x, "Trial")) {
    model <- x$model
  } else if (inherits(x, "AgentBasedModel")) {
    model <- x
  } else {
    stop("Input must be a Trial or AgentBasedModel.")
  }

  net <- model$get_network()
  behavior_vec <- vapply(model$agents, function(a) a$get_behavior(), character(1))
  net <- igraph::set_vertex_attr(net, "Behavior", value = behavior_vec)

  use_size_aes <- FALSE
  if (is.list(node_size)) {
    stopifnot(length(node_size) == 1, !is.null(names(node_size)))
    result <- .compute_node_size_measure(net, node_size)
    net <- result$net
    measure_name <- result$measure_name
    use_size_aes <- TRUE
  } else {
    stopifnot(is.numeric(node_size), length(node_size) == 1)
  }
  df <- tibble::tibble()
  if (is.null(layout)) {
    df <- ggnetwork::ggnetwork(net)
  } else {
    df <- ggnetwork::ggnetwork(net, layout = layout)
  }
  aes_base <- ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  if (use_size_aes) {
    aes_base$size <- rlang::sym(measure_name)
  }

  p <- 
    ggplot2::ggplot(df, mapping = aes_base) +
      ggnetwork::geom_edges(color="lightgrey", linewidth = edgewidth) +
    (if (use_size_aes) ggnetwork::geom_nodes(aes(color = Behavior)) 
     else ggnetwork::geom_nodes(aes(color = Behavior), size = node_size)) +

    ggplot2::scale_color_manual(values = setNames(behavior_colors, behaviors), 
                                limits = behaviors, na.value = "gray80") +
    ggnetwork::theme_blank()

  if (label) {
    p <- 
      p + 
        ggnetwork::geom_nodelabel_repel(ggplot2::aes(label = name), size = 1.5)
  }

  return (plot_mod(p))
}


# Internal helper
.compute_node_size_measure <- function(net, node_size) {
  measure_name <- names(node_size)[1]
  measure_fun <- node_size[[1]]

  assertthat::assert_that(
    is.function(measure_fun),
    msg = "node_size value must be a function (e.g. list(Degree = igraph::degree))"
  )

  measure_vec <- measure_fun(net)
  assertthat::assert_that(
    length(measure_vec) == igraph::vcount(net),
    msg = "node_size function must return one value per vertex"
  )

  net <- igraph::set_vertex_attr(net, measure_name, value = measure_vec)
  return (list(net = net, measure_name = measure_name))
}


#' Plot adoption counts of selected behaviors over time
#'
#' @param trial A Trial object
#' @param tracked_behaviors Character vector of behaviors to track (e.g., c("Adaptive", "Legacy"))
#' @return A ggplot object
#' @export
#' @examples
plot_prevalence <- function(trials_or_tibble, 
                            tracked_behaviors = 
                              c("Legacy", "Adaptive"),
                            theme_size = 16) {
  # Initialize the prevalence table, summarising if necessary 
  prevalence_tbl <- trials_or_tibble
  if (!inherits(trials_or_tibble, "tbl_df")) {
    prevalence_tbl <- summarise_prevalence(
      trials_or_tibble, tracked_behaviors = tracked_behaviors
    )
    # After summarise_prevalence() returns prevalence_tbl
    prevalence_tbl <- prevalence_tbl %>%
      dplyr::filter(Behavior %in% tracked_behaviors)
  }
  
  # Ensure all tracked behaviors are present
  prevalence_tbl <- dplyr::mutate(
    prevalence_tbl, 
    Behavior = factor(Behavior, levels = tracked_behaviors)
  ) %>% dplyr::arrange(Behavior)
  
  
  # Use socmod colors; lookup table for later loading
  socmod_behavior_colors <- setNames(
    list(
      SOCMOD_PALETTE[["green_1"]],
      SOCMOD_PALETTE[["red"]]
    ),
    tracked_behaviors
  )
  
  palette_subset <- socmod_behavior_colors[tracked_behaviors]
  
  # Plot dynamics
  p <- 
    prevalence_tbl %>%
      ggplot2::ggplot(
        ggplot2::aes(x = Step, y = Prevalence, color = Behavior)
      ) +
      ggplot2::geom_line(linewidth = 1.15) +
      ggplot2::theme_classic(base_size = theme_size) +
      ggplot2::scale_color_manual(
        values = unlist(palette_subset),
        limits = tracked_behaviors
      ) +
      ggplot2::guides(color = guide_legend(reverse = TRUE))
  
  return (p)
}


#' Summarize behavior prevalence over time within or across trials
#'
#' This function summarizes the prevalence of tracked behaviors over time,
#' either returning a summary for each individual trial or averaging across multiple trials.
#' Prevalence is normalized by the number of agents in each trial.
#'
#' @param trials_or_trial `Trial` object or a list of `Trial` objects
#' @param input_parameters Character vector or NULL (default), required for list of `Trial` input
#' @param tracked_behaviors Character vector of behavior names to include in the summary.
#'   Defaults to \code{"Adaptive"}.
#' @param between_trials Logical. If TRUE (default), returns a summary aggregated across trials.
#'   If FALSE, returns per-trial prevalence values with a `trial_id` column distinguishing replicates.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{Step}{The time step (from observations)}
#'     \item{Behavior}{The behavior being tracked}
#'     \item{Count}{The number of agents exhibiting this behavior at this Step}
#'     \item{Prevalence}{The fraction of agents exhibiting this behavior (Count / n_agents)}
#'     \item{trial_id}{The trial index (only included if \code{between_trials = FALSE})}
#'     \item{<input parameters>}{One column per input parameter in the Trial's model}
#'   }
#'
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
#' adaptive_fitness_vals <- c(0.9, 1.1) 
#' trials <-
#'   run_trials(
#'     abm_gen,
#'     n_trials_per_param = 2,
#'     stop = socmod::fixated,
#'     n_agents = 20,
#'     initial_prevalence = 0.1,
#'     adaptive_fitness = adaptive_fitness_vals
#' )
#' summary <- summarise_prevalence(
#'   trials, input_parameters = "adaptive_fitness", across_trials = FALSE
#' )
#'
#' @export
summarise_prevalence <- function(trials_or_trial,
                                 input_parameters = NULL,
                                 tracked_behaviors = c("Adaptive"),
                                 across_trials = TRUE) {
  trials <- if (inherits(trials_or_trial, "Trial")) {
    list(trials_or_trial)
  } else {
    stopifnot(is.list(trials_or_trial), all(purrr::map_lgl(trials_or_trial, ~ inherits(.x, "Trial"))))
    trials_or_trial
  }
  
  
  prevalence_tbl <- purrr::imap_dfr(trials, function(trial, trial_index) {
    obs <- trial$get_observations()
    n_agents <- trial$model$get_parameter("n_agents")
    
    obs <- dplyr::mutate(
      obs, Behavior = factor(Behavior, levels = tracked_behaviors)
    ) %>% dplyr::filter(Behavior %in% tracked_behaviors)
    
    summary <- 
      obs %>% 
        dplyr::group_by(Step, Behavior) %>%
        dplyr::summarise(Count = dplyr::n(), .groups = "drop") %>%
        tidyr::complete(Step, Behavior, fill = list(Count = 0)) %>%
        dplyr::mutate(
          Prevalence = Count / n_agents
        )
    
    param_list <- 
      trial$model$get_parameters()$as_list() %>% .clean_summary_params

    param_list$model_dynamics <- 
      param_list$model_dynamics$get_label()
    
    # Assign default label to this trial's model's graph if missing
    if (!"label" %in% names(igraph::graph_attr(param_list$graph))) {
      n <- igraph::gorder(param_list$graph)
      e <- igraph::gsize(param_list$graph)
      igraph::graph_attr(param_list$graph, "label") <- paste0("G(n=", n, ", e=", e, ")")
    }
    # Assign 
    param_list$graph <- igraph::graph_attr(param_list$graph, "label")
    
    # Append model parameters and any input parameters if they exist, 
    # starting with the case where there are no independent input_parameters
    if (is.null(input_parameters)) {
      across_trials <- FALSE
      # Nothing to do if we don't have any specified input_parameters
      input_parameters_list <- param_list
    # Now deal with the case where there are input parameters varied across trials
    } else {
      # Stop if any desired input_parameters are missing from param_list
      missing_params <- setdiff(input_parameters, names(param_list))
      if (length(missing_params) > 0) {
        stop("Missing parameters: ", paste(missing_params, collapse = ", "))
      }
      # Extract specified input parameters
      input_parameters_list <- param_list[input_parameters]
      # Append trial index
      input_parameters_list$trial_id <- trial_index
    }
    
    # Return the row summarized within the trial, maybe with input parameters
    return (
      tibble::as_tibble(input_parameters_list) %>% 
      dplyr::bind_cols(summary)
    )
  })
  
  if (across_trials) {
    prevalence_tbl <- prevalence_tbl %>%
      dplyr::group_by(across(all_of(input_parameters)), Step, Behavior) %>%
      dplyr::summarise(
        Count = mean(Count),
        Prevalence = mean(Prevalence),
        .groups = "drop"
      )
  }
  
  return(prevalence_tbl)
}



#' Summarize outcomes across trials by input parameters
#'
#' This function summarizes trial-level outcomes by grouping across input parameters.
#' It computes the mean of specified outcome measures across all trials sharing the same input parameter values.
#'
#' @param trials A list of `Trial` objects
#' @param input_parameters Character vector of parameter names to group by
#' @param outcome_measures Character vector of outcome variable names to summarize
#'
#' @return A tibble with one row per unique combination of input parameters,
#'   containing the mean of each specified outcome measure.
#'
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
#' adaptive_fitness_vals <- c(0.9, 1.1)
#' trials <-
#'   run_trials(
#'     abm_gen,
#'     n_trials_per_param = 2,
#'     stop = socmod::fixated,
#'     n_agents = 20,
#'     initial_prevalence = 0.1,
#'     adaptive_fitness = adaptive_fitness_vals
#' )
#' outcomes <- summarise_outcomes(
#'   trials, 
#'   input_parameters = "adaptive_fitness", 
#'   outcome_measures = c("success_rate", "mean_fixation_steps")
#' ) 
#' 
#' max_fix_time <- max(outcomes$Value[outcomes$Measure == "mean_fixation_steps"])
#' # Normalize to calculate mean fixation time as a fraction of maximum
#' outcomes_norm <- outcomes |>
#'   dplyr::mutate(Value = dplyr::case_when(
#'     Measure == "mean_fixation_steps" ~ Value / max_fix_time,
#'     TRUE ~ Value
#'   ))
#' # Rename and set order of Measure factors to avoid messing with the legend in plotting
#' outcomes_norm$Measure[outcomes_norm$Measure == "success_rate"] <- "Success rate"
#' outcomes_norm$Measure[outcomes_norm$Measure == "mean_fixation_steps"] <- "Normalized fixation time"
#' outcomes_norm$Measure <- factor(outcomes_norm$Measure, levels = c(
#'   "Success rate", "Normalized fixation time"
#' ))

#' @export
summarise_outcomes <- function(trials, input_parameters, 
                               outcome_measures = NULL) {
  assertthat::assert_that(
    is.list(trials),
    all(purrr::map_lgl(trials, ~ inherits(.x, "Trial")))
  )
  
  # outcomes <- purrr::map_dfr(trials, function(trial) {
  #   trial$get_outcomes()
  # }, .id = "trial_id")
  
  
  outcomes <- purrr::imap_dfr(trials, function(trial, trial_index) {
    
    param_list <- 
      trial$model$get_parameters()$as_list() %>% .clean_summary_params

    param_list$model_dynamics <- param_list$model_dynamics$get_label()
    graph_label <- igraph::graph_attr(param_list$graph, "label")
    param_list$graph <- param_list$graph_label
    
    
    row <- trial$model$get_parameters()$as_list()[input_parameters]
    row$adaptation_success <- trial$get_outcomes()$adaptation_success
    
    row$fixation_steps <- trial$get_outcomes()$fixation_steps
    row$trial_id <- trial_index
    
    return (tibble::as_tibble(row))
  })
  
  summary <- 
    dplyr::group_by(outcomes, across(all_of(input_parameters))) %>%
    dplyr::summarise(
      success_rate = mean(adaptation_success),
      mean_fixation_steps = mean(fixation_steps),
      .groups = "drop"
    )
  
  if (!is.null(outcome_measures)) {
    summary <- summary %>%
      tidyr::pivot_longer(all_of(outcome_measures),
                          names_to = "Measure",
                          values_to = "Value")
  }
  
  return (summary)
}


#' Initialize agents with adaptive and legacy behaviors
#'
#' Assigns behaviors and fitness values to agents in an AgentBasedModel.
#' Can initialize by proportion or fixed count of adaptive agents.
#'
#' @param model An `AgentBasedModel` instance.
#' @param initial_prevalence A proportion (0–1) or count of agents starting with the adaptive behavior.
#' @param adaptive_behavior Name of the adaptive behavior (default: "Adaptive").
#' @param adaptive_fitness Fitness value for adaptive behavior (default: 1.2).
#' @param legacy_behavior Name of the legacy behavior (default: "Legacy").
#' @param legacy_fitness Fitness value for legacy behavior (default: 1.0).
#'
#' @return Invisibly returns the model with updated agents.
#' 
#' @examples
#' # Create a model with 20 agents, 25% with adaptive behavior
#' abm <- 
#'   make_abm(n_agents = 20) |> initialize_agents(initial_prevalence = 0.25)
#'
#' # Count how many agents do each behavior
#' table(purrr::map_chr(abm$agents, ~ .x$get_behavior()))
#'
#' # Summarize fitness values by behavior
#' tibble::tibble(
#'   behavior = purrr::map_chr(abm$agents, ~ .x$get_behavior()),
#'   fitness = purrr::map_dbl(abm$agents, ~ .x$get_fitness())
#' ) |>
#'   dplyr::group_by(behavior) |>
#'   dplyr::summarise(count = dplyr::n(), 
#'                    mean_fitness = mean(fitness), 
#'                    .groups = "drop")
#'   
#' @export
initialize_agents <- function(model,
                              initial_prevalence = 0.1, 
                              adaptive_behavior = "Adaptive",
                              adaptive_fitness = 1.2,
                              legacy_behavior = "Legacy",
                              legacy_fitness = 1.0) {
  # Get number of each type of agent
  n_agents <- model$get_parameter("n_agents")
  
  # Handle either double- or integer-valued (i.e. % or count) initial_prevalence
  if (is.numeric(initial_prevalence)) {
    if (initial_prevalence <= 1) {
      n_adaptive <- round(n_agents * initial_prevalence)
    } else {
      n_adaptive <- as.integer(initial_prevalence)
    }
  } else {
    stop("initial_prevalence must be a numeric proportion (<=1) or integer count")
  }
  
  if (n_adaptive > n_agents) {
    stop("Number of adaptive agents exceeds total agents")
  }
  
  # Number of legacy agents is the difference between total and adaptive counts
  n_legacy <- n_agents - n_adaptive
  
  # Specify agent behaviors and fitnesses, assigned to agents below
  ids <- 1:n_agents
  adaptive_ids <- sample(ids, n_adaptive)
  legacy_ids <- setdiff(ids, adaptive_ids)
  # Each row here specifies one agent's attributes
  agent_spec <- tibble::tibble(
    id = c(adaptive_ids, legacy_ids),
    behavior = c(rep(adaptive_behavior, n_adaptive), 
                 rep(legacy_behavior, n_legacy)),
    fitness = c(rep(adaptive_fitness, n_adaptive), 
                rep(legacy_fitness, n_legacy))
  )
  
  # Set agent attributes using purrr::pwalk
  purrr::pwalk(agent_spec, \(id, behavior, fitness) {
    model$get_agent(id)$set_behavior(behavior)$set_fitness(fitness)
  })
  
  # Return the model to continue down the pipeline.
  return (invisible(model))
}


#' Custom color palette for scientific plots
#'
#' Recommended for use in `scale_color_manual()`.
#'
#' @return A named character vector of hex color codes
#' @export
SOCMOD_PALETTE <- c(
  red     = "#F24B4A",
  green_1 = "#007F7D",
  blue_1  = "#32BFFA",
  magenta = "#D000AC",
  blue_2  = "#320FFA",
  pink    = "#EE80FF",
  plum    = "#5E3B68",
  green_2 = "#32BF9A",
  sienna  = "#ED610F"
)

#' CVD-safe custom color palette for scientific plots
#'
#' Recommended for use in `scale_color_manual()`.
#'
#' @return A named character vector of hex color codes
#' @export
SOCMOD_PALETTE_CVD <- c(
  red     = "#E15759",
  green_1 = "#59A14F",
  blue_1  = "#32BFFA",
  magenta = "#B07AA1",
  blue_2  = "#4E79A7",
  pink    = "#EE80FF",
  plum    = "#5E3B68",
  green_2 = "#32BF9A",
  sienna  = "#ED610F"
)

# ------------------------------------------------------------------
# Helper functions for summarise_* and plot_* methods
# ------------------------------------------------------------------


#' Clean metadata parameters for summary functions
#'
#' Removes elements from a parameter list that are not safe to include
#' in `summarise_prevalence()` or `summarise_outcomes()`. An element is kept if:
#' - It is an atomic value of length 1 (e.g., a number or string)
#' - It is a single object instance (e.g., an R6 or S3 class object)
#'
#' Elements such as vectors of length > 1 or lists of objects are removed,
#' and a warning is issued listing the removed keys. This is necessary because 
#' may be helper parameters such as pre-computed lists of which agents are in 
#' which group, e.g., in homophily models.
#'
#' @param param_list A named list of metadata parameters.
#'
#' @return A cleaned version of \code{param_list} with unsupported entries removed.
#'
#' @keywords internal
.clean_summary_params <- function(param_list) {
  # Identify which parameters to keep
  keep <- vapply(
    param_list,
    function(x) {
      is.object(x) || (is.atomic(x) && length(x) == 1)
    },
    logical(1)
  )
  # Remove parameters that are not atomic length 1 or an object if necessary
  if (!all(keep)) {
    bad <- names(param_list)[!keep]
    warning(
      sprintf(
        "Removed non-summary-safe metadata: %s",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  return (param_list[keep])
}


