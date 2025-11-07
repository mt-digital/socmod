#' Agent-based model class
#'
#' Represents an agent-based model with specified parameters and agents.
#'
#' @section Methods:
#' \describe{
#'   \item{initialize(parameters = DEFAULT_PARAMETERS, agents)}{
#'     Creates a new AgentBasedModel with the given parameters and agents.
#'     \describe{
#'       \item{parameters}{A `ModelParameters` object specifying the model parameters. Defaults to `DEFAULT_PARAMETERS`.}
#'       \item{agents}{A list of agent instances for the model.}
#'     }
#'   }
#'   \item{get_parameters()}{Returns the `ModelParameters` instance for this model.}
#' }
#'
#' @export
AgentBasedModel <- R6::R6Class(
  "AgentBasedModel",
  public = list(
    agents = NULL,
    graph = NULL,

    #' @description Create a new AgentBasedModel instance
    #' @param parameters A `ModelParameters` object specifying the model parameters.
    #'   Defaults to `DEFAULT_PARAMETERS`
    #' @param agents A list of agent instances for the model
    initialize = function(parameters = DEFAULT_PARAMETERS, agents = NULL) {
      if (!inherits(parameters, "ModelParameters")) {
        stop("parameters must be a ModelParameters instance")
      }
      
      self$agents <- agents
      
      private$.parameters_instance <- parameters
      
      # otherwise read graph and n_agents from 
      graph <- parameters$get_graph()
      n_agents <- parameters$get_n_agents()

      # set n_agents for next step if user *only* gives agents — a bit hacky
      if (!is.null(agents) && is.null(graph) && is.null(n_agents)) {
        n_agents <- length(agents)
      } 

      # init graph...
      if (!is.null(graph)) {
        stopifnot(igraph::is_igraph(graph))
        self$graph <- graph
      # if n_agents given but not graph, init an empty graph by default
      } else if (!is.null(n_agents)) {
        self$graph <- igraph::make_empty_graph(n_agents, directed = FALSE)

        # update model parameters with new empty graph
        parameters$set_graph(self$graph)
      } # ... end of graph init
      
      # initialize agent names as "a1", "a2", etc
      if (is.null(igraph::V(self$graph)$name)) {
        igraph::V(self$graph)$name <- 
          paste0("a", seq_len(igraph::vcount(self$graph)))
      }
      
      # init with user-provided agents list if provided...
      if (!is.null(agents)) {
        # build named list to track agents in this abm's `agents` field
        self$agents <- agents
        names(self$agents) <- purrr::map_chr(self$agents, \(a) a$get_name())
        # user-provided agent names supersede igraph::graph names.
        graph_names <- igraph::V(self$graph)$name
        agent_names <- names(self$agents)
        
        if (!all(agent_names %in% graph_names)) {
          # overwrite igraph vertex names to match agent names
          igraph::V(self$graph)$name <- agent_names  
        }
        
        # ensure agents know who their neighbors are
        self$sync_network("neighbors_only")

        # set n_agents model param for O(1) lookup
        parameters$set_n_agents(length(agents))
      } else {

        legacy_fitness <- parameters$as_list()$legacy_fitness

        # Set default legacy_fitness (seems like this should be in DEFAULT_PARAMETERS).
        if (is.null(legacy_fitness)) {
          legacy_fitness <- 1.0
        }
        
        self$agents <- 
          purrr::map2(
            seq_len(igraph::vcount(self$graph)),
            igraph::V(self$graph)$name,
            \(i, nm) {
              Agent$new(id = i, name = nm, 
                        behavior = "Legacy", fitness = legacy_fitness)
            }
          )
        
        names(self$agents) <- purrr::map_chr(self$agents, \(a) a$get_name())
        self$sync_network("to_graph")
        self$sync_network("from_graph")
      }

      # Ensure a graph label exists, make up one if not: g(n_nodes,n_edges)
      graph_label <- igraph::graph_attr(self$graph, "label")
      if (is.null(graph_label)) {
        graph_label <- sprintf("g(%d,%d)", igraph::gorder(self$graph), igraph::gsize(self$graph))
      }
      self$graph <- igraph::set_graph_attr(self$graph, "label", graph_label)
      parameters$set_graph(self$graph)
      # Ensure the n_agents parameter is set
      parameters$set_n_agents(length(self$agents))
      # Set the ABM parameters once it contains all parameters
      self$set_parameters(parameters)
      
      return (invisible(self))
    },
    
    #' @description Synchronize agent and network fields
    #' @param direction "to_graph", "from_graph", or "neighbors_only"
    sync_network = function(direction = c("to_graph", "from_graph", "neighbors_only")) {
      
      direction <- match.arg(direction)
      
      for (agent in self$agents) {
        vname <- agent$get_name()
        
        if (direction == "from_graph") {
          
          igv <- igraph::V(self$graph)
          agent$set_name(igv[vname]$name)
          agent$set_behavior(igv[vname]$behavior_current)
          agent$set_next_behavior(igv[vname]$behavior_next)
          agent$set_fitness(igv[vname]$fitness_current)
          agent$set_next_fitness(igv[vname]$fitness_next)
          
        } else if (direction == "to_graph") {
          
          # Ensure graph vertex names match agent names before syncing
          igraph::V(self$graph)$name <- names(self$agents)
          vid <- agent$get_id()
          self$graph <- igraph::set_vertex_attr(
            self$graph, "behavior_current", index = vid, value = agent$get_behavior()
          )
          self$graph <- igraph::set_vertex_attr(
            self$graph,
            "behavior_next",
            index = vid,
            value = agent$get_next_behavior()
          )
          self$graph <- igraph::set_vertex_attr(
            self$graph, "fitness_current", index = vid, 
            value = agent$get_fitness()
          )
          self$graph <- igraph::set_vertex_attr(
            self$graph, "fitness_next", 
            index = vid, 
            value = agent$get_next_fitness()
          )
        }
      }
      
      if (direction %in% c("from_graph", "neighbors_only")) {
        for (agent in self$agents) {

          nbr_ids <- igraph::neighbors(self$graph, v = agent$get_name())
          
          neighbors <- purrr::map(
            nbr_ids,
            \(v) self$get_agent(igraph::V(self$graph)[v]$name)
          )
          
          agent$set_neighbors(Neighbors$new(neighbors))
        }
      }
    },
    
    #' @description Get the agent associated with a given ID or name
    #' @param key Integer index or character name
    get_agent = function(key) {
      if (is.character(key)) {
        return(self$agents[[key]])
      } else {
        return(self$agents[[key]])
      }
    },
    
    #' @description Return the igraph network (after syncing from agents)
    get_network = function() {
      # Ensure vertex names are consistent before syncing
      if (is.null(igraph::V(self$graph)$name) || 
          !all(names(self$agents) %in% igraph::V(self$graph)$name)) {
        igraph::V(self$graph)$name <- names(self$agents)
      }
      self$sync_network("to_graph")
      return(self$graph)
    },

    #' @description Get the of model parameters
    get_parameters = function() {
      return (private$.parameters_instance)
    },
    
    #' @description Set multiple model parameters
    #' @param params Named list of parameters to set
    set_parameters = function(params) {
      
      # If params is a ModelParameter instance, convert to list.
      if(inherits(params, "ModelParameters")) {

        private$.parameters_instance <- params
      
        # If it isn't ModelParameter nor a list, throw error.
      } else if (!is.list(params)) {
        stop(
          "Only ModelParameter instances and lists may be passed to Agent$set_parameters()."
        )
      } else {
        for (key in names(params)) {
          self$set_parameter(key, params[[key]])
        }
      }
    },
    
    #' @description Set a single model parameter
    #' @param key Parameter name
    #' @param value Parameter value
    set_parameter = function(key, value) {
      if (key == "learning_strategy") {
        private$.parameters_instance$set_learning_strategy(value)
      } else if (key == "graph") {
        private$.parameters_instance$set_graph(value)
      } else if (key == "n_agents") {
        private$.parameters_instance$set_n_agents(value)
      } else {
        private$.parameters_instance$add_auxiliary(key, value)      
      }

      return (invisible(self))
    },
    
    #' @description Get a single model parameter
    #' @param key Parameter name
    get_parameter = function(key) {
      return (private$.parameters_instance$as_list()[[key]])
    }
  ),
  
  private = list(
    .parameters = NULL,
    .parameters_instance = NULL
  )
  
)


#' Create an AgentBasedModel instance
#'
#' Initializes an `AgentBasedModel` with specified `parameters` and `agents`.
#' If `parameters` is `NULL`, the function constructs a `ModelParameters` object
#' using any additional arguments (`...`) passed to `make_model_parameters()`.
#'
#' @param parameters A `ModelParameters` instance specifying model context. If `NULL`, parameters are created using `make_model_parameters(...)`.
#' @param agents A list of `Agent` objects, typically created separately. Optional.
#' @param ... Additional arguments passed to `make_model_parameters()` if `parameters` is `NULL`.
#'
#' @return An `AgentBasedModel` instance.
#'
#' @examples
#' abm <- make_abm(n_agents = 10)
#' abm2 <- make_abm(parameters = make_model_parameters(n_agents = 10))
#' abm_g <- make_abm(graph = socmod::make_small_world(N = 20, k = 6, p = 0.2))
#' abm_g2 <- make_model_parameters(
#'   graph = socmod::make_small_world(N = 20, k = 6, p = 0.2)
#' ) |> make_abm()
#' @export
make_abm <- function(parameters = NULL, agents = NULL, ...) {

  if (is.null(parameters)) {
    dots <- list(...)
    parameters <- do.call(make_model_parameters, dots)
  }
  
  return (
    AgentBasedModel$new(
      parameters = parameters,
      agents = agents
    )
  )
}


#' Make an opinion dynamics ABM with OpinionAgents and social influence
#'
#' @export
make_opinion_abm <- function (parameters = NULL, agents = NULL, init_mean = 0.0,
                              init_sd = 1.0, ...) {
  if (is.null(parameters)) {
    dots <- list(...)
    parameters <- do.call(make_model_parameters, dots)
  }
  
  if (is.null(agents)) {
    n_agents <- parameters$get_n_agents()
    if (is.null(n_agents)) {
      stop("n_agents must be specified in parameters or agents must be provided.")
    }
    
    agents <- purrr::map2(
      seq_len(n_agents),
      paste0("a", seq_len(n_agents)),
      \(i, nm) {
        OpinionAgent$new(id = i, name = nm, 
                         init_op_mean = init_mean, 
                         init_op_sd = init_sd, cultural_complexity = 1)
      }
    )
  } else {
    # Ensure all agents are OpinionAgents
    if (!all(vapply(agents, \(a) inherits(a, "OpinionAgent"), logical(1)))) {
      stop("All agents must be OpinionAgent instances.")
    }
  }
  
  return (
    make_abm(
      agents = agents, model_dynamics = opinion_dynamics
    )
  )
}
