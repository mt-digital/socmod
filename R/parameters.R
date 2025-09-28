ModelParameters <- R6::R6Class(
  
  "ModelParameters",
  
  public = list(
    initialize = function(model_dynamics = success_bias_strategy, 
                          graph = NULL, n_agents = NULL,
                          auxiliary = list()) {
      
      private$.model_dynamics <- model_dynamics
      private$.graph <- graph
      private$.n_agents <- n_agents
      private$.auxiliary <- auxiliary
    },
    
    get_model_dynamics = function() {
      return (private$.model_dynamics)
    },

    set_model_dynamics = function(model_dynamics) {
      stopifnot(inherits(model_dynamics, "LearningStrategy"))
      private$.model_dynamics <- model_dynamics

      return (invisible(self))
    },
    
    get_graph = function() {
      return (private$.graph)
    },

    set_graph = function(graph) {
      private$.graph <- graph

      return (invisible(self))
    },
    
    get_n_agents = function() {
      return (private$.n_agents)
    },

    set_n_agents = function(n_agents) {
      private$.n_agents <- n_agents

      return (invisible(self))
    },
    
    get_auxiliary = function() {
      return (private$.auxiliary)
    },
    
    #' Overwrite existing auxiliary parameters.
    #' 
    #' @return self silently
    set_auxiliary = function(params) {
      private$.auxiliary <- params

      return (invisible(self))
    },
    
    #' Add a key-value pair to the auxiliary 
    #' variables.
    #' 
    #' @return self silently
    add_auxiliary = function(key, value) {
      private$.auxiliary[[key]] <- value
      return (invisible(self))
    },
    
    #' Get all parameter values as list
    #'
    #' @return list of parameters
    as_list = function() {

      return (
        modifyList(
          list(
            model_dynamics = self$get_model_dynamics(),
            graph = private$.graph,
            n_agents = self$get_n_agents()
          ),
          self$get_auxiliary() 
      ))
    }
  ),
  
  private = list(
    .model_dynamics = NULL,
    .graph = NULL,
    .n_agents = NULL,
    .auxiliary = list()
  )
)


#' Wrapper for initializing new ModelParameters instance.
#' 
#' @param model_dynamics Learning strategy to use; must be type LearningStrategy
#' @param graph Graph object to use; must inherit igraph
#' @param n_agents Number of agents in the model
#' @param ... Additional model parameters
#' @examples
#' # example code
#' 
#' @export
make_model_parameters <- 
  function(model_dynamics =
             success_bias_model_dynamics, 
           graph = NULL,
           n_agents = NULL,
           ...)   {
  return (
    ModelParameters$new(model_dynamics, graph, n_agents, list(...))  
  )
}


#' Default parameters to create an agent-based model.
DEFAULT_PARAMETERS <- make_model_parameters(
  model_dynamics = NULL, graph = NULL, n_agents = 10, auxiliary = list()
) 
