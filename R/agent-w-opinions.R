library(purrr)

# -------------------------------
# Stubbornness function
# -------------------------------

stubbornness <- function(o, alpha) {
  val <- 1.0 / (1.0 + abs(o)^alpha)
  val[val < 0.0] <- 0.0  # numerical guard; freezes opinions
  val
}


#' Opinion agent extends base socmod::Agent to add opinions and stubbornness
#'
#' @export
OpinionAgent <- R6::R6Class(
  classname = "OpinionAgent",
  inherit = socmod::Agent,
  
  public = list(
    # these are new fields in OpinionAgent that Agent doesn't have
    next_opinions = NULL,
    opinions = NULL,
    stubbornness = NULL,
    alpha = 1.0,  #  stubborn extremism increases with alpha
    
    # make opinion stepping internal, unlike current social learning dynamics
    step_opinions = function() {
      if (is.null(self$next_opinions)) {
        stop("next_opinions is NULL; cannot step opinions")
      }
      
      # commit new opinions
      self$opinions <- self$next_opinions
      self$next_opinions <- NULL
      
      # return this OpinionAgent silently for chaining
      return(invisible(self))
    },
    
    # init for OpinionAgent$new constructor—`id` and `...` go to Agent$new
    initialize = function(id, cultural_complexity = 2, 
                          bounded = FALSE, max_op_mag = 1.0,
                          init_op_mean = 0.0, init_op_sd = 1.0,
                          init_opinions = NULL, alpha = 1.0, ...) {
      
      # base Agent class init for id (req'd); opt'l are name, behavior, fitness
      super$initialize(id = id, ...)
      
      # use initial opinions if user provides
      if (!is.null(init_opinions)) {
        self$opinions <- init_opinions
        cultural_complexity <- length(self$opinions)
        
      # otherwise create random opinions...
      } else {
        # ...use a uniform distro over -1 to 1 if bounded...
        if (bounded) {
          self$opinions <- runif(cultural_complexity, 
                                 min = -max_op_mag, 
                                 max = max_op_mag)
        # ...or a normal distribution if not.
        } else {
          self$opinions <- rnorm(cultural_complexity, 
                                 mean = init_op_mean, 
                                 sd = init_op_sd)
        }
      }
      
      self$alpha <- alpha
      
      # init stubbornness vector, one entry for each opinion in opinion vector
      self$stubbornness <- stubbornness(self$opinions, self$alpha)
      
      return(invisible(self))
    }
  ),
  private = list()
)

