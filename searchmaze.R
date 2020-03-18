#' AStar subclass demonstrating 2D maze navigation.
#'
#' Has methods for all function arguments of astar.
#' Takes a 2d matrix with 0 for walkable tiles, and any other number for walls
#'
#' @examples
#' M <- matrix(ncol = 4, byrow = TRUE, c(
#'   0, 1, 0, 0,
#'   0, 1, 0, 1,
#'   0, 1, 0, 1,
#'   0, 0, 0, 0
#'   ))
#'
#' sm <- SearchMaze2D$new(M)
#' sm$run(c(1, 1), c(1, 4))
#'
#' @rdname SearchMaze2D_class
#' @export
SearchMaze2D <- R6::R6Class(
  "SearchMaze2D",
  inherit = AStar,
  public = list(
    # attributes
    M = NULL,
    M_dim = NULL,
    density_mat = NULL,
    coeff = NULL,
    adjacent = list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1)),
    # methods
    initialize = function (M, density_mat, coeff) {
      self$M <- M
      self$M_dim <- dim(M)
      self$density_mat <- density_mat
      self$coeff <- coeff
    },
    neighbors = function(node) {
      candidates <- lapply(self$adjacent, function(x) x + node)
      Filter(function(ind) all(ind > 0) & all(ind < self$M_dim + 1), candidates)
    },
    hash_func = function(x) {
      paste(x, collapse = '-')
    },
    is_goal_reached = function(src, dst) {
      identical(src, dst)
    },
    edge_distance = function(src, dst, density_mat, coeff) {
      if (self$M[dst[1], dst[2]] != 0) Inf else sum(abs(src - dst))+density_mat[dst[1],dst[2]]*coeff
    },
    cost_estimate = function(node, goal) {
      sum(abs(node - goal))
    }
  )
)
