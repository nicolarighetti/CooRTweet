#' edge_weight_contribution
#' This function is a private utility function that calculate the contribution of each node on their
#' edge, and a contribution index that summarizes the equality of contributions between nodes on an
#' edge. When the faster network is provided, the equilibrium index is computed on the subset of
#' faster nodes. Otherwise, it is computed on the full network.
#'
#'
#' @description When one user share the same object_id in the defined time window many times
#' (such as a spammer), along with other less active users, the edge weight between them is
#' inflated by the hyper-activity of that user. This situation can occur in the case of (1)
#' outliers in the network such as a spammer, but also (2) in the case where object_ids are
#' prone to repetition by their very nature, such as a hashtag. The second possibility appears
#' rarer if object_ids are analyzed that, in a normal common context normally do not repeat
#' identically repeatedly, such as URLs, leaving mainly the problem of spammers.
#' To account for these possibilities, we compute the relative contribution of node on their
#' edge, and a index indicating the equality of contributions between nodes on an edge, that has
#' a value of 1 when the contributions are equal and 0 when there is maximum inequality.
#'
#' @param coord_graph The coord_graph resulting from the previous steps.
#' @param incidence_matrix the incidence_matrix resulting from the previous steps.
#'
#' @return The coord_graph with additional attributes from_contrib, to_contrib, and contrib_index
#'
#'

edge_weight_contribution <- function(coord_graph, incidence_matrix, fast_net, fast_net_col){

  # Function to calculate contributions to edge weight
  calculate_contributions <- function(edge, incidence_matrix) {
    nodes_involved <- ends(coord_graph, edge, names = TRUE)
    shared_objects_indices <-
      which(incidence_matrix[nodes_involved[1],] &
              incidence_matrix[nodes_involved[2],])

    from_contrib <- sum(incidence_matrix[nodes_involved[1], shared_objects_indices])
    to_contrib <- sum(incidence_matrix[nodes_involved[2], shared_objects_indices])
    total_contrib <- from_contrib + to_contrib

    c(from_contrib = from_contrib / total_contrib, to_contrib = to_contrib / total_contrib)
  }

    # Apply the function to all edges in the unimodal graph
    edge_ids <- igraph::E(coord_graph)
    contributions <- sapply(edge_ids, calculate_contributions, incidence_matrix)

    # Assign the calculated contributions as edge attributes
    igraph::E(coord_graph)$from_contrib <- contributions[1,]
    igraph::E(coord_graph)$to_contrib <- contributions[2,]

    # Assign the calculated equality index
    igraph::E(coord_graph)$contrib_index <-
      1 - abs(igraph::E(coord_graph)$from_contrib - igraph::E(coord_graph)$to_contrib)

  return(coord_graph)
}
