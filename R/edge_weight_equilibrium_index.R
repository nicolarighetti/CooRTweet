#' edge_weight_equilibrium_index
#' Calculate a edge weight equilibrium index that summarizes the contribution of each vertex
#' to their total edge weight. When the faster network is provided, the equilibrium index
#' is computed on the subset of faster nodes. Otherwise, it is computed on the full network.
#'
#' This function is a private utility function that calculates summary measures of edge
#' weight contribution.
#'
#' @description When one user share the same object_id in the defined time window many times
#' (such as a spammer), along with other less active users, the edge weight between them is
#' inflated by the hyper-activity of that user. This situation can occur in the case of (1)
#' outliers in the network such as a spammer, but also (2) in the case where object_ids are
#' prone to repetition by their very nature, such as a hashtag. The second possibility appears
#' rarer if object_ids are analyzed that, in a normal common context normally do not repeat
#' identically repeatedly, such as URLs, leaving mainly the problem of spammers.
#' To account for these possibilities, we define a measure of relative contribution to the
#' edge weight (edge_eq), which can be used to analyze or filter the resulting graph.
#'
#' @param g The coord_graph resulting from the previous steps.
#' @param x A data.table (result from `detect_coordinated_groups`)
#'
#' @return The coord_graph with additional attribute edge_eq.
#'
#'

edge_weight_equilibrium_index <- function(g = coord_graph, x = x, df = df){

  # ------------------------------
  # If the faster network has been identified we calculate the filtered edge list for the
  # fast network and we calculate the edge weight equilibrium index for this subset
  if (fast_net == TRUE){

    fast_net_col <- names(x)[grep("time_window_", names(x))]
    x <- x[x[[fast_net_col]] == 1, ]

    df <- data.table::melt(x,
                           id.vars = c("object_id", "time_delta"),
                           measure.vars = patterns("^content_id", "^id_user"),
                           value.name = c("content_id", "id_user")
    )

    if (intent == "users") {
      subcols <- c(nodes, c("object_id", "content_id"))
    } else {
      subcols <- c(nodes, "object_id")
    }

    # Filtered 'df' object
    df <- unique(df[, subcols, with = FALSE])
    df <- df[, lapply(.SD, as.factor)]

    # Convert (filtered) edge list of g to data.table
    edge_dt <- as.data.table(as_data_frame(g))

    edge_dt <- edge_dt[edge_dt[[fast_net_col]] == 1, ]
  }

  # If the faster network has not been calculated we proceed with the unfiltered edge list
  # and the original tables

  # Convert (full) edge list of g to data.table
  edge_dt <- as.data.table(as_data_frame(g))

  # ------------------------------
  # Calculate the edge weight equilibrium index from the actions initiated by each users

  # Count actions initiated by each user for each object
  action_counts <- df[, .N, by = .(id_user, object_id)]
  setnames(action_counts, "N", "actions")

  # Merge edge data table with action counts
  edge_dt <- merge(edge_dt, action_counts, by.x = "from", by.y = "id_user", all.x = TRUE)
  edge_dt <-
    merge(
      edge_dt,
      action_counts,
      by.x = c("to", "object_id"),
      by.y = c("id_user", "object_id"),
      all.x = TRUE,
      suffixes = c("_from", "_to")
    )

  # Replace NA with 0 in actions columns
  edge_dt[is.na(actions_from), actions_from := 0]
  edge_dt[is.na(actions_to), actions_to := 0]

  # Calculate total actions
  edge_dt[, total_actions := actions_from + actions_to]

  # Calculate contributions to edge weight
  edge_dt[, contribution_from := fifelse(total_actions > 0, (actions_from / total_actions), 0)]
  edge_dt[, contribution_to := fifelse(total_actions > 0, (actions_to / total_actions), 0)]

  # Calculate the index of edge weight equilibrium (edge_eq) for each edge
  # based on the weight for the fast or full network (depending on the options)

  if (fast_net == TRUE){
    weight <- "weight_fast"
  } else {
    weight <- "weight"
  }

  edge_dt[,
          edge_eq := 1 - abs(
            (contribution_from / edge_dt[[weight]]) -
              (contribution_to / edge_dt[[weight]]))
  ]

  # Summarize edge_dt to get median, average, and standard deviation values for each unique pair
  summary_dt <- edge_dt[, .(median_edge_eq = median(edge_eq),
                            mean_edge_eq = mean(edge_eq),
                            sd_edge_eq = sd(edge_eq)),
                        by = .(from, to)]

  # Convert edge list from g to data.table for matching
  edges_graph_dt <- as.data.table(get.edgelist(g))
  setnames(edges_graph_dt, c("from", "to"))

  # Join summary_dt with edges_graph_dt to match the summary with the edges
  final_edge_attributes <-
    merge(edges_graph_dt,
          summary_dt,
          by = c("from", "to"),
          all.g = TRUE)

  # Assign the summary attributes to the edges of the igraph object
  E(g)$median_edge_eq <- final_edge_attributes$median_contribution
  E(g)$mean_edge_eq <- final_edge_attributes$mean_contribution
  E(g)$sd_edge_eq <- final_edge_attributes$sd_contribution

  return(g)
}
