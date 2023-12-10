#' generate_network
#'
#' Take the results of coordinated content detection and generate a network from the data. This function generates a two-mode (bipartite) incidence matrix first, and then projects the matrix to a weighted adjacency matrix.
#'
#' @param x a data.table (result from `detect_coordinated_groups`) with the Columns: `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`, `timedelta`
#' @param restrict_time_window if the data.table x has been updated with the restrict_time_window function and this parameter is set to TRUE, two columns weight_1 and weight_2 are created, the first containing the edge weights of the complete graph, the second those of the subgraph that includes the shares made in the narrower time window. It is implemented only for intent = "users".
#' @param edge_weight allows edges whose weight exceeds a certain threshold to be marked with a dichotomous 0/1 attribute. It is expressed in percentiles of the edge weight distribution in the network, and any numeric value between 0 and 1 can be assigned. The default value is "0.5" which represents the median value of the edges in the network. It is implemented only for intent = "users".
#' @param subgraph implemented only for intent = "users". if 1 reduces the graph to the subgraph whose edges have a value that exceeds the threshold given in the edge_weight parameter (weighted subgraph).
#'                 If 2 reduces the subgraph whose nodes exhibit coordinated behavior in the narrowest time window, as established with the restrict_time_window function, to the subgraph whose edges have a value that exceeds the threshold given in the edge_weight parameter (fast weighted subgraph).
#'                 If 3 reduces the graph to the subgraph whose nodes exhibit coordinated behavior in the narrowest time window established with the restrict_time_window function (fast subgraph).
#'                 The default value is NULL, meaning that no subgraph is created.
#'
#' @return A weighted, undirected network (igraph object) where the vertices (nodes) are users (or `content_ids`) and edges (links) are the membership in coordinated groups (`object_id`)
#'
#' @import data.table
#' @import Matrix
#' @import igraph
#' @export
#'


# This function is heaviliy inspired by User "majom" on StackOverflow:
# https://stackoverflow.com/questions/38991448/out-of-memory-error-when-projecting-a-bipartite-network-in-igraph

generate_network <- function(x, intent = c("users", "content", "objects"), restrict_time_window = FALSE, edge_weight = 0.5, subgraph = NULL) {
    object_id <- nodes <- patterns <- NULL

    # TODO: Add data validation
    if (intent == "users") {
        nodes <- "id_user"
    } else if (intent == "content") {
        nodes <- "content_id"
    } else if (intent == "objects") {
        nodes <- "id_user"
    }else {
        .NotYetImplemented()
    }

    # Validate the input
    if(!(is.numeric(edge_weight)) || edge_weight < 0 | edge_weight > 1){
        stop("edge_weight must be a numeric value between 0 and 1")
    }


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

    df <- unique(df[, subcols, with = FALSE])
    df <- df[, lapply(.SD, as.factor)]

    # Transform data to a sparse matrix
    incidence_matrix <- Matrix::sparseMatrix(
        i = as.numeric(df[, get(nodes)]),
        j = as.numeric(df[, object_id]),
        dims = c(
            length(unique(df[, get(nodes)])),
            length(unique(df[, object_id]))
        ),
        x = rep(1, length(as.numeric(df[, get(nodes)])))
    )

    row.names(incidence_matrix) <- levels(df[, get(nodes)])
    colnames(incidence_matrix) <- levels(df[, object_id])

    if (intent == "objects") {
        projected_adjacency_matrix <- Matrix::crossprod(incidence_matrix)
    } else {
        projected_adjacency_matrix <- Matrix::tcrossprod(incidence_matrix)
    }

    coord_graph <- igraph::graph_from_adjacency_matrix(
        projected_adjacency_matrix,
        mode = "upper",
        weighted = TRUE,
        diag = FALSE
    )

    # Optional attributes and subsets for networks of users -------------------------
    if (intent == "users"){

        # Add the restrict_time_window attribute to the graph
        if (restrict_time_window == TRUE){

            restrict_time_window_col <- names(x)[grep("time_window_", names(x))]

            # Create an edge list with the time_window_3600 attribute
            edge_list <- data.table(
                from = x$id_user,
                to = x$id_user_y,
                restrict_time_window_col = x[[restrict_time_window_col]]
            )

            filtered_edges <- edge_list[restrict_time_window_col == 1]
            filtered_edges[, c('min_edge', 'max_edge') := .(pmin(from, to), pmax(from, to))]
            g <- filtered_edges[, .(weight = .N), by = .(min_edge, max_edge)]
            g <- graph_from_data_frame(g, directed = FALSE)

            coord_graph <- igraph::graph.union(coord_graph, g)
            }

        # Add the weight_threshold attribute to the graph ---------------
        # The threshold applies to both the full network, and the subnetwork defined by the restricted time window

        # Set weight_threshold based on edge_weight
        threshold <- quantile(E(coord_graph)$weight_1, edge_weight)

        # Full network
        coord_graph <- set_edge_attr(coord_graph, "weight_threshold", value = ifelse(E(coord_graph)$weight_1 > threshold, 1, 0))

        # Sub-network defined by the restricted time window
        coord_graph <- set_edge_attr(coord_graph, "weight_threshold_fast", value = ifelse(E(coord_graph)$weight_2 > threshold, 1, 0))

        # Create subgraphs ---------------------
        # Full network above the edge weight threshold
        if(subgraph == 1){
            edges_to_keep <- E(coord_graph)[which(E(coord_graph)$weight_threshold == 1)]
            coord_graph <- subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
        }

        # Fast network above the edge weight threshold
        if(subgraph == 2){
            edges_to_keep <- E(coord_graph)[which(E(coord_graph)$weight_threshold_fast == 1)]
            coord_graph <- subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
        }

        # Fast network ---------------------
        if (subgraph == 3){
            edges_to_keep <- !is.na(E(coord_graph)$weight_2)
            coord_graph <- subgraph.edges(coord_graph, which(edges_to_keep), delete.vertices = TRUE)
        }
    }

    # Edge weight contribution ------------------
    # When one user share the same object_id in the defined time window many times (such as a spammer),
    # along with other less active users, the edge weight between them is inflated by the hyper-activity
    # of that user. To account for this possibility, we define a measure of relative contribution
    # to the edge weight, which can be used to analyze or filter the resulting graph.

    # Convert edge list of coord_graph to data.table
    edge_dt <- as.data.table(as_data_frame(coord_graph))

    # Count actions initiated by each user for each object
    action_counts <- df[, .N, by = .(id_user, object_id)]
    setnames(action_counts, "N", "actions")

    # Convert edge list of coord_graph to data.table
    edge_dt <- as.data.table(as_data_frame(coord_graph))
    setnames(edge_dt, c("V1", "V2", "weight"))

    # Merge edge data table with action counts
    edge_dt <- merge(edge_dt, action_counts, by.x = "V1", by.y = "id_user", all.x = TRUE)
    edge_dt <- merge(edge_dt, action_counts, by.x = c("V2", "object_id"), by.y = c("id_user", "object_id"), all.x = TRUE, suffixes = c("_V1", "_V2"))

    # Replace NA with 0 in actions columns
    edge_dt[is.na(actions_V1), actions_V1 := 0]
    edge_dt[is.na(actions_V2), actions_V2 := 0]

    # Calculate total actions
    edge_dt[, total_actions := actions_V1 + actions_V2]

    # Calculate contributions to edge weight
    edge_dt[, contribution_V1 := fifelse(total_actions > 0, (actions_V1 / total_actions) * weight, 0)]
    edge_dt[, contribution_V2 := fifelse(total_actions > 0, (actions_V2 / total_actions) * weight, 0)]

    # Calculate the index of contribution equilibrium for each edge
    edge_dt[, edge_weight_contribution_index := 1 - abs((contribution_V1 / weight) - (contribution_V2 / weight))]

    # Summarize edge_dt to get median, average, and standard deviation values for each unique pair
    summary_dt <- edge_dt[, .(# median_contribution_V1 = median(contribution_V1),
                              # mean_contribution_V1 = mean(contribution_V1),
                              # median_contribution_V2 = median(contribution_V2),
                              # mean_contribution_V2 = mean(contribution_V2),
                              median_edge_weight_contrib_index = median(edge_weight_contribution_index),
                              mean_edge_weight_contrib_index = mean(edge_weight_contribution_index),
                              sd_edge_weight_contrib_index = sd(edge_weight_contribution_index)),
                          by = .(V1, V2)]

    # Convert edge list from coord_graph to data.table for matching
    edges_graph_dt <- as.data.table(get.edgelist(coord_graph))
    setnames(edges_graph_dt, c("V1", "V2"))

    # Join summary_dt with edges_graph_dt to match the summary with the edges
    final_edge_attributes <- merge(edges_graph_dt, summary_dt, by = c("V1", "V2"), all.x = TRUE)

    # Assign the summary attributes to the edges of the igraph object
    # E(coord_graph)$median_contribution_V1 <- final_edge_attributes$median_contribution_V1
    # E(coord_graph)$mean_contribution_V1 <- final_edge_attributes$mean_contribution_V1
    # E(coord_graph)$median_contribution_V2 <- final_edge_attributes$median_contribution_V2
    # E(coord_graph)$mean_contribution_V2 <- final_edge_attributes$mean_contribution_V2
    E(coord_graph)$median_edge_weight_contrib_index <- final_edge_attributes$median_edge_weight_contrib_index
    E(coord_graph)$mean_edge_weight_contrib_index <- final_edge_attributes$mean_edge_weight_contrib_index
    E(coord_graph)$sd_edge_weight_contrib_index <- final_edge_attributes$sd_edge_weight_contrib_index


    return(coord_graph)
}
