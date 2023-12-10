#' generate_network
#'
#' Take the results of coordinated content detection and generate a network from the data. This function generates a two-mode (bipartite) incidence matrix first, and then projects the matrix to a weighted adjacency matrix.
#'
#' @param x a data.table (result from `detect_coordinated_groups`) with the Columns: `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`, `timedelta`
#' @param faster_nodes implemented only for intent = "users". If the data.table x has been updated with the restrict_time_window function and this parameter is set to TRUE, two columns weight_full and weight_fast are created, the first containing the edge weights of the full graph, the second those of the subgraph that includes the shares made in the narrower time window.
#' @param edge_weight implemented only for intent = "users". The edges with weight that exceeds a threshold are marked with 0 (not exceeding) or 1 (exceeding). The threshold is expressed in percentiles of the edge weight distribution in the full network and in the faster network, and any numeric value between 0 and 1 can be assigned. The default value is "0.5" which represents the median value of the edges in the network.
#' @param subgraph implemented only for intent = "users". if 1 reduces the graph to the subgraph whose edges have a value that exceeds the threshold given in the edge_weight parameter (weighted subgraph).
#'                 If 2 reduces the subgraph whose nodes exhibit coordinated behavior in the narrowest time window, as established with the restrict_time_window function, to the subgraph whose edges have a value that exceeds the threshold given in the edge_weight parameter (fast weighted subgraph).
#'                 If 3 reduces the graph to the subgraph whose nodes exhibit coordinated behavior in the narrowest time window established with the restrict_time_window function (fast subgraph).
#'                 The default value is 0, meaning that no subgraph is created.
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

generate_network <- function(x, intent = c("users", "content", "objects"), faster_nodes = FALSE, edge_weight = 0.5, subgraph = 0) {
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

    if(faster_nodes == TRUE){
        if(any(grepl("time_window_", names(x))) == FALSE){
            stop("faster_nodes = TRUE but input data is not available. Please check and update the dataset with 'restrict_time_window' function if necessary.")
        }
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
        if (faster_nodes == TRUE){

            faster_nodes_col <- names(x)[grep("time_window_", names(x))]

            # Create an edge list with the time_window_{...} attribute
            edge_list <- data.table(
                from = x$id_user,
                to = x$id_user_y,
                faster_nodes_col = x[[faster_nodes_col]]
            )

            filtered_edges <- edge_list[faster_nodes_col == 1]
            filtered_edges[, c('min_edge', 'max_edge') := .(pmin(from, to), pmax(from, to))]
            g <- filtered_edges[, .(weight = .N), by = .(min_edge, max_edge)]
            g <- graph_from_data_frame(g, directed = FALSE)

            coord_graph <- igraph::graph.union(coord_graph, g)

            # Rename 'weight_1'and 'weight_2'
            coord_graph <- set_edge_attr(coord_graph, "weight_full", value = E(coord_graph)$weight_1)
            coord_graph <- set_edge_attr(coord_graph, "weight_fast", value = E(coord_graph)$weight_2)

            # Remove old attributes
            coord_graph <- delete_edge_attr(coord_graph, "weight_1")
            coord_graph <- delete_edge_attr(coord_graph, "weight_2")

        }
    }

        # Add the weight_threshold attribute ---------------

        # Full network
        threshold_full <- quantile(E(coord_graph)$weight_full, edge_weight)

        coord_graph <- set_edge_attr(coord_graph, "weight_threshold_full", value = ifelse(E(coord_graph)$weight_full > threshold_full, 1, 0))

        # Sub-network of faster nodes
        if (faster_nodes == TRUE){
        threshold_fast <- quantile(E(coord_graph)$weight_fast, edge_weight)
        coord_graph <- set_edge_attr(coord_graph, "weight_threshold_fast", value = ifelse(E(coord_graph)$weight_fast > threshold_fast, 1, 0))
        }

        # Create subgraphs ---------------------

        # Full network > edge weight threshold
        if(subgraph == 1){
            edges_to_keep <- E(coord_graph)[which(E(coord_graph)$weight_threshold == 1)]
            coord_graph <- subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
        }

        # Faster network > edge weight threshold
        if(subgraph == 2){
            edges_to_keep <- E(coord_graph)[which(E(coord_graph)$weight_threshold_fast == 1)]
            coord_graph <- subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
        }

        # Faster network -----------------------
        if (subgraph == 3){
            edges_to_keep <- !is.na(E(coord_graph)$weight_2)
            coord_graph <- subgraph.edges(coord_graph, which(edges_to_keep), delete.vertices = TRUE)
        }

    return(coord_graph)


        # Under construction work -----------------
        # Note: it currently applies to the full network, but it should also apply to the faster
        # subnetwork, considering only the related data subset

        #' Calculate the contribution of each vertex to the edge weight
        #'
        #' This function is a private utility function that calculates summary measures of edge
        #' weight contribution.
        #'
        #' @description When one user share the same object_id in the defined time window many times
        #' (such as a spammer), along with other less active users, the edge weight between them is
        #' inflated by the hyper-activity of that user. To account for this possibility, we define a
        #' measure of relative contribution to the edge weight, which can be used to analyze or filter
        #' the resulting graph.
        #'
        #' @param g The coord_graph resulting from the previous steps.
        #'
        #' @return The coord_graph with additional attributes.
        #'
        #'

        edge_weight_contribution <- function(g = coord_graph){

            # Convert edge list of g to data.table
            edge_dt <- as.data.table(as_data_frame(g))

            # Count actions initiated by each user for each object
            action_counts <- df[, .N, by = .(id_user, object_id)]
            setnames(action_counts, "N", "actions")

            # Convert edge list of g to data.table
            edge_dt <- as.data.table(as_data_frame(g))

            # Merge edge data table with action counts
            edge_dt <- merge(edge_dt, action_counts, by.g = "from", by.y = "id_user", all.g = TRUE)
            edge_dt <- merge(edge_dt, action_counts, by.g = c("to", "object_id"), by.y = c("id_user", "object_id"), all.g = TRUE, suffixes = c("_from", "_to"))

            # Replace NA with 0 in actions columns
            edge_dt[is.na(actions_from), actions_from := 0]
            edge_dt[is.na(actions_to), actions_to := 0]

            # Calculate total actions
            edge_dt[, total_actions := actions_from + actions_to]

            # Calculate contributions to edge weight
            edge_dt[, contribution_from := fifelse(total_actions > 0, (actions_from / total_actions) * weight_full, 0)]
            edge_dt[, contribution_to := fifelse(total_actions > 0, (actions_to / total_actions) * weight_full, 0)]

            # Calculate the index of contribution equilibrium for each edge
            edge_dt[, contribution_index := 1 - abs((contribution_from / weight_full) - (contribution_to / weight_full))]

            # Summarize edge_dt to get median, average, and standard deviation values for each unique pair
            summary_dt <- edge_dt[, .(median_contribution = median(contribution_index),
                                      mean_contribution = mean(contribution_index),
                                      sd_contribution = sd(contribution_index)),
                                  by = .(from, to)]

            # Convert edge list from g to data.table for matching
            edges_graph_dt <- as.data.table(get.edgelist(g))
            setnames(edges_graph_dt, c("from", "to"))

            # Join summary_dt with edges_graph_dt to match the summary with the edges
            final_edge_attributes <- merge(edges_graph_dt, summary_dt, by = c("from", "to"), all.g = TRUE)

            # Assign the summary attributes to the edges of the igraph object
            E(g)$median_contribution <- final_edge_attributes$median_contribution
            E(g)$mean_contribution <- final_edge_attributes$mean_contribution
            E(g)$sd_contribution <- final_edge_attributes$sd_contribution

            return(g)
        }



}
