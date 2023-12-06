#' generate_network
#'
#' Take the results of coordinated content detection and generate a network from the data. This function generates a two-mode (bipartite) incidence matrix first, and then projects the matrix to a weighted adjacency matrix.
#'
#' @param x a data.table (result from `detect_coordinated_groups`) with the Columns: `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`, `timedelta`
#' @param restrict_time_window if the data.table x has been updated with the restrict_time_window function and this parameter is set to TRUE, a dichotomous attribute is added to the vertices of the graph with value 1, if the vertex does not participate in the coordinated activity in the narrower time window, and 0, otherwise (default FALSE). It is implemented only for intent = "users".
#' @param edge_weight allows edges whose weight exceeds a certain threshold to be marked with a dichotomous 0/1 attribute. It is expressed in percentiles of the edge weight distribution in the network, and any numeric value between 0 and 1 can be assigned. The default value is "0.5" which represents the median value of the edges in the network. It is implemented only for intent = "users".
#' @param weighted_subgraph if TRUE reduces the graph to the subgraph whose edges have a value that exceeds the threshold given in the edge_weight parameter (default FALSE). It is implemented only for intent = "users".
#' @param weighted_subgraph_fast if TRUE reduces the subgraph whose nodes exhibit coordinated behavior in the narrowest time window, as established with the restrict_time_window function, to the subgraph whose edges have a value that exceeds the threshold given in the edge_weight parameter (default FALSE). It is implemented only for intent = "users".
#' @param fast_subgraph if TRUE reduces the graph to the subgraph whose nodes exhibit coordinated behavior in the narrowest time window established with the restrict_time_window function (default FALSE). It is implemented only for intent = "users".
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

generate_network <- function(x, intent = c("users", "content", "objects"), restrict_time_window = FALSE, edge_weight = 0.5, weighted_subgraph = FALSE, weighted_subgraph_fast = FALSE, fast_subgraph = FALSE) {
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

    df <- data.table::melt(x,
        id.vars = c("object_id", "time_delta"),
        measure.vars = patterns("^content_id", "^id_user"),
        value.name = c("content_id", "id_user")
    )

    subcols <- c(nodes, "object_id")
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
            restrict_time_window_column <- names(x)[grep("time_window_", names(x))]
            restrict_time_window_data <- x[[restrict_time_window_column]]
            names(restrict_time_window_data) <- x[[nodes]]

            coord_graph <- set_vertex_attr(coord_graph, name = restrict_time_window_column, value = restrict_time_window_data[V(coord_graph)$name])
        }


        # Add the weight_threshold attribute to the graph ---------------
        # The threshold applies to both the full network, and the subnetwork defined by the restricted time window

        # Validate the input
        if(!(is.numeric(edge_weight)) || edge_weight < 0 | edge_weight > 1){
            stop("edge_weight must be a numeric value between 0 and 1")
        }

        if(weighted_subgraph == TRUE & weighted_subgraph_fast == TRUE){
            stop("weighted_subgraph or weighted_subgraph_fast are both TRUE. Please choose one and set the other to FALSE.")
        }

        if(weighted_subgraph_fast == TRUE & fast_subgraph == TRUE){
            stop("weighted_subgraph_fast and fast_subgraph are both TRUE. weighted_subgraph_fast is a subset of fast_subgraph. Please select only one and set the other to FALSE.")
        }

        # Set weight_threshold based on edge_weight
        threshold <- quantile(E(coord_graph)$weight, edge_weight)

        # Full network ---
        coord_graph <- set_edge_attr(coord_graph, "weight_threshold", value = ifelse(E(coord_graph)$weight > threshold, 1, 0))

        # Sub-network defined by the restricted time window ---

        vertex_indices <- which(get.vertex.attribute(coord_graph, restrict_time_window_column) == 1)
        fastest_vertices <- V(coord_graph)[vertex_indices]
        fast_coord_graph <- induced_subgraph(coord_graph, vids = fastest_vertices)

        edge_ids_fast <- apply(get.edgelist(fast_coord_graph), 1, paste, collapse = "-")
        fast_edge_weights <- ifelse(E(fast_coord_graph)$weight > threshold, 1, 0)
        names(fast_edge_weights) <- edge_ids_fast

        E(coord_graph)$weight_threshold_fast <- NA

        edge_ids_coord <- apply(get.edgelist(coord_graph), 1, paste, collapse = "-")

        matching_indices <- match(edge_ids_coord, names(fast_edge_weights))
        valid_indices <- !is.na(matching_indices)
        E(coord_graph)$weight_threshold_fast[valid_indices] <- fast_edge_weights[matching_indices[valid_indices]]

        # Keep only the heavier subgraph ---------------------
        # Full network
        if(weighted_subgraph == TRUE){
            weighted_subgraph_edges <- E(coord_graph)[which(E(coord_graph)$weight_threshold == 1)]
            coord_graph <- induced_subgraph(coord_graph, ends(coord_graph, weighted_subgraph_edges))
        }

        # Fast network
        if(weighted_subgraph_fast == TRUE){
            weighted_subgraph_fast_edges <- E(coord_graph)[which(E(coord_graph)$weight_threshold_fast == 1)]
            coord_graph <- induced_subgraph(coord_graph, ends(coord_graph, weighted_subgraph_fast_edges))
        }

        # Keep only the subgraph of nodes in the shorter time window ---------------------
        if (fast_subgraph == TRUE){
            vertex_indices <- which(get.vertex.attribute(coord_graph, restrict_time_window_column) == 1)
            fastest_vertices <- V(coord_graph)[vertex_indices]
            coord_graph <- induced_subgraph(coord_graph, vids = fastest_vertices)
        }
    }

    return(coord_graph)
}
