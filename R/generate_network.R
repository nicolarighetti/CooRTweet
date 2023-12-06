#' generate_network
#'
#' Take the results of coordinated content detection and generate a network from the data. This function generates a two-mode (bipartite) incidence matrix first, and then projects the matrix to a weighted adjacency matrix.
#'
#' @param x a data.table (result from `detect_coordinated_groups`) with the Columns: `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`, `timedelta`
#' @param restrict_time_window if the data.table x has been updated with the restrict_time_window function and this parameter is set to TRUE, a dichotomous attribute is added to the vertices of the graph with value 1, if the vertex does not participate in the coordinated activity in the narrower time window, and 0, otherwise (default FALSE). It is implemented only for intent = "users".
#' @param edge_weight allows edges whose weight exceeds a certain threshold to be marked with a dichotomous 0/1 attribute. Any numeric value can be assigned. The default value is "median_weight" which represents the median value of the edges in the network. It is implemented only for intent = "users".
#' @param weighted_subgraph if TRUE reduces the graph to the subgraph whose edges have a value that exceeds the threshold given in the edge_weight parameter (default FALSE). It is implemented only for intent = "users".
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

generate_network <- function(x, intent = c("users", "content", "objects"), restrict_time_window = FALSE, edge_weight = "median_weight", weighted_subgraph = FALSE, fast_subgraph = FALSE) {
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
        # Validate the input
        if(!(edge_weight %in% c("median_weight") || is.numeric(edge_weight))){
            stop("edge_weight must be either 'median_weight' or a numeric value.")
        }

        if(is.numeric(edge_weight) & edge_weight > max(E(coord_graph)$weight)){
            stop(paste("edge_weight is out of range: please choose a value between", min(E(coord_graph)$weight), "and", max(E(coord_graph)$weight)))
        }

        # Set weight_threshold based on edge_weight
        if(edge_weight == "median_weight"){
            median_weight <- median(E(coord_graph)$weight)
            coord_graph <- set_edge_attr(coord_graph, "weight_threshold", value = ifelse(E(coord_graph)$weight > median_weight, 1, 0))
        } else {
            coord_graph <- set_edge_attr(coord_graph, "weight_threshold", value = ifelse(E(coord_graph)$weight > edge_weight, 1, 0))
        }

        # Keep only the highly coordinated subgraph ---------------------
        if(weighted_subgraph == TRUE){
            # Filter edges with weight above the threshold
            weighted_subgraph_edges <- E(coord_graph)[weight_threshold == 1]

            # Create and return the subgraph
            coord_graph <- induced_subgraph(coord_graph, ends(coord_graph, weighted_subgraph_edges))
        }

        # Keep only the subgraph of nodes in the shorter time window ---------------------
        if (fast_subgraph == TRUE){
            # Get the indices of vertices where the attribute equals 1
            vertex_indices <- which(get.vertex.attribute(coord_graph, restrict_time_window_column) == 1)

            # Use these indices to select the vertices
            fastest_vertices <- V(coord_graph)[vertex_indices]

            # Create the subgraph
            coord_graph <- induced_subgraph(coord_graph, vids = fastest_vertices)
        }
    }

    return(coord_graph)
}
