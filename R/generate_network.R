#' generate_network
#'
#' Take the results of coordinated content detection and generate a network from the data. This function generates a two-mode (bipartite) incidence matrix first, and then projects the matrix to a weighted adjacency matrix.
#'
#' @param x a data.table (result from `detect_coordinated_groups`) with the Columns: `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`, `timedelta`
#' @param fast_net implemented only for intent = "users". If the data.table x has been updated with the restrict_time_window function and this parameter is set to TRUE, two columns weight_full and weight_fast are created, the first containing the edge weights of the full graph, the second those of the subgraph that includes the shares made in the narrower time window.
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

generate_network <- function(x, intent = c("users", "content", "objects"), fast_net = FALSE, edge_weight = 0.5, subgraph = 0, edge_contrib = FALSE) {
    object_id <- nodes <- patterns <- NULL

    # Validate the input
    if(!(is.numeric(edge_weight)) || edge_weight < 0 | edge_weight > 1){
        stop("edge_weight must be a numeric value between 0 and 1")
    }

    if(fast_net == TRUE){
        if(any(grepl("time_window_", names(x))) == FALSE){
            stop("fast_net = TRUE but input data is not available. Please check and update the dataset with 'restrict_time_window' function if necessary.")
        }
    }

    if(length(intent) != 1 || !intent %in% c("users", "content", "objects")){
        stop("The value of the 'intent' argument must be specified, and must be one of 'users', 'content', 'objects'")
    }


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

    # Reshape data
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
        if (fast_net == TRUE){

            fast_net_col <- names(x)[grep("time_window_", names(x))]

            # Create an edge list with the time_window_{...} attribute
            edge_list <- data.table::data.table(
                from = x$id_user,
                to = x$id_user_y,
                fast_net_col = x[[fast_net_col]]
            )

            filtered_edges <- edge_list[fast_net_col == 1]

            # ensure a consistent ordering of node pairs for each edge
            filtered_edges[, c('min_edge', 'max_edge') := .(pmin(from, to), pmax(from, to))]
            g <- filtered_edges[, .(weight = .N), by = .(min_edge, max_edge)]

            g <- igraph::graph_from_data_frame(g, directed = FALSE)

            # add attributed time_window_[...] with value 1 for subsequent filtering
            g <- igraph::set_edge_attr(g, fast_net_col, value = 1)

            coord_graph <- igraph::graph.union(coord_graph, g)

            # Rename 'weight_1'and 'weight_2'
            coord_graph <-
                igraph::set_edge_attr(coord_graph, "weight_full", value = igraph::E(coord_graph)$weight_1)
            coord_graph <-
                igraph::set_edge_attr(coord_graph, "weight_fast", value = igraph::E(coord_graph)$weight_2)

            # Remove old attributes
            coord_graph <- igraph::delete_edge_attr(coord_graph, "weight_1")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "weight_2")

            # Full network weight threshold
            threshold_full <- quantile(igraph::E(coord_graph)$weight_full, edge_weight)
            coord_graph <-
                igraph::set_edge_attr(
                    coord_graph,
                    "weight_threshold_full",
                    value = ifelse(igraph::E(coord_graph)$weight_full > threshold_full, 1, 0)
                )

            # Fast nodes subnetwork weight threshold
            threshold_fast <- quantile(igraph::E(coord_graph)$weight_fast, edge_weight, na.rm = TRUE)
            coord_graph <-
                igraph::set_edge_attr(
                    coord_graph,
                    "weight_threshold_fast",
                    value = ifelse(igraph::E(coord_graph)$weight_fast > threshold_fast, 1, 0)
                )

        } else {

            # Add the weight_threshold attribute to the full network only
            threshold_full <- quantile(igraph::E(coord_graph)$weight, edge_weight)
            coord_graph <-
                igraph::set_edge_attr(coord_graph,
                                      "weight_threshold",
                                      value = ifelse(igraph::E(coord_graph)$weight > threshold_full, 1, 0))
        }
    }

        # Create subgraphs ---------------------

        # Full network > edge weight threshold
        if(subgraph == 1){
            edges_to_keep <- igraph::E(coord_graph)[which(igraph::E(coord_graph)$weight_threshold == 1)]
            coord_graph <- igraph::subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
        }

        # Faster network > edge weight threshold
        if(subgraph == 2){
            edges_to_keep <- igraph::E(coord_graph)[which(igraph::E(coord_graph)$weight_threshold_fast == 1)]
            coord_graph <- igraph::subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
        }

        # Faster network -----------------------
        if (subgraph == 3){
            edge_attr_values <- igraph::get.edge.attribute(coord_graph, fast_net_col)
            edges_to_keep <- igraph::E(coord_graph)[which(edge_attr_values == 1)]
            coord_graph <- igraph::subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
        }

    # Calculate the edge contribution and the contribution index
    if(edge_contrib == TRUE){
        coord_graph <- edge_weight_contribution(coord_graph, incidence_matrix, fast_net, fast_net_col)
    }

        return(coord_graph)
}
