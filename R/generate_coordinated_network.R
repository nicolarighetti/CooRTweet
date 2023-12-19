#' generate_coordinated_network
#'
#' @description This function takes the results of `detect_coordinated_groups`
#' and generates a network from the data. It performs the second step in
#' coordinated detection analysis by identifying users who repeatedly engage in
#' identical actions within a predefined time window. The function offers
#' multiple options to identify various types of networks, allowing for
#' filtering based on different edge weights and facilitating the extraction
#' of distinct subgraphs. See details.
#'
#' @details Two users may coincidentally share the same objects within the same
#' time window, but it is unlikely that they do so repeatedly (Giglietto et al.,
#' 2020). Such repetition is thus considered an indicator of potential
#' coordination. This function utilizes percentile edge weight to represent
#' recurrent shares by the same user pairs within a predefined time window. By
#' considering the edge weight distribution across the data and setting the
#' percentile value *p* between 0 and 1, we can identify edges that fall within
#' the top *p* percentile of the edge weight distribution. Selecting a
#' sufficiently high percentile (e.g., 0.99) allows us to pinpoint users who
#' share an unusually high number of objects (for instance, more than 99% of
#' user pairs in the network) in the same time window.
#'
#' The graph also incorporates the contribution of each node within the pair to
#' the pair's edge weight, specifically, the number of shared `content_id` that
#' contribute to the edge weight. Additionally, an `edge_symmetry_score` is
#' included, which equals 1 in cases of equal contributions from both users and
#' approaches 0 as the contributions become increasingly unequal. This score,
#' along with the value of contributions, can be utilized for further filtering
#' or examining cases where the score is particularly low. Working with an
#' undirected graph, it is plausible that the activity of highly active users
#' disproportionately affects the weight of edges connecting them to less active
#' users. For instance, if user A shares the same objects (`object_id`) 100
#' times, and user B shares the same object only once, but within a time frame
#' that matches the `time_window` defined in the parameter for all of user A's
#' 100 shares, then the edge weight between A and B will be 100, although this
#' weight is almost entirely influenced by the hyperactivity of user A. The
#' `edge_symmetry_score`, along with the counts of shares by each user `user_id`
#' and `user_id_y` (`n_content_id` and `n_content_id_y`), allows for monitoring
#' and controlling this phenomenon.
#'
#' @param x a data.table (result from `detect_coordinated_groups`) with the
#' Columns: `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`,
#' `timedelta`
#' @param fast_net If the data.table x has been updated with the
#' restrict_time_window function and this parameter is set to TRUE, two columns
#' weight_full and weight_fast are created, the first containing the edge weights
#' of the full graph, the second those of the subgraph that includes the shares
#' made in the narrower time window.
#' @param edge_weight The edges with weight that exceeds a threshold are marked
#' with 0 (not exceeding) or 1 (exceeding). The threshold is expressed in
#' percentiles of the edge weight distribution in the full network and in the
#' faster network, and any numeric value between 0 and 1 can be assigned. The
#' default value is "0.5" which represents the median value of the edges in the
#' network.
#' @param subgraph Generate and return the following subgraph
#' - If 1 reduces the graph to the subgraph whose edges have a value that exceeds
#' the threshold given in the edge_weight parameter (weighted subgraph).
#' - If 2 reduces the subgraph whose nodes exhibit coordinated behavior in the
#' narrowest time window, as established with the restrict_time_window function,
#' to the subgraph whose edges have a value that exceeds the threshold given in
#' the edge_weight parameter (fast weighted subgraph).
#' - If 3 reduces the graph to the subgraph whose nodes exhibit coordinated
#' behavior in the narrowest time window established with the restrict_time_window
#' function (fast subgraph). The default value is 0, meaning that no subgraph is
#' created.
#' @param objects Keep track of the IDs of shared objects for further analysis with
#' `group_stats` (default FALSE). There could be a performance impact when this option
#' is set to TRUE, although the actual impact may vary. For smaller datasets, the
#' difference might be negligible. However, for very large datasets, or in scenarios
#' where optimal performance is crucial, you might experience a more significant slowdown.
#'
#' @return A weighted, undirected network (igraph object) where the vertices (nodes) are users and
#' edges (links) are the membership in coordinated groups (`object_id`).
#'
#' @references
#' Giglietto, F., Righetti, N., Rossi, L., & Marino, G. (2020). It takes a village to manipulate the media: coordinated link sharing behavior during 2018 and 2019 Italian elections. *Information, Communication & Society*, 23(6), 867-891.
#'
#' @import data.table
#' @import Matrix
#' @import igraph
#' @export
#'


generate_coordinated_network <- function(x, fast_net = FALSE, edge_weight = 0.5, subgraph = 0, objects = FALSE) {
    object_id <- nodes <- patterns <- NULL

    # Validate the input
    if(!(is.numeric(edge_weight)) || edge_weight < 0 | edge_weight > 1){
        stop("edge_weight must be a numeric value between 0 and 1")
    }

    if(fast_net == TRUE){
        if(any(grepl("time_window_", names(x))) == FALSE){
            stop("fast_net = TRUE but input data is not available. Please check and update the dataset with 'flag_speed_share' function if necessary.")
        }
    }

    if (subgraph == 2 || subgraph == 3) {
        if (!any(grepl("time_window_", names(x)))) {
            stop("Input data for the requested subgraph is not available. Please check and update the dataset with 'flag_speed_share' function if necessary.")
        }
    }

    # standardize the order of the vertices
    x[, `:=`(id_user = pmin(id_user, id_user_y), id_user_y = pmax(id_user, id_user_y))]

    # Aggregate edges and compute weight and edge_symmetry_score
    x_aggregated <- x[, .(
        # weight: Counts the number of connections (edges) between each pair of users
        weight = .N,

        # avg_time_delta: Calculates the average time difference across all connections for each user pair
        avg_time_delta = mean(time_delta),

        # n_content_id: Counts the number of unique content_ids for each user pair
        n_content_id = uniqueN(content_id),

        # n_content_id_y: Counts the number of unique content_id_ys for each user pair
        n_content_id_y = uniqueN(content_id_y),

        # Include object_ids if 'objects' is TRUE
        object_ids = if (objects == TRUE) paste(unique(object_id), collapse = ",") else NULL,

        # edge_symmetry_score: Computes a score representing the symmetry of content sharing between users.
        # This score is 1 when the sharing is perfectly symmetrical (equal contributions from both users),
        # and approaches 0 as the contribution becomes more unequal.
                          edge_symmetry_score = {
                              n_cid = uniqueN(content_id)
                              n_cidy = uniqueN(content_id_y)
                              min(n_cid, n_cidy) / max(n_cid, n_cidy)
                          }),
                      by = .(id_user, id_user_y)] # Grouping by pairs of users for the aggregation

    # Create the graph with edge weights
    coord_graph <- graph_from_data_frame(x_aggregated, directed = FALSE)

    # Optional attributes and subsets -------------------------
        # Add the restrict_time_window attribute to the graph
        if (fast_net == TRUE){

            fast_net_col_name <- names(x)[grep("time_window_", names(x))]
            fast_x <- x[get(fast_net_col_name) == 1]

            # Standardize the order of the vertices
            fast_x[, `:=`(id_user = pmin(id_user, id_user_y), id_user_y = pmax(id_user, id_user_y))]

            # Aggregate edges and compute weight and edge_symmetry_score
            fast_x_aggregated <- fast_x[, .(
                # weight: Counts the number of connections (edges) between each pair of users
                weight = .N,

                # avg_time_delta: Calculates the average time difference across all connections for each user pair
                avg_time_delta = mean(time_delta),

                # n_content_id: Counts the number of unique content_ids for each user pair
                n_content_id = uniqueN(content_id),

                # n_content_id_y: Counts the number of unique content_id_ys for each user pair
                n_content_id_y = uniqueN(content_id_y),

                # Include object_ids if 'objects' is TRUE
                object_ids = if (objects == TRUE) paste(unique(object_id), collapse = ",") else NULL,

                # edge_symmetry_score: Computes a score representing the symmetry of content sharing between users.
                # This score is 1 when the sharing is perfectly symmetrical (equal contributions from both users),
                # and approaches 0 as the contribution becomes more unequal.
                edge_symmetry_score = {
                    n_cid = uniqueN(content_id)
                    n_cidy = uniqueN(content_id_y)
                    min(n_cid, n_cidy) / max(n_cid, n_cidy)
                }),
                by = .(id_user, id_user_y)] # Grouping by pairs of users for the aggregation

            # Create the graph with edge weights
            fast_coord_graph <- graph_from_data_frame(fast_x_aggregated, directed = FALSE)

            # Merge with the full graph
            coord_graph <- igraph::graph.union(coord_graph, fast_coord_graph)

            # Rename
            coord_graph <-
                igraph::set_edge_attr(coord_graph, "weight_full", value = igraph::E(coord_graph)$weight_1)
            coord_graph <-
                igraph::set_edge_attr(coord_graph, "weight_fast", value = igraph::E(coord_graph)$weight_2)

            coord_graph <-
                igraph::set_edge_attr(coord_graph, "avg_time_delta_full", value = igraph::E(coord_graph)$avg_time_delta_1)
            coord_graph <-
                igraph::set_edge_attr(coord_graph, "avg_time_delta_fast", value = igraph::E(coord_graph)$avg_time_delta_2)

            coord_graph <-
                igraph::set_edge_attr(coord_graph, "n_content_id_full", value = igraph::E(coord_graph)$n_content_id_1)
            coord_graph <-
                igraph::set_edge_attr(coord_graph, "n_content_id_y_full", value = igraph::E(coord_graph)$n_content_id_y_1)

            coord_graph <-
                igraph::set_edge_attr(coord_graph, "n_content_id_fast", value = igraph::E(coord_graph)$n_content_id_2)
            coord_graph <-
                igraph::set_edge_attr(coord_graph, "n_content_id_y_fast", value = igraph::E(coord_graph)$n_content_id_y_2)

            coord_graph <-
                igraph::set_edge_attr(coord_graph, "edge_symmetry_score_full", value = igraph::E(coord_graph)$edge_symmetry_score_1)
            coord_graph <-
                igraph::set_edge_attr(coord_graph, "edge_symmetry_score_fast", value = igraph::E(coord_graph)$edge_symmetry_score_2)

            if (objects == TRUE){

                coord_graph <-
                    igraph::set_edge_attr(coord_graph, "object_ids_full", value = igraph::E(coord_graph)$object_ids_1)
                coord_graph <-
                    igraph::set_edge_attr(coord_graph, "object_ids_fast", value = igraph::E(coord_graph)$object_ids_2)
            }

            # Remove old attributes
            coord_graph <- igraph::delete_edge_attr(coord_graph, "weight_1")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "weight_2")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "avg_time_delta_1")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "avg_time_delta_2")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "n_content_id_1")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "n_content_id_y_1")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "n_content_id_2")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "n_content_id_y_2")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "edge_symmetry_score_1")
            coord_graph <- igraph::delete_edge_attr(coord_graph, "edge_symmetry_score_2")

            if (objects == TRUE){
                coord_graph <- igraph::delete_edge_attr(coord_graph, "object_ids_1")
                coord_graph <- igraph::delete_edge_attr(coord_graph, "object_ids_2")
            }

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
            threshold_full <- quantile(igraph::E(coord_graph)$weight, edge_weight, na.rm = TRUE)

            coord_graph <-
                igraph::set_edge_attr(coord_graph,
                                      "weight_threshold",
                                      value = ifelse(igraph::E(coord_graph)$weight > threshold_full, 1, 0))
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

            edges_to_keep <- igraph::E(coord_graph)[which(!is.na(igraph::E(coord_graph)$weight_threshold_fast))]
            coord_graph <- igraph::subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
        }

        return(coord_graph)
}
