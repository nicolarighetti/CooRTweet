#' generate_coordinated_network
#'
#' @description This function takes the results of \link{detect_groups}
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
#' approaches 0 as the contributions become increasingly unequal.
#' The edge_symmetry_score is determined as the proportion of the unique
#' content_ids (unique content) shared by each vertex to the total content_ids
#' shared by both users.
#' This score, along with the value of contributions, can be utilized for further
#' filtering or examining cases where the score is particularly low. Working with
#' an undirected graph, it is plausible that the activity of highly active users
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
#' @param x a data.table (result from \link{detect_groups}) with the
#' Columns: `object_id`, `account_id`, `account_id_y`, `content_id`, `content_id_y`,
#' `timedelta`
#' @param fast_net If the data.table x has been updated with the
#' \link{flag_speed_share} function and this parameter is set to TRUE, two columns
#' weight_full and weight_fast are created, the first containing the edge weights
#' of the full graph, the second those of the subgraph that includes the shares
#' made in the narrower time window.
#' @param edge_weight This parameter defines the edge weight threshold, expressed
#' as a percentile of the edge weight distribution within the network. This applies
#' also to the faster network, if 'fast_net' is set to TRUE (and the data is updated
#' using the \link{flag_speed_share} function). Edges with a weight exceeding this
#' threshold are marked as 0 (not exceeding) or 1 (exceeding). The parameter accepts
#' any numeric value between 0 and 1. The default value is set to "0.5", representing
#' the median value of edge weights in the network.
#' @param subgraph Generate and return the following subgraph (default value is 0,
#' meaning that no subgraph is created):
#' - If 1 reduces the graph to the subgraph whose edges have a value that exceeds
#' the threshold given in the edge_weight parameter (weighted subgraph).
#' - If 2 reduces the subgraph whose nodes exhibit coordinated behavior in the
#' narrowest time window (as established with the \link{flag_speed_share} function),
#' to the subgraph whose edges have a value that exceeds the threshold given in
#' the edge_weight parameter (fast weighted subgraph).
#' - If 3 reduces the graph to the subgraph whose nodes exhibit coordinated
#' behavior in the narrowest time window established with the \link{flag_speed_share}
#' function (fast subgraph), and the vertices adjacent to their edges. In other
#' words, this option identifies the fastest network, along with a contextual set
#' of accounts that shared the same objects but in the wider time window. It
#' also add a  vertex attribute color_v to facilitate further analyses or the
#' generation of the graph plot. This attribute is 1 when for the coordinated
#' accounts and 0 for the neighbor accounts.
#' @param objects Keep track of the IDs of shared objects for further analysis with
#' `group_stats` (default FALSE). There could be a performance impact when this
#' option is set to TRUE, although the actual impact may vary. For smaller datasets,
#' the difference might be negligible. However, for very large datasets, or in
#' scenarios where optimal performance is crucial, you might experience a more
#' significant slowdown.
#'
#' @return A weighted, undirected network (igraph object) where the vertices (nodes)
#' are users and edges (links) are the membership in coordinated groups (`object_id`).
#'
#' @references
#' Giglietto, F., Righetti, N., Rossi, L., & Marino, G. (2020). It takes a village to manipulate the media: coordinated link sharing behavior during 2018 and 2019 Italian elections. *Information, Communication & Society*, 23(6), 867-891.
#'
#' @import data.table
#' @import igraph
#' @importFrom stats quantile
#' @export
#'


generate_coordinated_network <- function(x,
                                         fast_net = FALSE,
                                         edge_weight = 0.5,
                                         subgraph = 0,
                                         objects = FALSE) {
    object_id <- account_id <- account_id_y <- time_delta <-
        content_id <- content_id_y <- NULL

    # Validate the input
    if (!(is.numeric(edge_weight)) ||
        edge_weight < 0 | edge_weight > 1) {
        stop("edge_weight must be a numeric value between 0 and 1")
    }

    if (fast_net == TRUE) {
        if (any(grepl("time_window_", names(x))) == FALSE) {
            stop(
                "fast_net = TRUE but input data is not available. Please check
                and update the dataset with 'flag_speed_share' function if
                necessary."
            )
        }
    }

    if (subgraph == 2 || subgraph == 3) {
        if (!any(grepl("time_window_", names(x)))) {
            stop(
                "Input data for the requested subgraph is not available. Please
                check and update the dataset with 'flag_speed_share' function if
                necessary."
            )
        }
    }

    if ("id_user" %in% colnames(x)) {
        data.table::setnames(x, "id_user", "account_id")
        warning(
            "Your data contained the column `id_user`, this name is deprecated,
            renamed it to `account_id`"
        )
    }

    # --------------------------------------------------------------------------
    # Generate the weighted graph ----------
    # This first part of code generates the graph from the result of the
    # detect_groups function. It is an indirect graph, the weight of whose edges
    # represents the number of shares made by pairs of vertices in the time window
    # established in the detect_groups function. It also includes an indicator
    # `edge_symmetry_score` that measures how much the overall edge weight is
    # determined by one or the other of the connected vertices, or by both
    # equivalently. Optionally, it includes objects shared by accounts identified
    # as coordinated, in the course of their coordinated activity.

    # create a true duplicate of result and avoid overwriting
    result <- x
    x <- data.table::copy(result)

    # standardize the order of the vertices
    x[, `:=`(
        account_id = pmin(account_id, account_id_y),
        account_id_y = pmax(account_id, account_id_y)
    )]

    # Aggregate edges and compute weight and edge_symmetry_score
    x_aggregated <- x[, .(
        # weight: Counts the number of connections (edges) between each pair of users
        weight = .N,

        # avg_time_delta: Calculates the average time difference across all
        # connections for each user pair
        avg_time_delta = mean(time_delta),

        # n_content_id: Counts the number of unique content_ids for each user pair
        n_content_id = uniqueN(content_id),

        # n_content_id_y: Counts the number of unique content_id_ys for each user pair
        n_content_id_y = uniqueN(content_id_y),

        # edge_symmetry_score: Computes a score representing the symmetry of
        # content sharing between users. This score is 1 when the sharing is
        # perfectly symmetrical (equal contributions from both users), and
        # approaches 0 as the contribution becomes more unequal.
        edge_symmetry_score = {
            n_cid <- uniqueN(content_id)
            n_cidy <- uniqueN(content_id_y)
            min(n_cid, n_cidy) / max(n_cid, n_cidy)
        }
    ),
    by = .(account_id, account_id_y)] # Grouping by pairs of users for the aggregation

    # Include object_ids if 'objects' is TRUE
    if (objects == TRUE) {
        x_aggregated_obj <- x[, .(
            object_ids = paste(unique(object_id), collapse = ",")),
            by = .(account_id, account_id_y)]

        x_aggregated <-
            x_aggregated[x_aggregated_obj, on = .(account_id, account_id_y), nomatch = 0]
    }

    # Create the graph with edge weights (full graph)
    coord_graph <-
        graph_from_data_frame(x_aggregated, directed = FALSE)


    # --------------------------------------------------------------------------
    # Other attributes ----------
    # This section of code controls the outputs for the different possible options
    # of the function `generate_coordinated_network`. The first part applies when
    # fast_net is TRUE. Specifically, this code is used to mark the shares made
    # in the restricted time window. It is also used to mark edges that exceed
    # the edge weight percentile threshold set by the user. This last operation
    # also applies when fast_net = FALSE (default), and the related code is found
    # at the end of this section.

    # Add the attributes for the faster network to the graph
    if (fast_net == TRUE) {

        # Identify the column name dynamically (as the specific name carries the
        # number of seconds in the restricted time window).
        fast_net_col_name <- names(x)[grep("time_window_", names(x))]

        # Creates a subset of the results where edges are created among users who
        # shared in the narrowest time window. The following operations are
        # performed on this data, until the resulting graph with its attributes i
        # s again merged with the starting full graph from which we started.
        fast_x <- x[get(fast_net_col_name) == 1]

        # Standardize the order of the vertices (the same account could
        # theoretically be in the account_id_y or account_id column alternately)
        fast_x[, `:=`(
            account_id = pmin(account_id, account_id_y),
            account_id_y = pmax(account_id, account_id_y)
        )]

        # Aggregate edges and compute edge weight and edge_symmetry_score for the
        # subgraph corresponding to the faster shares
        fast_x_aggregated <- fast_x[, .(
            # weight: Counts the number of connections (edges) between each pair
            # of users
            weight = .N,

            # avg_time_delta: Calculates the average time difference across all
            # connections for each user pair
            avg_time_delta = mean(time_delta),

            # n_content_id: Counts the number of unique content_ids for each
            # user pair
            n_content_id = uniqueN(content_id),

            # n_content_id_y: Counts the number of unique content_id_ys for each
            # user pair
            n_content_id_y = uniqueN(content_id_y),

            # edge_symmetry_score: Computes a score representing the symmetry of
            # content sharing between users. This score is 1 when the sharing is
            # perfectly symmetrical (equal contributions from both users), and
            # approaches 0 as the contribution becomes more unequal.
            edge_symmetry_score = {
                n_cid <- uniqueN(content_id)
                n_cidy <- uniqueN(content_id_y)
                min(n_cid, n_cidy) / max(n_cid, n_cidy)
            }
        ),
        by = .(account_id, account_id_y)] # Grouping by pairs of users for aggregation

        # Include object_ids if 'objects' is TRUE
        if (objects == TRUE) {
            fast_x_obj <- fast_x[, .(
                object_ids = paste(unique(object_id), collapse = ",")),
                by = .(account_id, account_id_y)]

            fast_x_aggregated <-
                fast_x_aggregated[fast_x_obj, on = .(account_id, account_id_y), nomatch = 0]
        }

        # Create the subgraph of faster-vertices with corresponding edge weights
        fast_coord_graph <-
            graph_from_data_frame(fast_x_aggregated, directed = FALSE)

        # Merge the fast-vertices graph with the original full graph
        coord_graph <-
            igraph::graph.union(coord_graph, fast_coord_graph)

        # Rename the attributes
        coord_graph <-
            igraph::set_edge_attr(coord_graph,
                                  "weight_full",
                                  value = igraph::E(coord_graph)$weight_1)
        coord_graph <-
            igraph::set_edge_attr(coord_graph,
                                  "weight_fast",
                                  value = igraph::E(coord_graph)$weight_2)

        coord_graph <-
            igraph::set_edge_attr(
                coord_graph,
                "avg_time_delta_full",
                value = igraph::E(coord_graph)$avg_time_delta_1
            )
        coord_graph <-
            igraph::set_edge_attr(
                coord_graph,
                "avg_time_delta_fast",
                value = igraph::E(coord_graph)$avg_time_delta_2
            )

        coord_graph <-
            igraph::set_edge_attr(coord_graph,
                                  "n_content_id_full",
                                  value = igraph::E(coord_graph)$n_content_id_1)
        coord_graph <-
            igraph::set_edge_attr(
                coord_graph,
                "n_content_id_y_full",
                value = igraph::E(coord_graph)$n_content_id_y_1
            )

        coord_graph <-
            igraph::set_edge_attr(coord_graph,
                                  "n_content_id_fast",
                                  value = igraph::E(coord_graph)$n_content_id_2)
        coord_graph <-
            igraph::set_edge_attr(
                coord_graph,
                "n_content_id_y_fast",
                value = igraph::E(coord_graph)$n_content_id_y_2
            )

        coord_graph <-
            igraph::set_edge_attr(
                coord_graph,
                "edge_symmetry_score_full",
                value = igraph::E(coord_graph)$edge_symmetry_score_1
            )
        coord_graph <-
            igraph::set_edge_attr(
                coord_graph,
                "edge_symmetry_score_fast",
                value = igraph::E(coord_graph)$edge_symmetry_score_2
            )

        if (objects == TRUE) {
            coord_graph <-
                igraph::set_edge_attr(coord_graph,
                                      "object_ids_full",
                                      value = igraph::E(coord_graph)$object_ids_1)
            coord_graph <-
                igraph::set_edge_attr(coord_graph,
                                      "object_ids_fast",
                                      value = igraph::E(coord_graph)$object_ids_2)
        }

        # Remove the attributes that have been renamed
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "weight_1")
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "weight_2")
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "avg_time_delta_1")
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "avg_time_delta_2")
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "n_content_id_1")
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "n_content_id_y_1")
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "n_content_id_2")
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "n_content_id_y_2")
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "edge_symmetry_score_1")
        coord_graph <-
            igraph::delete_edge_attr(coord_graph, "edge_symmetry_score_2")

        if (objects == TRUE) {
            coord_graph <- igraph::delete_edge_attr(coord_graph, "object_ids_1")
            coord_graph <-
                igraph::delete_edge_attr(coord_graph, "object_ids_2")
        }

        # Mark edges according to the edge weight percentile -------------------
        # Here we mark edges that exceed the user-defined threshold. The threshold
        # is expressed in edge weight percentile and considers the distribution
        # of edge weights in the graph. The threshold is applied (1) to the entire
        # graph, considering the relative entire distribution of edge weights
        # (`weight_threshold_full`), and (2) to the graph with the fastest shares,
        # (`weight_threshold_fast`) considering the distribution of edge weights
        # of only that subgraph. In both cases, the percentile that determines
        # the threshold is the same as defined by the user, but the distribution
        # to which it applies is different. If the fast_net = TRUE option is not
        # specified, the threshold is applied only to the integer graph (in this
        # case, the resulting attribute is named `weight_threshold`).

        # Full network weight threshold
        threshold_full <-
            quantile(igraph::E(coord_graph)$weight_full, edge_weight)

        coord_graph <-
            igraph::set_edge_attr(
                coord_graph,
                "weight_threshold_full",
                value = ifelse(
                    igraph::E(coord_graph)$weight_full > threshold_full,
                    1,
                    0
                )
            )

        # Fast subgraph weight threshold
        threshold_fast <-
            quantile(igraph::E(coord_graph)$weight_fast,
                     edge_weight,
                     na.rm = TRUE)

        coord_graph <-
            igraph::set_edge_attr(
                coord_graph,
                "weight_threshold_fast",
                value = ifelse(
                    igraph::E(coord_graph)$weight_fast > threshold_fast,
                    1,
                    0
                )
            )

    # Else, if the fast_net = TRUE option is not specified, the threshold is
    # applied only to the original full graph ---
    } else {
        # Add the weight_threshold attribute to the full network only
        threshold_full <-
            quantile(igraph::E(coord_graph)$weight, edge_weight, na.rm = TRUE)

        coord_graph <-
            igraph::set_edge_attr(
                coord_graph,
                "weight_threshold",
                value = ifelse(igraph::E(coord_graph)$weight > threshold_full, 1, 0)
            )
    }

    #  -------------------------------------------------------------------------
    # Create subgraphs ---------------------
    # This part of the code generates several subgraphs, if the related options
    # are chosen by the user. In case no options are selected (the default is
    # subgraph = 0) the output graph is returned as generated by the code above.

    # Check the input for the option subgraph = 1
    if (subgraph == 1) {
        if (!any(grepl("weight_threshold\\b", names(igraph::edge_attr(coord_graph)))) &&
            fast_net == TRUE) {
            stop(
                "The subgraph = 1 was requested, but the fast_graph = TRUE option
                was also specified, so the requested subgraph cannot be generated.
                Check the function documentation for details."
            )
        }
    }

    # Subgraph = 1 considers the full graph and generates the filtered subgraph
    # according to the percentile edge weight threshold
    if (subgraph == 1) {

        edges_to_keep <-
            igraph::E(coord_graph)[which(igraph::E(coord_graph)$weight_threshold == 1)]

        coord_graph <-
            igraph::subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
    }

    # Subgraph = 2 considers only the edges created in the narrowest time window
    # and creates the filtered subgraph according to the percentile edge weight
    # threshold applied to this subgraph of fastest shares.
    if (subgraph == 2) {

        edges_to_keep <-
            igraph::E(coord_graph)[which(igraph::E(coord_graph)$weight_threshold_fast == 1)]

        coord_graph <-
            igraph::subgraph.edges(coord_graph, edges_to_keep, delete.vertices = TRUE)
    }

    # Subgraph = 3 identifies the fastest vertices connected with an edge weight
    # higher than the threshold, and creates a subgraph that includes these
    # vertices and their edges, but also their neighbors vertices (A vertex is a
    # neighbor of another one, in other words, the two vertices are adjacent,
    # if they are incident to the same edge). In other words, this option
    # identifies the "context" of the fastest coordinated network.
    if (subgraph == 3) {

        # Selecting edges from 'coord_graph' where the 'weight_threshold_fast'
        # attribute equals 1 (i.e., coordinated)
        edges_to_keep <-
            igraph::E(coord_graph)[which(igraph::E(coord_graph)$weight_threshold_fast == 1)]

        # Retrieving the vertex IDs at both ends of the 'edges_to_keep'
        edges_to_keep <- igraph::ends(coord_graph, edges_to_keep)

        # Finding vertices that are adjacent to any of the vertices
        # in 'edges_to_keep' (contextual nodes)
        adjacent <- igraph::adjacent_vertices(coord_graph, edges_to_keep)

        # Creating a subgraph of 'coord_graph' containing only the vertices in
        # 'adjacent' and all edges between them
        coord_graph <- igraph::induced_subgraph(coord_graph, unlist(adjacent))

        # Assign a binary index to determine the color of coordinated vertices (1)
        # and their adjacent vertices (0)
        coord_vertices <- unique(as.vector(edges_to_keep))

        igraph::V(coord_graph)$color_v <-
            ifelse(names(igraph::V(coord_graph)) %in% coord_vertices, 1, 0)
        }

    return(coord_graph)
}
