#' generate_network
#'
#' Take the results of coordinated content detection and generate a network from the data. This function generates a two-mode (bipartite) incidence matrix first, and then projects the matrix to a weighted adjacency matrix.
#'
#' @param x a data.table (result from `detect_coordinated_groups`) with the Columns: `object_id`, `id_user`, `id_user_y`, `content_id`, `content_id_y`, `timedelta`
#' @param intent the intended network. The option `"users"` generates a network of users who are connected over the same content that they share (default). Option "content" generates a network based on content ids. Option "objects" generates a network of the coordinated content (`object_id`) that is connected via the users.
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

generate_network <- function(x, intent = c("users", "content", "objects")) {
    object_id = nodes = NULL

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

    return(coord_graph)
}
