#' coord_network_detection
#'
#' Utility function to detect coordinated elements by using parallel computing.
#'
#' @param dset_rt the grouped data frame to detect coordination.
#' @param time_window the number of seconds within which considering a tweet to be coordinated (default to 60 seconds).
#' @param parallel_cores number of cores to be used in parallel computing. Default to all available cores -1.
#' @param quick Default to FALSE. If TRUE, a quick and less computationally intensive algorithm is implemented to identify coordinated users.
#'
#' @return a dataframe of tweets marked with coordinated to be used in the subsequent coordination analysis.
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom foreach foreach %dopar%
#' @importFrom purrr map
#' @importFrom tidytable bind_rows.
#' @importFrom dplyr summarize mutate filter summarize group_by left_join
#' @importFrom igraph graph_from_data_frame V degree
#' @importFrom tidyr separate

utils::globalVariables(
  c(
    "dset_rt",
    "i",
    "time_interval",
    "group_id",
    "n",
    "start",
    "combn",
    "V1",
    "V2",
    "created_time_x",
    "created_time_y",
    "tweet_id_x",
    "tweet_id_y",
    "time_diff",
    "author_id_x",
    "author_id_y",
    "weight",
    "author_id_xy",
    "quick",
    "count",
    "tweet_date",
    "value"
  )
)

coord_network_detection <- function(dset_rt,
                                     time_window,
                                     min_repetition,
                                     parallel_cores,
                                     coord_time_distribution,
                                     quick) {
  # setup parallel backend
  if (is.null(parallel_cores)) {
    cores <- parallel::detectCores() - 1
  } else {
    cores <- parallel_cores
  }

  cl <- parallel::makeCluster(cores, type = "PSOCK")
  doSNOW::registerDoSNOW(cl)

  groups_n <- length(unique(dset_rt$group_id))

  # progress bar
  pb <- utils::txtProgressBar(max = groups_n, style = 3)

  progress <- function(n)
    utils::setTxtProgressBar(pb, n)

  progress_bar <- list(progress = progress)

  # quick FALSE ####
  if (quick == FALSE) {

    # cycle trough all URLs to find entities that shared the same link within the coordination internal
    edge_list_summary <-
      foreach::foreach(
        i = seq(1:groups_n),
        .packages = c("dplyr", "purrr", "lubridate"),
        .options.snow = progress_bar
      ) %dopar% {
        # show progress...
        utils::setTxtProgressBar(pb, pb$getVal() + 1)

        group_id <- unique(dset_rt$group_id)[i]
        dset_rt_i <- dset_rt[dset_rt$group_id == group_id,]

        # create a vector of couples "author_id, rt_datetime" to identify the set of unique actions
        dset_rt_i <-
          apply(dset_rt_i[, c("author_id", "rt_datetime", "tweet_id", "group_id")],
                1, function(x)
                  paste(x, collapse = ","))

        # generate all k=2 combinations of the n couples of author_id and rt_datetime, without duplicates
        # n! / k!(n-k)!
        if (length(dset_rt_i) > 1) {
          dset_rt_i <- as.data.frame(t(combn(
            dset_rt_i, m = 2, simplify = T
          )))
        }
      }

    parallel::stopCluster(cl)

    edge_list <- tidytable::bind_rows.(edge_list_summary)

    if(nrow(edge_list) == 0){
      message("\n### No network detected ###")
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }

    edge_list <- as.data.frame(cbind(
      do.call(rbind, strsplit(edge_list$V1, ",")),
      do.call(rbind, strsplit(edge_list$V2, ","))[,1:3]
    ))

    colnames(edge_list) <- c("author_id_x", "created_time_x", "tweet_id_x", "group_id",
                             "author_id_y", "created_time_y", "tweet_id_y")

    edge_list <- edge_list |>
      dplyr::mutate(
      created_time_x = as.numeric(created_time_x),
      created_time_y = as.numeric(created_time_y)
    ) |>
      dplyr::mutate(time_diff = abs(created_time_x - created_time_y))

    # if coord_time_distribution = TRUE, saving and tidying the full edge list
    if (coord_time_distribution == TRUE) {
      full_edge_list <- edge_list
      tidy_full_edge_list <- edge_list |>
        dplyr::mutate(
          created_time_x = as.character(created_time_x),
          created_time_y = as.character(created_time_y)
        ) |>
        tidyr::pivot_longer(cols = -c(group_id, time_diff)) |>
        dplyr::mutate(name = dplyr::case_when(
          name %in% c("author_id_x", "author_id_y") ~ "author_id",
          name %in% c("tweet_id_x", "tweet_id_y") ~ "tweet_id",
          name %in% c("created_time_x", "created_time_y") ~ "created_time"
        )) |>
        dplyr::filter(!is.na(name)) |>
        dplyr::group_by(name) |>
        dplyr::mutate(row = dplyr::row_number()) |>
        tidyr::pivot_wider(names_from = name, values_from = value) |>
        dplyr::select(-row)
    }

    # filter edge list by time_window
    edge_list <- edge_list |>
      dplyr::filter(time_diff <= time_window)

    if(nrow(edge_list) == 0){
      message("\n### No network detected ###")
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }

    el_df <- edge_list |>
      dplyr::mutate(
        created_time_x = as.character(created_time_x),
        created_time_y = as.character(created_time_y)
      ) |>
      dplyr::select(-time_diff) |>
      tidyr::pivot_longer(cols = -group_id)

    el <- edge_list |>
      tidyr::pivot_longer(cols = c(author_id_x, author_id_y),
                          values_to = "author_id") |>
      dplyr::select(-name) |>
      dplyr::select(author_id, group_id)
  }



  # quick TRUE ####
  if (quick == TRUE) {
    edge_list_summary <-
      foreach::foreach(
        i = seq(1:groups_n),
        .packages = c("dplyr", "purrr", "lubridate"),
        .options.snow = progress_bar
      ) %dopar% {
        # show progress...
        utils::setTxtProgressBar(pb, pb$getVal() + 1)

        group_id <- unique(dset_rt$group_id)[i]
        dset_rt_i <- dset_rt[dset_rt$group_id == group_id,]

        if (nrow(dset_rt_i) > 1) {
          dat.summary <- dset_rt_i |>
            dplyr::arrange(created_at) |>
            dplyr::mutate(cut = cut(created_at, breaks = paste(time_window, "sec"))) |>
            dplyr::group_by(cut) |>
            dplyr::mutate(
              count = dplyr::n(),
              author_id = list(author_id),
              tweet_id = list(tweet_id),
              tweet_date = list(created_at),
              group_id = group_id
            ) |>
            dplyr::select(cut, count, author_id, tweet_id, tweet_date, group_id) |>
            # subset the URLs shared by more than one entity
            dplyr::filter(count > 1) |>
            unique()
        }
      }

    parallel::stopCluster(cl)

    edge_list <- tidytable::bind_rows.(edge_list_summary)

    if(nrow(edge_list) == 0){
      message("\n### No network detected ###")
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }

    el_df <-
      edge_list[, c("author_id", "group_id", "tweet_id")]

    el <- el_df |>
      dplyr::mutate(author_id = as.character(author_id),
                    group_id = as.character(group_id)) |>
      tidyr::separate_rows(author_id, sep = ",") |>
      dplyr::mutate(author_id = gsub('"|c|\\(|\\)', "", author_id)) |>
      dplyr::mutate(author_id = gsub("[[:space:]]", "", author_id)) |>
      tidyr::separate_rows(tweet_id, sep = ",") |>
      dplyr::mutate(tweet_id = gsub('"|c|\\(|\\)', "", tweet_id)) |>
      dplyr::mutate(tweet_id = gsub("[[:space:]]", "", tweet_id)) |>
      dplyr::filter(nchar(author_id) > 0) |>
      dplyr::filter(nchar(tweet_id) > 0) |>
      dplyr::distinct() |>
      dplyr::mutate(group_id = as.character(group_id)) |>
      dplyr::mutate(group_id = paste0("g", group_id))
  }

  # SNA ####
  v1 <-
    data.frame(node = unique(el$author_id), type = 1)
  v2 <- data.frame(node = unique(el$group_id), type = 0)
  v <- rbind(v1, v2)

  g2.bp <-
    igraph::graph.data.frame(el, directed = T, vertices = v) # makes the bipartite graph
  g2.bp <-
    igraph::simplify(
      g2.bp,
      remove.multiple = T,
      remove.loops = T,
      edge.attr.comb = "min"
    ) # simplify the bipartite network to avoid problems with resulting edge weight in projected network
  full_g <-
    suppressWarnings(igraph::bipartite.projection(g2.bp, multiplicity = T)$proj2) # project entity-entity network

  # keep only highly coordinated entities
  igraph::V(full_g)$degree <- igraph::degree(full_g)
  coord_graph <-
    igraph::induced_subgraph(graph = full_g, vids = igraph::V(full_g)[igraph::V(full_g)$degree > 0]) # filter for degree
  # filter for edge weight
  coord_graph <-
    igraph::subgraph.edges(
      coord_graph,
      eids = which(igraph::E(coord_graph)$weight >= min_repetition),
      delete.vertices = T
    )

  if(igraph::gorder(coord_graph) == 0){
    message("\n### No network detected ###")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  # mark coordinated tweets
  if (quick == FALSE) {
    el <- el_df |>
      dplyr::mutate(name = dplyr::case_when(
        name %in% c("author_id_x", "author_id_y") ~ "author_id",
        name %in% c("tweet_id_x", "tweet_id_y") ~ "tweet_id"
      )) |>
      dplyr::filter(!is.na(name)) |>
      dplyr::group_by(name) |>
      dplyr::mutate(row = dplyr::row_number()) |>
      tidyr::pivot_wider(names_from = name, values_from = value) |>
      dplyr::select(-row)
  }

  el <- el |>
    dplyr::filter(author_id %in% igraph::V(coord_graph)$name)

  if (quick == TRUE) {
    el$group_id <- sub("g", "", el$group_id)
  }

  dset_rt$coordinated <-
    ifelse(
      paste0(dset_rt$tweet_id,
             dset_rt$group_id) %in%
        paste0(el$tweet_id,
               el$group_id),
      TRUE,
      FALSE
    )

  # find and annotate nodes-components
  igraph::V(coord_graph)$component <-
    igraph::components(coord_graph)$membership
  igraph::V(coord_graph)$cluster <-
    igraph::cluster_louvain(coord_graph)$membership
  igraph::V(coord_graph)$degree <-
    igraph::degree(coord_graph) # re-calculate the degree on the subgraph


  if (coord_time_distribution == TRUE) {
    return(list(edge_list, dset_rt, coord_graph, full_edge_list, tidy_full_edge_list))
  } else {
    return(list(edge_list, dset_rt, coord_graph))
  }
}
