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
#' @importFrom magrittr %>%

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
    "value",
    "filtered_comb",
    "dset_rt_i_x",
    "dset_rt_i_y",
    "rt_datetime",
    "full_edge_list",
    "tidy_full_edge_list"
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
        .packages = c("dplyr", "purrr", "lubridate", "base", "utils"),
        .options.snow = progress_bar
      ) %dopar% {
        # show progress...
        utils::setTxtProgressBar(pb, pb$getVal() + 1)

        group_id <- unique(dset_rt$group_id)[i]
        dset_rt_i <- dset_rt[dset_rt$group_id == group_id,]

        dset_rt_i <- dset_rt_i %>%
          dplyr::ungroup() %>%
          dplyr::select(author_id, rt_datetime, tweet_id, group_id) %>%
          dplyr::distinct()

        if (nrow(dset_rt_i) > 5000) {
          message(paste("working on", choose(
            n = nrow(dset_rt_i), k = 2
          )),
          "combinations. Please wait...")
        }

        if (nrow(dset_rt_i) >= 2) {
          all_comb <- combn(1:nrow(dset_rt_i), 2)

          all_times  <- combn(dset_rt_i$rt_datetime, 2)

          time_diff <-
            abs(apply(all_times, 2, function(x)
              x[1] - x[2]))

          within_time_window <- which(time_diff <= time_window)

          if (length(within_time_window) > 0) {
            filtered_comb <- all_comb[, within_time_window]

            # just one row
            if (is.null(nrow(filtered_comb))) {
              dset_rt_i_x <- dset_rt_i[filtered_comb, 1:3]
              dset_rt_i_y <- dset_rt_i[filtered_comb,]
              colnames(dset_rt_i_x) <-
                paste0(names(dset_rt_i_x), "_x")
              colnames(dset_rt_i_y)[1:3] <-
                paste0(names(dset_rt_i_y)[1:3], "_y")
            }

            # matrix
            if (!is.null(nrow(filtered_comb))) {
              dset_rt_i_x <- dset_rt_i[filtered_comb[1,], 1:3]
              dset_rt_i_y <- dset_rt_i[filtered_comb[2,],]
              colnames(dset_rt_i_x) <-
                paste0(names(dset_rt_i_x), "_x")
              colnames(dset_rt_i_y)[1:3] <-
                paste0(names(dset_rt_i_y)[1:3], "_y")
            }
            time_diff <- time_diff[within_time_window]
            dset_rt_i <- cbind(dset_rt_i_x, dset_rt_i_y, time_diff)
          }
        }
      }


    parallel::stopCluster(cl)

    edge_list <- tidytable::bind_rows.(edge_list_summary)

    if (nrow(edge_list) == 0) {
      message("\n### No network was detected ###")
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }

    el_df <- edge_list %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::pivot_longer(cols = -group_id)

    el <- edge_list %>%
      tidyr::pivot_longer(cols = c(author_id_x, author_id_y),
                          values_to = "author_id") %>%
      dplyr::select(-name) %>%
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
        dset_rt_i <- dset_rt[dset_rt$group_id == group_id, ]

        if (nrow(dset_rt_i) > 1) {
          dat.summary <- dset_rt_i %>%
            dplyr::arrange(created_at) %>%
            dplyr::mutate(cut = cut(created_at, breaks = paste(time_window, "sec"))) %>%
            dplyr::group_by(cut) %>%
            dplyr::mutate(
              count = dplyr::n(),
              author_id = list(author_id),
              tweet_id = list(tweet_id),
              tweet_date = list(created_at),
              group_id = group_id
            ) %>%
            dplyr::select(cut, count, author_id, tweet_id, tweet_date, group_id) %>%
            # subset the URLs shared by more than one entity
            dplyr::filter(count > 1) %>%
            unique()
        }
      }

    parallel::stopCluster(cl)

    edge_list <- tidytable::bind_rows.(edge_list_summary)

    if (nrow(edge_list) == 0) {
      message("\n### No network was detected ###")
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }

    el_df <-
      edge_list[, c("author_id", "group_id", "tweet_id")]

    el <- el_df %>%
      dplyr::mutate(author_id = as.character(author_id),
                    group_id = as.character(group_id)) %>%
      tidyr::separate_rows(author_id, sep = ",") %>%
      dplyr::mutate(author_id = gsub('"|c|\\(|\\)', "", author_id)) %>%
      dplyr::mutate(author_id = gsub("[[:space:]]", "", author_id)) %>%
      tidyr::separate_rows(tweet_id, sep = ",") %>%
      dplyr::mutate(tweet_id = gsub('"|c|\\(|\\)', "", tweet_id)) %>%
      dplyr::mutate(tweet_id = gsub("[[:space:]]", "", tweet_id)) %>%
      dplyr::filter(nchar(author_id) > 0) %>%
      dplyr::filter(nchar(tweet_id) > 0) %>%
      dplyr::distinct() %>%
      dplyr::mutate(group_id = as.character(group_id)) %>%
      dplyr::mutate(group_id = paste0("g", group_id))
  }

  # SNA ####
  v1 <-
    data.frame(node = unique(el$author_id), type = 1)
  v2 <- data.frame(node = unique(el$group_id), type = 0)
  v <- rbind(v1, v2)

  g2.bp <-
    igraph::graph.data.frame(el, directed = T, vertices = v)
  g2.bp <-
    igraph::simplify(
      g2.bp,
      remove.multiple = T,
      remove.loops = T,
      edge.attr.comb = "min"
    )
  full_g <-
    suppressWarnings(igraph::bipartite.projection(g2.bp, multiplicity = T)$proj2)

  igraph::V(full_g)$degree <- igraph::degree(full_g)
  coord_graph <-
    igraph::induced_subgraph(graph = full_g, vids = igraph::V(full_g)[igraph::V(full_g)$degree > 0])

  # filter for min_repetition
  coord_graph <-
    igraph::subgraph.edges(
      coord_graph,
      eids = which(igraph::E(coord_graph)$weight >= min_repetition),
      delete.vertices = T
    )

  if (igraph::gorder(coord_graph) == 0) {
    message("\n### No network was detected ###")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  # mark coordinated tweets
  if (quick == FALSE) {
    el <- el_df %>%
      dplyr::mutate(name = dplyr::case_when(
        name %in% c("author_id_x", "author_id_y") ~ "author_id",
        name %in% c("tweet_id_x", "tweet_id_y") ~ "tweet_id"
      )) %>%
      dplyr::filter(!is.na(name)) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      tidyr::pivot_wider(names_from = name, values_from = value) %>%
      dplyr::select(-row)
  }

  el <- el %>%
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

  # components and clusters
  igraph::V(coord_graph)$component <-
    igraph::components(coord_graph)$membership
  igraph::V(coord_graph)$cluster <-
    igraph::cluster_louvain(coord_graph)$membership
  igraph::V(coord_graph)$degree <-
    igraph::degree(coord_graph) # re-calculate the degree on the subgraph


  if (coord_time_distribution == TRUE) {
    return(list(
      edge_list,
      dset_rt,
      coord_graph,
      full_edge_list,
      tidy_full_edge_list
    ))
  } else {
    return(list(edge_list, dset_rt, coord_graph))
  }
}
