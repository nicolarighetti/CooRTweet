#' net_viz
#'
#' Create the coordinated network chart.
#'
#' @param coord_graph the igraph object representing the coordinated network.
#'
#' @return a plot in the 'Plots' window.
#'
#' @importFrom visNetwork toVisNetworkData visNetwork visNodes visEdges visIgraphLayout

utils::globalVariables(
  c(
    "cluster",
    "menu"
  )
)

net_viz <- function(coord_graph = coord_graph) {
  if (igraph::gorder(coord_graph) > 500) {
   input <- menu(
      c("Yes", "No"),
      title = paste0(
        "\nThe graph is large (",
        igraph::gorder(coord_graph),
        " nodes and ",
        igraph::gsize(coord_graph),
        " egdges), and the plot might take some time to render.\nReducing the size of the graph will reduce the time required to visualize the network.\nDo you want to plot just the nodes with highest weight?"
      )
    )

    if (input == 1) {
      input <- menu(
        c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
        print(summary(igraph::E(coord_graph)$weight)),
        title = "\nThis is the edge weigth distribution: where do you want to cut it?"
      )

      if (input == 1) {
        weight_cut <- summary(igraph::E(coord_graph)$weight)[1]
      }
      if (input == 2) {
        weight_cut <- summary(igraph::E(coord_graph)$weight)[2]
      }
      if (input == 3) {
        weight_cut <- summary(igraph::E(coord_graph)$weight)[3]
      }
      if (input == 4) {
        weight_cut <- summary(igraph::E(coord_graph)$weight)[4]
      }
      if (input == 5) {
        weight_cut <- summary(igraph::E(coord_graph)$weight)[5]
      }
      if (input == 6) {
        weight_cut <- summary(igraph::E(coord_graph)$weight)[6]
      }

      if (input == 1){
        coord_graph <-
          igraph::subgraph.edges(
            coord_graph,
            eids = which(igraph::E(coord_graph)$weight > weight_cut),
            delete.vertices = T
          )
      }

      if (input == 2 | input == 3 | input == 4 | input == 5){
        coord_graph <-
          igraph::subgraph.edges(
            coord_graph,
            eids = which(igraph::E(coord_graph)$weight >= weight_cut),
            delete.vertices = T
          )
      }

      if (input == 6){
        coord_graph <-
          igraph::subgraph.edges(
            coord_graph,
            eids = which(igraph::E(coord_graph)$weight >= weight_cut),
            delete.vertices = T
          )
      }

    }

    if (input == 2) {
      coord_graph <- coord_graph
      message("proceeding with the original network")
    }
  }

    vis_net <- visNetwork::toVisNetworkData(coord_graph)

    colors_10 <- c(
      "#8750a6",
      "#97b241",
      "#677fd7",
      "#d1a137",
      "#b84c7d",
      "#5bbc6d",
      "#ba4c41",
      "#48bf9e",
      "#b97736",
      "#74893a"
    )

    vis_net$nodes <- vis_net$nodes
    vis_net$edges <- vis_net$edges
    vis_net$edges$value <- vis_net$edges$weight

    vis_net$nodes$label <- vis_net$nodes$username
    vis_net$nodes$title <- vis_net$nodes$description
    vis_net$nodes$image <- vis_net$nodes$profile_image_url
    vis_net$nodes$value <- vis_net$nodes$degree
    vis_net$nodes$borderWidth <- 8

    top10_clusters <- vis_net$nodes |>
      dplyr::group_by(cluster) |>
      dplyr::summarize(n = dplyr::n()) |>
      dplyr::top_n(n = 10, wt = n)

    if (nrow(top10_clusters) < 10) {
      colors_10 <- colors_10[1:nrow(top10_clusters)]
    }

    for (i in 1:nrow(vis_net$nodes)) {
      for (j in 1:nrow(top10_clusters)) {
        if (vis_net$nodes$cluster[i] == top10_clusters$cluster[j])
          vis_net$nodes$color.border[i] <- colors_10[j]
      }
    }

    net_chart <-
      visNetwork::visNetwork(
        main = "Twitter Coordinated Network",
        submain = list(text = "CooRTweet",
                       style = "font-family:Arial;color:#000000;font-size:15px;text-align:center;"),
        vis_net$nodes,
        vis_net$edges,
        idToLabel = TRUE
      ) |>
      visNetwork::visIgraphLayout(
        layout = "layout_nicely",
        smooth = FALSE,
        physics = TRUE,
        randomSeed = 123
      ) |>
      visNetwork::visNodes(
        shape = "circularImage",
        scaling = list(min = 5, max = 50),
        font = '20px arial black'
      ) |>
      visNetwork::visEdges(
        width = 1,
        physics = FALSE,
        labelHighlightBold = TRUE,
        color = list(
          inherit = TRUE,
          opacity = 0.3,
          #color = "slategray",
          highlight = "black"
        )
      ) |>
      visNetwork::visPhysics(
        solver = "forceAtlas2Based",
        maxVelocity = 50,
        minVelocity = 0.1,
        timestep = 0.1,
        barnesHut = NULL,
        forceAtlas2Based = NULL,
        repulsion = NULL,
        hierarchicalRepulsion = NULL,
        stabilization = FALSE,
        adaptiveTimestep = NULL
      )

    return(net_chart)
  }
