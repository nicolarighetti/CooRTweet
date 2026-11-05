testthat::test_that("The default network contains only edges above the median weight", {
    result <- data.table::data.table(
        object_id = letters[1:6],
        account_id = c("a", "a", "a", "b", "b", "b"),
        account_id_y = c("b", "c", "c", "c", "c", "c"),
        content_id = paste0("x", 1:6),
        content_id_y = paste0("y", 1:6),
        time_delta = rep(1, 6)
    )

    filtered <- generate_coordinated_network(result)
    full <- generate_coordinated_network(result, subgraph = 0)

    testthat::expect_equal(igraph::ecount(filtered), 1L)
    testthat::expect_equal(sort(igraph::V(filtered)$name), c("b", "c"))
    testthat::expect_equal(igraph::E(filtered)$weight, 3)
    testthat::expect_equal(igraph::ecount(full), 3L)

    result[, time_window_10 := c(1, 1, 1, 1, 0, 0)]
    filtered_with_fast_data <- generate_coordinated_network(result, fast_net = TRUE)
    filtered_fast <- generate_coordinated_network(result, fast_net = TRUE, subgraph = 2)
    full_with_fast_data <- generate_coordinated_network(result, fast_net = TRUE, subgraph = 0)

    testthat::expect_equal(igraph::ecount(filtered_with_fast_data), 1L)
    testthat::expect_equal(sort(igraph::V(filtered_with_fast_data)$name), c("b", "c"))
    testthat::expect_equal(igraph::E(filtered_with_fast_data)$weight_threshold_full, 1)
    testthat::expect_equal(sort(igraph::V(filtered_fast)$name), c("a", "c"))
    testthat::expect_equal(igraph::E(filtered_fast)$weight_threshold_fast, 1)
    testthat::expect_equal(igraph::ecount(full_with_fast_data), 3L)
})
