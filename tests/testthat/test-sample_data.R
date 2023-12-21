library(data.table)

if (testthat:::on_cran()) {
    data.table::setDTthreads(threads = 2)
}

testthat::test_that("Sample dataset is consistent", {
    testthat::expect_equal(
        length(russian_coord_tweets$content_id),
        nrow(russian_coord_tweets)
    )
})

testthat::test_that("Self-coordinated posts are filtered out", {
    result <- detect_groups(russian_coord_tweets,
        min_participation = 5,
        time_window = 10,
        remove_loops = TRUE
    )

    coord_graph <- generate_coordinated_network(result)

    # Generate summary of user statistics
    summary_users <- user_stats(coord_graph, weight_threshold = "none")
    user_self_coord <- summary_users[account_id == "9fa51ef17278c01d13d313741eddfc0b"]
    testthat::expect_equal(nrow(user_self_coord), 0)
})

testthat::test_that("Self-coordinated posts are not filtered out", {
    result <- detect_groups(russian_coord_tweets,
                            min_participation = 5,
                            time_window = 10,
                            remove_loops = FALSE
    )

    coord_graph <- generate_coordinated_network(result)

    # Generate summary of user statistics
    summary_users <- user_stats(coord_graph, weight_threshold = "none")
    user_self_coord <- summary_users[account_id == "9fa51ef17278c01d13d313741eddfc0b"]
    testthat::expect_gt(nrow(user_self_coord), 0)
})
