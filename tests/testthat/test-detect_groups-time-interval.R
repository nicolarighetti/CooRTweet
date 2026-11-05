test_that("detect_groups includes both limits of a selected interval", {
  shares <- data.table::data.table(
    object_id = "url_1",
    account_id = c("A", "B", "C"),
    content_id = c("p1", "p2", "p3"),
    timestamp_share = c(0, 10, 30)
  )

  result <- detect_groups(data.table::copy(shares), time_window = 20,
                          time_window_low = 10, min_participation = 1)
  expect_equal(sort(result$time_delta), c(10, 20))

  exact <- detect_groups(data.table::copy(shares), time_window = 10,
                         time_window_low = 10, min_participation = 1)
  expect_equal(exact$time_delta, 10)
})

test_that("the default lower limit retains zero-second pairs and positional calls", {
  simultaneous <- data.table::data.table(
    object_id = "url_1",
    account_id = c("A", "B"),
    content_id = c("p1", "p2"),
    timestamp_share = c(0, 0)
  )
  zero <- detect_groups(data.table::copy(simultaneous), time_window = 0,
                        min_participation = 1)
  expect_equal(zero$time_delta, 0)

  shares <- data.table::data.table(
    object_id = "url_1",
    account_id = c("A", "B", "C"),
    content_id = c("p1", "p2", "p3"),
    timestamp_share = c(0, 10, 30)
  )
  default <- detect_groups(data.table::copy(shares), time_window = 20,
                           min_participation = 1)
  explicit <- detect_groups(data.table::copy(shares), time_window = 20,
                            min_participation = 1, time_window_low = 0)
  positional <- detect_groups(data.table::copy(shares), 20, 1)
  expect_equal(default, explicit)
  expect_equal(default, positional)
})

test_that("flag_speed_share only flags pairs already in the selected interval", {
  shares <- data.table::data.table(
    object_id = "url_1",
    account_id = c("A", "B", "C"),
    content_id = c("p1", "p2", "p3"),
    timestamp_share = c(0, 10, 30)
  )
  result <- detect_groups(data.table::copy(shares), time_window = 20,
                          time_window_low = 10, min_participation = 1)

  flagged <- flag_speed_share(data.table::copy(shares),
                              data.table::copy(result),
                              min_participation = 1, time_window = 15)
  expect_equal(flagged$time_window_15[order(flagged$time_delta)], c(1, 0))

  below_interval <- flag_speed_share(data.table::copy(shares),
                                     data.table::copy(result),
                                     min_participation = 1, time_window = 5)
  expect_true(all(below_interval$time_window_5 == 0))
})

test_that("detect_groups rejects invalid time interval limits", {
  shares <- data.table::data.table(
    object_id = "url_1",
    account_id = c("A", "B"),
    content_id = c("p1", "p2"),
    timestamp_share = c(0, 10)
  )
  expect_error(detect_groups(shares, time_window = -1), "time_window must")
  expect_error(detect_groups(shares, time_window = c(10, 20)), "time_window must")
  expect_error(detect_groups(shares, time_window_low = NA_real_),
               "time_window_low must")
  expect_error(detect_groups(shares, time_window_low = Inf),
               "time_window_low must")
  expect_error(detect_groups(shares, time_window_low = "2"),
               "time_window_low must")
  expect_error(detect_groups(shares, time_window = 10, time_window_low = 20),
               "must not exceed")
})
