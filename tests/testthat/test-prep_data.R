test_that("prep_data removes incomplete and duplicate shares by default", {
  input <- data.table::data.table(
    url = c("u1", "u1", "u1", NA_character_, "u2"),
    account = c("a", "a", "a", "b", "b"),
    post = c("p1", "p1", "p1", "p2", NA_character_),
    shared_at = c(100, 100, 100, 200, 300),
    note = c("first", "first", "other metadata", NA_character_, NA_character_)
  )
  before <- data.table::copy(input)

  result <- prep_data(input, "url", "account", "post", "shared_at")

  expect_equal(nrow(result), 1L)
  expect_equal(result$note, "first")
  expect_equal(names(input), names(before))
  expect_identical(input, before)
})

test_that("prep_data can preserve missing values and duplicate shares", {
  input <- data.table::data.table(
    object_id = c("u1", "u1", NA_character_),
    account_id = c("a", "a", "b"),
    content_id = c("p1", "p1", "p2"),
    timestamp_share = c(100, 100, 200)
  )

  expect_equal(nrow(prep_data(input, drop_na = FALSE)), 2L)
  expect_equal(nrow(prep_data(input, drop_duplicates = FALSE)), 2L)
  expect_identical(
    prep_data(input, drop_na = FALSE, drop_duplicates = FALSE),
    input
  )
  expect_error(prep_data(input, drop_na = NA), "TRUE or FALSE")
  expect_error(prep_data(input, drop_duplicates = 1), "TRUE or FALSE")
})

test_that("prep_data supports partial schemas and timestamp conversion", {
  partial <- data.table::data.table(object_id = c("u1", "u1", NA_character_),
                                    extra = c("a", "b", "c"))
  result <- prep_data(partial)
  expect_equal(nrow(result), 2L)

  dated <- data.table::data.table(
    object_id = c("u1", "u1", "u2"),
    account_id = c("a", "a", "b"),
    content_id = c("p1", "p1", "p2"),
    date = c("2021-01-01 00:00:00", "2021-01-01 00:00:00", NA_character_)
  )
  converted <- prep_data(dated, timestamp_share = "date")
  expect_equal(nrow(converted), 1L)
  expect_equal(converted$timestamp_share, 1609459200)
  expect_equal(nrow(prep_data(dated, timestamp_share = "date",
                              drop_na = FALSE, drop_duplicates = FALSE)), 3L)
})
