#' simulate_data
#'
#' @description
#' Create a simulated input and output of
#' \link{detect_groups} function.
#'
#' @details
#' This function generates a simulated dataset with fixed
#' numbers for coordinated users, uncoordinated users, and
#' shared objects. You can set minimum repetition and time window
#' and the coordinated users will "act" randomly within these restrictions.
#'
#'
#' @param n_users_coord the desired number of coordinated users.
#'
#' @param n_users_noncoord the desired number of non-coordinated users.
#'
#' @param n_objects the desired number of objects.
#'
#' @param min_repetition the minimum number of repeated coordinated
#' action to define two user as coordinated.
#'
#' @param time_window the time window of coordination.
#'
#' @return a list with two data frames: a data frame
#' with the columns required by the function detect_
#' coordinated_groups (`object_id`, `account_id`, `content_id`, `timestamp_share`)
#' and the output table of the same
#' \link{detect_groups} function and columns:
#' `object_id`, `account_id`, `account_id_y`,
#' `content_id`, `content_id_y`, `time_delta`.
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom stats na.omit rpois
#'
#' @export
#'

simulate_data <- function(
    n_users_coord = 5,
    n_users_noncoord = 4,
    n_objects = 5,
    min_repetition = 3,
    time_window = 10) {
    # Create User IDs --------------------------
    # create sets of IDs for both coordianted and non-coordinated users

    # TODO: assert that parameters are valid
    # minimum number of users: 2
    # min repetition: 1
    # min time_window: 1
    # min n_objects: 1

    # user_ids ----

    # Create a set of user_ids of length n_users_coord + n_user_non_coord
    # and extract coord users and non coord users
    total_users <- n_users_coord + n_users_noncoord
    random_numbers <- sample(999999999, total_users, replace = FALSE)
    user_ids <- paste0("user_", random_numbers)

    user_ids_coord <- user_ids[1:n_users_coord]
    user_ids_noncoord <- user_ids[
        (n_users_coord + 1):(n_users_coord + n_users_noncoord)
    ]

    # Share IDs ----

    # Function to generate random IDs for shares
    generate_random_strings <- function(n = 1) {
        a <- do.call(paste0, replicate(15, sample(LETTERS, n, TRUE), FALSE))
        paste0(
            a,
            sprintf("%04d", sample(999999999, n, TRUE)),
            sample(LETTERS, n, TRUE)
        )
    }

    # object_ids ----
    # These are the IDs of the content that is shared
    # in a coordinated fashion.

    # Create a set of object_ids of size n_objects
    object_ids <- unique(stringi::stri_rand_strings(n_objects,
        length = sample(c(5:10), n_objects, replace = TRUE),
        pattern = "[A-Za-z]"
    ))

    # timestamps ----

    # Choose a random starting date for the simulation
    start_date <- as.Date(
        sample(as.Date("2000-01-01"):as.Date("2020-12-31"), 1),
        origin = "1970-01-01"
    )

    # the first coordinated timestamp is at t0
    # all subsequent timestamps are counted from here
    t0 <- as.numeric(as.POSIXct(start_date))

    # Coordinated Behavior - Main structure -----------------

    # symmetric matrix of size n = n_users_coord and names in user_ids_coord
    coord_matrix <- matrix(0,
        nrow = n_users_coord, ncol = n_users_coord,
        dimnames = list(user_ids_coord, user_ids_coord)
    )

    # Repetitions sampled with Poisson distribution
    # which minimum is 0, so we add `min_repetition`
    coord_matrix[upper.tri(coord_matrix)] <- rpois(
        sum(upper.tri(coord_matrix)), 1
    ) + min_repetition

    # to data.frame
    coord_matrix[lower.tri(coord_matrix)] <- NA
    diag(coord_matrix) <- NA
    df_coord <- na.omit(data.frame(as.table(coord_matrix)))
    df_coord <- df_coord[rep(seq_len(nrow(df_coord)), df_coord$Freq), ]
    df_coord$Freq <- NULL
    rownames(df_coord) <- NULL
    colnames(df_coord) <- c("user_X", "user_Y")

    # add object ID
    df_coord$object_ID <- sample(
        object_ids,
        size = nrow(df_coord),
        replace = TRUE
    )

    # Non-Coordinated Behavior - Main structure ---------------

    # Use the same object_ids as above
    # symmetric matrix of size n = n_users_noncoord
    # and names in user_ids_noncoord

    noncoord_matrix <- matrix(0,
        nrow = n_users_noncoord, ncol = n_users_noncoord,
        dimnames = list(user_ids_noncoord, user_ids_noncoord)
    )

    # number of repeatet tweets sampled with a poisson distribution
    # non-coordinated users can theoretically still have a high number
    # of repeated content. But they must not do that within a short
    # time window
    noncoord_matrix[upper.tri(noncoord_matrix)] <- rpois(
        sum(upper.tri(noncoord_matrix)), 2
    ) + 1

    # to data.frame
    noncoord_matrix[lower.tri(noncoord_matrix)] <- NA
    diag(noncoord_matrix) <- NA
    df_noncoord <- na.omit(data.frame(as.table(noncoord_matrix)))
    df_noncoord <- df_noncoord[rep(
        seq_len(nrow(df_noncoord)), df_noncoord$Freq
    ), ]

    df_noncoord$Freq <- NULL
    rownames(df_noncoord) <- NULL
    colnames(df_noncoord) <- c("user_X", "user_Y")

    # add object ID
    df_noncoord$object_ID <- sample(object_ids,
        size = nrow(df_noncoord),
        replace = TRUE
    )


    # Timestamps -----------------------

    # assign coordinated timestamps using systematic sampling where the
    # sampling interval is higher than time_interval,
    # to avoid unplanned links between users.
    # Assing the sampled timestamp to users' A shares,
    # then assign to users' B shares a timestamps equal to
    # the A interval + noise lower than time_window

    # the sampling interval is more than 2x time_window to avoid overlaps
    sampling_interval <- (2 * time_window) + 1

    # start at t0 and then generate timestamps in the interval set above
    df_coord$share_time_A <- seq(
        from = t0,
        by = sampling_interval,
        length.out = nrow(df_coord)
    )

    # add share time to B adding number of seconds < time_interval
    delta <- sample(0:time_window, nrow(df_coord), replace = TRUE)
    df_coord$share_time_B <- df_coord$share_time_A + delta

    df_coord$delta <- abs(df_coord$share_time_A - df_coord$share_time_B)

    # mark as coordinated
    df_coord$coordinated <- as.logical("TRUE")

    # non-coordinated timestamps:
    # users could share the same content with an arbitrary time_delta
    # it has to be higher than time_window, so we roughly make it twice as large
    # minimum is time_window + 1, maximum is 2x time_window + 1
    noise <- 1 + time_window +
        sample.int(time_window, nrow(df_noncoord), replace = TRUE)

    # the sampling interval is needs to be larger than the noise
    sampling_interval <- max(noise) * 2

    # extract timestamps from coordinated timestamp set and with intervals
    # exceeding time_window.
    # Uncoordinated users start their time stamps at the last timestamp
    # of the coordinated users.

    t1 <- max(df_coord$share_time_B) + sampling_interval

    # now we sample timestamps for user A with a larger
    # interval than coordinated users

    df_noncoord$share_time_A <- seq(
        from = t1,
        by = sampling_interval,
        length.out = nrow(df_noncoord)
    )

    df_noncoord$share_time_B <- df_noncoord$share_time_A + noise

    df_noncoord$delta <- abs(df_noncoord$share_time_A -
        df_noncoord$share_time_B)

    # mark as non coordinated
    df_noncoord$coordinated <- as.logical("FALSE")

    output_table <- rbind(df_coord, df_noncoord)

    # shares IDs -----------------

    total_id_needed <- nrow(output_table) * 2
    share_ids <- generate_random_strings(n = total_id_needed)

    output_table$share_id_A <- share_ids[
        1:(length(share_ids) / 2)
    ]

    output_table$share_id_B <- share_ids[
        (1 + (length(share_ids) / 2)):length(share_ids)
    ]

    # subset and rename accordingly to the detect_groups output
    output_table <- output_table[
        ,
        c(
            "object_ID", "share_id_A", "share_id_B",
            "delta", "user_X", "user_Y", "coordinated",
            "share_time_A", "share_time_B"
        )
    ]

    colnames(output_table) <- c(
        "object_id", "content_id",
        "content_id_y", "time_delta", "account_id", "account_id_y",
        "coordinated", "share_time_A", "share_time_B"
    )

    # emergency check in case simulated data contains errors
    if (any(is.na(output_table))) {
        stop("Simulated data contains NAs!")
    }

    # Input data  -------------------------
    # (Created by reshaping the output_table)
    input_dataset <-
        unique(data.frame(
            object_id = c(
                output_table$object_id,
                output_table$object_id
            ),
            account_id = c(
                output_table$account_id,
                output_table$account_id_y
            ),
            content_id = c(
                output_table$content_id,
                output_table$content_id_y
            ),
            timestamp_share = c(
                output_table$share_time_A,
                output_table$share_time_B
            ),
            stringsAsFactors = FALSE
        ))

    # drop unecessary columns share_time_A/share_time_B
    output_table <- output_table[, -c(8:9)]

    output_list <- list(input_dataset, output_table)

    gc()

    return(output_list)
}
