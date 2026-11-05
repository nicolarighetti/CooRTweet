# Run from the repository root. The full anonymized data is retained in
# data-raw/ for reproducing the paper; only this sample enters the R package.
source_file <- file.path("data-raw", "german_elections_full.rda")
stopifnot(file.exists(source_file))
load(source_file)

start <- as.numeric(as.POSIXct("2021-08-15 00:00:00", tz = "UTC"))
end <- as.numeric(as.POSIXct("2021-09-08 00:00:00", tz = "UTC"))
german_elections_sample <- german_elections[
  !is.na(german_elections$timestamp) &
    german_elections$timestamp >= start &
    german_elections$timestamp < end,
]

stopifnot(
  nrow(german_elections_sample) == 97413L,
  any(grepl("^fb", german_elections_sample$account_id)),
  any(grepl("^tw", german_elections_sample$account_id)),
  any(!is.na(german_elections_sample$url_id)),
  any(!is.na(german_elections_sample$phash_id))
)

output_file <- file.path("data", "german_elections_sample.rda")
save(german_elections_sample, file = output_file, compress = "xz")
stopifnot(file.info(output_file)$size < 1000000)
