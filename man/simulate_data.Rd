% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_data.R
\name{simulate_data}
\alias{simulate_data}
\title{simulate_data}
\usage{
simulate_data(
  approx_size = 200,
  n_accounts_coord = 5,
  n_accounts_noncoord = 4,
  n_objects = 5,
  min_participation = 3,
  time_window = 10,
  lambda_coord = NULL,
  lambda_noncoord = NULL
)
}
\arguments{
\item{approx_size}{the approximate size of the desired dataset.
It automatically calculates the lambdas passed to \code{rpois()}, which is the
expected rate of occurrences. It only works when \code{lambda_coord} and
\code{lambda_noncoord} are NULL (default).}

\item{n_accounts_coord}{the desired number of coordinated accounts.}

\item{n_accounts_noncoord}{the desired number of non-coordinated accounts.}

\item{n_objects}{the desired number of objects.}

\item{min_participation}{the minimum number of repeated coordinated
action to define two accounts as coordinated.}

\item{time_window}{the time window of coordination.}

\item{lambda_coord}{\code{lambda} parameter for coordinated accounts passed to
\code{rpois()}, which is the expected rate of occurrences
(higher lambda means more coordinated shares).}

\item{lambda_noncoord}{\code{lambda} parameter for non-coordinated accounts passed to
\code{rpois()}, which is the expected rate of occurrences
(higher lambda means more non-coordinated shares).}
}
\value{
a list with two data frames: a data frame
with the columns required by the function detect_
coordinated_groups (\code{object_id}, \code{account_id}, \code{content_id}, \code{timestamp_share})
and the output table of the same
\link{detect_groups} function and columns:
\code{object_id}, \code{account_id}, \code{account_id_y},
\code{content_id}, \code{content_id_y}, \code{time_delta}.
}
\description{
Create a simulated input and output of
\code{\link{detect_groups}} function.
}
\details{
This function generates a simulated dataset with fixed
numbers for coordinated accounts, uncoordinated accounts, and
shared objects. The user can set minimum participation and time window parameters
and the coordinated accounts will "act" randomly within these restrictions.

The size of the resulting dataset can be adjusted using the \code{approx_size}
parameter, and the function will return approximately a dataset of the required
size. Additionally, the size of the dataset can also be adjusted with the
\code{lambda_coord} and \code{lambda_noncoord} parameters. These correspond to the lambda
for the \code{rpois} Poisson distribution used to populate the coordination matrix.
If lambda is between 0.0 and 1.0, the dataset will be smaller compared to
choosing lambdas greater than 1. The \code{approx_size} parameter also serves to
set the lambda of the \code{rpois} function in a more intuitive way.
}
\examples{
# Example usage of simulate_data
\dontrun{
set.seed(123) # For reproducibility
simulated_data <- simulate_data(
  n_accounts_coord = 100,
  n_accounts_noncoord = 50,
  n_objects = 20,
  min_participation = 2,
  time_window = 10
)

# Extract input
input_data <- simulated_data[[1]]

# Extract output and keep coordinated actors.
# This is expected correspond to CooRTweet results from `detect_group`
simulated_results <- simulated_data[[2]]
simulated_results <- simulated_results[simulated_results$coordinated == TRUE, ]
simulated_results$coordinated <- NULL

# Run CooRTweet using the input_data and the parameters used for simulation
results <- detect_groups(
  x = input_data,
  time_window = 10,
  min_participation = 2
)

# Sort data tables and check whether they are identical
data.table::setkeyv(simulated_results, names(simulated_results))
data.table::setkeyv(results, names(simulated_results))

identical(results, simulated_results)
}
}
