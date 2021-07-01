#' Randomization for Two-Group Comparisons
#'
#' Shuffles data for use in two-group simulations.
#'
#' @param d A data frame that contains the
#' @param groups The name of the variable (in quotes!) that identifies which
#' group each observation belongs to
#'
#' @return The original data frame with the group labels randomly shuffled
#' among observations
#'
#' @importFrom checkmate assert_data_frame
#'
#' @export
#'
#' @examples
#' # Make a data frame to shuffle
#' og_data <- data.frame(
#'   "group" = c(rep("Group A", 600), rep("Group B", 200)),
#'   "result" = c(rep("Failure", 402), rep("Success", 198),
#'                rep("Failure", 120), rep("Success", 80)),
#'   stringsAsFactors = TRUE)
#'
#' # Tabulate successes & failures in each group in original data
#' table(og_data$result, og_data$group)
#'
#' # Shuffle data with shuffle_two_groups
#' shuffle_data <- shuffle_two_groups(og_data, "group")
#'
#' # Tabulate successes & failures in each group in shuffled data
#' table(shuffle_data$result, shuffle_data$group)
#'
shuffle_two_groups <- function(d, groups) {

  checks <- makeAssertCollection()
  assert_data_frame(d, add = checks)
  assert_character(groups, add = checks)
  assert_subset(groups, names(d), add = checks)
  reportAssertions(checks)

  d[[groups]] <- d[[groups]][sample(1:nrow(d), size = nrow(d), replace = F)]

  return(d)
}
