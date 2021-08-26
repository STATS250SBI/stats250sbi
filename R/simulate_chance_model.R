#' Simulate from a Chance Model
#'
#' @param chanceSuccess A number between 0 and 1 representing the probability of
#' observing a "success" (however defined). Can be a decimal or a fraction;
#' e.g., 0.55 or 55/100.
#' @param numDraws The number of times to draw a poker chip from the bag needed
#' to complete one repetition of the simulation. Must be a positive, whole
#' number; if not, it will be rounded to the nearest whole number.
#' @param numRepetitions The number of times to repeat the simulation process.
#' Must be a positive, whole number; if not, it will be rounded to the nearest
#' whole number.
#'
#' @return A vector of simulated proportions of successes. The vector will be
#' the same length as `numRepetitions`.
#' @export
#'
#' @examples
#' # Run 100 repetitions of a simulation in which 26 fair coins are flipped,
#' # where "success" is heads. Store the results as `x`.
#' x <- simulate_chance_model(chanceSuccess = 0.5, numDraws = 26, numRepetitions = 100)
#'
#' # Make a histogram of the results
#' hist(x)
simulate_chance_model <- function(chanceSuccess, numDraws, numRepetitions) {

  checks <- checkmate::makeAssertCollection()
  checkmate::assert_number(chanceSuccess, lower = 0, upper = 1, add = checks)
  checkmate::assert_number(numDraws, lower = 1, add = checks)
  checkmate::assert_number(numRepetitions, lower = 1, add = checks)
  checkmate::reportAssertions(checks)

  numDraws <- round(numDraws)
  numRepetitions <- round(numRepetitions)

  replicate(numRepetitions, {
    draw <- sample(c("success", "failure"), size = numDraws,
                   replace = TRUE, prob = c(chanceSuccess, 1 - chanceSuccess))
    sum(draw == "success") / numDraws
  })
}
