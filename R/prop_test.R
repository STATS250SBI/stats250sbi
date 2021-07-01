#' Test of Equal or Given Proportions
#'
#' \code{prop_test} is used to test the null hypothesis that the proportions
#' (probabilities of success) in several groups are the same, or that they equal
#'  certain values.
#'
#' @param x A number (or vector) of successes (observed counts).
#' @param n A number (or vector) of sample sizes or counts of trials.
#' @param p The null hypothesis value of the proportion (or difference in proportions)
#' @param alternative A character string (in quotes) specifying the alternative
#' hypothesis. Must be one of "\code{two.sided}" (the default), "\code{greater}", or
#' "\code{less}". To compute a confidence interval, set this as "\code{two.sided}".
#' @param conf.level The confidence level of the returned confidence interval.
#' Must be a single number between 0 and 1.
#' @param correct A logical (\code{TRUE} or \code{FALSE}) indicating whether to
#' apply Yates' continuity correction when possible (leave set to \code{FALSE}
#' for STATS 250).
#'
#' @return A list with class "\code{htest}".
#' @export
#'
#' @details Note that this is different from \code{\link{prop.test}}, which will
#' return confidence intervals created through a method we do not discuss in
#' STATS 250.
#' @examples
#' # Computing a confidence interval for one proportion
#' # We have 1054 successes out of 1553 "trials", and will compute a 95% confidence interval
#' prop_test(x = 1054, n = 1553, conf.level = 0.95)
#'
#' # Performing a hypothesis test for one proportion
#' # We have 1054 successes out of 1553 "trials". Do we have evidence to say the
#' # population proportion is greater than 55.3%?
#' prop_test(x = 1054, n = 1553, p = 0.553, alternative = "greater")
#'
#' # Computing a confidence interval for a difference in two proportions
#' # We have 22 successes out of 32 in Group A, and 97 successes out of 168 in
#' # Group B. Let's make a 99% confidence interval for the difference in these
#' # two proportions.
#' prop_test(x = c(22, 97), n = c(32, 168), conf.level = 0.99)
#'
#' # Performing a hypothesis test for a difference in two proportions
#' # We have 22 successes out of 32 in Group A, and 97 successes out of 168 in
#' # Group B. Do we have evidence to say that the proportion of successes in
#' # Group A is less than the proportion of successes in Group B?
#' prop_test(x = c(22, 97), n = c(32, 168), alternative = "less")
prop_test <-
  function(x, n, p = NULL, alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95, correct = FALSE) {
    alternative <- match.arg(alternative)
    k <- length(x)
    if (k > 2)
      stop("This function can only handle 1 or 2 groups. Check your input for x.")
    pt <- suppressWarnings(prop.test(x, n, p, alternative, conf.level, correct))
    sd <- sqrt(pt$estimate * (1 - pt$estimate) / n)

    pt$statistic <-
      c("Z" = sqrt(unname(pt$statistic)) *
          ifelse(k == 1, sign(pt$estimate - pt$null.value),
                 sign((pt$estimate[1] - pt$estimate[2]) -
                        ifelse(is.null(pt$null.value), 0, pt$null.value)
                 )))
    pt$parameter <- NULL

    YATES <- if (correct && (k <= 2))
      0.5
    else 0

    if (k == 1) {
      z <- qnorm(if (alternative == "two.sided")
        (1 + conf.level)/2
        else conf.level)
      YATES <- min(YATES, abs(x - n * p))
      p.c <- pt$estimate + YATES/n
      p.u <- if (p.c >= 1)
        1
      else (p.c  + z * sqrt(p.c * (1 - p.c)/n))
      p.c <- pt$estimate - YATES/n
      p.l <- if (p.c <= 0)
        0
      else (p.c - z * sqrt(p.c * (1 - p.c)/n))
      CINT <- switch(alternative,
                     two.sided = c(max(p.l, 0), min(p.u, 1)),
                     greater = c(max(p.l, 0), 1),
                     less = c(0, min(p.u, 1)))
      attr(CINT, "conf.level") <- conf.level
      pt$conf.int <- CINT
    }

    return(pt)
  }
