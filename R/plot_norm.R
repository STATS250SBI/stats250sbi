#' Plot a Normal Distribution with Shading
#'
#' Draw a normal distribution curve and optional shading of a particular region.
#'
#' @param mean The mean of the normal distribution to plot (mu)
#' @param sd The standard deviation of the normal distribution to plot (sigma)
#' @param shadeValues Either a single number or a vector of two numbers which
#' identify the boundary or boundaries of the shaded region
#' @param direction A character string (in quotes) indicating the direction from
#' shadeValues to shade under the normal curve. Must be one of "\code{less}",
#' "\code{greater}", "\code{beyond}", or "\code{between}". See Details below.
#' @param col.shade The color of the shaded region
#' @param ... Other graphical parameters passed to \link[base]{plot} (e.g.,
#' \code{xlim}, \code{main}, etc.). Generally, these will not control the
#' shading, only the normal distribution curve.
#'
#' @return A plot of a normal distribution curve with optional shading.
#' @export
#'
#' @details The \code{direction} argument is used to control the region under
#' the normal distribution curve which is shaded. If \code{shadeValues} is a
#' single number, \code{direction} must be either "\code{less}" (in which case
#' the shaded region will be to the *left* of \code{shadeValues}) or
#' "\code{greater}" (the shaded region will be to the *right* of
#' \code{shadeValues}).
#'
#' If `shadeValues` is a *vector*, then `direction` must be either "`between`"
#' (the region between the two numbers in `shadeValues` will be shaded) or
#' "`beyond`" (the region below the smaller and above the larger of the
#' `shadeValues` will be shaded).
#'
#' @examples
#' # Probability of a value being between 3 and 3.5 in a N(3.39, 0.55) distribution
#' plot_norm(mean = 3.39, sd = 0.55,
#'           shadeValues = c(3, 3.5),
#'           direction = "between",
#'           col.shade = "forestgreen")
#'
#' # Probability of a value being greater than 1.96 in a N(0, 1) distributio
#' plot_norm(shadeValues = 1.96,
#'           direction = "greater",
#'           col.shade = "peachpuff")
plot_norm <- function(mean = 0, sd = 1, shadeValues = NULL,
                      direction = c("less", "greater", "beyond", "between"),
                      col.shade = "cornflowerblue",
                      ...) {

  checks <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(shadeValues, null.ok = TRUE, max.len = 2, add = checks)
  checkmate::assert_number(mean, na.ok = FALSE, finite = TRUE, add = checks)
  checkmate::assert_number(sd, na.ok = FALSE, finite = TRUE, add = checks)
  checkmate::reportAssertions(checks)

  direction <- match.arg(direction)

  dots <- list(...)
  if (any(names(dots) == "xlim")) {
    xlim <- dots$xlim
    dots$xlim <- NULL
  } else xlim <- c(mean - 3*sd, mean + 3 * sd)

  if (any(names(dots) == "xlab")) {
    xlab <- dots$xlab
    dots$xlab <- NULL
  } else xlab <- ""

  if (any(names(dots) == "main")) {
    main <- dots$main
    dots$main <- NULL
  } else main <- paste0("N(", mean, ", ", sd, ") Distribution")

  xRange <- seq(mean - 3 * sd, mean + 3 * sd, length = 300)
  height <- dnorm(xRange, mean = mean, sd = sd)

  do.call(plot, c(list(height ~ xRange, type = "l", axes = F, ylab = "",
                       xlab = xlab, main = main, xlim = xlim, frame.plot = F, ...),
                  dots))
  axis(1, at = seq(mean - 3 * sd, mean + 3 * sd, sd),
       labels = prettyNum(seq(mean - 3 * sd, mean + 3 * sd, sd)),
       pos = 0)

  if (length(shadeValues) == 2) {
    shadeValues <- sort(shadeValues)
    if (!any(c("beyond", "between") == direction))
      stop(paste("When you provide two shadeValues for shading the plot,",
                 "you must also specify direction as 'between' or 'beyond'.",
                 "Fix this and try again."))
    else if (direction == "between") {
      xShade <- seq(shadeValues[1], shadeValues[2], length = 100)
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade, xShade[100]),
              c(0, shadeHeight, 0), col = col.shade)
    }
    else if (direction == "beyond") {
      xShade <- c(seq(min(xRange), shadeValues[1], length = 100),
                  seq(shadeValues[2], max(xRange), length = 100))
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade[1:100], shadeValues[1]),
              c(0, shadeHeight[1:100], 0), col = col.shade)
      polygon(c(shadeValues[2], xShade[101:200], xShade[200]),
              c(0, shadeHeight[101:200], 0), col = col.shade)
    }

    text(x = shadeValues[1], y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues[1], digits = 3)),
         pos = 1, font = 2, offset = 0.6, col = col.shade)
    text(x = shadeValues[2], y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues[2], digits = 3)),
         pos = 1, font = 2, offset = 0.6, col = col.shade)

  } else if (length(shadeValues == 1)) {
    if (!any(c("less", "greater") == direction))
      stop(paste("When you provide one shadeValue for shading the plot,",
                 "you must also specify direction as 'less' or 'greater'.",
                 "Fix this and try again."))
    if (direction == "less") {
      xShade <- seq(mean - 3 * sd, shadeValues, length = 100)
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    } else if (direction == "greater") {
      xShade <- seq(shadeValues, mean + 3 * sd, length = 100)
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    }
    text(x = shadeValues, y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues, digits = 3)), pos = 1,
         font = 2, offset = 0.6, col = col.shade)
  }

}
