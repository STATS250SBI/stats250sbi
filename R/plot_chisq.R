#' Plot a Chi-square Distribution with Shading
#'
#' Draw a chi-square distribution curve with appropriate shading to the right
#' of the test statistic for chi-square hypothesis tests
#'
#' @param df The degrees of freedom for the chi-square distribution
#' @param shadeValues A single value that identifies the boundary of the shaded region
#' @param col.shade The color of the shaded region
#' @param ... Other graphical parameters passed to \link[base]{plot} (e.g.,
#' \code{xlim}, \code{main}, etc.). Generally, these will not control the
#' shading, only the normal distribution curve.
#'
#' @return A plot of a chi-square distribution curve with optional shading.
#' @export
#'
#' @details Since \code{shadeValues} is a
#' single number, the shaded region will be to the *right* of
#' \code{shadeValues} since we are interested only in chi-square hypothesis testing.
#'
#' @seealso \link{plot_norm}
#'
#' @examples
#'
#' # Shade the area for the chi-square test statistic of 4.1 with degrees of freedom 2.
#' plot_chisq(df = 2, shadeValues = 4.1)
#'
#' # Shade the area for the chi-square test statistic of 8.223 with degrees of freedom 10.
#' plot_chisq(df = 10, shadeValues = 8.233)
plot_chisq <- function(df, shadeValues = NULL,
                  # direction = c("less", "greater", "beyond", "between"),
                  # we only want "greater" in 250
                  col.shade = "cornflowerblue",
                  ...) {

  checks <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(shadeValues, null.ok = TRUE, max.len = 2, add = checks)
  checkmate::assert_number(df, na.ok = FALSE, finite = TRUE, add = checks)
  checkmate::reportAssertions(checks)

  direction <- "greater"

  if(length(shadeValues) != 1) {
    stop(paste("The chi-square hypothesis test requires that you have only one shadeValue. Fix this and try again"))
  }

  upperLimSV <- sign(max(shadeValues)) * ceiling(abs(max(shadeValues)))

  dots <- list(...)
  if (any(names(dots) == "xlim")) {
    xlim <- dots$xlim
    dots$xlim <- NULL
  }
  else xlim <- c(0, max(shadeValues + 1, qchisq(p = 0.025, df = df, lower.tail = FALSE) + 2))

  if (any(names(dots) == "xlab")) {
    xlab <- dots$xlab
    dots$xlab <- NULL
  }
  else xlab <- ""

  if (any(names(dots) == "main")) {
    main <- dots$main
    dots$main <- NULL
  }
  else main <- paste0("chi-square(", df, ") Distribution")

  xRange <- seq(xlim[1], xlim[2], length = 300)
  height <- dchisq(xRange, df = df)

  do.call(plot, c(list(height ~ xRange, type = "l", axes = F, ylab = "",
                       xlab = xlab, main = main, xlim = xlim, frame.plot = F),
                  dots))
  axis(1, at = seq(xlim[1], xlim[2], 1),
       labels = prettyNum(seq(xlim[1], xlim[2], 1)),
       pos = 0)

  xShade <- seq(shadeValues, xlim[2], length = 100)
  shadeHeight <- dchisq(xShade, df = df)
  polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
          col = col.shade)

  text(x = shadeValues, y = 0, xpd = TRUE,
       labels = prettyNum(formatC(shadeValues, digits = 3)), pos = 1,
       font = 2, offset = 0.6, col = col.shade)
}
