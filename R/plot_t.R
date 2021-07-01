if (!("checkmate" %in% installed.packages()))
  install.packages('checkmate')

plot_t <- function(df, shadeValues = NULL,
                   direction = c("less", "greater", "beyond", "between"),
                   col.shade = "cornflowerblue",
                   ...) {

  checks <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(shadeValues, null.ok = TRUE, max.len = 2, add = checks)
  checkmate::assert_number(df, na.ok = FALSE, finite = TRUE, add = checks)
  checkmate::reportAssertions(checks)

  direction <- match.arg(direction)

  lowerLimSV <- sign(min(shadeValues)) * ceiling(abs(min(shadeValues)))
  upperLimSV <- sign(max(shadeValues)) * ceiling(abs(max(shadeValues)))

  dots <- list(...)
  if (any(names(dots) == "xlim")) {
    xlim <- dots$xlim
    dots$xlim <- NULL
  } else xlim <- c(min(c(-3, lowerLimSV)), max(c(3, upperLimSV)))

  if (any(names(dots) == "xlab")) {
    xlab <- dots$xlab
    dots$xlab <- NULL
  } else xlab <- ""

  if (any(names(dots) == "main")) {
    main <- dots$main
    dots$main <- NULL
  } else main <- paste0("t(", df, ") Distribution")

  xRange <- seq(xlim[1], xlim[2], length = 300)
  height <- dt(xRange, df = df)

  do.call(plot, c(list(height ~ xRange, type = "l", axes = F, ylab = "",
                       xlab = xlab, main = main, xlim = xlim, frame.plot = F),
                  dots))
  axis(1, at = seq(xlim[1], xlim[2], 1),
       labels = prettyNum(seq(xlim[1], xlim[2], 1)),
       pos = 0)

  if (length(shadeValues) == 2) {
    shadeValues <- sort(shadeValues)
    if (!any(c("beyond", "between") == direction))
      stop(paste("When you provide two shadeValues for shading the plot,",
                 "you must also specify direction as 'between' or 'beyond'.",
                 "Fix this and try again."))
    else if (direction == "between") {
      xShade <- seq(shadeValues[1], shadeValues[2], length = 100)
      shadeHeight <- dt(xShade, df = df)
      polygon(c(xShade[1], xShade, xShade[100]),
              c(0, shadeHeight, 0), col = col.shade)
    }
    else if (direction == "beyond") {
      xShade <- c(seq(min(xRange), shadeValues[1], length = 100),
                  seq(shadeValues[2], max(xRange), length = 100))
      shadeHeight <- dt(xShade, df = df)
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
      xShade <- seq(xlim[1], shadeValues, length = 100)
      shadeHeight <- dt(xShade, df = df)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    } else if (direction == "greater") {
      xShade <- seq(shadeValues, xlim[2], length = 100)
      shadeHeight <- dt(xShade, df = df)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    }
    text(x = shadeValues, y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues, digits = 3)), pos = 1,
         font = 2, offset = 0.6, col = col.shade)
  }

}
