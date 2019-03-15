#' LaCroix color scales
#'
#' Create color scales using LaCroix palettes.
#' Currently only works with the flavor-specific palettes (not the "paired" palette).
#'
#' @keywords color palettes scales
#' @param name Name of color palette (LaCroix flavor) desired.
#' @param discrete Generate a discrete palette? Defaults to \code{FALSE}, aka continuous mapping.
#' @param ... parameters passed on to \link[ggplot2]{\code{discrete_scale}} (discrete mapping)
#' or \link[ggplot2]{\code{scale_color_gradientn}} (continuous mapping).
#' @return A Scale object.
#'
#' @details
#' Implementation heavily inspired by the \link[viridis] package.
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#'
#' @examples
#' library(ggplot2)
#'
#' # ggplot2 examples, but with more seltzer
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point(size=4, aes(color = factor(cyl))) +
#'     scale_color_lacroix("PassionFruit", discrete = TRUE) +
#'     theme_bw()
#'
#' p + geom_point(size=4, aes(color = hp)) +
#'     scale_color_lacroix("PassionFruit", discrete = FALSE) +
#'     theme_bw()
#'
#' @export

scale_color_lacroix <- function (name, discrete = FALSE, ...) {
  if (discrete==TRUE) {
    discrete_scale(aesthetics = "colour",
                   scale_name = name,
                   palette = lacroix_pal_discrete(name),
                   ...)
  } else {
    scale_color_gradientn(colours = lacroix_palette(name = name, type = "continuous"), ...)
  }
}

#' @rdname scale_color_lacroix
#' @export

scale_colour_lacroix <- scale_color_lacroix

#' @rdname scale_color_lacroix
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
#' @export

scale_fill_lacroix <- function (name, discrete = FALSE, ...) {
  if (discrete==TRUE) {
    discrete_scale(aesthetics = "fill",
                   scale_name = name,
                   palette = lacroix_pal_discrete(name),
                   ...)
  } else {
    scale_fill_gradientn(colours = lacroix_palette(name = name, type = "continuous"), ...)
  }
}

#' @keywords internal

lacroix_pal_discrete <- function (name) {
  function (n) {
    lacroix_palette(n = n, name = name, type = "discrete")
  }
}
