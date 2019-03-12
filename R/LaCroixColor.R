#' A LaCroix color palette generator
#'
#' This function allows you to generate color palettes based on LaCroix water flavors.
#'
#' @keywords color palettes
#' @param n: Number of colors desired If omitted, uses all colours.
#' @param name: Name of color palette (LaCroix flavor) desired.
#' @param type: Either "discrete", "continuous", or "paired".
#' @export
#' @examples
#' lacroix_palette()
lacroix_palette <- function(name, n, type = c("discrete","continuous","paired")) {

  if (missing(type)) {
    type <- "continuous"
  }

  type <- match.arg(type)

  if(type == "paired") {

    pal <- lacroix_palettes[["paired"]]

    if (missing(n)) {
      n <- length(pal)
    }

    if (n > length(pal)) {
      stop("Number of requested colors greater than what palette can offer")
    }

  } else {

    pal <- lacroix_palettes[[name]]
    if (is.null(pal))
      stop("Flavor not found!")

    if (missing(n)) {
      n <- length(pal[1,])
    }

    if (type == "discrete" && n > length(pal[1,])) {
      stop("Number of requested colors greater than what palette can offer")
    }
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal[1,])(n),
                discrete = pal[1,][as.numeric(pal[2,1:n])],
                paired = pal[1:n]
  )

  structure(out, class = "palette", name = ifelse(type == "paired", "paired", name))
}
#' example: lacroix_palette("MurePepino")
#' example: lacroix_palette("Pamplemousse", n = 50, type = "continuous")
