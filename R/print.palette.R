#' Function to print color palettes
#' @param n: Number of colors desired If omitted, uses all colours.
#' @param name: Name of color palette (LaCroix flavor) desired.
#' @export
print.palette <- function(x, ...) {
   
  fpath <- system.file(package="LaCroixColoR", "data", "LassigueDMato.ttf")
  sysfonts::font_add("DMato", regular=fpath)
  #fpath2 <- system.file(package="LaCroixColoR", "data")
  #extrafont::font_import(paths=fpath2, prompt=FALSE)
  #extrafont::loadfonts(device = "pdf", quiet = TRUE)
     
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  x11()
  ## Automatically use showtext to render text
  showtext::showtext_auto()
   
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  #rect(0, 0.9, n + 1, 1.1, border = NA) #col = rgb(1, 1, 1, 1)
  #showtext::showtext_begin()
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 8, family = "DMato")
  #showtext::showtext_end()
}
