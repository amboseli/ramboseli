#' List of Amboseli color palettes
#'
#' Use \code{\link{amboseli_palette}} to select palettes of desired length.
#'
#' @export
amboseli_palettes <- list(
  div_earthsky = list(NULL, #1
                      NULL, #2
                      c('#4f6f8e','#ffffe0','#573314'), #3
                      c('#4f6f8e','#baceca','#ecaa6f','#573314'), #4
                      c('#4f6f8e','#96b8bb','#ffffe0','#d18452','#573314'), #5
                      c('#4f6f8e','#81abb2','#d6e2d4','#f6cc98','#bf6f41',
                        '#573314'), #6
                      c('#4f6f8e','#73a2ab','#baceca','#ffffe0','#ecaa6f',
                        '#b26037','#573314'), #7
                      c('#4f6f8e','#699ba6','#a6c1c2','#e2e9d8','#f8daad',
                        '#dd945e','#a95730','#573314'), #8
                      c('#4f6f8e','#6297a2','#96b8bb','#ccdbd1','#ffffe0',
                        '#f4be86','#d18452','#a2502b','#573314'), #9
                      c('#4f6f8e','#5c939e','#8bb0b6','#baceca','#e9eeda',
                        '#f9e3b9','#ecaa6f','#c77848','#9c4b27','#573314'), #10
                      c("#4f6f8e","#588f9c","#81abb2","#acc5c5","#d6e2d4",
                        "#ffffe0","#f6cc98","#e19b63","#bf6f41","#974724",
                        "#573314") #11
  ),
  seq_swelling = list(NULL, #1
                      c('#574532','#da8485'), #2
                      c('#574532','#9c6250','#da8485'), #3
                      c('#574532','#855844','#b16d5f','#da8485'), #4
                      c('#574532','#7b533f','#9c6250','#bc7268','#da8485'), #5
                      c('#574532','#73503c','#8e5c48','#a96859','#c2766d',
                        '#da8485'), #6
                      c('#574532','#6f4e3a','#855844','#9c6250','#b16d5f',
                        '#c77871','#da8485'), #7
                      c('#574532','#6b4d39','#7f5541','#925d4a','#a56656',
                        '#b87064','#c97a73','#da8485'), #8
                      c('#574532','#694c38','#7b533f','#8c5a47','#9c6250',
                        '#ac6a5b','#bc7268','#cc7b76','#da8485') #9
  ),
  seq_dryseason = list(NULL, #1
                      c('#faf0e6','#453024'), #2
                      c('#faf0e6','#a6876f','#453024'), #3
                      c('#faf0e6','#c4a892','#836955','#453024'), #4
                      c('#faf0e6','#d2baa5','#a6876f','#745947','#453024'), #5
                      c('#faf0e6','#dbc4b2','#b89b82','#907661','#6a513f',
                        '#453024'), #6
                      c('#faf0e6','#e0cbba','#c4a892','#a6876f','#836955',
                        '#644b3a','#453024'), #7
                      c('#faf0e6','#e4d0c0','#cdb29d','#b4957c','#957a66',
                        '#7b604d','#604737','#453024'), #8
                      c('#faf0e6','#e7d5c5','#d2baa5','#bda088','#a6876f',
                        '#8b705c','#745947','#5d4434','#453024') #9
  ),
  seq_wetseason = list(NULL, #1
                       c('#fffff0','#3c640d'), #2
                       c('#fffff0','#a1ae81','#3c640d'), #3
                       c('#fffff0','#bfc8a6','#83945d','#3c640d'), #4
                       c('#fffff0','#cdd6b8','#a1ae81','#74874a','#3c640d'), #5
                       c('#fffff0','#d6dfc4','#b3be97','#8f9d6b','#6b8040',
                         '#3c640d'), #6
                       c('#fffff0','#dce3cb','#bfc8a6','#a1ae81','#83945d',
                         '#647b38','#3c640d'), #7
                       c('#fffff0','#e0e8d0','#c6d0b0','#adb990','#94a271',
                         '#7a8c52','#5f7733','#3c640d'), #7
                       c('#fffff0','#e4ead4','#cdd6b8','#b7c29c','#a1ae81',
                         '#8a9965','#74874a','#5c752f','#3c640d') #9
  )
)


#' A color palette generator
#'
#' @param n Number of colors desired. If omitted, uses all colours.
#' @param name Name of desired palette. Choices are:
#'   \code{amboseli_div}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colors.
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' make_palette("amboseli_div")
#' make_palette("amboseli_div", 5)
#'
#' # If you need more colors than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colors
#' pal <- make_palette(name = "amboseli_div", n = 100, type = "continuous")
#' image(volcano, col = pal)
make_palette <- function(name, n, type = c("discrete", "continuous")) {

  type <- match.arg(type)
  pal <- amboseli_palettes[[name]]

  if (is.null(pal)) {
    stop("Palette not found.")
  }

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete") {
    if (n > length(pal)) {
      stop(paste("Maximum number of colors for this palette is", length(pal)))
    }
    else if (is.null(pal[[n]])) {
      stop(paste("Number of requested colors must be at least",
                 length(Filter(is.null, pal)) + 1))
    }
    else {
      pal <- pal[[n]]
    }
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal[[length(pal)]])(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}


#' Print a color palette
#'
#' @param pal A vector of character hex RGB values
#' @param border Color for the cell borders
#' @param ...
#'
#' @export
#'
#' @examples
#' black_and_white <- c("#000000", "#FFFFFF")
#' print.palette(black_and_white)
print.palette <- function(palette, border = "light gray", ...) {
  n <- length(palette)
  if (length(palette > 0)) {
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE, xlab = "", ylab = "", ...)
    rect(0:(n - 1)/n, 0, 1:n/n, 1, col = palette, border = border)
  }
}
