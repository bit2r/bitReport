#' @name bitR_orange_paged
#' @rdname bitR_orange_paged
#' @title Create a paged HTML document suitable for printing with bitR Brand
#'
#' @param ... Arguments passed to \code{pagedown::\link[pagedown]{html_paged}}.
#' @references \url{https://pagedown.rbind.io}
#' @return An R Markdown output format.
#' @import pagedown
#' @export bitR_orange_paged
#'
bitR_orange_paged <- function(...) {
  cssfile <- function(...) {
    system.file("resources", "css", paste0(..., ".css"), package = "bitReport")
  }
  
  svgfile <- function(...) {
    system.file("resources", "svg", paste0(..., ".svg"), package = "bitReport")
  }
  
  pagedown::html_paged(
    css = c(cssfile('custom-fonts'), cssfile('custom-page-orange'), cssfile('custom-orange')), ...)
}

#' @rdname bitR_orange_paged
#' @export bitR_blue_paged
bitR_blue_paged <- function(...) {
  cssfile <- function(...) {
    system.file("resources", "css", paste0(..., ".css"), package = "bitReport")
  }
  
  svgfile <- function(...) {
    system.file("resources", "svg", paste0(..., ".svg"), package = "bitReport")
  }
  
  pagedown::html_paged(
    css = c(cssfile('custom-fonts'), cssfile('custom-page-blue'), cssfile('custom-blue')), ...)
}