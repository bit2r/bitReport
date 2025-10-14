#' @name report_paged
#' @rdname report_paged
#' @title Create a paged HTML document suitable for printing with bitR Brand
#'
#' @param ... Arguments passed to \code{pagedown::\link[pagedown]{html_paged}}.
#' @references \url{https://pagedown.rbind.io}
#' @return An R Markdown output format.
#' @import pagedown
#' @export report_paged
report_paged <- function(front_cover = NULL, 
                         other_css = NULL,
                         theme = c("violet", "green", "blue", "orange"),
                         ...) {
  # default front-cover
  if (is.null(front_cover)) {
    front_cover <-
      pkg_resource("image/cover.jpg")
  }
  
  # theme
  theme <- match.arg(theme)
  
  # css file
  paged_report_css <- pkg_resource(paste0("css/paged-report-", theme, ".css"))
  
  # template
  pagedown::html_paged(
    css = c(paged_report_css, other_css),
    front_cover = front_cover,
    ...
  )
}


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

#' @rdname report_paged
#' @export bitR_violet_paged
bitR_violet_paged <- function(front_cover = NULL,
                              ...) {
  # default front-cover
  if (is.null(front_cover)) {
    front_cover <-
      pkg_resource("image/cover.jpg")
  }
  
  cssfile <- function(...) {
    system.file("resources", "css", paste0(..., ".css"), package = "bitReport")
  }
  
  svgfile <- function(...) {
    system.file("resources", "svg", paste0(..., ".svg"), package = "bitReport")
  }
  
  pagedown::html_paged(
    css = c(cssfile('custom-fonts'), cssfile('custom-page-violet'), cssfile('custom-violet')),
    front_cover = front_cover,
    ...)
}

#' @rdname report_paged
#' @export bitR_green_paged
bitR_green_paged <- function(front_cover = NULL,
                             ...) {
  # default front-cover
  if (is.null(front_cover)) {
    front_cover <-
      pkg_resource("image/cover.jpg")
  }
  
  cssfile <- function(...) {
    system.file("resources", "css", paste0(..., ".css"), package = "bitReport")
  }
  
  svgfile <- function(...) {
    system.file("resources", "svg", paste0(..., ".svg"), package = "bitReport")
  }
  
  pagedown::html_paged(
    css = c(cssfile('custom-fonts'), cssfile('custom-page-green'), cssfile('custom-green')), 
    front_cover = front_cover,
    ...)
}

