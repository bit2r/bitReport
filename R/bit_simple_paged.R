#' Function for KANDLE simpled report template
#'
#' @param back_html logical. back-cover HTML including contact information
#' @param other_css Add extra css
#' @param number_sections Number section headings
#' @param theme Theme color
#' @param ... Arguments passed to pagedown::html_paged
#'
#' @return An R Markdown output format.
#'
#' @examples
#' \dontrun{
#' # Requires pandoc
#'
#' # Create an R Markdown example file
#' rmd_content <- "---
#'  title: 'Example bitReport HTML'
#'  subtitle: 'Paged Report'
#'  output: bitReport::bit_simpled
#'  ---
#'
#'  ## Introduction
#'
#'  This is an example of a bitReport-branded HTML paged report using `bitReport::bit_simpled`.
#'  "
#'
#' # Write the R Markdown content to a file
#' example_file <- tempfile(fileext = ".Rmd")
#' writeLines(rmd_content, example_file)
#'
#' # Render the R Markdown file
#' rmarkdown::render(example_file,
#'   output_format = bitReport::bit_simpled(),
#'   output_dir = tempdir()
#' )
#'
#' # View the rendered HTML file
#' browseURL(render_file)
#' }
#' @export
page_simpled <- function(back_html = TRUE,
                           other_css = NULL,
                           number_sections = FALSE,
                           theme = c("violet", "green", "blue", "orange"), 
                           ...) {
  theme <- match.arg(theme)
  
  # css file
  paged_simple_css <- pkg_resource(paste0("css/page-simpled-", theme, ".css"))
  custom_font <- pkg_resource("css/custom-fonts.css")
  
  # html back-cover
  if (back_html) {
    back_html <-
      pkg_resource("html/back_page_simpled.html")
  } else {
    back_html <- NULL
  }
  # template
  pagedown::html_paged(
    css = c(other_css, paged_simple_css, custom_font),
    includes = list(after_body = back_html),
    number_sections = number_sections,
    ...
  )
}

