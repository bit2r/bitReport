#' @export
sec2hms <- function(x) {
  x <- round(x) %>%
    lubridate::seconds_to_period()
  
  sprintf('%02d:%02d:%02d', x@hour, lubridate::minute(x),
          lubridate::second(x))
}

#' @export
hms2sec <- function(x) {
  sec <- substr(x, 7, 8) %>% as.integer()
  min <- substr(x, 4, 5) %>% as.integer() * 60
  
  min + sec
}


pkg_resource <- function(...) {
  system.file("resources", ..., package = "bitReport", mustWork = TRUE)
}


#' @export
str2css <- function(str_css) {
  args <- list()
  args$type <- "text/css"
  
  return(do.call(tags$style, c(list(HTML(str_css)), args)))
}


#' @export
pass_params <- function() {
  str_css <- NULL
  
  if (!is.null(params$title_color)) {
    str_css <- paste0(str_css, ".title {
 color: ", params$title_color, ";
}")}
  
  if (!is.null(params$subtitle_color)) {
    str_css <- paste0(str_css, "
   .subtitle {
 color: ", params$subtitle_color, ";
}")}  
  
  if (!is.null(params$bottom_brand)) {
    str_css <- paste0(str_css, "
   .pagedjs_page.pagedjs_left_page .pagedjs_margin-bottom-right>.pagedjs_margin-content::after {
 content: '", params$bottom_brand, " / ' var(--pagedjs-string-first-h2-date) !important;
}")
    
    str_css <- paste0(str_css, "
   .pagedjs_page.pagedjs_right_page .pagedjs_margin-bottom-left>.pagedjs_margin-content::after {
 content: '", params$bottom_brand, " / ' var(--pagedjs-string-first-h2-date) !important;
}")}
  
  if (!is.null(str_css)) {
    css_tag <- str2css(str_css)
    
    return(css_tag)
  }
}



