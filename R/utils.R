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
