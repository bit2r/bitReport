################################################################################
## modified https://github.com/UNHCR-WEB/unhcRstyle/blob/master/R/unhcr_ggplot_theme.R
################################################################################


#' @name theme_foundation
#' @rdname theme_foundation
#' @title  Base clean ggplot theme to build from
#' @export theme_foundation
#' @author ipda | unhcr.org | 2020
theme_foundation <- function(base_size=12, base_family="") {
  thm <- theme_grey(base_size = base_size, base_family = base_family)
  for (i in names(thm)) {
    if ("colour" %in% names(thm[[i]])) {
      thm[[i]]["colour"] <- list(NULL)
    }
    if ("fill" %in% names(thm[[i]])) {
      thm[[i]]["fill"] <- list(NULL)
    }
  }
  thm + theme(panel.border = element_rect(fill = NA),
              legend.background = element_rect(colour = NA),
              line = element_line(colour = "black"),
              rect = element_rect(fill = "white", colour = "black"),
              text = element_text(colour = "black"))
}


#' @name bit_theme
#' @rdname bit_theme
#' @title  Basic HLI theme for ggplot2
#' @export bit_theme
#' @author ipda | unhcr.org | 2020
#'
#' @description  \code{bit_theme} provides a basic \bold{HLI} theme
#' to use in \bold{ggplot2} commands.
#'
#' @param base_family Base font family (optional, character).
#' Default: \code{base_family = "Lato"}.
#' Options include \code{"mono"}, \code{"sans"} (default), and "serif".
#' @param base_size Base font size (optional, numeric).
#' Default: \code{base_size = 11}.
#' Recommendations use 11 for print and 16 for web
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot title family, face, size and margin
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justification, one of `[blmcrt]`
#' @param legend_title_family,legend_title_face,legend_title_size axis title font family, face and size
#' @param legend_title legend title if `TRUE` add legend title; default `FALSE`
#' @param plot_margin plot margin (specify with `ggplot2::margin()`)
#' @param grid_col,axis_col grid & axis colors
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis_text_size font size of axis text
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#'
#' @examples {
#'
#' if (!require(ggplot2)) {
#' install.packages('ggplot2')   # installs ggplot2
#' library('ggplot2')            # loads ggplot2
#'  }
#'  # Check if ggplot2 is loaded:
#'  if ("ggplot2" %in% (.packages())){
#'  message("Package 'ggplot2' is loaded.")
#'   } # if ("ggplot2" %in% (.packages())) end.
#' library(ggplot2)
#' library(tidyverse)
#' update_geom_font_defaults()
#'
#' # Bar chart
#' count(mpg, class) %>%
#'   ggplot(aes(class, n)) +
#'   geom_col() +
#'   geom_text(aes(label=n), nudge_y=3) +
#'   labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
#'        title="Bar chart example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by ...") +
#'   bit_theme(grid="Y") +
#'   theme(axis.text.y=element_blank())
#' }
#'
#' @family plot functions
#'
#' @import ggplot2

bit_theme <- function(base_family="NanumSquare",
                        base_size = 12,
                        plot_title_family=base_family,
                        plot_title_size = base_size*1.5,
                        plot_title_face = "bold",
                        plot_title_margin = base_size*0.75,
                        subtitle_family = base_family,
                        subtitle_size = base_size*1.25,
                        subtitle_face = "plain",
                        subtitle_margin = base_size,
                        strip_text_family = base_family,
                        strip_text_size = base_size,
                        strip_text_face = "plain",
                        caption_family = base_family,
                        caption_size = base_size*0.75,
                        caption_face = "plain",
                        caption_margin = base_size*0.75,
                        axis_text_size = base_size,
                        axis_title_family = subtitle_family,
                        axis_title_size = base_size*0.85,
                        axis_title_face = "plain",
                        axis_title_just = "rt",
                        legend_title_family = base_family,
                        legend_title_size = base_size*0.85,
                        legend_title_face = "plain",
                        legend_title = FALSE,
                        plot_margin = margin(15, 15, 15, 15),
                        grid_col = grey(.80, 1),
                        grid = TRUE,
                        axis_col = grey(.40, 1),
                        axis = FALSE,
                        ticks = FALSE) {

  ret <- theme_foundation(base_family = base_family, base_size = base_size)

  ## legend ----
  ret <- ret + theme(legend.background = element_blank())
  ret <- ret + theme(legend.key=element_blank())
  ret <- ret + theme(legend.position = "top")
  ret <- ret + theme(legend.justification = 'right')
  ret <- ret + theme(legend.direction = "horizontal")

  if (inherits(legend_title, "character") | legend_title == TRUE) {

    ret <- ret + theme(
      legend.title = element_text(
        size = legend_title_size, 
        family = legend_title_family,
        face = legend_title_face)
      )

  } else {
    ret <- ret + theme(legend.title = element_blank())
  }

  ## Line ----
  ret <- ret + theme(line = element_line(colour = grey(.80, 1)))
  ret <- ret + theme(rect = element_rect(fill = "transparent", linetype = 0, colour = NA))
  ret <- ret + theme(text = element_text(colour = grey(.15, 1)))

  ## grid ----
  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + theme(panel.grid = element_line(color = grid_col, size = 0.5))
    ret <- ret + theme(panel.grid.major = element_line(color = grid_col, size = 0.5))
    ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, size = 0.25))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x = element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y = element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y = element_blank())
    }
  } else {
    ret <- ret + theme(panel.grid = element_blank())
  }
  ret <- ret + theme(panel.spacing = grid::unit(2, "lines"))

  ## axis ----
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line = element_line(color = grey(.40, 1), size = 0.2))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = element_blank())
      } else {
        ret <- ret + theme(axis.line.x = element_line(color = axis_col, size = 0.2))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = element_blank())
      } else {
        ret <- ret + theme(axis.line.y = element_line(color = axis_col, size = 0.2))
      }
    } else {
      ret <- ret + theme(axis.line.x = element_line(color = axis_col, size = 0.2))
      ret <- ret + theme(axis.line.y = element_line(color = axis_col, size = 0.2))
    }
  } else {
    ret <- ret + theme(axis.line = element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)


  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size,
                                              margin=margin(t=0)))

  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size,
                                              margin=margin(r=0)))

  ret <- ret + theme(axis.title=element_text(size=axis_title_size,
                                             family=axis_title_family,
                                             color = grey(.30, 1)))

  ret <- ret + theme(axis.title.x=element_text(hjust=xj,
                                               size=axis_title_size,
                                               family=axis_title_family,
                                               face=axis_title_face))

  ret <- ret + theme(axis.title.y=element_text(hjust=yj,
                                               size=axis_title_size,
                                               family=axis_title_family,
                                               face=axis_title_face))

  ret <- ret + theme(axis.title.y.right=element_text(hjust=yj,
                                                     size=axis_title_size,
                                                     angle=90,
                                                     family=axis_title_family,
                                                     face=axis_title_face))

  ## text ----
  ret <- ret + theme(plot.title=element_text(hjust=0,
                                             size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family,
                                             face=plot_title_face))

  ret <- ret + theme(plot.subtitle=element_text(hjust=0,
                                                size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family,
                                                face=subtitle_face))

  ret <- ret + theme(plot.caption=element_text(hjust=0, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family,
                                               face=caption_face,
                                               color =  grey(.60, 1)))

  ret <- ret + theme(strip.text=element_text(hjust=0,
                                             size=strip_text_size,
                                             face=strip_text_face,
                                             family=strip_text_family))

  ## position ----
  ret <- ret + theme(plot.title.position = "plot")
  ret <- ret + theme(plot.caption.position = "plot")
  ret <- ret + theme(plot.margin=plot_margin)

  ret
}


#' @name update_geom_font_defaults
#' @rdname update_geom_font_defaults
#' @title  Update matching font defaults for text geoms
#' @export update_geom_font_defaults
#' @author ipda | unhcr.org | 2020
#' @description Updates [ggplot2::geom_label] and [ggplot2::geom_text] font defaults
#' @param family font family name
#' @param face font  face
#' @param size font size
#' @param color font color
update_geom_font_defaults <- function(family="NanumSquare", face="plain", size=3.5,
                                      color = grey(.15, 1)) {
  update_geom_defaults("text", list(family=family, face=face, size=size, color=color))
  update_geom_defaults("label", list(family=family, face=face, size=size, color=color))
}


#' @name bit_discrete
#' @rdname bit_discrete
#' @title  Ggplot scale color discrete and continuous palette based on the bit_pal_graphic
#' @export bit_discrete
#' @author ipda | unhcr.org | 2020
#' @examples {
#' library(scales)
#' ## to fix: the below...
#' #scales::show_col(bit_discrete(9))
#' }
bit_discrete <- function() {
  manual_pal(as.vector(t(bit_pal_graphic)))
}


#' @name bit_scale_colour_discrete
#' @rdname bit_scale_colour_discrete
#' @title  Discrete color scales based on the HLI graphic palette
#' @export bit_scale_colour_discrete
#' @author ipda | unhcr.org | 2020
#' @description See [bit_discrete()].
#' @aliases bit_scale_color_discrete
#' @inheritDotParams ggplot2::discrete_scale -expand -position
bit_scale_colour_discrete <- function(...) {
  discrete_scale(
    "colour",
    "bit_pal_graphic",
    bit_discrete(),
    ...
  )
}



#' @name bit_scale_fill_discrete
#' @rdname bit_scale_fill_discrete
#' @title  Discrete  fill scales based on the HLI graphic palette
#' @export bit_scale_fill_discrete
#' @author ipda | unhcr.org | 2020
#' @description See [bit_discrete()].
#' @aliases bit_scale_colour_discrete
#' @export bit_scale_fill_discrete
bit_scale_fill_discrete <- function(...) {
  discrete_scale(
    "fill",
    "bit_pal_graphic",
    bit_discrete(),
    ...
  )
}
