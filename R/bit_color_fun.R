################################################################################
## modified https://github.com/UNHCR-WEB/unhcRstyle/blob/master/R/unhcr_color_fun.R
################################################################################


#' @name usecol
#' @rdname usecol
#' @title Use a color palette (as is)
#'
#' @description  \code{usecol} allows using a color or color palette \code{pal}
#'  (e.g., for plotting).  \code{usecol} also allows modifying and combining
#'   color palettes in various ways.
#'
#' @param pal A color palette (as a vector of colors or color palettes).
#' Default: \code{pal = \link{bit_pal}}.
#'
#' @param n An integer value specifying the desired number of colors from the palette.
#' Default: \code{n = "all"} (i.e., use all colors of a color palette).
#' For the palettes defined by \strong{bit}, \code{n} is set to a pre-defined selection of
#' colors if the desired number of colors is smaller than the available number.
#' For all other palettes and values of \code{n} larger than \code{length(pal)},
#' \code{n} compresses or extends the palette using \code{\link{colorRampPalette}}.
#'
#' @param alpha A factor modifying the opacity alpha (as in \code{\link{adjustcolor}});
#' to a value in [0,1].
#' Default: \code{alpha = NA} (i.e., no modification of opacity).
#'
#' @param use_names A logical value indicating whether colors should be returned as a named vector.
#' Default: \code{use_names = FALSE}, for compatibility with \code{ggplot}.
#'
#' @param use_col_ramp A logical value specifying whether the default of using pre-selected colors
#' should be overridden and \code{\link{colorRampPalette}} should be used to process \code{n}.
#' Default: \code{use_col_ramp = FALSE}.
#'
#' @return A vector of colors (in character format).
#'
#' @examples {
#' usecol(pal = bit_pal, n = "all")  # default color palette
#' usecol(pal = bit_pal, n =  4)     # selecting n dedicated colors
#' usecol(pal = bit_pal, n = 20)     # extending color palette
#'
#' # Mixing a new color palette:
#' pal_1 <- usecol(pal = c(rev(pal_blue), "white", pal_red))
#' seecol(pal_1)
#'
#' # Mixing and extending a color palette:
#' pal_2 <- usecol(pal = c(rev(pal_green), "white", pal_red), n = 20)
#' seecol(pal_2)
#' }
#' @family color functions
#'
#' @seealso
#' \code{\link{seecol}} to plot color palettes;
#' \code{\link{bit_pal}} for the default bitR color palette.
#'
#' @export usecol

usecol <- function(pal = bit_pal,
                   n = "all",
                   alpha = NA,  # regulate transparency.
                   use_names = FALSE,  # should colors be returned as a named vector?
                   use_col_ramp = FALSE) {

  ## 1. Parse input
  parenv <- parent.frame()

  # parse_pal(pal = pal)


  pal_inp <- tryCatch(

    {
      suppressWarnings(parse_pal(pal = pal))  # suppress any warnings in parsing.

    },

    # Handle any errors by checking further:
    error = function(e) {

      # seecol always triggers this part
      pal <- deparse(substitute(expr = pal, env = parenv))

      ## Remove slashes and quotes:
      pal <- gsub("\\\\", "", pal)
      pal <- gsub("\"", "", pal)

      ## Reparse with new input:
      parse_pal(pal = pal)

    }
  )


  # Set n to length pal_inp, if n == "all"
  if (n == "all") { n <- length(pal_inp) }

  pal_def <- FALSE  # assume default, that an undefined palette is used.
  # Check this in the next step (this variable serves to control flow).

  if ( all(unlist(lapply(bit_all_pal, exists))) ) {  # test whether the palettes in bit_all_pal are defined.

    # Test whether equal to any palette:
    all_pals1 <- lapply(bit_all_pal, get)  # get all palettes from the first part.

  } else {  # if not all palettes are defined:

    all_pals1 <- NA

  }

  # Is the input one of the defined palettes?
  if ( !use_col_ramp ) {
    # execute, if not always the colorRamp should be used.

    pal_ix <-
      sapply(all_pals1, function(x) { return(isTRUE(all.equal(pal_inp, unlist(x)))) }
      )  # Test, whether specified palette is there.

    # If none fits, test for reversed palettes:
    rev_pal <- FALSE  # should the palette be reversed?

    if (!any(pal_ix)) {
      pal_ix <-
        sapply(all_pals1, function(x)
          isTRUE(all.equal(rev(pal_inp), x)))
      if (any(pal_ix)) {
        rev_pal <- TRUE
      }  # if palette is reversed, set pal_rev to TRUE.
    }

    # If input fits with any palette:
    if ( any(pal_ix) & length(pal_inp) >= n) {

      pal_name <- bit_all_pal[pal_ix]  # get palette name

      pal <- pal_inp  # redefine.

      # Define sets of known palettes:
      set1 <- pal_name %in% c("bit_pal_blue",
                              "bit_pal_grey",
                              "bit_pal_green",
                              "bit_pal_red",
                              "bit_pal_yellow")
      set2 <- pal_name %in%   "bit_pal_darkblue"
      set3 <- pal_name %in%   "bit_pal"
      set4 <- pal_name %in% c("bit_pal_primary",
                              "bit_pal_graphic")

      pal_set <- which(c(set1, set2, set3, set4))  # define a set number.

      # Determine the color output:
      out_col <- switch(pal_set,

                        # Get indices for pal_set:

                        # Set1
                        switch(n,
                               pal[5],
                               pal[c(5, 3)],
                               pal[c(5, 3, 2)],
                               pal[c(5, 4, 3, 2)],
                               pal[c(6, 5, 4, 3, 2)],
                               pal),

                        # Set2
                        switch(n,
                               pal[6],
                               pal[c(6, 4)],
                               pal[c(6, 4, 2)],
                               pal[c(6, 4, 3, 2)],
                               pal[c(6, 5, 4, 3, 2)],
                               pal),

                        # Set3
                        switch(
                          n,
                          pal[c("blue5")],
                          # 1 preferred color
                          pal[c("blue5", "blue3")],
                          # 2
                          pal[c("blue5", "blue3", "white")],
                          # 3
                          pal[c("blue5", "blue3", "white", "black")],
                          # 4
                          pal[c("blue5", "blue3", "white", "grey4", "black")],
                          # 5
                          pal[c("blue5", "blue3", "blue2", "white", "grey4", "black")],
                          # 6
                          pal[c("blue5", "blue3", "blue2", "white", "grey2", "grey4", "black")],
                          # 7
                          pal[c("blue5",
                                "blue4",
                                "blue3",
                                "blue2",
                                "white",
                                "grey2",
                                "grey4",
                                "black"
                          )],
                          # 8
                          pal[c("blue5",
                                "blue4",
                                "blue3",
                                "blue2",
                                "white",
                                "grey2",
                                "grey3",
                                "grey4",
                                "black"
                          )],
                          # 9
                          pal[c("blue5",
                                "blue4",
                                "blue3",
                                "blue2",
                                "blue1",
                                "white",
                                "grey2",
                                "grey3",
                                "grey4",
                                "black"
                          )],
                          # 10
                          pal  # all 11 colors of bit_pal
                        ),

                        # Set 4
                        pal[1:n]

      )

      if (rev_pal) {
        out_col <-
          rev(out_col)
      } # if palette was reversed, reverse result as well.

      pal_def <- TRUE  # set flag that palette is defined.

    }

  }


  # If no defined palette is used or the number exceeds
  # the number of colors use colorRamp
  if (!pal_def) {

    # Decide, whether to use colorRamp or not:
    if (n == length(pal_inp)) {

      out_col <- pal_inp

    } else {

      out_col <-
        colorRampPalette(pal_inp)(n)  # use the colorRamp (this swallows all names).

    }

  }


  # Name the palette (as comment attribute)
  comment(out_col) <- ifelse(pal_def, pal_name, "custom")

  # Search for names (if no names are given):
  if ( all(is.null(names(out_col))) ) {

    tst <- out_col

    # Names from defined kn palettes:
    kn_names <- names(unlist(all_pals1))[match(tst, unlist(all_pals1))]

    # Predefined color names:
    col_names <- colors()[match(
      rgb(t(col2rgb(tst)), maxColorValue = 255),
      c(rgb(t(col2rgb(colors())), maxColorValue = 255))
    )]

    kn_names[is.na(kn_names)] <- ""
    col_names[is.na(col_names)] <- ""

    # Processs name vectors (to avoid duplicates):
    col_names[col_names == kn_names] <- ""  # remove duplicates in col names.
    col_names[!col_names == "" & !kn_names == ""] <-
      paste0("/", col_names[!col_names == "" & !kn_names == ""])
    # adding a slash to distinguish different names for the same color.

    names(out_col) <- paste0(kn_names, col_names)

  }

  # Remove names if required (default)
  if ( !use_names ) { out_col <- unname(out_col) }

  if ( !(is.null(alpha) | is.na(alpha))) {

    cmnt <- comment(out_col)  # save palette name.
    out_col <- adjustcolor(out_col, alpha.f = alpha)
    comment(out_col) <- cmnt  # restore name.

  }

  return(out_col)

}



#' @name seecol
#' @rdname seecol
#' @title  Plot the colors of a palette or multiple palettes
#'
#' @description  \code{seecol} provides an interface to plotting (or "seeing")
#' the colors of a palette or comparing multiple color palettes.
#'
#' \code{seecol} has 2 main modes, based on the contents of its \code{pal} argument:
#'
#' \enumerate{
#'
#'   \item if \code{pal = "bit_all_pal"} or a list of multiple color palettes:
#'
#'   Plot visual vectors of all current color palettes for comparing them.
#'
#'   \item if \code{pal} is set to a specific color palette (or a vector of multiple colors or color palettes):
#'
#'   Plot the current color palette and optional details on its colors.
#'
#' }
#'
#' The \code{title} and \code{pal_names} arguments add control over plotted text labels.
#' However, the length of a character vector provided to \code{pal_names} must correspond
#' to the number of (custom) color palettes or colors.
#'
#' @param pal A single color palette (as a vector of colors),
#' multiple color palettes (as a list),
#' or a recognized keyword (as a character string).
#' Default: \code{pal = "bit_pal"}.
#'
#' Recognized keywords are:
#'
#' \enumerate{
#'
#'   \item \code{"bit_pal"}: All preferred colors and their gradients.
#'
#'   \item \code{"bit_pal_graphic"}: All preferred colors and their gradients.
#'
#'   \item \code{"bit_all_pal"}: All preferred colors and their gradients.
#'
#'   \item \code{"bit_pal_primary"}: All preferred colors and their gradients.
#'
#'   \item \code{"bit_pal_blue"}: All preferred colors and their gradients.
#'
#'   \item \code{"bit_pal_grey"}: All preferred colors and their gradients.
#'
#'   \item \code{"bit_pal_darkblue"}: All preferred colors and their gradients.
#'
#'   \item \code{"bit_pal_green"}: All preferred colors and their gradients.
#'
#'   \item \code{"bit_pal_red"}: All preferred colors and their gradients.
#'
#'   \item \code{ "bit_pal_yellow"}: All preferred colors and their gradients.
#'
#' }
#'
#' \code{seecol} does also recognize keywords (e.g., \code{"bit_pal"})
#'
#' @param n Number of colors to show or use.
#' If \code{n} is lower or higher than the length of the current
#' color palette \code{pal}, the color palette is reduced or extrapolated
#' (using \code{grDevices::colorRampPalette}).
#' Default: \code{n = "all"} (i.e., show all colors in palette).
#'
#' @param alpha A factor modifying the opacity alpha (as in \code{\link{adjustcolor}});
#' typically in [0,1]. If used, the value is shown in the plot title.
#' Default: \code{alpha = NA} (i.e., no modification of opacity).
#'
#' @param hex Should HEX color values be shown?
#' Default: \code{hex = NULL} (i.e., show HEX color values
#' when there is sufficient space to print them).
#'
#' @param rgb Should RGB color values be shown?
#' Default: \code{rgb = NULL} (i.e., show RGB color values
#' when there is sufficient space to print them).
#'
#' @param col_bg Color of plot background.
#' Default: \code{col_bg = NULL}.
#'
#' @param col_brd Color of shape borders (if shown).
#' Default: \code{col_brd = NULL}.
#'
#' @param lwd_brd Line width of shape borders (if shown).
#' Default: \code{lwd_brd = NULL}.
#'
#' @param grid Show grid in the color plot?
#' Default: \code{grid = TRUE}.
#'
#' @param title Plot title (as a character string).
#' Default: \code{title = NA} creates a default title.
#'
#' @param pal_names Names of color palettes or colors
#' (as a character vector).
#' Default: \code{pal_names = NA} (for default names).
#'
#' @param ... Other graphical parameters
#' (passed to \code{plot}).
#'
#' @seealso
#' \code{\link{usecol}} for using a color palette;
#' \code{\link{bit_pal_all}} for the default bit color palette
#'
#' @examples {
#' # See multiple color palettes:
#' seecol()  # default: seecol(pal = "all")
#'
#' # See details of one color palette:
#' seecol(bit_pal)  # see a specific color palette
#'
#' # Combining colors or color palettes:
#' seecol(c(rev(bit_pal_blue), bit_pal_green))        # combine color palettes
#' seecol(c("black", "firebrick", "gold"))          # combine color names
#'
#' # Using n to reduce or extend color palettes:
#' seecol(n =  3)  # viewing reduced ranges of all palettes
#' seecol(n = 12)  # viewing extended ranges of all palettes
#'
#' seecol(bit_pal, n = 5,
#'        title = "Reduced version of bit_pal (n = 5)")  # reducing bit_pal
#' seecol(bit_pal_green, n = 8,
#'        title = "Extended version of bit_pal_green (n = 8)")  # extending bit_pal_green
#'
#' # Combining and extending color palettes:
#' seecol(c(rev(bit_pal_blue), "white", bit_pal_red), n = 17,
#'        title = "Diverging custom color palette (with 17 colors)")
#'
#' # Defining custom color palettes:
#' pal_mpg <- c("#007367", "white", "#D0D3D4")  # mixing hex values and color names
#'
#' # Viewing extended color palette:
#' seecol(pal_mpg, n = 9, title = "Custom color palette")
#'
#'
#' ## Viewing color palettes from other packages:
#' library(RColorBrewer)
#' # viewing "RdBu" palette from RColorBrewer
#' seecol(brewer.pal(name = "RdBu", n = 11))
#'
#' ## Extending color palettes
#' # extending palette to 15 colors
#' seecol(brewer.pal(name = "RdBu", n = 11), n = 15)
#' }
#'
#'
#' @import graphics
#' @import grDevices
#' @import utils
#'
#' @export seecol

seecol <- function(pal = "bit_pal_all",  # which palette to output?
                   n = "all",
                   alpha = NA,
                   hex = NULL,      # determine by crowdedness, whether hex values should be shown in detail view.
                   rgb = NULL,      # determine, whether rgb values should be shown in detail view (defaults to TRUE)
                   col_bg = NULL,   # color of background
                   col_brd = NULL,  # border color of the boxes
                   lwd_brd = NULL,  # line width of box borders
                   grid = TRUE,     # show grid?
                   title = NA,      # plot title? Using default title = NA constructs a default title
                   pal_names = NA,  # names of color palettes or colors (as character vector)
                   ...              # additional arguments to plot.default().
) {

  ## 1. Preparations
  op <- par(no.readonly = TRUE)  # save original plotting settings.
  keys <- c("bit_pal", "bit_pal_graphic", "bit_all_pal", "bit_pal_primary",
            "bit_pal_blue", "bit_pal_grey", "bit_pal_darkblue", "bit_pal_green", "bit_pal_red", "bit_pal_yellow"
  )

  # Robustify inputs
  if ( !(is.null(hex) | is.logical(hex)) ) stop("Please specify a valid value for 'hex'.")
  if ( !(is.null(rgb) | is.logical(rgb)) ) stop("Please specify a valid value for 'rgb'.")

  # Check for keyword:
  by_key <- tryCatch(
    {
      all(pal %in% keys)
    },
    error = function(e) {
      FALSE
    },
    silent = TRUE
  )

  # Check whether pal input is a list:
  compare <- tryCatch(
    {
      is.list(pal) & any(lapply(pal, length) > 1)   # get length of each component.
    },
    error = function(e) {FALSE},
    silent = TRUE
  )

  # Getting a list of palettes by keyword:
  if (by_key) {

    # Plot title:
    # Define title given keyword:
    if (is.na(title)){
      if (pal %in% c("all", "bit_all_pal", "all_bit") ) title <- "See all bit color palettes"
      if (pal %in% c("graphic", "bit_graphic", "graphic_bit")) title <- "See all datagraphic bit color palettes"
    }

    pal_tmp <- getpal_key(pal = pal, n = n, alpha = alpha)  # get the color by key.

  } else if (compare){

    pal_tmp <- lapply(X = pal, usecol, n = n, alpha = alpha, use_names = TRUE)  # get all palettes separately.

    if (is.na(title)){
      title <- "Compare a custom set of color palettes"
    }

    # Set or get palette names:
    if ((!any(is.na(pal_names))) &                # pal_names were provided
        (length(pal_names) == length(pal_tmp))){  # and of appropriate length:

      names(pal_tmp) <- pal_names  # use custom pal_names.

    } else { # assign names from comment attribute:

      names(pal_tmp) <- lapply(pal_tmp, comment)

      if (is.null(names(pal_tmp))){ # no names exist:

        names(pal_tmp) <- paste0("pal_", 1:length(pal_tmp))  # use default names.

      } else if (any(names(pal_tmp) == "custom")) {

        ix_cp <- (names(pal_tmp) == "custom")

        if ((!any(is.na(pal_names))) &           # pal_names were provided
            (length(pal_names) == sum(ix_cp))){  # and of appropriate length:

          names(pal_tmp)[ix_cp] <- pal_names  # use custom pal_names.

        } else { # use default names:

          names(pal_tmp)[ix_cp] <- paste0("pal_", which(ix_cp))

        }
      }
    }

  } else { # if no keyword or list was provided: One palette

    # Get palette:
    pal_tmp <- usecol(pal = pal, n = n, alpha = alpha, use_names = TRUE)  # create a list of length 1.

    # Debugging:
    nm <- ifelse(length(unlist(pal_tmp)) == 1 | comment(pal_tmp) == "custom",
                 "", paste0(" ", comment(pal_tmp)))

    pl <- ifelse(length(unlist(pal_tmp)) == 1, names(pal_tmp), "palette")  # classify as palette or not.

    cst <- ifelse(comment(pal_tmp) == "custom" & length(unlist(pal_tmp)) != 1, "custom ", "")

    if (is.na(title)){
      title <- paste0("See ", cst, "color ", pl, nm)  # assemble title.
    }

    pal_tmp <- list(pal_tmp)  # list the palette (leave the comment attribute).

  }

  if ( n != "all" | !is.na(alpha) ) {
    n_txt <- ifelse(n != "all", paste0("n = ", n), "")
    alp_txt <- ifelse(!is.na(alpha), paste0("alpha = ", alpha), "")
    comma <- ifelse(nchar(n_txt) == 0 | nchar(alp_txt) == 0, "", ", ")

    if (is.na(title)){
      title <- paste0(title, " (", alp_txt, comma, n_txt, ")")
    }
  }

  # Check interplay of col_brd and lwd_brd:
  if (!is.null(lwd_brd) && (lwd_brd <= 0)){
    message("Setting (lwd_brd <= 0) is not allowed: Using lwd_brd = NULL.")
    lwd_brd <- NULL  # correct to default
  }

  if (!is.null(col_brd) && is.null(lwd_brd)){
    message("Setting col_brd requires lwd_brd: Using lwd_brd = 1.")
    lwd_brd <- 1   # correct to sensible value
  }

  if (!is.null(lwd_brd) && is.null(col_brd)){
    message("Setting lwd_brd requires col_brd: Using col_brd = 'white'.")
    col_brd <- "white"   # correct to sensible value
  }


  ## 2. Plotting parameters
  distance <- 0   # set distance of boxes?
  xlen <- 1       # set x length of color boxes.

  # Get maximum number of colors:
  max_ncol <- max(unlist(lapply(pal_tmp, FUN = length)))

  # Determine xlim based on distance, maximum number of colors, and box length:
  xlim <- c(0 - distance, (1 + distance) * xlen) * max_ncol * xlen

  # Determine ylim as number of colors in the palette:
  ylim <- c(0, length(pal_tmp) + 0.2)

  # Bind palette(s) to their color index:
  pal_mat <- cbind(pal_tmp, length(pal_tmp):1)  # ToDo: Note that a single palette needs to be a list of length 1!


  ## 3. Plotting
  # 1. multiple list entries --> compare palettes
  # 2. only one list entry --> details of a palette

  # 3.1 Plot a list of palettes
  if (length(pal_tmp) > 1) {

    # Set margins:
    par(mar = c(3, 6, 3, 1))

    # Set bg color:
    par(bg = col_bg)

    # Create empty plot:
    plot(x = 0, type = "n", xlim = xlim, ylim = ylim,
         xaxt = "n", yaxt = "n",  # hide axes.
         xlab = "", ylab = "",
         main = title,
         bty = "n",
         ...  # other graphical parameters
    )

    if (grid) {

      x_vals <- 0:max(ylim)

      dims <- max(ylim) * max_ncol
      grfac <- c(3, 4, 5)[c(dims < 100, dims > 100 & dims < 150, dims > 150)]
      # ensure an appropriate number of vertical lines using gridfactor.
      y_vals <- 1:max_ncol
      y_vals <- y_vals[(y_vals %% grfac) == 0]  # steps of 5
      y_vals <- y_vals - xlen/2

      abline(h = x_vals,
             v = y_vals,
             col = grey(.50, .50),
             lwd = .5)

    } # if (grid) etc.

    # Dynamic updating of ylen on number of palettes:
    ylen <- 0.8

    # Add the color vectors:
    # if (is.null(lwd_brd)) { lwd_brd <- 0 } # set default lwd_brd

    apply(pal_mat, MARGIN = 1, FUN = function(row) {
      plot_col(x = row[[1]], ypos = row[2], plot.new = FALSE,
               ylen = ylen, col_brd = col_brd, lwd = lwd_brd)
    })

    # Label pal names:
    cex_lbl <- .90

    text(x = 0, y = 1:length(pal_tmp),
         labels = rev(names(pal_tmp)),
         cex = cex_lbl, pos = 2, xpd = TRUE,
         offset = 1  # 1 character.
    )

    txt_ind <- paste0("[", 1:max_ncol, "]")
    cex_ind <- par("cex")
    wdth_ind <- sum(strwidth(txt_ind, cex = cex_ind))
    pos_ind <- seq(0.5, (max_ncol - 0.5), by = 1)

    while (wdth_ind > xlim[2]) {

      txt_ind <- txt_ind[seq(1, length(txt_ind), by = 2)]  # only show every 2nd index.
      pos_ind <- pos_ind[seq(1, length(pos_ind), by = 2)]
      wdth_ind <- sum(strwidth(txt_ind, cex = cex_ind))  # is the width small enough?

    } # while end.

    # Labels of color indices:
    cex_ixs <- .80
    yix <- -0.02 * length(pal_tmp)  # dynamic positioning of indices.

    text(x = pos_ind, y = yix, labels = txt_ind, pos = 1, xpd = TRUE,
         cex = cex_ixs, col = grey(0, 2/3))

  } else {  # if length(pal_tmp) list is NOT > 1:


    # 3.2 Detailed view of 1 palette
    names(pal_tmp) <- NULL  # remove first order names!

    pal_tmp <- unlist(pal_tmp)

    # Set margins:
    par(mar = c(3, 2, 3, 1))

    # Set bg color:
    par(bg = col_bg)

    # Create empty plot:
    plot(x = 0, type = "n", xlim = xlim, ylim = c(-1, 2),
         xaxt = "n", yaxt = "n",  # hide axes.
         xlab = "", ylab = "",
         main = title,
         bty = "n",
         ...  # other graphical parameters
    )

    # Text elements:
    txt_pos <- seq(0.5, length(pal_tmp) - 0.5)

    # y positions:
    y_names <- 1.5
    y_circ  <- 1.2
    y_rect  <- 0.6
    y_rgb   <- c(-0.50, -0.65, -0.80)

    # Grid:
    if (grid) {

      abline(# h = c(c(0.6, 1.2, 1.6, -0.1), c(y_rgb, -0.95) + 0.07),
        # h = c(-.88, -.43, -.10, y_rect, y_circ),
        h = c(-1, -.05, y_rect, y_circ),
        # v = txt_pos,
        col = grey(.5, .5),
        lwd = .3)

      xpos <- txt_pos
      if ( length(xpos) > 10 ) xpos <- xpos[xpos %% 5 == 0.5]  # steps of five.

      segments(
        x0 = xpos,
        y0 = 2, y1 = -0.2,
        col = grey(.5, .5),
        lwd = .3
      )

    } # if (grid) etc.

    # Find cex so that it is as large as possible:
    cex_lim <- 0.7  # lower limit for cex.

    # Determine whether to display hex values:
    cex_hex <- 0.9  # was par("cex")

    placeholder <- ifelse(is.na(alpha), " #XXXXXX", " #XXXXXXXX")

    wdth_hex <- strwidth(placeholder, cex = cex_hex) * max_ncol + strwidth("Hex: ")  # is the width small enough?

    while (wdth_hex > xlim[2]) {

      cex_hex <- cex_hex - 0.1
      wdth_hex <- strwidth(placeholder, cex = cex_hex) * max_ncol + strwidth("Hex: ")  # is the width small enough?

    }

    # If hex is NULL, determine based on width and max cex.
    # Otherwise use the provided value:
    if ( is.null(hex) ) {

      hex <- ifelse(wdth_hex > xlim[2] | cex_hex < cex_lim, FALSE, TRUE)  # test, whether hex can be displayed.

    }

    # Determine, whether to display rgb values:
    cex_rgb <- 0.9
    wdth_rgb <- strwidth(" 999 ", cex = cex_rgb) * max_ncol
    while (wdth_rgb > xlim[2]) {

      cex_rgb <- cex_rgb - 0.1
      wdth_rgb <- strwidth(" 999 ", cex = cex_rgb) * max_ncol  # is the width small enough?

    }

    # If rgb is NULL, determine based on width and max cex.
    # Otherwise use the provided value:
    if ( is.null(rgb) ) {
      rgb <- ifelse(wdth_rgb > xlim[2] | cex_rgb < cex_lim, FALSE, TRUE)
    }

    # Plot rectangles:
    # if (is.null(lwd_brd)) { lwd_brd <- 1 } # set default lwd_brd

    plot_col(x = pal_tmp, ypos = y_rect, shape = "rect", ylen = 0.5, plot.new = FALSE, col_brd = col_brd, lwd = lwd_brd#,
             # ...  # other graphical parameters
    )

    # Plot circles:
    circle_len <- ifelse(((xlim[2] / 10) < 0.7), (xlim[2] / 10), .70)

    plot_col(x = pal_tmp, ypos = y_circ, shape = "circle", xlen = circle_len, plot.new = FALSE, col_brd = col_brd, lwd = lwd_brd#,
             # ...  # other graphical parameters
    )

    # Color names:
    if ((!any(is.na(pal_names))) &                # pal_names were provided
        (length(pal_names) == length(pal_tmp))){  # and of appropriate length:

      col_names <- pal_names  # use custom pal_names.

    } else { # default color names:

      col_names <- names(pal_tmp)

    }

    y_names <- y_circ + (circle_len * 4 / max_ncol)  # determine y_names based on circle position and size.

    text(x = txt_pos, y = y_names, labels = col_names, # pos = 3,
         srt = 45, xpd = TRUE, offset = 1, cex = 0.8,
         adj = c(0, 0))

    # Spacing of indices:
    txt_ind <- paste0("[", 1:length(pal_tmp), "]")
    cex_ind <- par("cex")
    wdth_ind <- sum(strwidth(txt_ind, cex = cex_ind))
    pos_ind <- txt_pos

    while (wdth_ind > xlim[2]) {

      txt_ind <- txt_ind[seq(1, length(txt_ind), by = 2)]  # only show every second index.
      pos_ind <- pos_ind[seq(1, length(pos_ind), by = 2)]
      wdth_ind <- sum(strwidth(txt_ind, cex = cex_ind))  # is the width small enough?

    }

    # Color indices:
    cex_ixs <- .80
    text(x = pos_ind, y = 0, labels = txt_ind, pos = 3, xpd = TRUE,
         cex = cex_ixs, col = grey(0, 2/3))

    # Hex values:
    if (hex) {

      ## Convert to hex if not already given in this format:
      if (!all(isHexCol(pal_tmp))) {
        pal_tmp <- rgb(t(col2rgb(pal_tmp)), maxColorValue = 255)
      }

      yhex <- -0.25

      # Plot the values:
      text(x = 0, y = yhex, labels = "Hex:", font = 2, pos = 2, offset = 0, xpd = TRUE,
           cex = cex_hex)

      text(x = txt_pos, y = yhex, labels = pal_tmp, pos = NULL, xpd = TRUE,
           cex = cex_hex, srt = 0)

    } # if (hex) etc.

    # RGB values:
    if (rgb) {

      text(x = rep(0, 3),
           y = y_rgb,
           labels = c("R:", "G:", "B:"), font = 2,
           pos = 2, offset = 0, xpd = TRUE,
           cex = cex_rgb)

      text(x = matrix(rep(txt_pos, 3), nrow = 3, byrow = TRUE),
           y = matrix(rep(y_rgb, length(txt_pos) + 1), nrow = 3),
           labels = col2rgb(pal_tmp),
           pos = 2, offset = -.67, xpd = TRUE,
           cex = cex_rgb)

    } # if (rgb) etc.

  }  # if (length(pal_tmp) > 1) etc.

  # Reset plotting parameters:
  par(op)
  # Invisibly return pal_tmp palette(s):
  invisible(pal_tmp)
}



#' @name newpal
#' @rdname newpal
#' @title  Define new color palettes.
#'
#' @description \code{newpal} allows defining new color palettes
#' (as data frames).
#'
#' @param col A required vector of colors
#' (specified by their R color names, HEX codes, or RGB values).
#'
#' @param names An optional character vector of names.
#' Default: \code{names = NA}, yielding numeric names.
#'
#' @param as_df Should the new color palette be returned as
#' a data frame (rather than as a vector)?
#' Default: \code{as_df = FALSE}.
#'
#' @examples {
#'
#' newpal(col = c("black", "white"), names = c("dark", "bright"))
#'
#' # Example: 3 ways of defining a new color palette:
#'
#' # (1) From R color names
#' pal_flag_de <- newpal(col = c("black", "firebrick3", "gold"),
#'                       names = c("Schwarz", "Rot", "Gold"))
#'
#' seecol(pal_flag_de, title = "Colors in the flag of Germany")
#'
#' # (2) From HEX values
#' # (a) Google logo colors:
#' # Source: https://www.schemecolor.com/google-logo-colors.php
#' color_google <- c("#4285f4", "#34a853", "#fbbc05", "#ea4335")
#' names_google <- c("blueberry", "sea green", "selective yellow", "cinnabar")
#' pal_google   <- newpal(color_google, names_google)
#' seecol(pal_google, title = "Colors of the Google logo", col_brd = "white", lwd_brd = 10)
#'
#' # (b) German flag revised
#' #' # Based on a different source at
#' # <https://www.schemecolor.com/germany-flag-colors.php>:
#' pal_flag_de_2 <- newpal(col = c("#000000", "#dd0000", "#ffce00"),
#'                         names = c("black", "red", "gold")
#'                         )
#' seecol(pal_flag_de_2, title = "Colors of the German flag (www.schemecolor.com)")
#'
#' # (c) MPG colors:
#' pal_mpg <- newpal(col = c("#007367", "white", "#D0D3D4"),
#'                   names = c("mpg green", "white", "mpg grey")
#'                   )
#' seecol(pal_mpg, title = "Colors of the Max Planck Society")
#'
#' # (3) From RGB values
#' # Barrier-free color palette
#' # Source: Okabe & Ito (2002): Color Universal Design (CUD):
#' #         Fig. 16 of <https://jfly.uni-koeln.de/color/>:
#'
#' # (a) Vector of colors (as RGB values):
#' o_i_colors <- c(rgb(  0,   0,   0, maxColorValue = 255),  # black
#'                 rgb(230, 159,   0, maxColorValue = 255),  # orange
#'                 rgb( 86, 180, 233, maxColorValue = 255),  # skyblue
#'                 rgb(  0, 158, 115, maxColorValue = 255),  # green
#'                 rgb(240, 228,  66, maxColorValue = 255),  # yellow
#'                 rgb(  0, 114, 178, maxColorValue = 255),  # blue
#'                 rgb(213,  94,   0, maxColorValue = 255),  # vermillion
#'                 rgb(204, 121, 167, maxColorValue = 255)   # purple
#' )
#'
#' # (b) Vector of color names:
#' o_i_names <- c("black", "orange", "skyblue", "green", "yellow", "blue", "vermillion", "purple")
#'
#' # (c) Use newpal() to combine colors and names:
#' pal_okabe_ito <- newpal(col = o_i_colors,
#'                         names = o_i_names)
#'
#' seecol(pal_okabe_ito,
#'        title = "Color-blind friendly color scale (Okabe & Ito, 2002)")
#'
#' # Compare custom color palettes:
#' my_pals <- list(pal_flag_de, pal_flag_de_2, pal_google, pal_mpg, pal_okabe_ito)
#' seecol(my_pals, col_brd = "white", lwd_brd = 5,
#'        title = "Comparing custom color palettes")
#' }
#'
#' @import graphics
#' @import grDevices
#' @import utils
#'
#' @export newpal

newpal <- function(col,            # a vector of colors
                   names = NA,     # a vector of names
                   as_df = FALSE   # return palette as df?
                   # ...           # additional arguments to usecol().
) {

  ## 0. Preparations
  outpal <- NA  # initialize
  # 0. Robustify inputs:
  if ( any(is.na(col)) ) stop("'col' must be a vector of (named or hex) colors without NA values.")

  if ( any(!isCol(col)) ) stop("'col' must be a vector containing ONLY (named or hex) colors.")

  if ( any(!is.na(names)) && ((length(col) != length(names))) ) {

    message(paste0("Length of 'col' = ", length(col),
                   " vs. 'names' = ",    length(names), ". Using default (numeric) names..."))
    names <- NA
  }

  # 1. Create data.frame or vector of col
  outpal <- col  # copy col vector
  # 2. Add names:
  if ( all(!is.na(names)) ) {
    names(outpal) <- names
  } else {
    names(outpal) <- as.character(1:length(col))
  }
  # # Apply ... arguments:
  # outpal <- usecol(pal = outpal, use_names = TRUE, ...)
  # If return as_df:
  if (as_df) {
    outpal <- data.frame(outpal, stringsAsFactors = FALSE) # df as column
    outpal <- t(outpal) # df as row
    outpal <- data.frame(outpal, row.names = NULL, stringsAsFactors = FALSE)
  }
  # 2. Return
  return(outpal)

}
