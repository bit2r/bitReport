################################################################################
## modified https://github.com/UNHCR-WEB/unhcRstyle/blob/master/R/unhcr_color_util.R
################################################################################


#' @name rgb2hex
#' @rdname rgb2hex
#' @title  rgb2hex color conversion function in grDevices
#' @description Utility functions to access and plot color palettes
#' @export rgb2hex
#' @keywords internal
#' @author ipda | unhcr.org | 2020
#' @examples {
#' rgb2hex(255, 255, 255)
#' rgb2hex(0, 0, 0)
#' }
rgb2hex <- function(R, G, B) {
  rgb(R, G, B, maxColorValue = 255)
}


# col2rgb("black", alpha = FALSE)
# col2rgb("black", alpha = TRUE)
# col2rgb("black")
# col2rgb("white", alpha = FALSE)
# col2rgb("white", alpha = TRUE)
# col2rgb("#FFFFFF")


#' @name col2hex
#' @rdname col2hex
#' @title  col2hex color conversion function in grDevices
#' @description Utility functions to access and plot color palettes
#' @export col2hex
#' @author ipda | unhcr.org | 2020
#' @examples {
#' hex1 <- col2hex("black", alpha = 255/2)
#' hex2 <- col2hex("white", alpha = 255/2)
#' hex3 <- col2hex("gold", alpha = 255/2)
#' hex4 <- col2hex("steelblue", alpha = 255/2)
#' #seecol(pal = c(hex1, hex2, hex3, hex4), n = "all")
#' }
col2hex <- function(col, alpha = alpha) {
  rgb(t(col2rgb(col)), alpha = alpha, maxColorValue = 255)
}



#' @name isHexCol
#' @rdname isHexCol
#' @title  isHexCol color conversion function in grDevices
#' @description Helper function to detect HEX-colors
#' @export isHexCol
#' @keywords internal
#' @author ipda | unhcr.org | 2020
#' @examples {
#' isHexCol("black")
#' isHexCol(col2hex("black"))
#' isHexCol(rgb2hex(0, 0, 0))
#' }
isHexCol <- function(color) {
  return(grepl(pattern = "^#[0-9A-Fa-f]{6,}", color))
}


#' @name isCol
#' @rdname isCol
#' @title  isColl color conversion function in grDevices
#' @description Helper function to detect any color (in an individual character string)
#' @export isCol
#' @keywords internal
#' @author ipda | unhcr.org | 2020
#' @examples {
#' isCol("white")
#' isCol(col2hex("black", alpha = 255/2))
#' isCol(NA)
#' isCol("bumblebee")
#' # BUT note:
#' isCol(col2rgb("white"))  # => FALSE FALSE FALSE
#'  }
isCol <- function(color) {
  return(isHexCol(color) | color %in% colors())
}



#' @name parse_pal
#' @rdname parse_pal
#' @title  Parse a palette input: Color getting functions
#' @description Helper function
#' @export parse_pal
#' @keywords internal
#' @author ipda | unhcr.org | 2020
parse_pal <- function(pal) {

  # get the calling environment.
  parenv <- parent.frame()

  ## Check if pal is legible (already a color palette):
  vector_input <- tryCatch(
    {
      all(sapply(pal, isCol))
    },

    error = function(e) {
      # return FALSE if not all are colors.
      return(FALSE)
    },
    silent = TRUE
  )
  # if the input is a color vector (or list).
  if ( vector_input ) {
    out <- pal
  } else {
    # otherwise Deparse the argument:
    # if the calling environment is the global env
    if ( identical(parenv , globalenv()) ) {
      # get the palette
      tmp <- noquote(deparse(substitute(pal)))
      # if the calling environment is another function:
    } else {
      # get input from function.
      tmp <- noquote(deparse(substitute(expr = pal,
                                        env = parent.frame())))
      # unquote input.
      tmp <- noquote(tmp)
    }

    ## Split the input string; getting everything within the parentheses:
    if ( grepl("\\(", tmp) ) {  # only if any parenthesis exists.
      tmp <- sub(".*?\\(+(.*)\\).*", "\\1", tmp, perl=TRUE)
      # .\*?   matches anything but stops at the first match of what follows
      # \\s+   matches one or more blank spaces
      # (.\*)  matches any number of characters, because it is in parentheses
      # it becomes a capture group and is stored in the variable \1
      # \\s    waits for another blank, this time, the last one
      # .*     matches anything after the last blank
    }

    # Split get elements of the input at ',' and remove whitespace and quotes.
    elem <- gsub(" |\"", "", unlist(strsplit(tmp, split = ",")))

    ## Check, whether any element is wrapped in one or more functions:
    parens <- grepl("\\(", elem)   # are there any parentheses left?
    funs <- rep(NA, length(elem))  # initialize vector.
    funs[parens] <- gsub(" *\\(.*", "", elem[parens])  # get any functions.

    ## Now remove the functions:
    elem <- sub(".*?\\(+(.*)\\).*", "\\1", elem, perl = TRUE)

    # Existence checks : Now ask for every element, whether it exists:
    elemex <- sapply(elem, function(x) exists(x) & x != "pal")
    # also ask, whether the element is named pal, to prevent name conflicts!
    # Was: elemex <- sapply(elem, exists)
    if ( any(!elemex) ) {  # only if not all inputs have been resolved
      # Those which are still unknown: Are those colors?
      elemex[!elemex] <- sapply(elem[!elemex], isCol)
    }
    # Prefix those which do not exist with "pal_":
    if ( any(!elemex) ) {  # only if not all inputs have been resolved
      elem[!elemex] <- paste0("pal_", elem[!elemex])
      elemex[!elemex] <- sapply(elem[!elemex], exists)
    }
    # Handle undefined palettes:
    if (!all(elemex)) {
      nex <- gsub("pal_", "", elem[!elemex])  # remove any "pal_" string parts.
      if ( length(nex) > 1) {
        errmsg <- paste0("Inputs ", paste0("\"", nex, "\"", collapse = ", "), " do not exist")
      } else {
        errmsg <- paste0("Input \"", nex, "\" does not exist")
      }
      stop(errmsg)
    }
    # Get all palettes:
    out <- lapply(elem, function(x) if( isCol(x) ) x else get(x) )
    # Apply any previously detected functions:
    if ( any(!is.na(funs)) ) {
      out[!is.na(funs)] <- apply(rbind(out, funs), MARGIN = 2, FUN = function(x) {
        if(!is.na(x$funs)) eval(call(x$funs, x$out)) # apply function to all non-NA elements.
      })[!is.na(funs)]
    }
    # Create the output:
    out <- unname(out)  # finish the palette by removing upper level (palette) names.
  }
  out <- unlist(out)
  # Provide missing names, by using the color:
  ix_nameless <- is.null(names(out)) | names(out) == ""
  names(out)[ix_nameless] <- out[ix_nameless]
  # Return elements:
  return(out)
} # parse_pal end.


#' @name getpal_key
#' @rdname getpal_key
#' @title  Get a palette or list of palettes by keyword
#' @description Helper function
#' @export getpal_key
#' @keywords internal
#' @author ipda | unhcr.org | 2020
getpal_key <- function(pal = "all", n = "all", alpha = NA) {
  # Process the 'pal' argument: Getting palettes by keyword
  keys <- c("all", "bit_all", "all_bit",  # all palettes
            "graphic", "bit_graphic", "graphic_bit"   # the graphic palettes.
  )

  # Throw an error, if no valid keyword is specified:
  if ( !pal %in% keys ) {
    stop('Invalid keyword specified. Allowed keywords are
                            c("all", "bit_all", "all_bit")')
  } else {

    if ( pal %in% keys[1:3] )   key <- "all"
    if ( pal %in% keys [4:6] )  key <- "graphic"
  }
  # Get all color palettes with the prefix "pal_" from the environment
  # Distinguish 2 cases
  pal_names <- switch(
    key,
    all = bit_all_pal,
    graphic = bit_all_pal_graphic,
  )
  # Get list of palettes specified by keyword:
  lst_pal <- sapply(pal_names, get)
  # Indicator, whether these are actually color palettes:
  is_pal <- lapply(
    lst_pal,
    FUN = function(x) {
      if ( !typeof(x) %in% c("vector", "list") ) {
        is_color <- FALSE
      } else {
        is_color <- isHexCol(color = x)
      }
      return(all(is_color))  # are all entries colors?
    }
  )
  # Remove all non-colors:
  tmp <- lst_pal[unlist(is_pal)]
  # Check if palette is non-empty:
  if (length(tmp) == 0) {
    stop("No color palettes defined in the current environment.")
  }
  # If only color subsets should be displayed:
  if (n != "all" ) {
    # Get the subset of each palette , as defined in usecol():
    out <- lapply(tmp, FUN = usecol, n = n, alpha = alpha, use_names = TRUE)
  } else {
    if ( !is.na(alpha) ) {
      # adjust for alpha if specified.
      out <- lapply(tmp, FUN = adjustcolor, alpha.f = alpha)

    } else {
      out <- tmp  # if n n is specified return list as is.
    }
  }
  pal_nm <- names(out)  # get palette names from listnames.
  return(out)
}



#' @name plot_shape
#' @rdname plot_shape
#' @title  Plot a shape in a certain color
#' @description Helper function
#' @export plot_shape
#' @keywords internal
#' @author ipda | unhcr.org | 2020
plot_shape <- function(pos_x, pos_y,  # midpoint of shape
                       col_fill,      # fill color
                       col_brd = NA,
                       xlen = 1, ylen = 1,  # height of axis lengths
                       shape = "rect",      # shape
                       ...  # other graphics parameters (e.g., lwd): passed to symbols()
) {

  # Prepare inputs for vectorized solution
  len_max <- max(c(length(pos_y), length(pos_x)))  # get length of longer position vector.

  # Recycle all vectors to length of longest vector:
  pos_x <- rep(pos_x, length.out = len_max)
  pos_y <- rep(pos_y, length.out = len_max)
  xlen  <- rep(xlen,  length.out = len_max)
  ylen  <- rep(ylen,  length.out = len_max)

  # Rectangles
  if (shape == "rect") {
    symbols(x = pos_x, y = pos_y,
            rectangles = cbind(xlen, ylen),  # as matrix: width and height of rectangles
            add = TRUE,
            inches = FALSE,  # use unit on x axis
            fg = col_brd,    # line color
            bg = col_fill,   # filling
            ...              # other graphics parameters (e.g., lwd)
    )
  }

  # Circles
  if (shape == "circle") {
    symbols(x = pos_x, y = pos_y,
            circles = xlen/2,  # as vector (only using xlen): radii of circles
            add = TRUE,
            inches = FALSE,  # use unit on x axis
            fg = col_brd,    # line color
            bg = col_fill,   # filling
            ...              # graphics parameters (e.g., lwd)
    )
  }
}


#' @name plot_col
#' @rdname plot_col
#' @title  Plot a vector of colors (as circles or rectangles)
#' @description Helper function
#' @export plot_col
#' @keywords internal
#' @author ipda | unhcr.org | 2020
plot_col <- function(x,         # a *vector* of colors to be plotted.
                     ypos = 1,  # position on y axis.
                     shape = "rect",
                     xlen = 1, ylen = 1,
                     distance = 0,     # distance between shapes (to be subtracted from size).
                     plot.new = TRUE,  # TODO: Set to FALSE once done!
                     ...               # other graphics parameters (e.g., lwd)
) {

  # 1. Handle inputs
  # Key parameters:
  len_x <- length(x)  # length of vector x (i.e., nr. of colors to plot)
  # Should a new plot be created?
  if (plot.new) {
    if (distance > 0) {
      xlim <- c(0 - distance * len_x, len_x * (1 + distance))
    } else {
      xlim <- c(0, len_x)
    }
    plot(x = 0, type = "n", xlim = xlim, ylim = c(0, 2))  # create an empty plot.
  } else {
    # Check for graphic device:
    if (dev.cur() == 1) {
      stop("No graphic device to be plotted on.  Please open a plot or set plot.new to 'TRUE'.")
    }
  }

  # 2. Position parameters
  # Shape centers:
  xpos <- (1:len_x) - 0.5  # Note: Subtracting .5 assumes a shape width of 1.
  # +++ here now +++: Allow scaling shape widths to fill a FIXED total width
  #                   (e.g., each shape a width of 10/len_x).
  # Adjust xpos by distance:
  mid <- mean(xpos)  # get midpoint.
  add <- cumsum(rep(distance, sum(xpos < mid)))  # values to be added to the first half.
  sub <- add * (-1)                              # values to be subtracted from the second half.
  xpos <- xpos + if(len_x %% 2 == 0) c(rev(sub), add) else  # even numbers: no center position needed.
    c(rev(sub), 0, add)                                     # odd numbers: include the middle (0).
  # Recycle other constants (to len_x):
  ypos <- rep(ypos, length.out = len_x)
  xlen <- rep(xlen, length.out = len_x)
  ylen <- rep(ylen, length.out = len_x)

  # 3. Plot shapes
  plot_shape(pos_x = xpos,  # x positions of the shapes.
             pos_y = ypos,  # position in y dimension (given).
             xlen = xlen, ylen = ylen,  # length of the axes.
             col_fill = unlist(x),  # filling color.
             shape = shape,  # shape parameter.
             ...  # graphics parameters (e.g., lwd)
  )
}
