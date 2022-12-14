% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit_color_def.R
\docType{data}
\name{bit_pal_blue}
\alias{bit_pal_blue}
\title{bitR blue color palette}
\format{
An object of class \code{data.frame} with 1 rows and 6 columns.
}
\usage{
bit_pal_blue
}
\description{
BLUE:  PANTONE 300 C/U / CMYK 99 50 0 0
RGB 0 114 188 / HEX #0072BC
\code{bit_pal_blue} provides an additional bitR color palette
as a data frame containing 6 colors (shades of \code{\link{Blue}}).

See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
}
\examples{
{
bit_pal_blue
dim(bit_pal_blue)  # 1 6

# Preferred color:
bit_pal_blue[5]    # preferred (named) color "blue5" (as df)
bit_pal_blue[[5]]  # preferred color value "#0072BC"

# Access by position:
bit_pal_blue[5]    # named color "blue5" (as df)
bit_pal_blue[[5]]  # color value "#0072BC"

# Access by name:
bit_pal["blue5"]    # color "blue5" (as df)
bit_pal[["blue5"]]  # color value "#0072BC"

# Plotting palette:
seecol(bit_pal_blue)
}
}
\seealso{
\code{\link{bit_pal}} for the bitR default color palette with 5 main colors of \code{\link{bit_pal_blue}};
\code{\link{bit_pal_darkblue}} for an alternative blue bitR color palette;
\code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
\code{\link{seecol}} to show color palettes;
\code{\link{usecol}} to use color palettes.

Other color palettes: 
\code{\link{bit_pal_darkblue}},
\code{\link{bit_pal_graphic}},
\code{\link{bit_pal_green}},
\code{\link{bit_pal_grey}},
\code{\link{bit_pal_orange}},
\code{\link{bit_pal_primary}},
\code{\link{bit_pal_red}},
\code{\link{bit_pal_yellow}},
\code{\link{bit_pal}}
}
\concept{color palettes}
\keyword{datasets}
