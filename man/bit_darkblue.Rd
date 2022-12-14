% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit_color_def.R
\docType{data}
\name{bit_darkblue}
\alias{bit_darkblue}
\title{Preferred color darkblue (as HEX character value)}
\format{
An object of class \code{character} of length 1.
}
\usage{
bit_darkblue
}
\description{
bitR color darkblue.

\code{darkblue} provides the preferred color of \code{\link{bit_pal_darkblue}}
(as an atomic HEX character value) and is defined as
\code{\link{bit_pal_darkblue}[[6]]}.

See \url{https://media.unhcr.org/archive/Data-visualisation-colour-palette-2CZ7A2HOVFP9.html} for details.
}
\examples{
{
bit_darkblue # HEX character "#18375F" (as value)
all.equal(bit_darkblue, bit_pal_darkblue[[6]])  # TRUE (same HEX values)
seecol(bit_darkblue)  # view color and details
}
}
\seealso{
\code{\link{bit_pal_darkblue}} for the corresponding color palette;
\code{\link{bit_pal}} for the bitR default color palette with all 5 colors of \code{\link{bit_pal_blue}};
\code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
\code{\link{seecol}} to show color palettes;
\code{\link{usecol}} to use color palettes.

Other preferred colors: 
\code{\link{bit_black}},
\code{\link{bit_blue}},
\code{\link{bit_green}},
\code{\link{bit_grey}},
\code{\link{bit_red}},
\code{\link{bit_white}},
\code{\link{bit_yellow}}
}
\concept{preferred colors}
\keyword{datasets}
