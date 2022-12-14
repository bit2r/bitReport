% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit_color_def.R
\docType{data}
\name{bit_pal_orange}
\alias{bit_pal_orange}
\title{bitR orange color palette}
\format{
An object of class \code{data.frame} with 1 rows and 6 columns.
}
\usage{
bit_pal_orange
}
\description{
ORANGE:  PANTONE 300 C/U / CMYK 0 68 100 0
RGB 243 115 33 / HEX #F37321
\code{bit_pal_orange} 추가 bitR 색상 팔레트 제공
6가지 색상을 포함하는 데이터 프레임 (shades of \code{\link{Orange}}).
}
\examples{
{
bit_pal_orange
dim(bit_pal_orange)  # 1 6

# 인덱스를 이용한 선택:
bit_pal_orange[5]    # named color "orange5" (as df)
bit_pal_orange[[5]]  # color value "#F37321"

# 이름을 이용한 선택:
bit_pal["orange5"]    # color "orange5" (as df)
bit_pal[["orange5"]]  # color value "#F37321"

# 팔레트 시각화:
seecol(bit_pal_orange)
}
}
\seealso{
\code{\link{bit_pal}} 5가지 주요 색상이 있는 bitR 기본 색상 팔레트. \code{\link{bit_pal_orange}};
\code{\link{bit_pal_darkblue}} bitR 색상 팔레트 대체 색상의 경우;
\code{\link{bit_pal_primary}} 기본 색상이 있는 bitR 색상 팔레트의 경우;
\code{\link{seecol}} 팔래트 색상 보기;
\code{\link{usecol}} 팔레트 색상 사용하기.

Other color palettes: 
\code{\link{bit_pal_blue}},
\code{\link{bit_pal_darkblue}},
\code{\link{bit_pal_graphic}},
\code{\link{bit_pal_green}},
\code{\link{bit_pal_grey}},
\code{\link{bit_pal_primary}},
\code{\link{bit_pal_red}},
\code{\link{bit_pal_yellow}},
\code{\link{bit_pal}}
}
\concept{color palettes}
\keyword{datasets}
