################################################################################
## modified https://github.com/UNHCR-WEB/unhcRstyle/blob/master/R/unhcr_color_def.R
################################################################################


#  bitR Brandbook ----
## Color sources

# Sources for color definitions:
# https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html
# https://media.unhcr.org/archive/Data-visualisation-colour-palette-2CZ7A2HOVFP9.html

# Primary colors: The bitR logo blue should be used as a main colour as well as an accent colour in necessary circumstances.

# bit_pal -----

#' @name bit_pal
#' @rdname bit_pal
#' @title bit default (web/sRGB) Primary color palette
#'
#' @description bitR primary color palette. \code{bit_pal}
#' provides the default bitR color palette
#' as a data frame containing 11 colors.
#'
#' This is the primary (web/sRGB) scale.
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_pal
#' dim(bit_pal)  # 1 11
#'
#' # Access by position:
#' bit_pal[1]    # 1nd named color "blue5" (as df)
#' bit_pal[[1]]  # 2nd color value "#0072BC"
#'
#' # Access by name:
#' bit_pal["blue5"]    # color "blue5" (as df)
#' bit_pal[["blue5"]]  # color value "#0072BC"
#'
#' # Plotting palette:
#' seecol(bit_pal)
#' }
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal_blue}} for the bitR default blue palette;
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export
#'
bit_pal <- data.frame(                                 #  element:
  "blue1" = rgb(204, 227, 242, maxColorValue = 255),  #  1. blue1 (non-transparent):  20%
  "blue2" = rgb(153, 199, 228, maxColorValue = 255),  #  2. blue2 (non-transparent):  40%
  "blue3" = rgb(102, 170, 215, maxColorValue = 255),  #  3. blue3 (non-transparent):  60%
  "blue4" = rgb( 51, 142, 201, maxColorValue = 255),  #  4. blue4 (non-transparent):  80%
  "blue5" = rgb(  0, 114, 188, maxColorValue = 255),  #  5. blue5 (non-transparent): 100%: preferred color: "blue"
  "black" = rgb(  0,   0,   0, maxColorValue = 255),  #  6. black: preferred color: "black"
  "grey5" = rgb( 51,  51,  51, maxColorValue = 255),  #  7. grey5 (non-transparent):  80% black
  "grey4" = rgb(102, 102, 102, maxColorValue = 255),  #  8. grey4 (non-transparent):  60% black
  "grey3" = rgb(153, 153, 153, maxColorValue = 255),  #  9. grey3 (non-transparent):  40% black
  "grey2" = rgb(204, 204, 204, maxColorValue = 255),  # 10. grey2 (non-transparent):  20% black
  "white" = rgb(255, 255, 255, maxColorValue = 255),  # 11. white: preferred color: "white"
  stringsAsFactors = FALSE)

# Sensibly sorted version (position):
bit_pal <- bit_pal[c(5:1, 11:6)] # blue (1 as default) > white (6) > grey > black (11)



# bit_pal_orange -----
#' @name bit_pal_orange
#' @rdname bit_pal_orange
#' @title bitR orange color palette
#'
#' @description
#' ORANGE:  PANTONE 300 C/U / CMYK 0 68 100 0
#' RGB 243 115 33 / HEX #F37321
#' \code{bit_pal_orange} 추가 bitR 색상 팔레트 제공
#' 6가지 색상을 포함하는 데이터 프레임 (shades of \code{\link{Orange}}).
#'
#' @examples {
#' bit_pal_orange
#' dim(bit_pal_orange)  # 1 6
#'
#' # 인덱스를 이용한 선택:
#' bit_pal_orange[5]    # named color "orange5" (as df)
#' bit_pal_orange[[5]]  # color value "#F37321"
#'
#' # 이름을 이용한 선택:
#' bit_pal["orange5"]    # color "orange5" (as df)
#' bit_pal[["orange5"]]  # color value "#F37321"
#'
#' # 팔레트 시각화:
#' seecol(bit_pal_orange)
#' }
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal}} 5가지 주요 색상이 있는 bitR 기본 색상 팔레트. \code{\link{bit_pal_orange}};
#' \code{\link{bit_pal_darkblue}} bitR 색상 팔레트 대체 색상의 경우;
#' \code{\link{bit_pal_primary}} 기본 색상이 있는 bitR 색상 팔레트의 경우;
#' \code{\link{seecol}} 팔래트 색상 보기;
#' \code{\link{usecol}} 팔레트 색상 사용하기.
#'
#' @export bit_pal_orange

bit_pal_orange <- data.frame(                               #  element:
  "orange1" = rgb(254, 220, 180, maxColorValue = 255),  #  1. orange1 (non-transparent):  20%
  "orange2" = rgb(252, 195, 145, maxColorValue = 255),  #  2. orange2 (non-transparent):  40%
  "orange3" = rgb(249, 170, 125, maxColorValue = 255),  #  3. orange3 (non-transparent):  60%
  "orange4" = rgb(247, 142,  95, maxColorValue = 255),  #  4. orange4 (non-transparent):  80%
  "orange5" = rgb(243, 115,  33, maxColorValue = 255),  #  5. orange5 (non-transparent): 100%: preferred color: "Orange"
  "orange6" = rgb(214,  98,  24, maxColorValue = 255),  #  5. orange6 (non-transparent): Darker shade
  stringsAsFactors = FALSE)

# bit_orange -----
#' @name bit_orange
#' @rdname bit_orange
#' @title bitR color orange
#'
#' @description 기본 오렌지 색상 (HEX 값으로): #F37321
#' \code{bit_orange}는 \code{\link{bit_pal_orange}}의 기본 색상을 제공하며,
#' (HEX 문자값 형식의) \code{\link{bit_pal_orange}[[5]]}로 정의됩니다.
#'
#' @examples {
#' bit_orange  # HEX 문자 형식: "#F37321"
#' all.equal(bit_orange, bit_pal_orange[[5]])  # TRUE (HEX값과 동일)
#'
#' seecol(bit_orange)  # 색상을 조회
#' }
#' @family 선호 색상
#'
#' @seealso
#' \code{\link{bit_pal_orange}} bitR 색상 팔레트 대체 색상의 경우;
#' \code{\link{bit_pal}} 5가지 주요 색상이 있는 bitR 기본 색상 팔레트. \code{\link{bit_pal_orange}};
#' \code{\link{bit_pal_primary}} 기본 색상이 있는 bitR 색상 팔레트의 경우;
#' \code{\link{seecol}} 팔래트 색상 보기;
#' \code{\link{usecol}} 팔레트 색상 사용하기.
#'
#' @export bit_orange

bit_orange <- as.character(bit_pal_orange[[5]])
# bit_pal_orange 혹은 bit_pal의 orange.5의 HEX 색상
names(bit_orange) <- "Orange"





# bit_pal_blue -----
#' @name bit_pal_blue
#' @rdname bit_pal_blue
#' @title bitR blue color palette
#'
#' @description
#' BLUE:  PANTONE 300 C/U / CMYK 99 50 0 0
#' RGB 0 114 188 / HEX #0072BC
#' \code{bit_pal_blue} provides an additional bitR color palette
#' as a data frame containing 6 colors (shades of \code{\link{Blue}}).
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_pal_blue
#' dim(bit_pal_blue)  # 1 6
#'
#' # Preferred color:
#' bit_pal_blue[5]    # preferred (named) color "blue5" (as df)
#' bit_pal_blue[[5]]  # preferred color value "#0072BC"
#'
#' # Access by position:
#' bit_pal_blue[5]    # named color "blue5" (as df)
#' bit_pal_blue[[5]]  # color value "#0072BC"
#'
#' # Access by name:
#' bit_pal["blue5"]    # color "blue5" (as df)
#' bit_pal[["blue5"]]  # color value "#0072BC"
#'
#' # Plotting palette:
#' seecol(bit_pal_blue)
#' }
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal}} for the bitR default color palette with 5 main colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_darkblue}} for an alternative blue bitR color palette;
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_pal_blue

bit_pal_blue <- data.frame(                               #  element:
  "blue1" = rgb(204, 227, 242, maxColorValue = 255),  #  1. blue1 (non-transparent):  20%
  "blue2" = rgb(153, 199, 228, maxColorValue = 255),  #  2. blue2 (non-transparent):  40%
  "blue3" = rgb(102, 170, 215, maxColorValue = 255),  #  3. blue3 (non-transparent):  60%
  "blue4" = rgb( 51, 142, 201, maxColorValue = 255),  #  4. blue4 (non-transparent):  80%
  "blue5" = rgb(  0, 114, 188, maxColorValue = 255),  #  5. blue5 (non-transparent): 100%: preferred color: "Blue"
  "blue6" = rgb(  0,  86, 141, maxColorValue = 255),  #  5. blue6 (non-transparent): Darker shade
  stringsAsFactors = FALSE)

# bit_blue -----
#' @name bit_blue
#' @rdname bit_blue
#' @title bitR color blue
#'
#' @description Preferred color blue (as HEX character value): ----
#' \code{bit_blue} provides the preferred color of \code{\link{bit_pal_blue}}
#' (as an atomic HEX character value) and is defined as
#' \code{\link{bit_pal_blue}[[5]]}.
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_blue  # HEX character "#0072BC" (as value)
#' all.equal(bit_blue, bit_pal_blue[[5]])  # TRUE (same HEX values)
#'
#' seecol(bit_blue)  # view color and details
#' }
#' @family preferred colors
#'
#' @seealso
#' \code{\link{bit_pal_blue}} for the corresponding color palette;
#' \code{\link{bit_pal}} for the bitR default color palette with 5 main colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_blue

bit_blue <- as.character(bit_pal_blue[[5]])
# HEX color value of blue.5 of bit_pal_blue OR bit_pal
names(bit_blue) <- "Blue"


# bit_pal_grey -----
#' @name bit_pal_grey
#' @rdname bit_pal_grey
#' @title bitR grey color palette
#'
#' @description   Using a Rich black for text can make printing
#'  difficult and the text less sharp. We recommend body copy
#'  is set in C0 M0 Y0 K100. \code{pal_grey} provides an additional bitR color palette
#' as a data frame containing 6 colors (shades of \code{\link{black}}).
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_pal_grey
#' dim(bit_pal_grey)  # 1 6
#' bit_pal_grey[4]    # preferred (named) color "grey4"
#' bit_pal_grey[[4]]  # preferred color "grey4" OR "#666666"
#'
#' # Plotting palette:
#' seecol(bit_pal_grey)
#' }
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal}} for the bitR default color palette with 5 main colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_darkblue}} for alternative to grey bitR color palettes;
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_pal_grey

bit_pal_grey <- data.frame(                               #  Element:
  "grey1" = rgb(230, 230, 230, maxColorValue = 255),  #  1.
  "grey2" = rgb(204, 204, 204, maxColorValue = 255),  #  2.
  "grey3" = rgb(153, 153, 153, maxColorValue = 255),  #  3.
  "grey4" = rgb(102, 102, 102, maxColorValue = 255),  #  4. preferred color: "grey"
  "grey5" = rgb( 51,  51,  51, maxColorValue = 255),  #  5.
  "black" = rgb(  0,   0,   0, maxColorValue = 255),  #  6.
  stringsAsFactors = FALSE)

# bit_grey -----
#' @name bit_grey
#' @rdname bit_grey
#' @title bitR color grey
#'
#' @description   \code{bit_grey} provides the preferred color of \code{\link{bit_pal_grey}}
#' (as an atomic HEX character value) and is defined as
#' \code{\link{bit_pal_grey}[[4]]}.
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_grey  # HEX character "#666666" (as value)
#' all.equal(bit_grey, bit_pal_grey[[4]])  # TRUE (same HEX values)
#'
#' seecol(bit_grey)  # view color and details
#' }
#' @family preferred colors
#'
#' @seealso
#' \code{\link{bit_pal_grey}} for the corresponding color palette;
#' \code{\link{bit_pal}} for the bitR default color palette with 5 main colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show and use color palettes.
#'
#' @export bit_grey

bit_grey <- bit_pal_grey[[4]]  # HEX color value of grey.4 of bit_pal_grey
names(bit_grey) <- "Grey"

# bit_black -----
#' @name bit_black
#' @rdname bit_black
#' @title bitR color black
#'
#' @description   \code{bit_black} provides the preferred color of black
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_black  # "#000000"
#' }
#' @family preferred colors
#'
#' @seealso
#' \code{\link{bit_pal_grey}} for the corresponding color palette;
#' \code{\link{bit_pal}} for the bitR default color palette with 5 main colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show and use color palettes.
#'
#' @export bit_black
bit_black <- bit_pal_grey[[6]]  # HEX color value of grey.6 of bit_pal_grey
names(bit_black) <- "Black"

# bit_white -----
#' @name bit_white
#' @rdname bit_white
#' @title bitR color white
#'
#' @description   \code{bit_white} provides the preferred color of white
#' (as an atomic HEX character value)
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_white  # "#FFFFFF"
#' }
#' @family preferred colors
#'
#' @seealso
#' \code{\link{bit_pal_grey}} for the corresponding color palette;
#' \code{\link{bit_pal}} for the bitR default color palette with 5 main colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show and use color palettes.
#'
#' @export bit_white
bit_white <- bit_pal[[6]]  # HEX color value of white of bit_pal
names(bit_white) <- "White"



# bit_pal_yellow -----
#' @name bit_pal_yellow
#' @rdname bit_pal_yellow
#' @title bitR yellow color palette.
#'
#' @description  The secondary accent colour may be used in some circumstances
#' for a graphic element.
#' YELLOW: PANTONE PROCESS YELLOW C/U
#'   CMYK 3 1 100 0
#'   RGB 250 235 0
#'   HEX #FAEB00
#'   \code{bit_pal_yellow} provides an additional bitR color palette
#' as a data frame containing 6 colors (shades of
#' \code{\link{bit_yellow}}).
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_pal_yellow
#' dim(bit_pal_yellow)  # 1 6
#' bit_pal_yellow[5]    # preferred (named) color "yellow5"
#' bit_pal_yellow[[5]]  # preferred color "yellow5" OR "#FAEB00"
#'
#' # Plotting palette:
#' seecol(bit_pal_yellow)
#' }
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal}} for the bitR default color palette with all 5 colors of \code{\link{bit_yellow}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_pal_yellow

bit_pal_yellow <- data.frame(                               #  element:
  "yellow1" = rgb(254, 251, 204, maxColorValue = 255),  #  1. yellow1 (non-transparent)
  "yellow2" = rgb(253, 247, 153, maxColorValue = 255),  #  2. yellow2 (non-transparent)
  "yellow3" = rgb(252, 243, 102, maxColorValue = 255),  #  3. yellow3 (non-transparent)
  "yellow4" = rgb(251, 239,  51, maxColorValue = 255),  #  4. yellow4 (non-transparent)
  "yellow5" = rgb(250, 235,   0, maxColorValue = 255),  #  5. yellow5 (non-transparent): preferred color: "yellow"
  "yellow6" = rgb(200, 188,   0, maxColorValue = 255),  #  6. yellow6 (non-transparent)
  stringsAsFactors = FALSE)


# bit_yellow -----
#' @name bit_yellow
#' @rdname bit_yellow
#' @title Preferred color yellow (as HEX character value)
#'
#' @description bitR color yellow.
#'
#' \code{yellow} provides the preferred color of \code{\link{bit_pal_yellow}}
#' (as an atomic HEX character value) and is defined as
#' \code{\link{bit_pal_yellow}[[5]]}.
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_yellow  # HEX character "#FAEB00" (as value)
#' all.equal(bit_yellow, bit_pal_yellow[[5]])  # TRUE (same HEX values)
#' seecol(bit_yellow)  # view color and details
#' }
#'
#' @family preferred colors
#'
#' @seealso
#' \code{\link{bit_pal_yellow}} for the corresponding color palette;
#' \code{\link{bit_pal}} for the bit default color palette with all 5 colors of \code{\link{bit_pal_yellow}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export

bit_yellow <- bit_pal_yellow[[5]]  # HEX color value of yellow.5 of bit_pal_yellow
names(bit_yellow) <- "Yellow"




# bit_pal_darkblue -----
#' @name bit_pal_darkblue
#' @rdname bit_pal_darkblue
#' @title bitR darkblue color palette.
#'
#' @description  darkblue
#'  CMYK 99 83 36 27
#'  RGB 24 55 95
#'  HEX #18375F
#' \code{bit_pal_darkblue} provides an additional bitR color palette
#' as a data frame containing 6 colors (shades of
#' \code{\link{bit_darkblue}}).
#'
#' See \url{https://media.unhcr.org/archive/Data-visualisation-colour-palette-2CZ7A2HOVFP9.html} for details.
#'
#' @examples {
#' bit_pal_darkblue
#' dim(bit_pal_darkblue)  # 1 6
#' bit_pal_darkblue[6]    # preferred (named) color "darkblue6"
#' bit_pal_darkblue[[6]]  # preferred color "darkblue6" OR "#18375F"
#'
#' # Plotting palette:
#' seecol(bit_pal_darkblue)
#' }
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal}} for the bitR default color palette with all 5 colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_blue}} for an alternative blue bitR color palette;
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_pal_darkblue

bit_pal_darkblue <- data.frame(                               #  element:
  "darkblue1" = rgb(232, 235, 239, maxColorValue = 255),  #  1. darkblue1 (non-transparent)
  "darkblue2" = rgb(209, 215, 223, maxColorValue = 255),  #  2. darkblue2 (non-transparent)
  "darkblue3" = rgb(163, 175, 191, maxColorValue = 255),  #  3. darkblue3 (non-transparent)
  "darkblue4" = rgb(116, 135, 159, maxColorValue = 255),  #  4. darkblue4 (non-transparent)
  "darkblue5" = rgb( 70,  95, 127, maxColorValue = 255),  #  5. darkblue5 (non-transparent)
  "darkblue6" = rgb( 24,  55,  95, maxColorValue = 255),  #  6. darkblue6 (non-transparent): preferred color: "darkblue"
  stringsAsFactors = FALSE)


# bit_darkblue -----
#' @name bit_darkblue
#' @rdname bit_darkblue
#' @title Preferred color darkblue (as HEX character value)
#'
#' @description bitR color darkblue.
#'
#' \code{darkblue} provides the preferred color of \code{\link{bit_pal_darkblue}}
#' (as an atomic HEX character value) and is defined as
#' \code{\link{bit_pal_darkblue}[[6]]}.
#'
#' See \url{https://media.unhcr.org/archive/Data-visualisation-colour-palette-2CZ7A2HOVFP9.html} for details.
#'
#' @examples {
#' bit_darkblue # HEX character "#18375F" (as value)
#' all.equal(bit_darkblue, bit_pal_darkblue[[6]])  # TRUE (same HEX values)
#' seecol(bit_darkblue)  # view color and details
#' }
#' @family preferred colors
#'
#' @seealso
#' \code{\link{bit_pal_darkblue}} for the corresponding color palette;
#' \code{\link{bit_pal}} for the bitR default color palette with all 5 colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export

bit_darkblue <- bit_pal_darkblue[[6]]  # HEX color value of darkblue.6 of bit_pal_darkblue
names(bit_darkblue) <- "Darkblue"



# bit_pal_green -----
#' @name bit_pal_green
#' @rdname bit_pal_green
#' @title bitR greem color palette.
#'
#' @description   bitR green color palette.
#' GREEN:  CMYK 90 0 52 0 / RGB 0 179 152 / HEX #00B398
#' \code{pal_green} provides an additional bitR color palette
#' as a data frame containing 6 colors (shades of
#' \code{\link{green}}).
#'
#' See \url{https://media.unhcr.org/archive/Data-visualisation-colour-palette-2CZ7A2HOVFP9.html} for details.
#'
#' @examples {
#' bit_pal_green
#' dim(bit_pal_green)  # 1 6
#' bit_pal_green[5]    # preferred (named) color "green5"
#' bit_pal_green[[5]]  # preferred color "green5" OR "#00B398"
#'
#' # Plotting palette:
#' seecol(bit_pal_green)
#' }
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal}} for the bitR default color palette with all 5 colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_pal_green

bit_pal_green <- data.frame(                               #  element:
  "green1" = rgb(204, 240, 234, maxColorValue = 255),  #  1. green1 (non-transparent)
  "green2" = rgb(153, 225, 214, maxColorValue = 255),  #  2. green2 (non-transparent)
  "green3" = rgb(102, 209, 193, maxColorValue = 255),  #  3. green3 (non-transparent)
  "green4" = rgb( 51, 194, 173, maxColorValue = 255),  #  4. green4 (non-transparent)
  "green5" = rgb(  0, 179, 152, maxColorValue = 255),  #  5. green5 (non-transparent): preferred color: "green"
  "green6" = rgb(  0, 134, 114, maxColorValue = 255),  #  6. green6 (non-transparent)
  stringsAsFactors = FALSE)



# _green -----
#' @name bit_green
#' @rdname bit_green
#' @title Preferred color darkblue (as HEX character value)
#'
#' @description bitR color green.
#'
#' \code{green} provides the preferred color of \code{\link{bit_pal_green}}
#' (as an atomic HEX character value) and is defined as
#' \code{\link{bit_pal_green}[[5]]}.
#'
#' See \url{https://media.unhcr.org/archive/Data-visualisation-colour-palette-2CZ7A2HOVFP9.html} for details.
#'
#' @examples {
#' bit_green # HEX character "#00B398" (as value)
#' all.equal(bit_green, bit_pal_green[[5]])  # TRUE (same HEX values)
#'
#' seecol(bit_green)  # view color and details
#' }
#' @family preferred colors
#'
#' @seealso
#' \code{\link{bit_pal_green}} for the corresponding color palette;
#' \code{\link{bit_pal}} for the bitR default color palette with all 5 colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_green

bit_green <- bit_pal_green[[5]]  # HEX color value of green.5 of bit_pal_green
names(bit_green) <- "Green"


# bit_pal_red -----
#' @name bit_pal_red
#' @rdname bit_pal_red
#' @title bitR red color palette.
#'
#' @description   bitR red color palette.
#'  RED: CMYK 0 87 52 0 / RGB 239 74 96 / HEX #EF4A60
#'
#' \code{pal_red} provides an additional bitR color palette
#' as a data frame containing 6 colors (shades of
#' \code{\link{bit_red}}).
#'
#' See \url{https://media.unhcr.org/archive/Data-visualisation-colour-palette-2CZ7A2HOVFP9.html} for details.
#'
#' @examples {
#' bit_pal_red
#' dim(bit_pal_red)  # 1 6
#' bit_pal_red[5]    # preferred (named) color "red5"
#' bit_pal_red[[5]]  # preferred color "red5" OR "#EF4A60"
#'
#' # Plotting palette:
#' seecol(bit_pal_red)
#' }
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal}} for the bitR default color palette with all 5 colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_pal_red

bit_pal_red <- data.frame(                               #  element:
  "red1" = rgb(252, 219, 223, maxColorValue = 255),  #  1. red1 (non-transparent)
  "red2" = rgb(249, 183, 191, maxColorValue = 255),  #  2. red2 (non-transparent)
  "red3" = rgb(245, 146, 160, maxColorValue = 255),  #  3. red3 (non-transparent)
  "red4" = rgb(242, 110, 128, maxColorValue = 255),  #  4. red4 (non-transparent)
  "red5" = rgb(239,  74,  96, maxColorValue = 255),  #  5. red5 (non-transparent): preferred color: "red"
  "red6" = rgb(179,  56,  72, maxColorValue = 255),  #  6. red6 (non-transparent)
  stringsAsFactors = FALSE)


# bit_red -----
#' @name bit_red
#' @rdname bit_red
#' @title Preferred color red (as HEX character value)
#'
#' @description bitR color red.
#'
#' \code{red} provides the preferred color of \code{\link{bit_pal_red}}
#' (as an atomic HEX character value) and is defined as
#' \code{\link{bit_pal_red}[[5]]}.
#'
#' See \url{https://media.unhcr.org/archive/Data-visualisation-colour-palette-2CZ7A2HOVFP9.html} for details.
#'
#' @examples {
#' bit_red  # HEX character "#EF4A60" (as value)
#' all.equal(bit_red, bit_pal_red[[5]])  # TRUE (same HEX values)
#' seecol(bit_red)  # view color and details
#' }
#' @family preferred colors
#'
#' @seealso
#' \code{\link{bit_pal_red}} for the corresponding color palette;
#' \code{\link{bit_pal}} for the bitR default color palette with all 5 colors of \code{\link{bit_pal_blue}};
#' \code{\link{bit_pal_primary}} for a bitR color palette with primary colors;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_red

bit_red <- bit_pal_red[[5]]  # HEX color value of red.5 of bit_pal_red
names(bit_red) <- "Red"


# bit_pal_primary -----
#' @name bit_pal_primary
#' @rdname bit_pal_primary
#' @title Scale of all 4 preferred colors, Other combinations of bitR color palettes
#'
#' @description  \code{bit_pal_primary} provides an additional bitR color palette
#' that collects the primary bitR color as a data frame containing 4 colors.
#'
#' See \url{https://media.unhcr.org/archive/Brand-Book-for-external-partners-2CZ7A2P0CGAY.html} for details.
#'
#' @examples {
#' bit_pal_primary
#' dim(bit_pal_primary)  # 1 4
#'
#' # Access by position:
#' bit_pal_primary[1]    # color blue (as df)
#' bit_pal_primary[[1]]  # color value "#0072BC"
#'
#' # Access by name:
#' bit_pal_primary["Blue"]   # color "blue5" (as df)
#' bit_pal_primary[["Blue"]] # color value "#0072BC"
#'
#' # Plotting palette:
#' seecol(bit_pal_primary)
#' }
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal}} for the default bitR color palette;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_pal_primary
bit_pal_primary <- data.frame( # Element:
  "Blue"     = bit_blue,          # bit_pal_blue[[5]],     #  1. blue
  "Black"    = bit_black,         # bit_pal_grey[[6]],     #  2. black
  "White"    = bit_white,         # bit_pal[[6]],          #  3. white
  "Yellow"   = bit_yellow,        # bit_pal_yellow[[5]],   #  4. yellow
  stringsAsFactors = FALSE)






# bit_pal_graphic -----
#' @name bit_pal_graphic
#' @rdname bit_pal_graphic
#' @title Scale of all XX preferred colors for graphic creation
#'
#' @description  bitR preferred graphic colors to be use as a main set for graphics creation.
#' \code{bit_pal_graphic} provides an additional bitR color palette
#' that collects the preferred bitR colors for graphics as a data frame containing 6 colors.
#'
#' See \url{https://media.unhcr.org/archive/Data-visualisation-colour-palette-2CZ7A2HOVFP9.html} for details.
#'
#' @examples {
#' bit_pal_graphic
#' dim(bit_pal_graphic)
#'
#' # Access by position:
#' bit_pal_graphic[1]    # color blue (as df)
#' bit_pal_graphic[[1]]  # color value "#0072BC"
#'
#' # Access by name:
#' bit_pal_graphic["Blue"]   # color "blue5" (as df)
#' bit_pal_graphic[["Blue"]] # color value "#0072BC"
#'
#' # Plotting palette:
#' seecol(bit_pal_graphic)
#' }
#'
#' @family color palettes
#'
#' @seealso
#' \code{\link{bit_pal}} for the default bitR color palette;
#' \code{\link{seecol}} to show color palettes;
#' \code{\link{usecol}} to use color palettes.
#'
#' @export bit_pal_graphic

bit_pal_graphic <- data.frame( # Element:
  "Blue"     = bit_pal_blue[[5]],     #  1. blue
  "Darkblue" = bit_pal_darkblue[[6]], #  2. darkblue
  "Green"    = bit_pal_green[[5]],    #  3. green
  "Grey3"    = bit_pal_grey[[3]],     #  4. grey3
  "Red"      = bit_pal_red[[5]],      #  5. red
  "Blue3"    = bit_pal_blue[[3]], #  6. blue3
  stringsAsFactors = FALSE)



# bit_all_pal_graphic -----
#' @name bit_all_pal_graphic
#' @rdname bit_all_pal_graphic
#' @title Look-up list of palettes
#' @export bit_all_pal_graphic
bit_all_pal_graphic <- c("bit_pal_blue",
                         "bit_pal_grey",
                         "bit_pal_darkblue",
                         "bit_pal_green",
                         "bit_pal_red",
                         "bit_pal_yellow",
                         "bit_pal_graphic")

#' @name bit_all_pal
#' @rdname bit_all_pal
#' @title Look-up list of palettes
#' @export bit_all_pal
bit_all_pal <- c("bit_pal",
                 "bit_pal_primary",
                 "bit_all_pal_graphic")


