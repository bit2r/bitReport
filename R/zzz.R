.onAttach <- function(libname, pkgname) {
  # Korean font
  font_path <- system.file("fonts", "NanumSquare", package = "bitReport")

  sysfonts::font_add(
    family = "NanumSquare",
    regular = paste(font_path, "NanumSquareOTF_acR.otf", sep = "/")
  )

  showtext::showtext_auto()
}