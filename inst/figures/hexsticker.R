library(hexSticker)
library(showtext)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Teko", "teko")
## Automatically use showtext to render text for future devices
showtext_auto()

imgurl <- system.file("figures/Report.png", package = "hliReport")
sticker(imgurl, package = "hliReport", 
        p_size = 22, p_y = 1.55, p_color = "#31373E", p_family = "teko",
        h_fill = "#1E86EE", h_color = "#C76630", 
        s_x = 1, s_y = 0.8, s_width = 0.5, #s_height = .25,
        filename = "inst/figures/imgfile.png")

