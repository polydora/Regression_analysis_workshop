# Setup chunk for presentations

options(rmarkdown.paste_image_dir = 'img')

# output options
options(width = 87, scipen = 6, digits = 4)

library(knitr)
# "Сетка" размеров картинок для более аккуратной верстки
# widths:
wS = 4; wM  = 6; wL = 8; wXL = 12
# heights:
hXXS = 2.5; hXS = 3.5; hS = 4; hM = 4.5; hL = 6; hXL = 7.5; hXXL = 8; hXXXL = 9

knitr::opts_template$set(
  # Узкие картинки
  SxXXS = list(fig.width = wS, fig.height = hXXS),
  SxXS = list(fig.width = wS, fig.height = hXS),
  SxS = list(fig.width = wS, fig.height = hS),
  SxM = list(fig.width = wS, fig.height = hM),
  SxL = list(fig.width = wS, fig.height = hL), # полный столбец
  SxXL = list(fig.width = wS, fig.height = hXL),
  SxXXL = list(fig.width = wS, fig.height = hXXL),
  SxXXXL = list(fig.width = wS, fig.height = hXXXL),

  # чуть больше ширины столбца
  MxXXS = list(fig.width = wM, fig.height = hXXS),
  MxXS = list(fig.width = wM, fig.height = hXS),
  MxS = list(fig.width = wM, fig.height = hS),
  MxM = list(fig.width = wM, fig.height = hM), # размер по умолчанию
  MxL = list(fig.width = wM, fig.height = hL),
  MxXL = list(fig.width = wM, fig.height = hXL),
  MxXXL = list(fig.width = wM, fig.height = hXXL),
  MxXXXL = list(fig.width = wM, fig.height = hXXXL),

  # 3/4 слайда
  LxXXS = list(fig.width = wL, fig.height = hXXS),
  LxXS = list(fig.width = wL, fig.height = hXS),
  LxS = list(fig.width = wL, fig.height = hS),
  LxM = list(fig.width = wL, fig.height = hM),
  LxL = list(fig.width = wL, fig.height = hL),
  LxXL = list(fig.width = wL, fig.height = hXL),
  LxXXL = list(fig.width = wL, fig.height = hXXL),
  LxXXXL = list(fig.width = wL, fig.height = hXXXL),

  # на ширину слайда
  XLxXXS = list(fig.width = wXL, fig.height = hXXS),
  XLxXS = list(fig.width = wXL, fig.height = hXS),
  XLxS = list(fig.width = wXL, fig.height = hS),
  XLxM = list(fig.width = wXL, fig.height = hM),
  XLxL = list(fig.width = wXL, fig.height = hL),
  XLxXL = list(fig.width = wXL, fig.height = hXL), # полный слайд с заголовком
  XLxXXL = list(fig.width = wXL, fig.height = hXXL),
  XLxXXXL = list(fig.width = wXL, fig.height = hXXXL)  # полный слайд без заголовка
)

# chunk default options
opts_chunk$set(message = FALSE, tidy = FALSE, warning = FALSE, comment = "", opts.label='MxM', fig.showtext = TRUE, echo=TRUE)

library("xaringanthemer")
style_duo_accent(
  # primary_color = "#9B3C17",
  # secondary_color = "#649015",
  primary_color = "#9B3C17",
  secondary_color = "#201030",
  # secondary_color = "#4682B4",
  base_font_size = "22px",
  text_font_size = "1rem",
  header_h1_font_size = "2.2rem",
  header_h2_font_size = "1.7rem",
  header_h3_font_size = "1.5rem",
  header_font_google = google_font("Arsenal", languages = c("latin", "cyrillic", "greek")),
  header_font_family_fallback = "Georgia, serif",
  text_font_google   = google_font("Nunito", "400", "400i", languages = c("latin", "cyrillic")),
  text_font_family_fallback = "Calibri, sans-serif",
  code_font_google   = google_font("Ubuntu Mono", languages = c("latin", "cyrillic")),
  code_font_family_fallback = "Courier New, monospace",
  outfile = "assets/xaringan-themer.css"
)

library("kableExtra")
options(knitr.kable.NA = '')
library("showtext")

