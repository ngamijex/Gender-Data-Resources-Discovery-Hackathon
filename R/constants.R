# ── R/constants.R ─────────────────────────────────────────────────────────────
# Design tokens that mirror CSS :root — used in plotly chart styling.
# Palette: white canvas · vivid red #d62027 · coral-red #d86367
#          salmon #f8d6c6 · light pink #f4d2d3 · near-black ink

CLR <- c(
  primary        = "#d62027",   # vivid red — main brand
  primary_dark   = "#b01a1f",   # hover / dark red
  primary_900    = "#8b0d11",   # deepest red
  primary_light  = "#f8d6c6",   # salmon — label backgrounds
  primary_pale   = "#fdf0ec",   # blush whisper

  secondary      = "#d86367",   # coral-red — secondary accent
  accent         = "#d86367",   # same coral
  accent_light   = "#f4d2d3",   # light pink
  accent_pale    = "#fdf0ec",   # blush

  ink            = "#1a0f0f",   # near-black
  ink_700        = "#2d1c1c",   # dark ink — headings
  ink_500        = "#5c3c3c",   # mid ink — secondary text
  ink_300        = "#9a7070",   # muted — captions
  ink_100        = "#d9c4c4",   # pale — hairlines

  bg_page        = "#ffffff",   # pure white
  bg_surface     = "#fdf0ec",   # very light blush
  bg_inset       = "#f8d6c6",   # salmon inset
  bg_white       = "#ffffff",

  success        = "#2d6e44",
  info           = "#1a5c8a",
  warning        = "#c07718",
  border         = "#f4d2d3"    # light pink border
)
