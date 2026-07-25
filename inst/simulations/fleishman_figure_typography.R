## Shared typography constants for the Fleishman simulation figures.
##
## ggplot2 uses points for theme text and mm-like units for geom_text().
## Keep both scales explicit so the multi-panel figures stay consistent.

FLEISHMAN_FONT_FAMILY <- "serif"

FLEISHMAN_TEXT <- list(
  main_title = 20,
  panel_title = 18,
  section_title = 15,
  axis_title = 15,
  axis_text = 12,
  legend = 18,
  strip = 15,
  heatmap_row = 14.5,
  power_strip = 18,
  panel_letter = 28,
  panel_description = 18
)

FLEISHMAN_GEOM_TEXT <- list(
  panel_number = 5.0,
  panel_letter = 8.0,
  block_title = 5.4,
  heatmap_cell = 5.3,
  inset = 4.6
)

FLEISHMAN_LINEHEIGHT <- list(
  panel_title = 1.05,
  block_title = 0.95
)

fleishman_panel_title <- function(letter, description) {
  sprintf(
    "<b>%s</b> <span style='font-size:%spt'>%s</span>",
    letter,
    FLEISHMAN_TEXT$panel_description,
    description
  )
}
