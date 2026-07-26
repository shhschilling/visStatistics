# Route all test graphics to a null device.
#
# visstat() draws multi-panel diagnostic layouts. When the active device is
# the interactive one (for example the RStudio Plots pane), plot.new() fails
# with "figure margins too large" whenever that pane is smaller than the
# requested layout. Opening a null PDF device for the whole suite makes the
# tests independent of the interactive device size.
#
# The device is closed again in teardown-graphics.R.
pdf(NULL)
