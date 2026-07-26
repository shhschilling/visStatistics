# Close the null device opened in setup-graphics.R.
while (dev.cur() > 1) dev.off()
