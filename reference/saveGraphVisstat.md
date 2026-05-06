# Saves Graphical Output with plot capture capability

Closes all graphical devices with
[`dev.off()`](https://rdrr.io/r/grDevices/dev.html) and saves the output
only if both `fileName` and `type` are provided. Enhanced version that
can capture plots before closing devices.

## Usage

``` r
saveGraphVisstat(
  fileName = NULL,
  type = NULL,
  fileDirectory = getwd(),
  oldfile = NULL,
  capture_env = NULL
)
```

## Arguments

- fileName:

  name of file to be created in directory `fileDirectory` without file
  extension '.`type`'.

- type:

  see `Cairo()`.

- fileDirectory:

  path of directory, where graphic is stored. Default setting current
  working directory.

- oldfile:

  old file of same name to be overwritten

- capture_env:

  Environment to store captured plots. If NULL, no capture occurs.

## Value

NULL, if no `type` or `fileName` is provided, file path if graph is
created

## Examples

``` r

# very simple KDE (adapted from example in Cairo())
openGraphCairo(type = "png", fileDirectory = tempdir())
plot(rnorm(4000), rnorm(4000), col = "#ff000018", pch = 19, cex = 2)
# save file 'norm.png' in directory specified in fileDirectory
saveGraphVisstat("norm", type = "png", fileDirectory = tempdir())
file.remove(file.path(tempdir(), "norm.png")) # remove file 'norm.png'
#> [1] TRUE
```
