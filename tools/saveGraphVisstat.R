#' Save plots from a visstat object to files
#'
#' @param object A visstat object
#' @param filename Base filename without extension
#' @param type Plot file type: "png" or "pdf"
#' @param width Width in inches
#' @param height Height in inches
#' @param ... Further arguments passed to graphics devices
#'
#' @export
saveGraphVisstat <- function(object, filename = "visstat_plot", type = "png",
                             width = 7, height = 5, ...) {
  stopifnot(inherits(object, "visstat"))
  stopifnot(type %in% c("png", "pdf"))

  open_device <- function(file) {
    switch(type,
      png = png(file, width = width, height = height, units = "in", res = 300, ...),
      pdf = pdf(file, width = width, height = height, ...)
    )
  }

  # Save main plot
  if (!is.null(object$plot)) {
    file_main <- paste0(filename, "_main.", type)
    open_device(file_main)
    print(object$plot)
    dev.off()
  }

  # Save combined assumption plots if both are present
  if (!is.null(object$qqplot) && !is.null(object$residuals_plot)) {
    file_diag <- paste0(filename, "_assumptions.", type)
    open_device(file_diag)
    op <- par(mfrow = c(1, 2))
    print(object$qqplot)
    print(object$residuals_plot)
    par(op)
    dev.off()
  }
}
