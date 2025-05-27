#' Print method for visstat object
#'
#' @param x An object of class 'visstat'
#' @param ... Additional arguments (ignored)
#' @export
print.visstat <- function(x, ...) {
  cat("Object of class 'visstat'\n")
  cat("Available components:\n")
  print(names(x))
  invisible(x)
}

#' Summary method for visstat object
#'
#' @param object An object of class 'visstat'
#' @param ... Additional arguments (ignored)
#' @export
summary.visstat <- function(object, ...) {
  cat("Summary of visstat object\n\n")

  if (!is.null(object$test)) {
    cat("--- Test Summary ---\n")
    print(object$test)
  } else {
    cat("No statistical test result found.\n")
  }

  if (!is.null(object$posthoc_summary)) {
    cat("\n--- Post Hoc Summary ---\n")
    print(object$posthoc_summary)
  }

  invisible(object)
}

#' Plot method for visstat object
#'
#' @param x An object of class 'visstat'
#' @param ... Additional arguments (ignored)
#' @export
plot.visstat <- function(x, ...) {
  if (!is.null(x$plot)) {
    print(x$plot)
  } else {
    warning("No main plot available in the visstat object.")
  }

  # Only show assumption diagnostics if both plots exist
  if (!is.null(x$qqplot) && !is.null(x$residuals_plot)) {
    message("\n--- Assumption Diagnostics ---")
    if (interactive()) dev.new()
    op <- par(mfrow = c(1, 2))
    print(x$qqplot)
    print(x$residuals_plot)
    par(op)
  } else if (!is.null(x$qqplot) || !is.null(x$residuals_plot)) {
    warning("Assumption diagnostics not shown: both qqplot and residuals_plot must be present.")
  }

  invisible(x)
}
