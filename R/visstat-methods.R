
#' Print method for visstat objects
#'
#' Displays a brief summary of the statistical test results and, if available, 
#' assumption tests and post hoc comparisons.
#'
#' @param x An object of class \code{"visstat"}, returned by \code{visstat()}.
#' @param ... Currently unused. Included for S3 method compatibility.
#'
#' @return The object \code{x}, invisibly.
#'
#' @details This function is automatically called when a \code{visstat} object is printed
#' to the console. It provides a quick overview of the statistical analysis results.
#'
#' @seealso \code{\link{summary.visstat}}, \code{\link{plot.visstat}}, \code{\link{visstat}}
#'
#' @export
print.visstat <- function(x, ...) {
  cat("Object of class 'visstat'\n")
  if (is.list(x)) {
    if (!is.null(x$test) && !is.null(x$test$method)) {
      cat("Test used:", x$test$method, "\n")
    }
    if (!is.null(x$test) && !is.null(x$test$p.value)) {
      cat("p-value:", signif(x$test$p.value, 3), "\n")
    }
    cat("\nAvailable components:\n")
    print(names(x))
  } else {
    cat("(No named elements in object)\n")
  }
  invisible(x)
}




#' Summary method for visstat objects
#'
#' Displays the full statistical test results and, if available, 
#' assumption tests and post hoc comparisons.
#'
#' @param object An object of class \code{"visstat"}, returned by \code{visstat()}.
#' @param ... Currently unused. Included for S3 method compatibility.
#'
#' @return The object \code{object}, invisibly.
#'
#' @details This method provides a full textual report of the statistical test results
#' returned by \code{visstat()}, and prints the contents of \code{posthoc_summary} if present.
#'
#' @seealso \code{\link{print.visstat}}, \code{\link{visstat}}
#'
#' @export
summary.visstat <- function(object, ...) {
  cat("Summary of visstat object\n\n")
  
  cat("--- Named components ---\n")
  print(names(object))
  
  cat("\n--- Contents ---\n")
  for (name in names(object)) {
    cat(paste0("\n$", name, ":\n"))
    print(object[[name]])
  }
  
  cat("\n--- Attributes ---\n")
  print(attributes(object)[setdiff(names(attributes(object)), c("names", "class"))])
  
  invisible(object)
}




#' Report only saved plot files for visstat objects
#'
#' This method reports the file paths of plots that were saved to disk during
#' the statistical analysis, if the user requested file output (e.g., PNG or PDF)
#' via the \code{graphicsoutput} argument.
#'
#' Note that plots are shown during execution of \code{visstat()},
#' even if no files are saved. This method does not re-render or replay those plots.
#'
#' All file-based plots in \code{visStatistics} are generated using the \code{Cairo()}
#' device and their file paths are collected in the object's \code{"plot_paths"} attribute.
#'
#' In interactive sessions, this method prints the list of saved files.
#' In non-interactive contexts, it suppresses output.
#'
#' @param x An object of class \code{"visstat"}, returned by \code{visstat()}.
#' @param ... Currently unused. Included for S3 method compatibility.
#'
#' @return Invisibly returns \code{x}. Used for its side effect of reporting file paths.
#'
#' @details The \code{visstat()} function may produce several plots per test type
#' (e.g., main effect, post hoc comparisons, assumption checks). These are saved to
#' disk, and their file paths are collected in the object's \code{"plot_paths"} attribute.
#'
#' In an interactive session, \code{plot()} will print these paths to the console.
#' In non-interactive sessions, it quietly lists them. No plots are shown or rendered
#' within R.
#'
#' @seealso \code{\link{visstat}}, \code{\link[Cairo]{Cairo}}, \code{\link{print.visstat}}, \code{\link{summary.visstat}}
#'
#' @export
plot.visstat <- function(x, ...) {
  paths <- attr(x, "plot_paths")

  if (is.null(paths) || length(paths) == 0) {
    message("No plot files recorded in object.")
    return(invisible(x))
  }

  if (!interactive()) {
    message("Plots saved (non-interactive session):")
    for (p in paths) message("  ", p)
    return(invisible(x))
  }

  message("Plots saved during visstat() execution:")
  for (p in paths) {
    message("  ", p)
  }

  invisible(x)
}




# old function
# plot.visstat <- function(x, ...) {
#   message("Plotting is managed by visstat(), not via plot().")
#   invisible(x)
# }