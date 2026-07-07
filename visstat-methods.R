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
  
  # Show available components
  cat("--- Named components ---\n")
  print(names(object))
  
  # Print contents, with special handling for captured_plots
  cat("\n--- Contents ---\n")
  for (name in names(object)) {
    cat(paste0("\n$", name, ":\n"))
    if (name == "captured_plots") {
      .print_captured_plots_summary(object[[name]])
    } else {
      print(object[[name]])
    }
  }
  
  
  invisible(object)
}

# Helper function for captured plots in plots 
.print_captured_plots <- function(plots) {
  if (is.list(plots) && length(plots) > 0) {
    cat(sprintf("List of %d plot object(s)\n", length(plots)))
  } else {
    print(plots)
  }
}
# Helper function for captured plots in summary 
.print_captured_plots_summary <- function(plots) {
  if (is.list(plots) && length(plots) > 0) {
    cat(sprintf("List of %d plot object(s)\n", length(plots)))
  } else if (is.list(plots) && length(plots) == 0) {
    cat("Empty plot list\n")
  } else {
    # Could be multiple plots in different format, don't assume count
    cat("Plot object(s) (not displayed in summary)\n")
  }
}

#' Plot method for visstat objects
#'
#' Displays captured plots or reports saved plot file paths.
#' 
#' @param x An object of class "visstat".
#' @param ... Currently unused.
#'
#' @return Invisibly returns x. Used for its side effect.
#' @export
plot.visstat <- function(x, ...) {
  path <- attr(x, "plot_paths")
  capture <- attr(x, "captured_plots")
  
  if (!is.null(path) && length(path) > 0) {
    for (i in seq_along(path)) {
      message("Plot [", i, "] stored in ", path[[i]])
    }
    return(invisible(x))
  }
  
  if (!is.null(capture) && length(capture) > 0) {
    for (i in seq_along(capture)) {
      replayPlot(capture[[i]])
      Sys.sleep(0.5)  # Small delay to ensure RStudio registers the plot in its history
    } 
    return(invisible(x))
  }
  
  invisible(x)
}