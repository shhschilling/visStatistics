#' Print method for visstat objects
#'
#' Displays a brief summary of the statistical test results and, if
#' available, assumption tests and post hoc comparisons.
#'
#' @param x An object of class \code{"visstat"}, returned by
#'   \code{visstat()}.
#' @param ... Currently unused. Included for S3 method compatibility.
#'
#' @return The object \code{x}, invisibly.
#'
#' @details Quick overview of the statistical analysis results.
#'
#' @examples
#' anova <- visstat(npk$block, npk$yield)
#' print(anova)
#'
#' @seealso \code{\link{summary.visstat}},
#'   \code{\link{plot.visstat}}, \code{\link{visstat}}
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

    if (!is.null(x$effect_size) && !is.null(x$effect_size$name)) {
      cat(
        "Effect size:",
        x$effect_size$name,
        "=",
        signif(x$effect_size$estimate, 3),
        "\n"
      )
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
#' @param object An object of class \code{"visstat"}, returned by
#'   \code{visstat()}.
#' @param ... Currently unused. Included for S3 method compatibility.
#'
#' @return The object \code{object}, invisibly.
#'
#' @details This method provides a full textual report of the statistical
#' test results returned by \code{visstat()}, and prints the contents of
#' \code{posthoc_summary} if present.
#'
#' @seealso \code{\link{print.visstat}}, \code{\link{visstat}}
#'
#' @examples
#' anova <- visstat(npk$block, npk$yield)
#' summary(anova)
#'
#' @export

summary.visstat <- function(object, ...) {
  
  cat("Summary of visstat object\n\n")
  
  cat("--- Named components ---\n")
  print(names(object))
  
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


# Helper function for captured plots in print()

.print_captured_plots <- function(plots) {
  
  if (is.list(plots) && length(plots) > 0) {
    
    cat(sprintf("List of %d plot object(s)\n", length(plots)))
    
  } else {
    
    print(plots)
    
  }
  
}


# Helper function for captured plots in summary()

.print_captured_plots_summary <- function(plots) {
  
  if (is.list(plots) && length(plots) > 0) {
    
    cat(sprintf("List of %d plot object(s)\n", length(plots)))
    
  } else if (is.list(plots) && length(plots) == 0) {
    
    cat("Empty plot list\n")
    
  } else {
    
    cat("Plot object(s) (not displayed in summary)\n")
    
  }
  
}


#' Plot method for visstat objects
#'
#' Replays captured plots or reports saved plot file paths from a
#' \code{visstat} object.
#'
#' When called without \code{which}, the method lists all available plots
#' (either as file paths or as indices of captured plots).
#' When called with \code{which}, the selected plot is displayed:
#' for file-based output the stored path is printed, for captured plots
#' the plot is replayed via \code{replayPlot()}.
#'
#' @param x An object of class \code{"visstat"}, returned by
#'   \code{\link{visstat}()}.
#' @param which Integer selecting a single plot to display (1, 2, ...).
#'   If \code{NULL} (the default), all available plots are listed without
#'   being drawn.
#' @param ... Currently unused. Included for S3 method compatibility.
#'
#' @return The object \code{x}, invisibly. Called for its side effect.
#'
#' @seealso \code{\link{print.visstat}},
#'   \code{\link{summary.visstat}}, \code{\link{visstat}}
#'
#' @examples
#' # File-based output: plot() lists stored paths
#' anova_path <- visstat(
#'   npk$block,
#'   npk$yield,
#'   graphicsoutput = "png",
#'   plotDirectory = tempdir()
#' )
#'
#' plot(anova_path)
#'
#' # Interactive output: plot() lists available plots,
#' # plot(obj, which = n) replays a specific one
#' linreg <- visstat(trees$Height, trees$Girth)
#'
#' plot(linreg)
#' plot(linreg, which = 2)
#'
#' @export

plot.visstat <- function(x, which = NULL, ...) {
  
  path <- attr(x, "plot_paths")
  capture <- attr(x, "captured_plots")
  
  # File-based plots ---------------------------------------------------
  
  if (!is.null(path) && length(path) > 0) {
    
    if (!is.null(which)) {
      
      message("Plot [", which, "] stored in ", path[[which]])
      
    } else {
      
      for (i in seq_along(path)) {
        
        message("Plot [", i, "] stored in ", path[[i]])
        
      }
      
    }
    
    return(invisible(x))
    
  }
  
  # Captured plots -----------------------------------------------------
  
  if (!is.null(capture) && length(capture) > 0) {
    
    if (!is.null(which)) {
      
      oldpar <- graphics::par(no.readonly = TRUE)
      
      on.exit(graphics::par(oldpar), add = TRUE)
      
      replayPlot(capture[[which]])
      
    } else {
      
      for (i in seq_along(capture)) {
        
        message(
          "Plot [", i,
          "] captured. Use plot(obj, which = ", i,
          ") to display."
        )
        
      }
      
    }
    
    return(invisible(x))
    
  }
  
  invisible(x)
  
}
