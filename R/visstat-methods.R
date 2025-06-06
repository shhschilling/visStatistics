

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
  
  # Filter and print non-empty, non-standard attributes
  attrs <- attributes(object)[setdiff(names(attributes(object)), c("names", "class"))]
  non_empty_attrs <- attrs[!vapply(attrs, function(x)
    is.null(x) ||
      (is.atomic(x) && length(x) == 0) ||
      (is.list(x) && length(x) == 0),
    logical(1)
  )]
  
  if (length(non_empty_attrs) > 0) {
    cat("\n--- Attributes ---\n")
    print(non_empty_attrs)
  }
  
  invisible(object)
}

# Helper function for captured plots summary
.print_captured_plots_summary <- function(plots) {
  if (is.list(plots) && length(plots) > 0) {
    cat(sprintf("List of %d plot object(s)\n", length(plots)))
  } else {
    print(plots)
  }
}


#' Plot method for visstat objects
#'
#' This method can replay captured plots or report saved file paths.
#' Preserves all existing functionality while adding plot replay capability.
#'
#' @param x An object of class \code{"visstat"}, returned by \code{visstat()}.
#' @param replay Logical. If TRUE, attempts to replay captured plots. 
#'   If FALSE (default), reports file paths (existing behavior).
#' @param which Integer vector specifying which plots to display (by index).
#'   If NULL (default), all plots are considered.
#' @param ask Logical. If TRUE and multiple plots exist, asks user before 
#'   displaying each plot. Only relevant when replay=TRUE.
#' @param ... Currently unused. Included for S3 method compatibility.
#'
#' @return Invisibly returns \code{x}. Used for its side effect of displaying 
#'   plots or reporting file paths.
#' @method plot visstat
#' @export
plot.visstat <- function(x, replay = FALSE, which = NULL, ask = FALSE, ...) {
  
  paths <- attr(x, "plot_paths")
  captured_plots <- attr(x, "captured_plots")
  
  if (!replay) {
    if (is.null(paths) || length(paths) == 0) {
      message("No plot files recorded in object.")
      if (!is.null(captured_plots) && length(captured_plots) > 0) {
        message("Tip: Use plot(object, replay=TRUE) to replay ", length(captured_plots), " captured plots.")
      } else {
        message("Tip: Use graphicsoutput parameter in visstat() to save plots.")
      }
      return(invisible(x))
    }
    
    if (!is.null(which)) {
      if (any(which < 1 || which > length(paths))) {
        stop("'which' contains invalid indices. Valid range: 1 to ", length(paths))
      }
      paths <- paths[which]
    }
    
    if (!interactive()) {
      message("Plots saved (non-interactive session):")
      for (i in seq_along(paths)) message("  [", i, "] ", paths[i])
    } else {
      message("Plots saved during visstat() execution:")
      for (i in seq_along(paths)) message("  [", i, "] ", paths[i])
      if (!is.null(captured_plots) && length(captured_plots) > 0) {
        message("\nTip: Use plot(object, replay=TRUE) to replay captured plots.")
      }
    }
    
    return(invisible(x))
  }
  
  if (is.null(captured_plots) || length(captured_plots) == 0) {
    message("No captured plots found in visstat object.")
    message("This may occur when plots were saved to files or capture failed.")
    
    if (!is.null(paths) && length(paths) > 0) {
      message("\nSaved plot files are available:")
      for (i in seq_along(paths)) {
        exists_status <- if (file.exists(paths[i])) "" else 
          message("  [", i, "] ", exists_status, " ", paths[i])
      }
    }
    
    return(invisible(x))
  }
  
  if (!is.null(which)) {
    if (any(which < 1 || which > length(captured_plots))) {
      stop("'which' contains invalid indices. Valid range: 1 to ", length(captured_plots))
    }
    plots_to_replay <- captured_plots[which]
    indices_to_show <- which
  } else {
    plots_to_replay <- captured_plots
    indices_to_show <- seq_along(captured_plots)
  }
  
  num_plots <- length(plots_to_replay)
  message("Replaying ", num_plots, " captured plot(s)...")
  
  if (num_plots > 1) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    
    if (num_plots == 2) {
      par(mfrow = c(1, 2))
    } else if (num_plots <= 4) {
      par(mfrow = c(2, 2))
    }
  }
  
  success_count <- 0
  for (i in seq_along(plots_to_replay)) {
    plot_index <- indices_to_show[i]
    
    if (ask && interactive() && i > 1) {
      response <- readline(prompt = paste0("Replay plot ", plot_index, "? [y/n]: "))
      if (tolower(substr(response, 1, 1)) != "y") next
    }
    
    tryCatch({
      replayPlot(plots_to_replay[[i]])
      if (num_plots > 1) {
        mtext(paste("Plot", plot_index, "of", length(captured_plots)), 
              side = 3, line = 0.5, cex = 0.8, col = "darkgray")
      }
      success_count <- success_count + 1
    }, error = function(e) {
      warning("Could not replay plot ", plot_index, ": ", e$message)
    })
    
    if (ask && interactive() && i < num_plots) {
      readline("Press Enter to continue...")
    }
  }
  
  message("Successfully replayed ", success_count, " out of ", num_plots, " plots.")
  invisible(x)
}