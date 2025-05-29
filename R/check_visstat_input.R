check_visstat_input <- function(x, y, ...) {
  dots <- list(...)
  dot_names <- names(dots)
  unnamed <- if (is.null(dot_names)) dots else dots[dot_names == ""]
  total_positional <- 2 + length(unnamed)

  allowed_classes <- c("numeric", "integer", "factor","character")

  if (total_positional == 2 &&
      inherits(x, allowed_classes) &&
      inherits(y, allowed_classes)) {
    return(invisible(TRUE))  # valid vector input
  }

  if (total_positional == 3 &&
      is.data.frame(x) &&
      is.character(y) &&
      is.character(unnamed[[1]])) {
    return(invisible(TRUE))  # valid data.frame input
  }

  stop("Invalid input. Use visstat(x, y, ...) or visstat(data, 'y', 'x', ...), 
       with x and y of class numeric, integer,  factor or character")
}