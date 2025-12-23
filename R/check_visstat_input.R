check_visstat_input <- function(x, y=NULL, ...,data=NULL) {
  dots <- list(...)
  dot_names <- names(dots)
  # Extract unnamed arguments: if no names at all, all are unnamed
  # If names exist, unnamed ones have empty string as name
  if (is.null(dot_names)) {
    unnamed <- dots
  } else {
    unnamed <- dots[dot_names == ""]
  }
  total_positional <- 2 + length(unnamed)
  
  
  
  allowed_classes <- c("numeric", "integer", "factor", "character")
  
  
  # Case 0: Formula interface - visstat(y ~ x, data = dataframe)
  if (inherits(x, "formula")) {
    if (is.null(data)) {
      stop("Formula input requires a 'data' argument.")
    }
    
    vars <- all.vars(x)
    if (length(vars) != 2) {
      stop("Formula must be of the form 'y ~ x'.")
    }
    
    yvar <- vars[1]
    xvar <- vars[2]
    
    # Check if columns exist in dataframe
    missing_cols <- vars[!vars %in% names(data)]
    if (length(missing_cols) > 0) {
      stop(paste("Column(s) not found in dataframe:", paste(missing_cols, collapse = ", ")))
    }
    
    # Check if columns have allowed classes
    for (col in vars) {
      if (!inherits(data[[col]], allowed_classes)) {
        stop(paste("Column", col, "must be of class:", paste(allowed_classes, collapse = ", ")))
      }
    }
    
    return(invisible(TRUE))  # valid formula input
  }
  
  # Case 1: Formula interface - visstat(y ~ x, data = dataframe)
  # Note: Formula validation is handled in the main visstat() function
  # before calling check_visstat_input, so we don't need to check it here
  
  # Case 2: Standard form - visstat(x_vector, y_vector)
  if (total_positional == 2 &&
      inherits(x, allowed_classes) &&
      inherits(y, allowed_classes)) {
    return(invisible(TRUE))  # valid vector input
  }
  
  # Case 3: Legacy form - visstat(dataframe, "y_column", "x_column")
  if (total_positional == 3 &&
      is.data.frame(x) &&
      is.character(y) &&
      length(y) == 1 &&
      length(unnamed) > 0 &&
      is.character(unnamed[[1]]) &&
      length(unnamed[[1]]) == 1) {
    
    # Check if columns exist in the dataframe
    columns <- c(y, unnamed[[1]])
    missing_cols <- columns[!columns %in% names(x)]
    if (length(missing_cols) > 0) {
      stop(paste("Column(s) not found in dataframe:", paste(missing_cols, collapse = ", ")))
    }
    
    # Check if columns have allowed classes
    for (col in columns) {
      if (!inherits(x[[col]], allowed_classes)) {
        stop(paste("Column", col, "must be of class:", paste(allowed_classes, collapse = ", ")))
      }
    }
    
    return(invisible(TRUE))  # valid data.frame input
  }
  
  # If none of the valid cases match, provide helpful error message
  stop("Invalid input. Use one of the following forms:
       1. visstat(y ~ x, data = dataframe)  [Formula handled before this function]
       2. visstat(x_vector, y_vector)
       3. visstat(dataframe, 'y_column', 'x_column')
       
       All variables must be of class: ", paste(allowed_classes, collapse = ", "))
}