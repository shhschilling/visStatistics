visstat_graphics_par <- function(plot_args, defaults = list()) {
  if (is.null(plot_args)) {
    plot_args <- list()
  }

  par_names <- names(formals(graphics::par))
  par_args <- plot_args[intersect(names(plot_args), par_names)]
  if (length(defaults) > 0 || length(par_args) > 0) {
    graphics::par(modifyList(defaults, par_args))
  }

  invisible(plot_args[setdiff(names(plot_args), par_names)])
}

visstat_graphics_arg <- function(plot_args, name, default = NULL) {
  if (!is.null(plot_args) && !is.null(plot_args[[name]])) {
    plot_args[[name]]
  } else {
    default
  }
}
