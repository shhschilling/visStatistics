# Settable base-graphics parameters.
#
# Hard-coded rather than queried, because `names(formals(graphics::par))` is
# only c("...", "no.readonly") -- intersecting against it matches nothing and
# silently discards every parameter the caller supplies -- while
# `names(graphics::par(no.readonly = TRUE))` needs an open device and would
# open one as a side effect when called before any plot exists.
visstat_par_names <- c(
  "adj", "ann", "ask", "bg", "bty", "cex", "cex.axis", "cex.lab",
  "cex.main", "cex.sub", "col", "col.axis", "col.lab", "col.main",
  "col.sub", "crt", "err", "family", "fg", "fig", "fin", "font",
  "font.axis", "font.lab", "font.main", "font.sub", "lab", "las", "lend",
  "lheight", "ljoin", "lmitre", "lty", "lwd", "mai", "mar", "mex",
  "mfcol", "mfg", "mfrow", "mgp", "mkh", "new", "oma", "omd", "omi",
  "pch", "pin", "plt", "ps", "pty", "smo", "srt", "tck", "tcl", "usr",
  "xaxp", "xaxs", "xaxt", "xlog", "xpd", "yaxp", "yaxs", "yaxt",
  "ylbias", "ylog"
)

# Per-plot graphical arguments the package honours through
# visstat_graphics_arg() but that par() does not accept.
visstat_extra_graphics_args <- c("cex.names", "main", "xlab", "ylab")


visstat_graphics_par <- function(plot_args, defaults = list()) {
  if (is.null(plot_args)) {
    plot_args <- list()
  }

  par_args <- plot_args[intersect(names(plot_args), visstat_par_names)]
  if (length(defaults) > 0 || length(par_args) > 0) {
    graphics::par(modifyList(defaults, par_args))
  }

  invisible(plot_args[setdiff(names(plot_args), visstat_par_names)])
}

visstat_graphics_arg <- function(plot_args, name, default = NULL) {
  if (!is.null(plot_args) && !is.null(plot_args[[name]])) {
    plot_args[[name]]
  } else {
    default
  }
}


# Reject unrecognised arguments passed through `...`.
#
# `visstat()` declares `...` ahead of its named parameters, so a misspelled
# argument name never reaches the parameter it was meant for: it lands in the
# dots, is treated as a graphical parameter, and is then dropped by the filter
# in visstat_graphics_par(). The call then silently runs under the defaults the
# caller believed they had overridden -- for instance `group_tset = "welch"`
# leaves the automatic route in place. Fail instead, and name the nearest
# legitimate argument.
visstat_check_dots <- function(plot_args, call_args = NULL) {
  if (length(plot_args) == 0) {
    return(invisible(NULL))
  }

  allowed <- c(visstat_par_names, visstat_extra_graphics_args)
  unknown <- setdiff(names(plot_args), allowed)
  if (length(unknown) == 0) {
    return(invisible(NULL))
  }

  if (is.null(call_args)) {
    call_args <- setdiff(names(formals(visstat)), "...")
  }
  candidates <- unique(c(call_args, "route", allowed))

  described <- vapply(unknown, function(nm) {
    distances <- utils::adist(nm, candidates, ignore.case = TRUE)[1, ]
    closest <- candidates[which.min(distances)]
    tolerance <- max(2L, as.integer(ceiling(nchar(nm) / 3)))
    if (min(distances) <= tolerance) {
      paste0("'", nm, "' (did you mean '", closest, "'?)")
    } else {
      paste0("'", nm, "'")
    }
  }, character(1))

  stop(
    "Unrecognised argument", if (length(unknown) > 1L) "s" else "", ": ",
    paste(described, collapse = ", "),
    ". Named arguments must match a visstat() parameter or a base graphics ",
    "parameter.",
    call. = FALSE
  )
}
