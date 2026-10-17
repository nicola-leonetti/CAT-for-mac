# Purpose   : replacements for the functions CAT used from packages that are
#             not available on current macOS / R versions:
#               rPref    psel(), high(), low()         (MLR-DOE/Pareto front)
#               StatDA   edaplot()                      (Univariate/EDA plot)
#               rggobi   ggobi(), displays(), ...       (Exploration, via loon)
#               playwith playwith()                     (Plot Editing/2D)
#               zoom     zm() on the Quartz device
# Licence   : GPL-3 (same as CAT)

# ---------------------------------------------------------------------------
# rPref: Pareto preferences (only the operators used by DOE_Pareto.r)
# ---------------------------------------------------------------------------

high <- function(x) structure(list(as.numeric(x)), class = "cat_pref")
low <- function(x) structure(list(-as.numeric(x)), class = "cat_pref")
`*.cat_pref` <- function(e1, e2) structure(c(unclass(e1), unclass(e2)), class = "cat_pref")

# returns the rows of df that are not dominated (all criteria to maximise)
psel <- function(df, pref, ...) {
  df <- as.data.frame(df)
  S <- do.call(cbind, unclass(pref))
  n <- nrow(S)
  keep <- vapply(seq_len(n), function(i) {
    if (anyNA(S[i, ])) return(FALSE)
    dom <- apply(S, 1, function(o) !anyNA(o) && all(o >= S[i, ]) && any(o > S[i, ]))
    !any(dom)
  }, TRUE)
  df[keep, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# StatDA::edaplot (Filzmoser et al., GPL >= 3), unchanged
# ---------------------------------------------------------------------------

edaplot <- function(data, scatter = TRUE, box = TRUE, P.plot = TRUE, D.plot = TRUE,
                    P.main = paste("Histogram of", deparse(substitute(data))),
                    P.sub = NULL, P.xlab = deparse(substitute(data)), P.ylab = default,
                    P.ann = par("ann"), P.axes = TRUE, P.frame.plot = P.axes, P.log = FALSE,
                    P.logfine = c(2, 5, 10), P.xlim = NULL, P.cex.lab = 1.4, B.range = 1.5,
                    B.notch = FALSE, B.outline = TRUE, B.border = par("fg"), B.col = NULL,
                    B.pch = par("pch"), B.cex = 1, B.bg = NA, H.breaks = "Sturges",
                    H.freq = TRUE, H.include.lowest = TRUE, H.right = TRUE, H.density = NULL,
                    H.angle = 45, H.col = NULL, H.border = NULL, H.labels = FALSE,
                    S.pch = ".", S.col = par("col"), S.bg = NA, S.cex = 1, D.lwd = 1, D.lty = 1) {
  xhi <- hist(data, plot = FALSE, breaks = H.breaks, include.lowest = H.include.lowest,
              right = H.right)
  bxp <- boxplot(data, plot = FALSE, range = B.range, notch = B.notch, outline = B.outline)
  dens <- density(data)
  if (P.plot) {
    x <- xhi$breaks
    if (H.freq) {
      y <- xhi$counts
      default <- "Frequency"
      D.plot <- FALSE
    } else {
      y <- xhi$density
      default <- "Relative frequency"
    }
    h <- -max(y, dens$y) / 8
    a <- if (scatter && box) 2 else if ((scatter & !box) | (!scatter && box)) 1 else 0
    if (D.plot) {
      plot(x, c(y, a * h), type = "n", main = P.main, sub = P.sub, xlab = P.xlab,
           ylab = P.ylab, ann = P.ann, frame.plot = P.frame.plot,
           ylim = c(min(c(y, a * h)), max(y, dens$y)), xaxt = "n", yaxt = "n",
           xlim = P.xlim, cex.lab = P.cex.lab)
      if (P.axes) {
        ay <- axTicks(2)
        axis(2, at = ay[ay >= 0], labels = ay[ay >= 0])
        if (P.log) axis(1, at = log10(alog <- sort(c((10^(-50:50)) %*% t(P.logfine)))),
                        labels = alog)
        else axis(1)
      }
      lines(dens, lwd = D.lwd, lty = D.lty)
    } else {
      plot(x, c(y, a * h), type = "n", main = P.main, sub = P.sub, xlab = P.xlab,
           ylab = P.ylab, ann = P.ann, axes = P.axes, frame.plot = P.frame.plot,
           xlim = P.xlim, cex.lab = P.cex.lab)
    }
    hist(data, add = TRUE, axes = FALSE, breaks = H.breaks, freq = H.freq,
         include.lowest = H.include.lowest, right = H.right, density = H.density,
         angle = H.angle, col = H.col, border = H.border, labels = H.labels)
    if (scatter && box) {
      rect(xhi$breaks[1], h, xhi$breaks[length(xhi$breaks)], 0)
      points(data, runif(length(data), 0.1, 0.9) * h, pch = S.pch, col = S.col,
             bg = S.bg, cex = S.cex)
      rect(xhi$breaks[1], 2 * h, xhi$breaks[length(xhi$breaks)], 0)
      boxplot(data, add = TRUE, horizontal = TRUE, boxwex = -h * 1.2, at = h * 1.5,
              axes = FALSE, range = B.range, notch = B.notch, outline = B.outline,
              border = B.border, col = B.col, pch = B.pch, bg = B.bg, cex = B.cex,
              xlim = P.xlim)
    } else if (scatter && !box) {
      rect(xhi$breaks[1], h, xhi$breaks[length(xhi$breaks)], 0)
      points(data, runif(length(data), 0.1, 0.9) * h, pch = S.pch, col = S.col,
             bg = S.bg, cex = S.cex)
    } else if (!scatter && box) {
      rect(xhi$breaks[1], h, xhi$breaks[length(xhi$breaks)], 0)
      boxplot(data, add = TRUE, horizontal = TRUE, boxwex = -h * 1.2, at = h * 0.5,
              axes = FALSE, range = B.range, notch = B.notch, outline = B.outline,
              border = B.border, col = B.col, pch = B.pch, bg = B.bg, cex = B.cex,
              xlim = P.xlim)
    }
  }
  invisible(list(H = xhi, B = bxp))
}

# ---------------------------------------------------------------------------
# rggobi -> loon (interactive, linked plots with brushing, built on Tcl/Tk)
# ---------------------------------------------------------------------------

.cat_ggobi <- new.env()

ggobi <- function(data, ...) {
  name <- deparse(substitute(data), nlines = 1L)
  # the CAT script passes a temporary copy: show what the user typed
  v <- get0("variable", envir = .GlobalEnv)
  if (is.list(v) && is.character(v$input) && length(v$input) == 1) name <- v$input
  df <- as.data.frame(data, stringsAsFactors = FALSE)
  num <- df[vapply(df, is.numeric, TRUE)]
  if (!ncol(num)) stop("the data set has no numeric columns")
  grp <- df[!vapply(df, is.numeric, TRUE)]
  color <- if (ncol(grp)) as.factor(grp[[1]]) else NULL
  if (!requireNamespace("loon", quietly = TRUE)) {
    dev.new(title = paste("Exploration -", name))
    pairs(num, col = if (is.null(color)) 1 else as.integer(color))
    return(invisible(NULL))
  }
  colargs <- if (is.null(color)) list() else list(color = color)
  if (ncol(num) <= 6 && ncol(num) >= 2) {
    utils::capture.output(
      p <- do.call(loon::l_pairs, c(list(num, showHistograms = TRUE, linkingGroup = "CAT",
                                         title = paste("Exploration -", name)), colargs)))
    plots <- list(p)   # the whole scatter plot matrix is one display
  } else {
    p <- .cat_explorer(num, name, colargs)
    plots <- list(p)
  }
  g <- structure(list(plots = plots, data = df), class = "cat_ggobi")
  .cat_ggobi$last <- g
  message("Exploration window opened (loon): drag to brush points, the Loon Inspector ",
          "window changes colours, sizes, selected variables, ...")
  invisible(g)
}

# one scatter plot with the choice of the variables on the axes (the GGobi
# default view) for data sets with many variables
.cat_explorer <- function(num, name, colargs) {
  vars <- colnames(num)
  if (is.null(vars)) vars <- paste0("V", seq_len(ncol(num)))
  colnames(num) <- vars
  top <- tktoplevel()
  tkwm.title(top, paste("Exploration -", name))
  ctl <- ttkframe(top, padding = 6)
  tkpack(ctl, side = "top", fill = "x")
  xv <- tclVar(vars[1]); yv <- tclVar(vars[min(2, length(vars))])
  tkpack(ttklabel(ctl, text = "X:"), side = "left")
  cx <- ttkcombobox(ctl, textvariable = xv, values = as.tclObj(vars, drop = FALSE),
                    state = "readonly", width = 18)
  tkpack(cx, side = "left", padx = 4)
  tkpack(ttklabel(ctl, text = "Y:"), side = "left")
  cy <- ttkcombobox(ctl, textvariable = yv, values = as.tclObj(vars, drop = FALSE),
                    state = "readonly", width = 18)
  tkpack(cy, side = "left", padx = 4)
  p <- do.call(loon::l_plot, c(list(parent = top, x = num[[1]],
                                     y = num[[min(2, ncol(num))]],
                                     xlabel = vars[1], ylabel = vars[min(2, length(vars))],
                                     linkingGroup = "CAT", showScales = TRUE,
                                     itemLabel = rownames(num), showItemLabels = TRUE),
                                colargs))
  tkpack(p, side = "top", fill = "both", expand = TRUE)
  upd <- function() {
    x <- tclvalue(xv); y <- tclvalue(yv)
    loon::l_configure(p, x = num[[x]], y = num[[y]], xlabel = x, ylabel = y)
    loon::l_scaleto_world(p)
  }
  tkbind(cx, "<<ComboboxSelected>>", upd)
  tkbind(cy, "<<ComboboxSelected>>", upd)
  p
}

displays <- function(gobi) {
  if (is.null(gobi)) gobi <- .cat_ggobi$last
  if (is.null(gobi)) stop("no exploration window: use Exploration/Load Dataset first")
  gobi$plots
}

ggobi_get <- function(...) .cat_ggobi$last

ggobi_display_save_picture <- function(display, path = "ggobi.png", filetype = "png",
                                       plot.only = TRUE) {
  path <- normalizePath(path, mustWork = FALSE)
  grob <- tryCatch(loon::loonGrob(display), error = function(e)
    stop("the exploration window has been closed: use Exploration/Load Dataset again",
         call. = FALSE))
  switch(filetype,
         pdf = grDevices::pdf(path, width = 8, height = 8),
         ps = , eps = grDevices::postscript(path, width = 8, height = 8),
         bmp = grDevices::bmp(path, width = 8, height = 8, units = "in", res = 150, type = "quartz"),
         jpg = , jpeg = grDevices::jpeg(path, width = 8, height = 8, units = "in", res = 150,
                                        quality = 95, type = "quartz"),
         grDevices::png(path, width = 8, height = 8, units = "in", res = 150, type = "quartz"))
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::grid.draw(grob)
  message("Picture saved in ", path)
  invisible(path)
}

# ---------------------------------------------------------------------------
# playwith -> small plot editor for lattice plots
# ---------------------------------------------------------------------------

playwith <- function(expr, title = NULL, ...) {
  obj <- expr
  if (!inherits(obj, "trellis")) {
    print(obj)
    return(invisible(obj))
  }
  name <- if (is.null(title)) deparse(substitute(expr), nlines = 1L) else title
  dev.new(title = paste("Plot editing -", name))
  dev <- dev.cur()
  print(obj)
  .cat_plot_editor(obj, dev, name)
}

.cat_plot_editor <- function(obj, dev, name) {
  orig <- obj
  cur <- obj
  top <- tktoplevel()
  tkwm.title(top, paste("Plot editing -", name))
  f <- ttkframe(top, padding = 10)
  tkpack(f, fill = "both", expand = TRUE)
  lab <- function(x) if (is.null(x)) "" else if (is.list(x)) paste(x[[1]], collapse = " ")
                     else paste(as.character(x), collapse = " ")
  lim <- function(x, k) if (is.numeric(x) && length(x) == 2) format(signif(x[k], 6)) else ""
  vars <- list(main = tclVar(lab(obj$main)), xlab = tclVar(lab(obj$xlab)),
               ylab = tclVar(lab(obj$ylab)),
               xmin = tclVar(lim(obj$x.limits, 1)), xmax = tclVar(lim(obj$x.limits, 2)),
               ymin = tclVar(lim(obj$y.limits, 1)), ymax = tclVar(lim(obj$y.limits, 2)),
               cex = tclVar("1"), fontsize = tclVar("12"))
  row <- 0
  add <- function(label, w) {
    tkgrid(ttklabel(f, text = label), row = row, column = 0, sticky = "w", pady = 2)
    tkgrid(w, row = row, column = 1, columnspan = 3, sticky = "ew", pady = 2)
    row <<- row + 1
  }
  add("Title", ttkentry(f, textvariable = vars$main, width = 36))
  add("X label", ttkentry(f, textvariable = vars$xlab, width = 36))
  add("Y label", ttkentry(f, textvariable = vars$ylab, width = 36))
  limrow <- function(label, a, b) {
    tkgrid(ttklabel(f, text = label), row = row, column = 0, sticky = "w", pady = 2)
    tkgrid(ttkentry(f, textvariable = a, width = 12), row = row, column = 1, sticky = "w")
    tkgrid(ttklabel(f, text = "to"), row = row, column = 2)
    tkgrid(ttkentry(f, textvariable = b, width = 12), row = row, column = 3, sticky = "w")
    row <<- row + 1
  }
  limrow("X range", vars$xmin, vars$xmax)
  limrow("Y range", vars$ymin, vars$ymax)
  add("Point size", ttkspinbox(f, from = 0.2, to = 5, increment = 0.1,
                               textvariable = vars$cex, width = 6))
  add("Font size", ttkspinbox(f, from = 6, to = 30, increment = 1,
                              textvariable = vars$fontsize, width = 6))
  tkgrid.columnconfigure(f, 1, weight = 1)
  tkgrid.columnconfigure(f, 3, weight = 1)

  num <- function(v) suppressWarnings(as.numeric(tclvalue(v)))
  labelled <- integer(0)
  point_labels <- function(pa) {
    labs <- names(pa$x)
    if (is.null(labs) && !is.null(pa$subscripts)) labs <- as.character(pa$subscripts)
    if (is.null(labs)) labs <- as.character(seq_along(pa$x))
    labs
  }
  build <- function() {
    o <- orig
    args <- list(main = tclvalue(vars$main), xlab = tclvalue(vars$xlab),
                 ylab = tclvalue(vars$ylab))
    xl <- c(num(vars$xmin), num(vars$xmax))
    yl <- c(num(vars$ymin), num(vars$ymax))
    if (!anyNA(xl) && is.numeric(o$x.limits)) args$xlim <- xl
    if (!anyNA(yl) && is.numeric(o$y.limits)) args$ylim <- yl
    o <- do.call(update, c(list(o), args))
    cex <- num(vars$cex); fs <- num(vars$fontsize)
    o$par.settings <- modifyList(as.list(o$par.settings),
      list(plot.symbol = list(cex = if (is.na(cex)) 1 else cex),
           superpose.symbol = list(cex = rep(if (is.na(cex)) 1 else cex, 7)),
           fontsize = list(text = if (is.na(fs)) 12 else fs, points = 8)))
    if (length(labelled)) {
      old_panel <- o$panel
      if (is.character(old_panel)) old_panel <- get(old_panel, mode = "function")
      sel <- labelled
      labs <- point_labels(o$panel.args[[1]])
      o$panel <- function(x, y, ...) {
        old_panel(x, y, ...)
        if (isTRUE(lattice::panel.number() == 1))
          lattice::panel.text(x[sel], y[sel], labs[sel], pos = 3, cex = 0.8)
      }
    }
    o
  }
  redraw <- function() {
    if (!(dev %in% dev.list())) {
      dev.new(title = paste("Plot editing -", name))
      dev <<- dev.cur()
    }
    dev.set(dev)
    cur <<- build()
    print(cur)
    .gd_render(dev, force = TRUE)
  }
  zoom <- function() {
    redraw()
    pts <- .gd_clicks(dev, 2, "click two opposite corners of the zoom area")
    if (nrow(pts) < 2) return()
    xy <- .gd_panel_coords(dev, cur, pts)
    xs <- sort(xy$x); ys <- sort(xy$y)
    tclvalue(vars$xmin) <- format(signif(xs[1], 6)); tclvalue(vars$xmax) <- format(signif(xs[2], 6))
    tclvalue(vars$ymin) <- format(signif(ys[1], 6)); tclvalue(vars$ymax) <- format(signif(ys[2], 6))
    redraw()
  }
  identify_pts <- function() {
    redraw()
    pa <- cur$panel.args[[1]]
    if (is.null(pa$x) || is.null(pa$y) || !is.numeric(pa$x) || !is.numeric(pa$y)) {
      message("Identify works on scatter plots only")
      return()
    }
    pts <- .gd_clicks(dev, 1000, "click on the points to label them, Esc to finish")
    if (!nrow(pts)) return()
    xy <- .gd_panel_coords(dev, cur, pts)
    sx <- diff(xy$xlim); sy <- diff(xy$ylim)
    for (k in seq_along(xy$x)) {
      dist <- sqrt(((pa$x - xy$x[k]) / sx)^2 + ((pa$y - xy$y[k]) / sy)^2)
      i <- which.min(dist)
      if (length(i) && dist[i] < 0.03) labelled <<- union(labelled, i)
    }
    labs <- point_labels(pa)
    message("Identified: ", paste(labs[labelled], collapse = ", "))
    redraw()
  }
  reset <- function() {
    labelled <<- integer(0)
    tclvalue(vars$main) <- lab(orig$main); tclvalue(vars$xlab) <- lab(orig$xlab)
    tclvalue(vars$ylab) <- lab(orig$ylab)
    tclvalue(vars$xmin) <- lim(orig$x.limits, 1); tclvalue(vars$xmax) <- lim(orig$x.limits, 2)
    tclvalue(vars$ymin) <- lim(orig$y.limits, 1); tclvalue(vars$ymax) <- lim(orig$y.limits, 2)
    tclvalue(vars$cex) <- "1"; tclvalue(vars$fontsize) <- "12"
    redraw()
  }
  save_as <- function() {
    file <- tclvalue(tkgetSaveFile(title = "Save plot", initialfile = "plot.pdf",
                                   filetypes = "{{PDF} {.pdf}} {{PNG} {.png}} {{JPEG} {.jpg}} {{TIFF} {.tif}}"))
    if (!nzchar(file)) return()
    ext <- tolower(tools::file_ext(file))
    o <- build()
    switch(ext,
           png = png(file, width = 8, height = 7, units = "in", res = 300),
           jpg = , jpeg = jpeg(file, width = 8, height = 7, units = "in", res = 300, quality = 95),
           tif = , tiff = tiff(file, width = 8, height = 7, units = "in", res = 300),
           pdf(file, width = 8, height = 7))
    print(o)
    dev.off()
    if (dev %in% dev.list()) dev.set(dev)
    message("Plot saved in ", file)
  }
  safe <- function(f) {
    force(f)
    function() tryCatch(f(), error = function(e) .cat_report_error(e))
  }
  bf <- ttkframe(f)
  tkgrid(bf, row = row, column = 0, columnspan = 4, pady = c(10, 0), sticky = "ew")
  for (b in list(list("Apply", redraw), list("Zoom", zoom), list("Identify", identify_pts),
                 list("Reset", reset), list("Save as...", save_as)))
    tkpack(ttkbutton(bf, text = b[[1]], command = safe(b[[2]])), side = "left", padx = 2)
  done <- tclVar(0)
  tkpack(ttkbutton(bf, text = "Close", command = function() tclvalue(done) <- 1),
         side = "right", padx = 2)
  tkbind(top, "<Return>", safe(redraw))
  tcl("wm", "protocol", top, "WM_DELETE_WINDOW", function() tclvalue(done) <- 1)
  tkwait.variable(done)
  tkdestroy(top)
  invisible(cur)
}

