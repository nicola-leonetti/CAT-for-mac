# Purpose   : graphics windows for CAT on macOS.
#             R's Quartz screen device and Tcl/Tk (used by all CAT dialogs)
#             cannot share the same process: after a Quartz window has been
#             closed, the next Tk window crashes R (a long-standing problem of
#             the native macOS Tk). Plots are therefore drawn on an invisible
#             recording device (pdf(NULL) with display list) and shown in Tk
#             windows, re-rendered (off-screen Quartz bitmap) whenever the plot
#             or the window size changes.
#             Like the Windows R graphics window, each window has a File menu
#             to save the plot (PDF, PNG, JPEG, TIFF) or copy it to the
#             clipboard. Closing the window closes the device and vice versa.
# Licence   : GPL-3 (same as CAT)

.gd <- new.env()
.gd$wins <- list()        # device number (character) -> window state
.gd$last_render <- 0

cat_device <- function(width = 7, height = 7, pointsize = 12, title = NULL, ...) {
  width <- if (is.numeric(width) && !is.na(width)) width else 7
  height <- if (is.numeric(height) && !is.na(height)) height else 7
  grDevices::pdf(file = NULL, width = width, height = height, pointsize = pointsize)
  grDevices::dev.control("enable")
  d <- grDevices::dev.cur()
  .gd_window(d, title, width, height)
  invisible(d)
}

# the Windows/X11/Quartz screen devices map to the same window
quartz <- function(title = NULL, width = 7, height = 7, pointsize = 12, ...)
  cat_device(width = width, height = height, pointsize = pointsize, title = title)
x11 <- X11 <- quartz

.gd_title <- function(d, title) {
  t <- if (is.null(title) || !nzchar(title)) "R Graphics" else as.character(title)
  t <- gsub("%d", d, t, fixed = TRUE)
  active <- identical(as.integer(grDevices::dev.cur()), as.integer(d))
  sprintf("%s: Device %d%s", t, as.integer(d), if (active) " (ACTIVE)" else "")
}

.gd_window <- function(d, title, width, height) {
  key <- as.character(d)
  w <- .gd$wins[[key]]
  if (!is.null(w) && as.character(tkwinfo("exists", w$top)) == "1") {
    # device number reused by a new device: reuse its window
    w$title <- title
    w$sig <- NULL
    .gd$wins[[key]] <- w
    tkwm.title(w$top, .gd_title(d, title))
    return(invisible(w))
  }
  top <- tktoplevel()
  tkwm.withdraw(top)
  wpx <- as.integer(width * 72); hpx <- as.integer(height * 72)
  img <- tkimage.create("photo", width = wpx, height = hpx)
  lab <- tklabel(top, image = img, borderwidth = 0, highlightthickness = 0,
                 background = "white", width = wpx, height = hpx)
  tkpack(lab, fill = "both", expand = TRUE)
  w <- list(top = top, img = img, lab = lab, title = title, sig = NULL, size = c(0L, 0L),
            pending = FALSE)
  .gd$wins[[key]] <- w

  mb <- tkmenu(top)
  tkconfigure(top, menu = mb)
  fm <- tkmenu(mb, tearoff = FALSE)
  tkadd(mb, "cascade", label = "File", menu = fm)
  for (fmt in c("PDF", "PNG", "JPEG", "TIFF")) local({
    ff <- fmt
    tkadd(fm, "command", label = paste0("Save as ", ff, "..."),
          command = function() .gd_save_dialog(d, ff))
  })
  tkadd(fm, "separator")
  tkadd(fm, "command", label = "Copy to Clipboard", accelerator = "Command-C",
        command = function() .gd_copy(d))
  tkadd(fm, "separator")
  tkadd(fm, "command", label = "Close", accelerator = "Command-W",
        command = function() .gd_close_window(d))
  ctx <- tkmenu(top, tearoff = FALSE)
  tkadd(ctx, "command", label = "Copy to Clipboard", command = function() .gd_copy(d))
  tkadd(ctx, "command", label = "Save as PDF...", command = function() .gd_save_dialog(d, "PDF"))
  tkadd(ctx, "command", label = "Save as PNG...", command = function() .gd_save_dialog(d, "PNG"))
  popup <- function(X, Y) tcl("tk_popup", ctx, X, Y)
  w$popup <- popup
  .gd$wins[[key]] <- w
  tkbind(lab, "<Button-2>", popup)
  tkbind(lab, "<Button-3>", popup)
  tkbind(lab, "<Control-Button-1>", popup)
  tkbind(top, "<Command-c>", function() .gd_copy(d))
  tkbind(top, "<Command-w>", function() .gd_close_window(d))
  tkbind(lab, "<Configure>", function() .gd_schedule(d))
  tcl("wm", "protocol", top, "WM_DELETE_WINDOW", function() .gd_close_window(d))
  tkwm.title(top, .gd_title(d, title))
  .gd_place(top, length(.gd$wins))
  tkwm.deiconify(top)
  invisible(w)
}

# cascade new windows to the right of the CAT window
.gd_place <- function(top, n) {
  sw <- as.integer(tkwinfo("screenwidth", top))
  x <- max(40L, sw - 560L - 30L * ((n - 1L) %% 8L))
  y <- 60L + 30L * ((n - 1L) %% 8L)
  tkwm.geometry(top, sprintf("+%d+%d", x, y))
}

.gd_schedule <- function(d) {
  key <- as.character(d)
  w <- .gd$wins[[key]]
  if (is.null(w) || isTRUE(w$pending)) return(invisible())
  w$pending <- TRUE
  .gd$wins[[key]] <- w
  tcl("after", 120, function() {
    w <- .gd$wins[[key]]
    if (is.null(w)) return()
    w$pending <- FALSE
    .gd$wins[[key]] <- w
    .gd_render(d, force = TRUE)
  })
  invisible()
}

.gd_is_open <- function(d) as.character(d) %in% as.character(grDevices::dev.list())

.gd_record <- function(d) {
  cur <- grDevices::dev.cur()
  if (cur != d) grDevices::dev.set(d)
  on.exit(if (cur != d && cur > 1 && .gd_is_open(cur)) grDevices::dev.set(cur))
  tryCatch(grDevices::recordPlot(), error = function(e) NULL)
}

# renders device d into its window (only if the plot or the size changed)
.gd_render <- function(d, force = FALSE) {
  key <- as.character(d)
  w <- .gd$wins[[key]]
  if (is.null(w) || as.character(tkwinfo("exists", w$top)) != "1") return(invisible())
  if (!.gd_is_open(d)) return(invisible())
  sz <- .gd_size(d)
  W <- sz[1]; H <- sz[2]
  rp <- .gd_record(d)
  if (is.null(rp)) return(invisible())
  same <- identical(w$size, c(W, H)) && !is.null(w$sig) && identical(w$sig, rp[[1]])
  if (same && !force) return(invisible())
  file <- tempfile(fileext = ".png")
  ok <- .gd_replay_to(rp, file, "png", W, H)
  if (ok) tkconfigure(w$img, file = file, width = 0, height = 0)
  unlink(file)
  w$sig <- rp[[1]]
  w$size <- c(W, H)
  .gd$wins[[key]] <- w
  invisible()
}

# replays a recorded plot on a file device; size in points (1/72 inch)
.gd_replay_to <- function(rp, file, type, W, H, res = 72) {
  cur <- grDevices::dev.cur()
  opened <- tryCatch({
    switch(type,
           png = grDevices::png(file, width = W * res / 72, height = H * res / 72, res = res,
                                type = "quartz", bg = "white"),
           jpeg = grDevices::jpeg(file, width = W * res / 72, height = H * res / 72, res = res,
                                  type = "quartz", quality = 95, bg = "white"),
           tiff = grDevices::tiff(file, width = W * res / 72, height = H * res / 72, res = res,
                                  type = "quartz", bg = "white"),
           pdf = grDevices::pdf(file, width = W / 72, height = H / 72))
    grDevices::dev.cur()
  }, error = function(e) NULL)
  if (is.null(opened)) return(FALSE)
  ok <- tryCatch({
    if (length(rp[[1]])) suppressWarnings(grDevices::replayPlot(rp)) else graphics::plot.new()
    TRUE
  }, error = function(e) FALSE)
  grDevices::dev.off(opened)
  if (cur > 1 && .gd_is_open(cur)) grDevices::dev.set(cur)
  ok
}

.gd_refresh_all <- function() {
  # windows of devices closed from R
  for (key in names(.gd$wins)) {
    if (!.gd_is_open(key)) {
      w <- .gd$wins[[key]]
      .gd$wins[[key]] <- NULL
      try(tkdestroy(w$top), silent = TRUE)
    }
  }
  for (key in names(.gd$wins)) try(.gd_render(as.integer(key)), silent = TRUE)
  .gd_titles()
  .gd$last_render <- proc.time()[["elapsed"]]
  invisible()
}

.gd_titles <- function() {
  for (key in names(.gd$wins)) {
    w <- .gd$wins[[key]]
    t <- .gd_title(as.integer(key), w$title)
    if (!identical(t, w$shown_title)) {
      try(tkwm.title(w$top, t), silent = TRUE)
      w$shown_title <- t
      .gd$wins[[key]] <- w
    }
  }
}

.gd_close_window <- function(d) {
  key <- as.character(d)
  w <- .gd$wins[[key]]
  .gd$wins[[key]] <- NULL
  if (.gd_is_open(d)) grDevices::dev.off(d)
  if (!is.null(w)) try(tkdestroy(w$top), silent = TRUE)
  invisible()
}

.gd_size <- function(d) {
  w <- .gd$wins[[as.character(d)]]
  W <- as.integer(tkwinfo("width", w$lab)); H <- as.integer(tkwinfo("height", w$lab))
  if (W < 20 || H < 20) {   # window not on screen yet: its requested size
    W <- as.integer(tclvalue(tkcget(w$lab, "-width")))
    H <- as.integer(tclvalue(tkcget(w$lab, "-height")))
  }
  c(W, H)
}

.gd_save_dialog <- function(d, fmt) {
  ext <- switch(fmt, PDF = ".pdf", PNG = ".png", JPEG = ".jpg", TIFF = ".tif")
  w <- .gd$wins[[as.character(d)]]
  f <- tclvalue(tkgetSaveFile(parent = w$top, title = paste("Save plot as", fmt),
                              initialdir = getwd(), initialfile = paste0("Rplot", ext),
                              defaultextension = ext))
  if (!nzchar(f)) return(invisible())
  sz <- .gd_size(d)
  rp <- .gd_record(d)
  ok <- .gd_replay_to(rp, f, tolower(fmt), sz[1], sz[2], res = if (fmt == "PDF") 72 else 300)
  if (ok) message("Plot saved in ", f) else message("The plot could not be saved")
  invisible()
}

.gd_copy <- function(d) {
  sz <- .gd_size(d)
  f <- tempfile(fileext = ".png")
  if (.gd_replay_to(.gd_record(d), f, "png", sz[1], sz[2], res = 144)) {
    system2("osascript", c("-e", shQuote(sprintf(
      "set the clipboard to (read (POSIX file \"%s\") as «class PNGf»)", f))),
      stdout = FALSE, stderr = FALSE)
  }
  unlink(f)
  invisible()
}

# Sys.sleep() is used by CAT to animate plots (3D rotation): show each frame
Sys.sleep <- function(time) {
  if (length(.gd$wins)) {
    .gd_refresh_all()
    tcl("update")
  }
  base::Sys.sleep(time)
}

# ---------------------------------------------------------------------------
# mouse input on the plot windows (locator, identify, zoom)
# ---------------------------------------------------------------------------

# waits for up to n clicks on the window of device d; Esc or a right click
# ends earlier. Returns a 2-column matrix of positions in points from the
# top-left corner of the plot.
.gd_clicks <- function(d, n, prompt = NULL) {
  w <- .gd$wins[[as.character(d)]]
  if (is.null(w)) stop("the plot window is not open")
  .gd_render(d)
  pts <- matrix(numeric(0), ncol = 2)
  done <- tclVar(0)
  old_title <- tclvalue(tkwm.title(w$top))
  tkwm.title(w$top, paste(old_title, "-", prompt %||% "click on the plot, Esc to finish"))
  tkconfigure(w$lab, cursor = "crosshair")
  tkbind(w$lab, "<ButtonPress-1>", function(x, y) {
    pts <<- rbind(pts, c(as.numeric(x), as.numeric(y)))
    if (nrow(pts) >= n) tclvalue(done) <- 1
  })
  stop_fun <- function() tclvalue(done) <- 1
  tkbind(w$top, "<Escape>", stop_fun)
  tkbind(w$lab, "<Button-2>", stop_fun)
  tkbind(w$lab, "<Button-3>", stop_fun)
  tcl("wm", "protocol", w$top, "WM_DELETE_WINDOW", stop_fun)
  tkraise(w$top)
  tkfocus(w$top)
  tkwait.variable(done)
  if (as.character(tkwinfo("exists", w$top)) == "1") {
    tkbind(w$lab, "<ButtonPress-1>", "")
    tkbind(w$top, "<Escape>", "")
    tkbind(w$lab, "<Button-2>", w$popup)
    tkbind(w$lab, "<Button-3>", w$popup)
    tkconfigure(w$lab, cursor = "")
    tkwm.title(w$top, old_title)
    tcl("wm", "protocol", w$top, "WM_DELETE_WINDOW", function() .gd_close_window(d))
  }
  pts
}

# runs f() with the plot of device d replayed on an invisible device of the
# same size as the window (so that device coordinates = window points)
.gd_with_replay <- function(d, f) {
  sz <- .gd_size(d)
  rp <- .gd_record(d)
  cur <- grDevices::dev.cur()
  grDevices::pdf(NULL, width = sz[1] / 72, height = sz[2] / 72)
  tmp <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(tmp)
    if (.gd_is_open(cur)) grDevices::dev.set(cur)
  })
  suppressWarnings(grDevices::replayPlot(rp))
  f(sz)
}

.gd_is_cat_device <- function(d = grDevices::dev.cur()) as.character(d) %in% names(.gd$wins)

# base graphics: window points -> user coordinates
.gd_user_coords <- function(d, pts) {
  .gd_with_replay(d, function(sz) {
    list(x = graphics::grconvertX(pts[, 1], "device", "user"),
         y = graphics::grconvertY(sz[2] - pts[, 2], "device", "user"))
  })
}

locator <- function(n = 512, type = "n", ...) {
  d <- grDevices::dev.cur()
  if (!.gd_is_cat_device(d)) return(graphics::locator(n = n, type = type, ...))
  pts <- .gd_clicks(d, n)
  if (!nrow(pts)) return(NULL)
  xy <- .gd_user_coords(d, pts)
  if (type == "p") graphics::points(xy$x, xy$y, ...)
  if (type %in% c("l", "o")) graphics::lines(xy$x, xy$y, ...)
  xy
}

identify <- function(x, y = NULL, labels = seq_along(x), pos = FALSE, n = length(x),
                     plot = TRUE, offset = 0.5, tolerance = 0.25, ...) {
  d <- grDevices::dev.cur()
  if (!.gd_is_cat_device(d))
    return(graphics::identify(x, y, labels = labels, pos = pos, n = n, plot = plot,
                              offset = offset, tolerance = tolerance, ...))
  xy <- grDevices::xy.coords(x, y)
  pts <- .gd_clicks(d, n, "click on the points to label them, Esc to finish")
  if (!nrow(pts)) return(integer(0))
  dev_xy <- .gd_with_replay(d, function(sz)
    cbind(graphics::grconvertX(xy$x, "user", "device"),
          sz[2] - graphics::grconvertY(xy$y, "user", "device")))
  sel <- integer(0)
  for (k in seq_len(nrow(pts))) {
    dist <- sqrt((dev_xy[, 1] - pts[k, 1])^2 + (dev_xy[, 2] - pts[k, 2])^2)
    i <- which.min(dist)
    if (length(i) && dist[i] <= tolerance * 72 && !(i %in% sel)) sel <- c(sel, i)
  }
  if (plot && length(sel)) graphics::text(xy$x[sel], xy$y[sel], labels[sel], pos = 3,
                                          offset = offset, ...)
  if (pos) list(ind = sel, pos = rep(3L, length(sel))) else sel
}

# zoom on a base graphics plot by changing the limits of plot.window() in the
# recorded plot (as the zoom package does)
.gd_zoom_record <- function(rp, xlim, ylim) {
  for (i in seq_along(rp[[1]])) {
    a <- rp[[1]][[i]][[2]]
    f <- a[[1]]
    if (is.list(f) && identical(f$name, "C_plot_window")) {
      a[[2]] <- xlim
      a[[3]] <- ylim
      rp[[1]][[i]][[2]] <- a
    }
  }
  rp
}

zm <- function(type = "session", rp = NULL) {
  d <- grDevices::dev.cur()
  if (!.gd_is_cat_device(d)) stop("draw a plot first")
  orig <- .gd_record(d)
  message("Zoom: click two opposite corners of the area to enlarge; ",
          "a single click and Esc restores the original plot; Esc alone finishes")
  repeat {
    pts <- .gd_clicks(d, 2, "click two corners to zoom, Esc to finish")
    if (nrow(pts) == 0) break
    grDevices::dev.set(d)
    if (nrow(pts) == 1) {
      grDevices::replayPlot(orig)
    } else {
      xy <- .gd_user_coords(d, pts)
      grDevices::dev.set(d)
      grDevices::replayPlot(.gd_zoom_record(.gd_record(d), sort(xy$x), sort(xy$y)))
    }
    .gd_render(d, force = TRUE)
  }
  invisible()
}

# lattice: window points -> native coordinates of panel (column, row) of the
# trellis object obj shown on device d
.gd_panel_coords <- function(d, obj, pts, column = 1, row = 1) {
  sz <- .gd_size(d)
  cur <- grDevices::dev.cur()
  grDevices::pdf(NULL, width = sz[1] / 72, height = sz[2] / 72)
  tmp <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(tmp)
    if (.gd_is_open(cur)) grDevices::dev.set(cur)
  })
  print(obj)
  lattice::trellis.focus("panel", column, row, highlight = FALSE)
  lim <- lattice::current.panel.limits()
  ll <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"))
  ur <- grid::deviceLoc(grid::unit(1, "npc"), grid::unit(1, "npc"))
  x0 <- grid::convertX(ll$x, "points", TRUE); x1 <- grid::convertX(ur$x, "points", TRUE)
  y0 <- sz[2] - grid::convertY(ll$y, "points", TRUE)
  y1 <- sz[2] - grid::convertY(ur$y, "points", TRUE)
  lattice::trellis.unfocus()
  list(x = lim$xlim[1] + (pts[, 1] - x0) / (x1 - x0) * diff(lim$xlim),
       y = lim$ylim[1] + (pts[, 2] - y0) / (y1 - y0) * diff(lim$ylim),
       xlim = lim$xlim, ylim = lim$ylim)
}
