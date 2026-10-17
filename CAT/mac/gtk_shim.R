# Purpose   : minimal RGtk2 compatibility layer implemented on top of Tcl/Tk.
#             CAT's dialog library (mylib/gvfib.r) and a few scripts were
#             written for RGtk2, which is no longer available on CRAN and does
#             not run on current macOS. This file re-implements the (small)
#             subset of the RGtk2 API used by CAT so that the original code
#             runs unchanged.
#
#             Gtk builds widgets without a parent and packs them later, while
#             Tk needs the parent at creation time. Widgets are therefore kept
#             as a virtual tree and realised as Tk widgets when the top-level
#             window is shown (children added afterwards are realised
#             immediately).
# Licence   : GPL-3 (same as CAT)

suppressPackageStartupMessages(library(tcltk))

`%||%` <- function(a, b) if (is.null(a)) b else a

.gtk <- new.env()
.gtk$level <- 0L          # nesting level of gtkMain()
.gtk$waits <- list()      # tclVar per gtkMain() level
.gtk$toplevels <- list()  # currently realised shim top-levels
.gtk$counter <- 0L

GtkResponseType <- c(none = -1, reject = -2, accept = -3, "delete-event" = -4,
                     ok = -5, cancel = -6, close = -7, yes = -8, no = -9,
                     apply = -10, help = -11)

# ---------------------------------------------------------------------------
# object model
# ---------------------------------------------------------------------------

.gtk_new <- function(kind, ..., class = NULL) {
  w <- new.env(parent = emptyenv())
  .gtk$counter <- .gtk$counter + 1L
  w$id <- .gtk$counter
  w$kind <- kind
  w$children <- list()
  w$props <- list()
  w$data <- list()
  w$signals <- list()
  w$tk <- NULL
  w$parent <- NULL
  w$sensitive <- TRUE
  w$visible <- FALSE
  extra <- list(...)
  for (n in names(extra)) assign(n, extra[[n]], envir = w)
  class(w) <- c(class, paste0("Gtk", kind), "GtkShim")
  w
}

.gtk_get <- function(w, field) get0(field, envir = w, inherits = FALSE)
.gtk_setvar <- function(w, value) {
  v <- .gtk_get(w, "var")
  tclvalue(v) <- value
  invisible()
}
.gtk_set <- function(w, field, value) assign(field, value, envir = w)

# obj$method(...) as in RGtk2; internal fields are reachable the same way
`$.GtkShim` <- function(x, name) {
  m <- .gtk_methods[[tolower(name)]]
  if (!is.null(m)) return(function(...) m(x, ...))
  if (exists(name, envir = x, inherits = FALSE)) return(get(name, envir = x))
  stop(sprintf("RGtk2 compatibility layer: method '%s' is not available", name),
       call. = FALSE)
}

`[<-.GtkShim` <- function(x, i, value) {
  props <- .gtk_get(x, "props")
  props[[i]] <- value
  .gtk_set(x, "props", props)
  .gtk_apply_props(x)
  x
}

`[.GtkShim` <- function(x, i) .gtk_get(x, "props")[[i]]

print.GtkShim <- function(x, ...) {
  cat("<", class(x)[1], " (Tk compatibility widget)>\n", sep = "")
  invisible(x)
}

.gtk_is_realized <- function(w) {
  tkw <- .gtk_get(w, "tk")
  !is.null(tkw) && as.character(tkwinfo("exists", tkw)) == "1"
}

.gtk_label_text <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  x <- as.character(x[1])
  if (is.na(x)) "NA" else x
}

.gtk_stock_label <- function(x) {
  map <- c("gtk-ok" = "OK", "gtk-cancel" = "Cancel", "gtk-yes" = "Yes",
           "gtk-no" = "No", "gtk-close" = "Close", "gtk-apply" = "Apply",
           "gtk-help" = "Help")
  if (x %in% names(map)) map[[x]] else x
}

# ---------------------------------------------------------------------------
# constructors
# ---------------------------------------------------------------------------

gtkWindow <- function(type = "toplevel", show = TRUE) {
  w <- .gtk_new("Window", title = "", deletable = TRUE, modal = FALSE,
                default_size = NULL, size_request = NULL)
  w$props[["border-width"]] <- 0
  if (isTRUE(show)) .gtk_show(w)
  w
}
gtkWindowNew <- gtkWindow

gtkDialogNewWithButtons <- function(title = NULL, parent = NULL, flags = 0, ...,
                                    show = TRUE) {
  args <- list(...)
  d <- .gtk_new("Dialog", title = .gtk_label_text(title), deletable = TRUE,
                modal = FALSE, default_size = NULL, size_request = NULL)
  d$props[["border-width"]] <- 6
  content <- gtkVBox(spacing = 6)
  actions <- gtkHBox(spacing = 6)
  d$content <- content
  d$actions <- actions
  d$response_buttons <- list()
  if (length(args) >= 2) {
    for (k in seq(1, length(args) - 1, by = 2)) {
      lab <- .gtk_stock_label(as.character(args[[k]]))
      resp <- as.numeric(args[[k + 1]])
      b <- gtkButton(lab)
      local({
        rr <- resp
        gSignalConnect(b, "clicked", function(button) .gtk_emit(d, "response", rr))
      })
      actions$packEnd(b, expand = FALSE, fill = FALSE)
      d$response_buttons[[length(d$response_buttons) + 1]] <- list(button = b, response = resp)
    }
  }
  if (isTRUE(show)) .gtk_show(d)
  d
}
gtkDialog <- function(show = TRUE) gtkDialogNewWithButtons(show = show)

gtkHBox <- function(homogeneous = FALSE, spacing = 0, show = TRUE)
  .gtk_new("HBox", homogeneous = homogeneous, spacing = spacing)
gtkVBox <- function(homogeneous = FALSE, spacing = 0, show = TRUE)
  .gtk_new("VBox", homogeneous = homogeneous, spacing = spacing)
gtkHBoxNew <- gtkHBox
gtkVBoxNew <- gtkVBox

gtkTable <- function(rows = 1, columns = 1, homogeneous = FALSE, show = TRUE)
  .gtk_new("Table", nrows = rows, ncols = columns, homogeneous = homogeneous,
           col_spacing = list(), row_spacing = list())
gtkTableNew <- gtkTable

gtkAlignment <- function(xalign = 0.5, yalign = 0.5, xscale = 1, yscale = 1,
                         show = TRUE)
  .gtk_new("Alignment", xalign = xalign %||% 0.5, yalign = yalign %||% 0.5,
           xscale = xscale %||% 1, yscale = yscale %||% 1)
gtkAlignmentNew <- gtkAlignment

gtkFrame <- function(label = NULL, show = TRUE)
  .gtk_new("Frame", text = .gtk_label_text(label))

gtkLabel <- function(str = NULL, show = TRUE)
  .gtk_new("Label", text = .gtk_label_text(str))
gtkLabelNew <- gtkLabel

gtkEntryNew <- function(show = TRUE)
  .gtk_new("Entry", var = tclVar(""), width = 20L)
gtkEntry <- function(max = 0, show = TRUE) {
  e <- gtkEntryNew()
  if (max > 0) e$width <- max
  e
}

gtkEntrySetMaxLength <- function(object, max) {
  if (max > 0) .gtk_set(object, "width", as.integer(max) + 2L)
  if (.gtk_is_realized(object)) tkconfigure(object$tk, width = .gtk_get(object, "width"))
  invisible()
}

gtkEntryGetText <- function(object) tclvalue(.gtk_get(object, "var"))
gtkEntrySetText <- function(object, text) {
  .gtk_setvar(object, .gtk_label_text(text))
  invisible()
}

gtkButton <- function(label = NULL, stock.id = NULL, show = TRUE)
  .gtk_new("Button", text = .gtk_stock_label(.gtk_label_text(label %||% stock.id)))
gtkButtonNew <- function(show = TRUE) gtkButton("")
gtkButtonNewWithLabel <- function(label, show = TRUE) gtkButton(label)

gtkCheckButtonNewWithLabel <- function(label, show = TRUE)
  .gtk_new("CheckButton", text = .gtk_label_text(label), var = tclVar(0))
gtkCheckButtonNew <- function(show = TRUE) gtkCheckButtonNewWithLabel("")
gtkCheckButton <- function(label = NULL, show = TRUE)
  gtkCheckButtonNewWithLabel(.gtk_label_text(label))

.gtk_radio <- function(group, label) {
  if (is.null(group)) {
    grp <- new.env(parent = emptyenv())
    grp$var <- tclVar("")
    grp$n <- 0L
  } else if (inherits(group, "GtkRadioButton")) {
    grp <- .gtk_get(group, "group")
  } else {
    grp <- .gtk_get(group[[1]], "group")
  }
  grp$n <- grp$n + 1L
  value <- paste0("r", grp$n)
  # Gtk activates the first button of a group by default
  if (grp$n == 1L) tclvalue(grp$var) <- value
  .gtk_new("RadioButton", text = .gtk_label_text(label), group = grp, value = value)
}
gtkRadioButtonNewWithLabel <- function(group = NULL, label, show = TRUE)
  .gtk_radio(group, label)
gtkRadioButtonNewWithLabelFromWidget <- function(group = NULL, label, show = TRUE)
  .gtk_radio(group, label)
gtkRadioButton <- function(group = NULL, label = NULL, show = TRUE)
  .gtk_radio(group, label)

gtkComboBoxNewText <- function(show = TRUE)
  .gtk_new("ComboBox", items = character(0), active = -1L)
gtkComboBoxNew <- gtkComboBoxNewText

gtkToggleButtonGetActive <- function(object) .gtk_methods$getactive(object)
gtkToggleButtonSetActive <- function(object, is.active)
  .gtk_methods$setactive(object, is.active)

gtkScrolledWindow <- function(hadjustment = NULL, vadjustment = NULL, show = TRUE)
  .gtk_new("ScrolledWindow")
gtkScrolledWindowNew <- gtkScrolledWindow

gtkTreeView <- function(model = NULL, show = TRUE) {
  v <- .gtk_new("TreeView", model = NULL, columns = list())
  sel <- .gtk_new("TreeSelection", view = v, mode = "single")
  v$selection <- sel
  if (!is.null(model)) .gtk_methods$setmodel(v, model)
  v
}
gtkTreeViewNew <- function(show = TRUE) gtkTreeView()
gtkTreeViewNewWithModel <- function(model, show = TRUE) gtkTreeView(model)

gtkCellRendererText <- function() .gtk_new("CellRendererText")
gtkCellRendererToggle <- function() .gtk_new("CellRendererToggle")

rGtkDataFrame <- function(frame = data.frame()) {
  m <- .gtk_new("Model", frame = as.data.frame(frame, stringsAsFactors = FALSE),
                views = list(), class = "RGtkDataFrame")
  m
}

dim.RGtkDataFrame <- function(x) dim(.gtk_get(x, "frame"))
dimnames.RGtkDataFrame <- function(x) dimnames(.gtk_get(x, "frame"))
as.data.frame.RGtkDataFrame <- function(x, ...) .gtk_get(x, "frame")
`[.RGtkDataFrame` <- function(x, i, j, drop = TRUE) {
  f <- .gtk_get(x, "frame")
  if (missing(i) && missing(j)) return(f)
  if (missing(i)) return(f[, j, drop = drop])
  if (missing(j)) return(f[i, , drop = drop])
  f[i, j, drop = drop]
}
`[<-.RGtkDataFrame` <- function(x, i, j, value) {
  f <- .gtk_get(x, "frame")
  if (missing(i)) f[, j] <- value
  else if (missing(j)) f[i, ] <- value
  else f[i, j] <- value
  .gtk_set(x, "frame", f)
  .gtk_model_changed(x)
  x
}

# ---------------------------------------------------------------------------
# signals
# ---------------------------------------------------------------------------

gSignalConnect <- function(obj, signal, f, data = NULL, after = FALSE,
                           user.data.first = FALSE) {
  handlers <- .gtk_get(obj, "signals")
  handlers[[length(handlers) + 1]] <- list(signal = signal, f = f,
                                           data = data, has_data = !missing(data),
                                           first = user.data.first)
  .gtk_set(obj, "signals", handlers)
  invisible(length(handlers))
}
gSignalHandlerDisconnect <- function(obj, id) {
  handlers <- .gtk_get(obj, "signals")
  if (id <= length(handlers)) handlers[[id]]$signal <- ""
  .gtk_set(obj, "signals", handlers)
  invisible()
}

.gtk_call_handler <- function(h, args) {
  if (h$has_data) {
    args <- if (isTRUE(h$first)) c(list(h$data), args) else c(args, list(h$data))
  }
  fm <- names(formals(h$f))
  if (!("..." %in% fm) && length(args) > length(fm)) args <- args[seq_along(fm)]
  do.call(h$f, args)
}

# Emits a signal; errors are reported on the console like RGtk2 did instead of
# raising a Tcl error.
.gtk_emit <- function(obj, signal, ...) {
  handlers <- Filter(function(h) identical(h$signal, signal), .gtk_get(obj, "signals"))
  res <- NULL
  for (h in handlers) {
    res <- tryCatch(.gtk_call_handler(h, c(list(obj), list(...))),
                    error = function(e) {
                      .cat_report_error(e)
                      NULL
                    })
  }
  invisible(res)
}

.cat_report_error <- function(e) {
  call <- conditionCall(e)
  msg <- conditionMessage(e)
  if (!is.null(call)) {
    dcall <- paste(deparse(call, nlines = 1L), collapse = "")
    message("Error in ", dcall, " : ", msg)
  } else {
    message("Error: ", msg)
  }
}

# ---------------------------------------------------------------------------
# main loop
# ---------------------------------------------------------------------------

gtkMain <- function() {
  lvl <- .gtk$level + 1L
  .gtk$level <- lvl
  v <- tclVar(0)
  .gtk$waits[[lvl]] <- v
  on.exit({
    .gtk$waits[[lvl]] <- NULL
    .gtk$level <- lvl - 1L
  })
  if (length(.gtk$toplevels) == 0) return(invisible())
  # plots drawn so far must be visible while the user answers
  if (exists(".gd_refresh_all", mode = "function")) try(.gd_refresh_all(), silent = TRUE)
  hook <- getOption("CAT.gtkMain.hook")
  if (is.function(hook)) hook(lvl)
  tkwait.variable(v)
  invisible()
}

gtkMainQuit <- function() {
  lvl <- .gtk$level
  if (lvl > 0 && length(.gtk$waits) >= lvl && !is.null(.gtk$waits[[lvl]]))
    tclvalue(.gtk$waits[[lvl]]) <- 1
  invisible()
}

gtkMainIteration <- function() {
  tcl("update")
  invisible(FALSE)
}
gtkEventsPending <- function() FALSE

# A gtkMain() loop must not wait forever when its windows vanish without a
# call to gtkMainQuit() (e.g. after an error in a callback).
.gtk_after_toplevel_destroyed <- function() {
  lvl <- .gtk$level
  if (lvl > 0 && length(.gtk$toplevels) == 0) {
    v <- .gtk$waits[[lvl]]
    tcl("after", "idle", function() {
      if (.gtk$level == lvl && length(.gtk$toplevels) == 0 &&
          !is.null(.gtk$waits[[lvl]]) && identical(.gtk$waits[[lvl]], v))
        tclvalue(v) <- 1
    })
  }
}

# ---------------------------------------------------------------------------
# realisation (virtual tree -> Tk widgets)
# ---------------------------------------------------------------------------

.gtk_no_vstretch <- c("Entry", "ComboBox", "CheckButton", "RadioButton", "Button")

.gtk_anchor <- function(xalign, yalign) {
  h <- if (is.null(xalign)) "w" else if (xalign <= 0.25) "w" else if (xalign >= 0.75) "e" else ""
  v <- if (is.null(yalign)) "" else if (yalign <= 0.25) "n" else if (yalign >= 0.75) "s" else ""
  a <- paste0(v, h)
  if (a == "") "center" else a
}

.gtk_realize <- function(w, parent) {
  kind <- .gtk_get(w, "kind")
  tkw <- switch(kind,
    Label = {
      p <- .gtk_get(w, "props")
      ttklabel(parent, text = .gtk_get(w, "text"),
               anchor = .gtk_anchor(p$xalign, p$yalign), justify = "left")
    },
    Entry = ttkentry(parent, textvariable = .gtk_get(w, "var"),
                     width = .gtk_get(w, "width")),
    Button = {
      b <- ttkbutton(parent, text = .gtk_get(w, "text"),
                     command = function() .gtk_emit(w, "clicked"))
      b
    },
    CheckButton = ttkcheckbutton(parent, text = .gtk_get(w, "text"),
                                 variable = .gtk_get(w, "var"),
                                 command = function() .gtk_emit(w, "toggled")),
    RadioButton = {
      grp <- .gtk_get(w, "group")
      ttkradiobutton(parent, text = .gtk_get(w, "text"), variable = grp$var,
                     value = .gtk_get(w, "value"),
                     command = function() .gtk_emit(w, "toggled"))
    },
    ComboBox = {
      items <- .gtk_get(w, "items")
      width <- max(c(8L, nchar(items))) + 2L
      cb <- ttkcombobox(parent, state = "readonly", width = min(width, 60L))
      if (length(items)) tkconfigure(cb, values = as.tclObj(items, drop = FALSE))
      act <- .gtk_get(w, "active")
      if (act >= 0 && act < length(items)) tcl(cb, "current", act)
      tkbind(cb, "<<ComboboxSelected>>", function() {
        .gtk_set(w, "active", as.integer(tclvalue(tcl(cb, "current"))))
        .gtk_emit(w, "changed")
      })
      cb
    },
    VBox = , HBox = , Alignment = ttkframe(parent),
    Frame = ttklabelframe(parent, text = .gtk_get(w, "text")),
    Table = ttkframe(parent),
    ScrolledWindow = ttkframe(parent),
    TreeView = .gtk_realize_treeview(w, parent),
    stop("RGtk2 compatibility layer: cannot display widget of type ", kind)
  )
  .gtk_set(w, "tk", tkw)
  if (!isTRUE(.gtk_get(w, "sensitive"))) .gtk_set_state(w, FALSE)
  for (ch in .gtk_get(w, "children")) .gtk_realize_child(w, ch)
  tkw
}

.gtk_realize_child <- function(container, ch) {
  child <- ch$w
  ctk <- .gtk_get(container, "tk")
  if (.gtk_is_realized(child)) return(invisible())
  ckind <- .gtk_get(child, "kind")
  tkc <- .gtk_realize(child, ctk)
  kind <- .gtk_get(container, "kind")
  o <- ch$opts
  leaf_fixed <- ckind %in% .gtk_no_vstretch
  if (kind %in% c("VBox", "HBox")) {
    vertical <- kind == "VBox"
    spacing <- as.numeric(.gtk_get(container, "props")$spacing %||%
                            .gtk_get(container, "spacing") %||% 0)
    side <- if (vertical) { if (o$end) "bottom" else "top" } else { if (o$end) "right" else "left" }
    fill <- if (vertical) {
      if (o$fill && !leaf_fixed) "both" else "x"
    } else {
      if (o$fill) { if (leaf_fixed) "x" else "both" } else { if (leaf_fixed) "none" else "y" }
    }
    pad <- o$padding + spacing / 2
    if (vertical) tkpack(tkc, side = side, expand = o$expand, fill = fill, pady = pad)
    else tkpack(tkc, side = side, expand = o$expand, fill = fill, padx = pad)
  } else if (kind == "Table") {
    xo <- o$xoptions; yo <- o$yoptions
    sticky <- paste0(if ("fill" %in% yo && !leaf_fixed) "ns" else "",
                     if ("fill" %in% xo) "ew" else "")
    if (ckind == "Label") sticky <- "nsew"
    tkgrid(tkc, row = o$top, column = o$left,
           rowspan = max(1, o$bottom - o$top), columnspan = max(1, o$right - o$left),
           sticky = sticky, padx = o$xpadding, pady = max(o$ypadding, 2))
    if ("expand" %in% xo) for (cc in seq(o$left, max(o$left, o$right - 1)))
      tkgrid.columnconfigure(ctk, cc, weight = 1)
    if ("expand" %in% yo) for (rr in seq(o$top, max(o$top, o$bottom - 1)))
      tkgrid.rowconfigure(ctk, rr, weight = 1)
    for (cs in names(.gtk_get(container, "col_spacing")))
      tkgrid.columnconfigure(ctk, as.integer(cs), pad = .gtk_get(container, "col_spacing")[[cs]])
  } else if (kind == "Alignment") {
    xa <- .gtk_get(container, "xalign"); ya <- .gtk_get(container, "yalign")
    xs <- .gtk_get(container, "xscale"); ys <- .gtk_get(container, "yscale")
    fill <- if (xs >= 1 && xa == 0.5) { if (ys >= 1) "both" else "x" } else { if (ys >= 1 && !leaf_fixed) "y" else "none" }
    tkpack(tkc, anchor = .gtk_anchor(xa, ya), fill = fill, expand = TRUE)
  } else if (kind == "ScrolledWindow") {
    .gtk_scroll(container, child, tkc)
  } else if (kind %in% c("Window", "Dialog", "Frame")) {
    tkpack(tkc, expand = TRUE, fill = "both")
  }
  invisible()
}

.gtk_scroll <- function(sw, child, tkc) {
  ctk <- .gtk_get(sw, "tk")
  ys <- ttkscrollbar(ctk, orient = "vertical", command = function(...) tkyview(tkc, ...))
  xs <- ttkscrollbar(ctk, orient = "horizontal", command = function(...) tkxview(tkc, ...))
  tkconfigure(tkc, yscrollcommand = function(...) tkset(ys, ...),
              xscrollcommand = function(...) tkset(xs, ...))
  tkgrid(tkc, row = 0, column = 0, sticky = "nsew")
  tkgrid(ys, row = 0, column = 1, sticky = "ns")
  tkgrid(xs, row = 1, column = 0, sticky = "ew")
  tkgrid.rowconfigure(ctk, 0, weight = 1)
  tkgrid.columnconfigure(ctk, 0, weight = 1)
}

.gtk_set_state <- function(w, sensitive) {
  tkw <- .gtk_get(w, "tk")
  if (is.null(tkw)) return(invisible())
  kind <- .gtk_get(w, "kind")
  if (kind == "ComboBox") {
    tkconfigure(tkw, state = if (sensitive) "readonly" else "disabled")
  } else if (kind %in% c("Entry", "Button", "CheckButton", "RadioButton", "Label")) {
    tcl(tkw, "state", if (sensitive) "!disabled" else "disabled")
  }
  for (ch in .gtk_get(w, "children")) .gtk_set_state(ch$w, sensitive)
  invisible()
}

.gtk_apply_props <- function(w) {
  if (!.gtk_is_realized(w)) return(invisible())
  p <- .gtk_get(w, "props")
  kind <- .gtk_get(w, "kind")
  if (kind == "Label" && (!is.null(p$xalign) || !is.null(p$yalign)))
    tkconfigure(w$tk, anchor = .gtk_anchor(p$xalign, p$yalign))
  invisible()
}

.gtk_buttons <- function(w) {
  out <- list()
  if (.gtk_get(w, "kind") == "Button") out <- list(w)
  for (ch in .gtk_get(w, "children")) out <- c(out, .gtk_buttons(ch$w))
  if (.gtk_get(w, "kind") == "Dialog") out <- c(out, .gtk_buttons(.gtk_get(w, "actions")))
  out
}

.gtk_find_button <- function(w, labels) {
  for (b in .gtk_buttons(w)) {
    if (tolower(.gtk_get(b, "text")) %in% tolower(labels) && isTRUE(.gtk_get(b, "sensitive")))
      return(b)
  }
  NULL
}

.gtk_first_entry <- function(w) {
  kind <- .gtk_get(w, "kind")
  if (kind %in% c("Entry")) return(w)
  kids <- .gtk_get(w, "children")
  if (kind == "Dialog") kids <- c(list(list(w = .gtk_get(w, "content"))), kids)
  for (ch in kids) {
    r <- .gtk_first_entry(ch$w)
    if (!is.null(r)) return(r)
  }
  NULL
}

.gtk_center <- function(top) {
  tcl("update", "idletasks")
  w <- as.integer(tkwinfo("reqwidth", top))
  h <- as.integer(tkwinfo("reqheight", top))
  main <- getOption("CAT.mainwindow")
  if (!is.null(main) && as.character(tkwinfo("exists", main)) == "1" &&
      as.character(tkwinfo("ismapped", main)) == "1") {
    x <- as.integer(tkwinfo("rootx", main)) + (as.integer(tkwinfo("width", main)) - w) %/% 2
    y <- as.integer(tkwinfo("rooty", main)) + (as.integer(tkwinfo("height", main)) - h) %/% 3
  } else {
    x <- (as.integer(tkwinfo("screenwidth", top)) - w) %/% 2
    y <- (as.integer(tkwinfo("screenheight", top)) - h) %/% 3
  }
  tkwm.geometry(top, sprintf("+%d+%d", max(x, 0L), max(y, 22L)))
}

.gtk_show <- function(w) {
  kind <- .gtk_get(w, "kind")
  if (!(kind %in% c("Window", "Dialog"))) {
    .gtk_set(w, "visible", TRUE)
    return(invisible())
  }
  if (.gtk_is_realized(w)) {
    tkwm.deiconify(w$tk)
    tkraise(w$tk)
    return(invisible())
  }
  top <- tktoplevel()
  tkwm.withdraw(top)
  .gtk_set(w, "tk", top)
  tkwm.title(top, .gtk_get(w, "title"))
  bw <- as.numeric(.gtk_get(w, "props")[["border-width"]] %||% 0)
  inner <- ttkframe(top, padding = max(bw, 4))
  tkpack(inner, expand = TRUE, fill = "both")
  .gtk_set(w, "inner", inner)
  if (kind == "Dialog") {
    content <- .gtk_get(w, "content")
    actions <- .gtk_get(w, "actions")
    ctk <- .gtk_realize(content, inner)
    tkpack(ctk, side = "top", expand = TRUE, fill = "both")
    sep <- ttkseparator(inner, orient = "horizontal")
    tkpack(sep, side = "top", fill = "x", pady = 6)
    atk <- .gtk_realize(actions, inner)
    tkpack(atk, side = "top", fill = "x")
  }
  for (ch in .gtk_get(w, "children")) {
    tkc <- .gtk_realize(ch$w, inner)
    tkpack(tkc, expand = TRUE, fill = "both")
  }
  ds <- .gtk_get(w, "default_size")
  if (!is.null(ds)) tkwm.geometry(top, sprintf("%dx%d", as.integer(ds[1]), as.integer(ds[2])))
  sr <- .gtk_get(w, "size_request")
  if (!is.null(sr)) tkwm.minsize(top, as.integer(sr[1]), as.integer(sr[2]))
  # closing the window behaves like pressing Cancel (Gtk dialogs in CAT are
  # not deletable, which on macOS would leave no way to dismiss them)
  tcl("wm", "protocol", top, "WM_DELETE_WINDOW", function() .gtk_close_request(w))
  tkbind(top, "<Escape>", function() .gtk_close_request(w))
  tkbind(top, "<Return>", function() .gtk_default_action(w))
  tkbind(top, "<KP_Enter>", function() .gtk_default_action(w))
  key <- as.character(w$id)
  .gtk$toplevels[[key]] <- w
  tkbind(top, "<Destroy>", function(W) {
    if (identical(as.character(W), as.character(top$ID))) {
      .gtk$toplevels[[key]] <- NULL
      .gtk_after_toplevel_destroyed()
    }
  })
  .gtk_set(w, "visible", TRUE)
  if (is.null(ds)) .gtk_center(top)
  tkwm.deiconify(top)
  tkraise(top)
  tcl("update", "idletasks")
  e <- .gtk_first_entry(w)
  if (!is.null(e) && .gtk_is_realized(e)) {
    tkfocus(e$tk)
    tcl(e$tk, "selection", "range", 0, "end")
  } else {
    tkfocus(top)
  }
  invisible()
}

.gtk_close_request <- function(w) {
  if (.gtk_get(w, "kind") == "Dialog") {
    .gtk_emit(w, "response", GtkResponseType[["delete-event"]])
    return(invisible())
  }
  b <- .gtk_find_button(w, c("Cancel", "No", "Close", "Annulla"))
  if (!is.null(b)) {
    .gtk_emit(b, "clicked")
  } else if (isTRUE(.gtk_get(w, "deletable"))) {
    .gtk_emit(w, "delete-event")
    .gtk_methods$destroy(w)
  } else {
    tkbell()
  }
  invisible()
}

.gtk_default_action <- function(w) {
  if (.gtk_get(w, "kind") == "Dialog") {
    rb <- .gtk_get(w, "response_buttons")
    for (r in rb) if (r$response %in% c(GtkResponseType[["ok"]], GtkResponseType[["yes"]],
                                        GtkResponseType[["accept"]])) {
      .gtk_emit(w, "response", r$response)
      return(invisible())
    }
  }
  b <- .gtk_find_button(w, c("OK", "Yes"))
  if (!is.null(b)) .gtk_emit(b, "clicked")
  invisible()
}

# ---------------------------------------------------------------------------
# tree view (used for the workspace browser and list selection)
# ---------------------------------------------------------------------------

.gtk_realize_treeview <- function(v, parent) {
  tv <- ttktreeview(parent, show = "headings", selectmode = "browse", height = 12)
  .gtk_set(v, "tk", tv)
  .gtk_treeview_columns(v)
  .gtk_treeview_fill(v)
  tkbind(tv, "<ButtonRelease-1>", function(x, y) .gtk_treeview_click(v, x, y))
  tv
}

.gtk_treeview_columns <- function(v) {
  tv <- .gtk_get(v, "tk")
  cols <- .gtk_get(v, "columns")
  if (!length(cols)) return(invisible())
  ids <- paste0("c", seq_along(cols))
  tkconfigure(tv, columns = as.tclObj(ids, drop = FALSE))
  for (k in seq_along(cols)) {
    toggle <- .gtk_get(cols[[k]]$cell, "kind") == "CellRendererToggle"
    tcl(tv, "heading", ids[k], text = cols[[k]]$title)
    tcl(tv, "column", ids[k], width = if (toggle) 40 else 120,
        stretch = if (toggle) 0 else 1, anchor = if (toggle) "center" else "w")
  }
  invisible()
}

.gtk_treeview_fill <- function(v) {
  if (!.gtk_is_realized(v)) return(invisible())
  tv <- .gtk_get(v, "tk")
  model <- .gtk_get(v, "model")
  kids <- as.character(tcl(tv, "children", ""))
  if (length(kids)) tcl(tv, "delete", kids)
  if (is.null(model)) return(invisible())
  f <- .gtk_get(model, "frame")
  cols <- .gtk_get(v, "columns")
  if (!nrow(f) || !length(cols)) return(invisible())
  for (r in seq_len(nrow(f))) {
    vals <- vapply(cols, function(cl) {
      x <- f[r, cl$index + 1L]
      if (.gtk_get(cl$cell, "kind") == "CellRendererToggle")
        if (isTRUE(as.logical(x))) "☑" else "☐"
      else format(x)
    }, "")
    tcl(tv, "insert", "", "end", id = paste0("row", r - 1L),
        values = as.tclObj(vals, drop = FALSE))
  }
  invisible()
}

.gtk_treeview_click <- function(v, x, y) {
  tv <- .gtk_get(v, "tk")
  item <- tclvalue(tcl(tv, "identify", "item", x, y))
  col <- tclvalue(tcl(tv, "identify", "column", x, y))
  if (item == "" || col == "") return(invisible())
  k <- as.integer(sub("#", "", col))
  cols <- .gtk_get(v, "columns")
  if (k < 1 || k > length(cols)) return(invisible())
  cell <- cols[[k]]$cell
  if (.gtk_get(cell, "kind") == "CellRendererToggle") {
    path <- sub("row", "", item)
    .gtk_emit(cell, "toggled", path)
    .gtk_treeview_fill(v)
    tcl(tv, "selection", "set", item)
  }
  invisible()
}

.gtk_model_changed <- function(model) {
  for (v in .gtk_get(model, "views")) .gtk_treeview_fill(v)
  invisible()
}

# ---------------------------------------------------------------------------
# methods ($ dispatch, case-insensitive like the RGtk2 usage in CAT)
# ---------------------------------------------------------------------------

.gtk_add_child <- function(container, child, opts) {
  .gtk_set(child, "parent", container)
  kids <- .gtk_get(container, "children")
  kids[[length(kids) + 1]] <- list(w = child, opts = opts)
  .gtk_set(container, "children", kids)
  if (.gtk_is_realized(container)) {
    kind <- .gtk_get(container, "kind")
    if (kind %in% c("Window", "Dialog")) {
      tkc <- .gtk_realize(child, .gtk_get(container, "inner"))
      tkpack(tkc, expand = TRUE, fill = "both")
    } else {
      .gtk_realize_child(container, kids[[length(kids)]])
    }
  }
  invisible()
}

.gtk_methods <- list(
  add = function(w, widget) {
    .gtk_add_child(w, widget, list(expand = TRUE, fill = TRUE, padding = 0, end = FALSE))
  },
  packstart = function(w, child, expand = TRUE, fill = TRUE, padding = 0) {
    .gtk_add_child(w, child, list(expand = isTRUE(expand), fill = isTRUE(fill),
                                  padding = padding, end = FALSE))
  },
  packend = function(w, child, expand = TRUE, fill = TRUE, padding = 0) {
    .gtk_add_child(w, child, list(expand = isTRUE(expand), fill = isTRUE(fill),
                                  padding = padding, end = TRUE))
  },
  attach = function(w, child, left.attach, right.attach, top.attach, bottom.attach,
                    xoptions = c("expand", "fill"), yoptions = c("expand", "fill"),
                    xpadding = 0, ypadding = 0) {
    .gtk_add_child(w, child, list(left = left.attach, right = right.attach,
                                  top = top.attach, bottom = bottom.attach,
                                  xoptions = xoptions, yoptions = yoptions,
                                  xpadding = xpadding, ypadding = ypadding))
  },
  attachdefaults = function(w, widget, left.attach, right.attach, top.attach, bottom.attach) {
    .gtk_methods$attach(w, widget, left.attach, right.attach, top.attach, bottom.attach)
  },
  setcolspacing = function(w, column, spacing) {
    cs <- .gtk_get(w, "col_spacing")
    cs[[as.character(column)]] <- spacing
    .gtk_set(w, "col_spacing", cs)
    invisible()
  },
  setrowspacing = function(w, row, spacing) invisible(),
  setcolspacings = function(w, spacing) invisible(),
  setrowspacings = function(w, spacing) invisible(),
  setspacing = function(w, spacing) { w["spacing"] <- spacing; invisible() },
  gettext = function(w) {
    if (.gtk_get(w, "kind") == "Entry") tclvalue(.gtk_get(w, "var")) else .gtk_get(w, "text")
  },
  settext = function(w, str) {
    str <- .gtk_label_text(str)
    if (.gtk_get(w, "kind") == "Entry") {
      .gtk_setvar(w, str)
    } else {
      .gtk_set(w, "text", str)
      if (.gtk_is_realized(w)) tkconfigure(w$tk, text = str)
    }
    invisible()
  },
  setlabel = function(w, str) .gtk_methods$settext(w, str),
  getlabel = function(w) .gtk_get(w, "text"),
  appendtext = function(w, text) {
    .gtk_set(w, "items", c(.gtk_get(w, "items"), .gtk_label_text(text)))
    if (.gtk_is_realized(w))
      tkconfigure(w$tk, values = as.tclObj(.gtk_get(w, "items"), drop = FALSE))
    invisible()
  },
  getactive = function(w) {
    kind <- .gtk_get(w, "kind")
    if (kind == "ComboBox") return(as.integer(.gtk_get(w, "active")))
    if (kind == "CheckButton") return(tclvalue(.gtk_get(w, "var")) == "1")
    if (kind == "RadioButton") {
      grp <- .gtk_get(w, "group")
      return(identical(tclvalue(grp$var), .gtk_get(w, "value")))
    }
    FALSE
  },
  setactive = function(w, value) {
    kind <- .gtk_get(w, "kind")
    if (kind == "ComboBox") {
      value <- suppressWarnings(as.integer(value))
      if (length(value) == 0 || is.na(value)) value <- -1L
      .gtk_set(w, "active", value)
      if (.gtk_is_realized(w)) {
        if (value >= 0 && value < length(.gtk_get(w, "items"))) tcl(w$tk, "current", value)
        else tkset(w$tk, "")
      }
    } else if (kind == "CheckButton") {
      .gtk_setvar(w, if (isTRUE(as.logical(value))) 1 else 0)
    } else if (kind == "RadioButton") {
      grp <- .gtk_get(w, "group")
      if (isTRUE(as.logical(value))) tclvalue(grp$var) <- .gtk_get(w, "value")
    }
    invisible()
  },
  getactivetext = function(w) {
    a <- .gtk_get(w, "active")
    items <- .gtk_get(w, "items")
    if (a >= 0 && a < length(items)) items[a + 1] else NULL
  },
  setsensitive = function(w, sensitive) {
    .gtk_set(w, "sensitive", isTRUE(as.logical(sensitive)))
    .gtk_set_state(w, isTRUE(as.logical(sensitive)))
    invisible()
  },
  getsensitive = function(w) isTRUE(.gtk_get(w, "sensitive")),
  settitle = function(w, title) {
    .gtk_set(w, "title", .gtk_label_text(title))
    if (.gtk_is_realized(w)) tkwm.title(w$tk, .gtk_label_text(title))
    invisible()
  },
  setdeletable = function(w, setting) { .gtk_set(w, "deletable", isTRUE(setting)); invisible() },
  setmodal = function(w, modal) { .gtk_set(w, "modal", isTRUE(modal)); invisible() },
  setdefaultsize = function(w, width, height) {
    .gtk_set(w, "default_size", c(width, height))
    if (.gtk_is_realized(w)) tkwm.geometry(w$tk, sprintf("%dx%d", as.integer(width), as.integer(height)))
    invisible()
  },
  setsizerequest = function(w, width = -1, height = -1) {
    .gtk_set(w, "size_request", c(max(width, 1), max(height, 1)))
    if (.gtk_is_realized(w) && .gtk_get(w, "kind") %in% c("Window", "Dialog"))
      tkwm.minsize(w$tk, max(width, 1), max(height, 1))
    invisible()
  },
  resize = function(w, width, height) .gtk_methods$setdefaultsize(w, width, height),
  setposition = function(w, position) invisible(),
  setborderwidth = function(w, border.width) { w["border-width"] <- border.width; invisible() },
  showall = function(w) { .gtk_show(w); invisible() },
  show = function(w) { .gtk_show(w); invisible() },
  present = function(w) { .gtk_show(w); invisible() },
  hide = function(w) {
    if (.gtk_is_realized(w) && .gtk_get(w, "kind") %in% c("Window", "Dialog")) tkwm.withdraw(w$tk)
    invisible()
  },
  destroy = function(w) {
    if (.gtk_is_realized(w)) tkdestroy(w$tk)
    .gtk_set(w, "tk", NULL)
    invisible()
  },
  grabfocus = function(w) { if (.gtk_is_realized(w)) tkfocus(w$tk); invisible() },
  setdata = function(w, key, data) {
    d <- .gtk_get(w, "data")
    d[key] <- list(data)
    .gtk_set(w, "data", d)
    invisible()
  },
  getdata = function(w, key) .gtk_get(w, "data")[[key]],
  getcontentarea = function(w) .gtk_get(w, "content"),
  getactionarea = function(w) .gtk_get(w, "actions"),
  getvbox = function(w) .gtk_get(w, "content"),
  run = function(w) {
    res <- GtkResponseType[["none"]]
    gSignalConnect(w, "response", function(d, r) { res <<- r; gtkMainQuit() })
    .gtk_show(w)
    gtkMain()
    res
  },
  setpolicy = function(w, hscrollbar.policy, vscrollbar.policy) invisible(),
  # tree view / model
  setmodel = function(w, model) {
    .gtk_set(w, "model", model)
    .gtk_set(model, "views", c(.gtk_get(model, "views"), list(w)))
    .gtk_treeview_fill(w)
    invisible()
  },
  getmodel = function(w) .gtk_get(w, "model"),
  getselection = function(w) .gtk_get(w, "selection"),
  setmode = function(w, type) { .gtk_set(w, "mode", type); invisible() },
  insertcolumnwithattributes = function(w, position, title, cell, ...) {
    attrs <- list(...)
    idx <- as.integer(attrs$text %||% attrs$active %||% attrs[[1]] %||% 0L)
    cols <- .gtk_get(w, "columns")
    entry <- list(title = .gtk_label_text(title), cell = cell, index = idx)
    if (position < 0 || position >= length(cols)) cols[[length(cols) + 1]] <- entry
    else cols <- append(cols, list(entry), after = position)
    .gtk_set(w, "columns", cols)
    if (.gtk_is_realized(w)) { .gtk_treeview_columns(w); .gtk_treeview_fill(w) }
    invisible(length(cols))
  },
  getselected = function(w) {
    v <- .gtk_get(w, "view")
    model <- .gtk_get(v, "model")
    sel <- character(0)
    if (.gtk_is_realized(v)) sel <- as.character(tcl(v$tk, "selection"))
    if (!length(sel)) return(list(retval = FALSE, model = model, iter = NULL))
    row <- as.integer(sub("row", "", sel[1]))
    list(retval = TRUE, model = model, iter = .gtk_new("TreeIter", row = row))
  },
  getvalue = function(w, iter, column) {
    f <- .gtk_get(w, "frame")
    row <- .gtk_get(iter, "row")
    v <- f[row + 1L, column + 1L]
    # a Gtk list store holds strings, not factors
    if (is.factor(v)) v <- as.character(v)
    list(retval = NULL, value = v)
  },
  getpath = function(w, iter) .gtk_new("TreePath", indices = list(.gtk_get(iter, "row"))),
  getindices = function(w) .gtk_get(w, "indices"),
  tostring = function(w) as.character(.gtk_get(w, "indices")[[1]]),
  setframe = function(w, frame = data.frame()) {
    .gtk_set(w, "frame", as.data.frame(frame, stringsAsFactors = FALSE))
    .gtk_model_changed(w)
    invisible()
  },
  getframe = function(w) .gtk_get(w, "frame")
)
