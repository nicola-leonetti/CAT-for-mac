# Purpose   : spreadsheet-like data frame editor replacing RGtk2Extras::dfedit.
#             It is used by CAT for "Data Handling/Load/Copy and Paste" (data
#             copied from Excel or similar are pasted with Command-V) and to
#             modify/create matrices in the workspace manager and in DoE.
#             Cells: click to select, double-click / Return / typing to edit,
#             Tab and arrows to move, Command-V to paste a block at the
#             selected cell (the table grows as needed), Command-C to copy.
#             Column headers: double-click to rename.
# Licence   : GPL-3 (same as CAT)

.dfe <- new.env()   # last editor (used by automated tests)

dfedit <- function(items, dataset.name = deparse(substitute(items)),
                   size = c(600, 300), col.width = 64, editable = TRUE,
                   autosize = TRUE, update = TRUE, modal = TRUE, ...) {
  force(dataset.name)
  ed <- .dfe_new(items)
  .dfe$last <- ed
  res <- .dfe_window(ed, dataset.name, size, col.width)
  res
}

# --- model -----------------------------------------------------------------

.dfe_fmt <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    # element-wise: format() on a vector pads to a common number of decimals
    return(vapply(x, function(v) if (is.na(v)) "" else
      format(v, digits = 15, trim = TRUE, drop0trailing = TRUE), ""))
  }
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

.dfe_new <- function(items) {
  ed <- new.env()
  if (is.null(dim(items))) items <- data.frame(V1 = items)
  df <- as.data.frame(items, stringsAsFactors = FALSE, optional = TRUE)
  if (!ncol(df)) df <- data.frame(V1 = NA)
  if (!nrow(df)) df[1, ] <- NA
  ed$orig <- items
  ed$cells <- do.call(cbind, lapply(df, .dfe_fmt))
  if (is.null(dim(ed$cells))) ed$cells <- matrix(ed$cells, nrow = nrow(df))
  ed$colnames <- if (is.null(colnames(df))) paste0("V", seq_len(ncol(df))) else colnames(df)
  rn <- rownames(df)
  ed$auto_rownames <- is.null(rn) || identical(rn, as.character(seq_len(nrow(df))))
  ed$rownames <- if (is.null(rn)) as.character(seq_len(nrow(df))) else rn
  ed$was_matrix <- is.matrix(items)
  ed$cur <- c(1L, 1L)
  ed$changed <- FALSE
  ed
}

.dfe_resize <- function(ed, nr, nc) {
  cells <- ed$cells
  if (nr > nrow(cells)) {
    cells <- rbind(cells, matrix("", nr - nrow(cells), ncol(cells)))
    ed$rownames <- c(ed$rownames, as.character((length(ed$rownames) + 1):nr))
  }
  if (nc > ncol(cells)) {
    cells <- cbind(cells, matrix("", nrow(cells), nc - ncol(cells)))
    ed$colnames <- c(ed$colnames, paste0("V", (length(ed$colnames) + 1):nc))
  }
  ed$cells <- cells
  invisible()
}

.dfe_as_number <- function(v) {
  v <- trimws(v)
  na <- v == "" | toupper(v) %in% c("NA", "NAN", "#N/A")
  num <- suppressWarnings(as.numeric(v))
  if (all(!is.na(num) | na)) return(num)
  # decimal comma (e.g. data copied from an Italian Excel)
  if (!any(grepl(".", v[!na], fixed = TRUE))) {
    num2 <- suppressWarnings(as.numeric(sub(",", ".", v, fixed = TRUE)))
    if (all(!is.na(num2) | na)) return(num2)
  }
  NULL
}

.dfe_result <- function(ed) {
  cols <- lapply(seq_len(ncol(ed$cells)), function(j) {
    v <- ed$cells[, j]
    num <- .dfe_as_number(v)
    if (!is.null(num)) num else { v[trimws(v) == ""] <- NA; v }
  })
  names(cols) <- make.unique(ifelse(nzchar(ed$colnames), ed$colnames,
                                    paste0("V", seq_along(ed$colnames))))
  df <- as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE)
  if (!ed$auto_rownames && !anyDuplicated(ed$rownames)) rownames(df) <- ed$rownames
  df
}

# --- view ------------------------------------------------------------------

.dfe_window <- function(ed, name, size, col.width) {
  top <- tktoplevel()
  tkwm.title(top, if (grepl("^[._]", name) || !nzchar(name)) "Data editor" else paste("Data editor -", name))
  w <- max(as.numeric(size[1]), 520); h <- max(as.numeric(size[2]), 320)
  tkwm.geometry(top, sprintf("%dx%d", as.integer(min(w, 1200)), as.integer(min(h + 120, 800))))
  ed$top <- top
  ed$colw <- max(as.integer(col.width), 70L)

  tb <- ttkframe(top, padding = 4)
  tkpack(tb, side = "top", fill = "x")
  btn <- function(text, f) {
    b <- ttkbutton(tb, text = text, command = f)
    tkpack(b, side = "left", padx = 2)
  }
  btn("Add row", function() { .dfe_resize(ed, nrow(ed$cells) + 1, ncol(ed$cells)); .dfe_refresh(ed) })
  btn("Add column", function() { .dfe_resize(ed, nrow(ed$cells), ncol(ed$cells) + 1); .dfe_refresh(ed) })
  btn("Delete row", function() .dfe_delete(ed, "row"))
  btn("Delete column", function() .dfe_delete(ed, "col"))
  btn("Paste ⌘V", function() .dfe_paste(ed))
  ed$status <- tclVar("")
  st <- ttklabel(top, textvariable = ed$status, padding = c(6, 2))
  tkpack(st, side = "bottom", fill = "x")

  bottom <- ttkframe(top, padding = 6)
  tkpack(bottom, side = "bottom", fill = "x")
  done <- tclVar("")
  ok <- ttkbutton(bottom, text = "OK", default = "active",
                  command = function() { .dfe_commit_edit(ed); tclvalue(done) <- "ok" })
  cancel <- ttkbutton(bottom, text = "Cancel", command = function() tclvalue(done) <- "cancel")
  tkpack(ok, side = "right", padx = 4)
  tkpack(cancel, side = "right", padx = 4)
  hint <- ttklabel(bottom, foreground = "gray50",
                   text = "Double-click a cell to edit • ⌘V pastes data copied from Excel at the selected cell")
  tkpack(hint, side = "left")

  frm <- ttkframe(top)
  tkpack(frm, side = "top", fill = "both", expand = TRUE)
  tv <- ttktreeview(frm, show = "headings", selectmode = "none")
  ed$tv <- tv
  ys <- ttkscrollbar(frm, orient = "vertical", command = function(...) tkyview(tv, ...))
  xs <- ttkscrollbar(frm, orient = "horizontal", command = function(...) tkxview(tv, ...))
  tkconfigure(tv, yscrollcommand = function(...) { tkset(ys, ...); .dfe_place_cursor(ed) },
              xscrollcommand = function(...) { tkset(xs, ...); .dfe_place_cursor(ed) })
  tkgrid(tv, row = 0, column = 0, sticky = "nsew")
  tkgrid(ys, row = 0, column = 1, sticky = "ns")
  tkgrid(xs, row = 1, column = 0, sticky = "ew")
  tkgrid.rowconfigure(frm, 0, weight = 1)
  tkgrid.columnconfigure(frm, 0, weight = 1)
  # selected cell outline
  ed$hl <- lapply(1:4, function(i) tkframe(tv, background = "#2f6fdc", width = 2, height = 2))

  tkbind(tv, "<ButtonPress-1>", function(x, y) .dfe_click(ed, x, y))
  tkbind(tv, "<Double-Button-1>", function(x, y) .dfe_double(ed, x, y))
  tkbind(tv, "<Configure>", function() .dfe_place_cursor(ed))
  for (k in c("<Command-v>", "<Control-v>", "<<Paste>>"))
    tkbind(top, k, function() .dfe_paste(ed))
  for (k in c("<Command-c>", "<Control-c>", "<<Copy>>"))
    tkbind(top, k, function() .dfe_copy(ed))
  .dfe_bind_break(tv, "<Return>", function() .dfe_edit(ed))
  .dfe_bind_break(tv, "<Up>", function() .dfe_move(ed, -1, 0))
  .dfe_bind_break(tv, "<Down>", function() .dfe_move(ed, 1, 0))
  .dfe_bind_break(tv, "<Left>", function() .dfe_move(ed, 0, -1))
  .dfe_bind_break(tv, "<Right>", function() .dfe_move(ed, 0, 1))
  .dfe_bind_break(tv, "<Tab>", function() .dfe_move(ed, 0, 1))
  tkbind(tv, "<BackSpace>", function() .dfe_set(ed, ed$cur[1], ed$cur[2], ""))
  tkbind(tv, "<Delete>", function() .dfe_set(ed, ed$cur[1], ed$cur[2], ""))
  tkbind(tv, "<KeyPress>", function(A, K) {
    if (nchar(A) == 1 && grepl("[[:print:]]", A) && !(K %in% c("Tab", "Return")))
      .dfe_edit(ed, initial = A)
  })
  tcl("wm", "protocol", top, "WM_DELETE_WINDOW", function() { .dfe_commit_edit(ed); tclvalue(done) <- "ok" })

  .dfe_refresh(ed)
  tkfocus(tv)
  tkwait.variable(done)
  result <- tclvalue(done)
  tkdestroy(top)
  if (result == "cancel") {
    orig <- ed$orig
    return(if (is.null(dim(orig))) data.frame(V1 = orig) else as.data.frame(orig, stringsAsFactors = FALSE))
  }
  .dfe_result(ed)
}

.dfe_refresh <- function(ed) {
  tv <- ed$tv
  nc <- ncol(ed$cells)
  ids <- c("rn", paste0("c", seq_len(nc)))
  kids <- as.character(tcl(tv, "children", ""))
  if (length(kids)) tcl(tv, "delete", kids)
  tkconfigure(tv, columns = as.tclObj(ids, drop = FALSE))
  tcl(tv, "heading", "rn", text = "")
  tcl(tv, "column", "rn", width = 60, stretch = 0, anchor = "w")
  for (j in seq_len(nc)) local({
    jj <- j
    tcl(tv, "heading", ids[jj + 1], text = ed$colnames[jj],
        command = function() .dfe_rename(ed, jj))
    tcl(tv, "column", ids[jj + 1], width = ed$colw, stretch = 0, anchor = "e")
  })
  for (i in seq_len(nrow(ed$cells))) {
    tcl(tv, "insert", "", "end", id = paste0("r", i),
        values = as.tclObj(c(ed$rownames[i], ed$cells[i, ]), drop = FALSE))
  }
  tclvalue(ed$status) <- sprintf("%d rows × %d columns", nrow(ed$cells), nc)
  tcl("after", "idle", function() .dfe_place_cursor(ed))
  invisible()
}

.dfe_set <- function(ed, i, j, value) {
  ed$cells[i, j] <- value
  tcl(ed$tv, "set", paste0("r", i), paste0("c", j), value)
  ed$changed <- TRUE
  invisible()
}

.dfe_cell_at <- function(ed, x, y) {
  tv <- ed$tv
  item <- tclvalue(tcl(tv, "identify", "item", x, y))
  col <- tclvalue(tcl(tv, "identify", "column", x, y))
  region <- tclvalue(tcl(tv, "identify", "region", x, y))
  k <- suppressWarnings(as.integer(sub("#", "", col))) - 1L
  i <- suppressWarnings(as.integer(sub("r", "", item)))
  list(region = region, i = i, j = k)
}

.dfe_click <- function(ed, x, y) {
  .dfe_commit_edit(ed)
  tkfocus(ed$tv)
  p <- .dfe_cell_at(ed, x, y)
  if (p$region == "cell" && !is.na(p$i) && !is.na(p$j) && p$j >= 1) {
    ed$cur <- c(p$i, p$j)
    .dfe_place_cursor(ed)
  }
}

.dfe_double <- function(ed, x, y) {
  p <- .dfe_cell_at(ed, x, y)
  if (p$region == "heading" && !is.na(p$j) && p$j >= 1) .dfe_rename(ed, p$j)
  else if (p$region == "cell" && !is.na(p$j) && p$j >= 1) .dfe_edit(ed)
  else if (p$region == "cell" && identical(p$j, 0L)) .dfe_rename_row(ed, p$i)
}

.dfe_bbox <- function(ed, i, j) {
  bb <- as.character(tcl(ed$tv, "bbox", paste0("r", i), paste0("c", j)))
  if (length(bb) != 4) return(NULL)
  as.integer(bb)
}

.dfe_place_cursor <- function(ed) {
  bb <- .dfe_bbox(ed, ed$cur[1], ed$cur[2])
  if (is.null(bb)) {
    for (f in ed$hl) tkplace.forget(f)
    return(invisible())
  }
  x <- bb[1]; y <- bb[2]; w <- bb[3]; h <- bb[4]
  tkplace(ed$hl[[1]], x = x, y = y, width = w, height = 2)
  tkplace(ed$hl[[2]], x = x, y = y + h - 2, width = w, height = 2)
  tkplace(ed$hl[[3]], x = x, y = y, width = 2, height = h)
  tkplace(ed$hl[[4]], x = x + w - 2, y = y, width = 2, height = h)
  tclvalue(ed$status) <- sprintf("%d rows × %d columns    selected: row %s, column %s",
                                 nrow(ed$cells), ncol(ed$cells),
                                 ed$rownames[ed$cur[1]], ed$colnames[ed$cur[2]])
  invisible()
}

.dfe_move <- function(ed, di, dj) {
  .dfe_commit_edit(ed)
  i <- min(max(ed$cur[1] + di, 1L), nrow(ed$cells))
  j <- min(max(ed$cur[2] + dj, 1L), ncol(ed$cells))
  ed$cur <- c(i, j)
  tcl(ed$tv, "see", paste0("r", i))
  .dfe_scroll_to_col(ed, j)
  .dfe_place_cursor(ed)
}

# binds an R function to a key and stops the default class binding
.dfe_bind_break <- function(w, event, f) {
  .Tcl(sprintf("bind %s %s {%s; break}", w$ID, event, .Tcl.callback(f, w$env)))
}

.dfe_scroll_to_col <- function(ed, j) {
  bb <- .dfe_bbox(ed, ed$cur[1], j)
  width <- as.integer(tkwinfo("width", ed$tv))
  if (is.null(bb) || bb[1] < 60 || bb[1] + bb[3] > width) {
    total <- 60 + ed$colw * ncol(ed$cells)
    frac <- max(0, (60 + ed$colw * (j - 1) - 60) / total)
    tkxview.moveto(ed$tv, frac)
  }
}

.dfe_edit <- function(ed, initial = NULL) {
  .dfe_commit_edit(ed)
  i <- ed$cur[1]; j <- ed$cur[2]
  tcl(ed$tv, "see", paste0("r", i))
  tcl("update", "idletasks")
  bb <- .dfe_bbox(ed, i, j)
  if (is.null(bb)) return(invisible())
  var <- tclVar(if (is.null(initial)) ed$cells[i, j] else initial)
  e <- ttkentry(ed$tv, textvariable = var, justify = "right")
  tkplace(e, x = bb[1], y = bb[2], width = max(bb[3], 80), height = bb[4])
  ed$editor <- list(entry = e, var = var, i = i, j = j)
  tkfocus(e)
  if (is.null(initial)) tcl(e, "selection", "range", 0, "end") else tcl(e, "icursor", "end")
  .dfe_bind_break(e, "<Return>", function() { .dfe_commit_edit(ed); .dfe_move(ed, 1, 0) })
  .dfe_bind_break(e, "<Tab>", function() { .dfe_commit_edit(ed); .dfe_move(ed, 0, 1) })
  .dfe_bind_break(e, "<Escape>", function() .dfe_cancel_edit(ed))
  tkbind(e, "<FocusOut>", function() .dfe_commit_edit(ed))
  invisible()
}

.dfe_commit_edit <- function(ed) {
  edt <- ed$editor
  if (is.null(edt)) return(invisible())
  ed$editor <- NULL
  .dfe_set(ed, edt$i, edt$j, tclvalue(edt$var))
  tkdestroy(edt$entry)
  tkfocus(ed$tv)
  invisible()
}

.dfe_cancel_edit <- function(ed) {
  edt <- ed$editor
  if (is.null(edt)) return(invisible())
  ed$editor <- NULL
  tkdestroy(edt$entry)
  tkfocus(ed$tv)
  invisible()
}

.dfe_ask <- function(ed, title, label, value) {
  res <- NULL
  d <- tktoplevel(ed$top)
  tkwm.title(d, title)
  f <- ttkframe(d, padding = 12)
  tkpack(f, fill = "both", expand = TRUE)
  var <- tclVar(value)
  tkgrid(ttklabel(f, text = label), row = 0, column = 0, sticky = "w", padx = 4)
  e <- ttkentry(f, textvariable = var, width = 24)
  tkgrid(e, row = 0, column = 1, padx = 4)
  done <- tclVar(0)
  okf <- function() { res <<- tclvalue(var); tclvalue(done) <- 1 }
  bf <- ttkframe(f)
  tkgrid(bf, row = 1, column = 0, columnspan = 2, pady = c(10, 0), sticky = "e")
  tkpack(ttkbutton(bf, text = "OK", command = okf), side = "right", padx = 2)
  tkpack(ttkbutton(bf, text = "Cancel", command = function() tclvalue(done) <- 1),
         side = "right", padx = 2)
  tkbind(d, "<Return>", okf)
  tkbind(d, "<Escape>", function() tclvalue(done) <- 1)
  tcl("wm", "protocol", d, "WM_DELETE_WINDOW", function() tclvalue(done) <- 1)
  tkfocus(e)
  tcl(e, "selection", "range", 0, "end")
  tkwait.variable(done)
  tkdestroy(d)
  res
}

.dfe_rename <- function(ed, j) {
  nm <- .dfe_ask(ed, "Column name", "Column name:", ed$colnames[j])
  if (!is.null(nm) && nzchar(nm)) {
    ed$colnames[j] <- nm
    tcl(ed$tv, "heading", paste0("c", j), text = nm)
  }
}

.dfe_rename_row <- function(ed, i) {
  if (is.na(i)) return(invisible())
  nm <- .dfe_ask(ed, "Row name", "Row name:", ed$rownames[i])
  if (!is.null(nm) && nzchar(nm)) {
    ed$rownames[i] <- nm
    ed$auto_rownames <- FALSE
    tcl(ed$tv, "set", paste0("r", i), "rn", nm)
  }
}

.dfe_delete <- function(ed, what) {
  .dfe_commit_edit(ed)
  i <- ed$cur[1]; j <- ed$cur[2]
  if (what == "row" && nrow(ed$cells) > 1) {
    ed$cells <- ed$cells[-i, , drop = FALSE]
    ed$rownames <- ed$rownames[-i]
    if (ed$auto_rownames) ed$rownames <- as.character(seq_len(nrow(ed$cells)))
    ed$cur[1] <- min(i, nrow(ed$cells))
  } else if (what == "col" && ncol(ed$cells) > 1) {
    ed$cells <- ed$cells[, -j, drop = FALSE]
    ed$colnames <- ed$colnames[-j]
    ed$cur[2] <- min(j, ncol(ed$cells))
  } else {
    tkbell()
  }
  .dfe_refresh(ed)
}

.dfe_clipboard <- function() {
  txt <- tryCatch(tclvalue(tcl("clipboard", "get")), error = function(e) "")
  if (!nzchar(txt)) txt <- paste(system2("pbpaste", stdout = TRUE), collapse = "\n")
  txt
}

.dfe_paste <- function(ed) {
  .dfe_commit_edit(ed)
  txt <- .dfe_clipboard()
  txt <- gsub("\r\n?", "\n", txt)
  txt <- sub("\n+$", "", txt)
  if (!nzchar(txt)) {
    tkbell()
    return(invisible())
  }
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  rows <- strsplit(lines, "\t", fixed = TRUE)
  nc <- max(lengths(rows))
  block <- t(vapply(rows, function(r) c(r, rep("", nc - length(r))), character(nc)))
  if (nc == 1) block <- matrix(block, ncol = 1)
  i0 <- ed$cur[1]; j0 <- ed$cur[2]
  is_num <- function(v) !is.null(.dfe_as_number(v))
  header <- FALSE; rnames <- FALSE
  if (nrow(block) > 1 && i0 == 1) {
    first <- block[1, ]
    data_cols_numeric <- vapply(seq_len(nc), function(k) is_num(block[-1, k]), TRUE)
    if (any(data_cols_numeric & !vapply(first, is_num, TRUE)) || all(!vapply(first, is_num, TRUE) & nzchar(first))) {
      header <- .dfe_yesno(ed, "Use the first pasted row as column names?")
    }
  }
  if (header) {
    hdr <- block[1, ]
    block <- block[-1, , drop = FALSE]
  }
  if (j0 == 1 && nc > 1 && nrow(block) >= 1) {
    fc <- block[, 1]
    if (!is_num(fc) && !anyDuplicated(fc) && all(nzchar(fc))) {
      rnames <- .dfe_yesno(ed, "Use the first pasted column as row names?")
    }
  }
  if (rnames) {
    rn <- block[, 1]
    block <- block[, -1, drop = FALSE]
    if (header) hdr <- hdr[-1]
  }
  # a fresh 1 x 1 table (Copy and Paste loading) is replaced by the pasted data
  fresh <- nrow(ed$cells) == 1 && ncol(ed$cells) == 1 && ed$cells[1, 1] %in% c("", "0")
  if (fresh) {
    ed$cells <- matrix("", nrow(block), ncol(block))
    ed$colnames <- paste0("V", seq_len(ncol(block)))
    ed$rownames <- as.character(seq_len(nrow(block)))
    ed$auto_rownames <- TRUE
    i0 <- 1L; j0 <- 1L
  }
  .dfe_resize(ed, i0 + nrow(block) - 1L, j0 + ncol(block) - 1L)
  if (length(block))
    ed$cells[i0:(i0 + nrow(block) - 1L), j0:(j0 + ncol(block) - 1L)] <- block
  if (header) ed$colnames[j0:(j0 + length(hdr) - 1L)] <- hdr
  if (rnames) {
    ed$rownames[i0:(i0 + length(rn) - 1L)] <- rn
    ed$auto_rownames <- FALSE
  }
  ed$changed <- TRUE
  ed$cur <- c(i0, j0)
  .dfe_refresh(ed)
  invisible()
}

.dfe_yesno <- function(ed, msg) {
  f <- getOption("CAT.test.yesno")
  if (is.function(f)) return(f(msg))
  tclvalue(tkmessageBox(parent = ed$top, type = "yesno", icon = "question",
                        title = "Paste", message = msg)) == "yes"
}

.dfe_copy <- function(ed) {
  v <- ed$cells[ed$cur[1], ed$cur[2]]
  tcl("clipboard", "clear")
  tcl("clipboard", "append", v)
}
