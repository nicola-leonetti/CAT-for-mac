# Purpose   : main CAT window for macOS. It reproduces the Windows R-GUI used by
#             CAT: a menu bar with the CAT menus (built by mylib/menu.r through
#             winMenuAdd/winMenuAddItem) on top of an R console where results
#             are printed and R commands can be typed.
# Licence   : GPL-3 (same as CAT)

suppressPackageStartupMessages(library(tcltk))

.con <- new.env()
.con$busy <- FALSE
.con$buffer <- character(0)   # lines of an incomplete command
.con$history <- character(0)
.con$hpos <- 0L
.con$menus <- list()          # "Top/Sub" -> tk menu
.con$tops <- character(0)     # top-level CAT menus, in order
.con$labels <- list()         # menu bar labels
.con$outfile <- NULL
.con$outpos <- 0
.con$outcon <- NULL

# ---------------------------------------------------------------------------
# output: everything R prints goes to a log file that is copied into the
# console widget (this also captures output written by compiled code)
# ---------------------------------------------------------------------------

.cat_start_output <- function(file) {
  .con$outfile <- file
  if (file.exists(file)) unlink(file)
  zz <- file(file, open = "wt", encoding = "UTF-8")
  .con$outcon <- zz
  sink(zz, type = "output")
  sink(zz, type = "message")
  .con$outpos <- 0
  invisible()
}

.cat_flush_output <- function() {
  if (is.null(.con$outcon)) return(invisible())
  try(flush(.con$outcon), silent = TRUE)
  size <- file.size(.con$outfile)
  if (is.na(size) || size <= .con$outpos) return(invisible())
  rc <- file(.con$outfile, open = "rb")
  on.exit(close(rc))
  seek(rc, .con$outpos)
  bytes <- readBin(rc, "raw", n = size - .con$outpos)
  .con$outpos <- .con$outpos + length(bytes)
  txt <- rawToChar(bytes)
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) txt <- iconv(txt, "latin1", "UTF-8")
  .cat_write(txt, "output")
  invisible()
}

.cat_poll <- function() {
  try(.cat_flush_output(), silent = TRUE)
  .con$ticks <- (.con$ticks %||% 0L) + 1L
  if (.con$ticks %% 3L == 0L) try(.gd_refresh_all(), silent = TRUE)
  tcl("after", 150, .cat_poll)
}

# writes text before the input line (the prompt and what the user is typing
# stay at the bottom)
.cat_write <- function(txt, tag = "output") {
  w <- .con$text
  if (is.null(w) || !nchar(txt)) return(invisible())
  .Tcl("set ::catcon::output 1")
  on.exit(.Tcl("set ::catcon::output 0"))
  tkinsert(w, "promptStart", txt, tag)
  # keep the console from growing without limits (like buflines in Rgui)
  nlines <- as.integer(tclvalue(tkindex(w, "end")))
  if (!is.na(nlines) && nlines > 20000) tkdelete(w, "1.0", sprintf("%d.0", nlines - 15000))
  tksee(w, "insert")
  invisible()
}

.cat_prompt <- function(continuation = FALSE) {
  w <- .con$text
  .Tcl("set ::catcon::output 1")
  on.exit(.Tcl("set ::catcon::output 0"))
  tkmark.set(w, "promptStart", "end-1c")
  tcl(w, "mark", "gravity", "promptStart", "left")
  tkinsert(w, "end", if (continuation) "+ " else "> ", "prompt")
  tkmark.set(w, "inputStart", "end-1c")
  tcl(w, "mark", "gravity", "inputStart", "left")
  tkmark.set(w, "insert", "end")
  tksee(w, "insert")
  invisible()
}

# ---------------------------------------------------------------------------
# command evaluation (console input and menu actions)
# ---------------------------------------------------------------------------

.cat_set_busy <- function(busy) {
  .con$busy <- busy
  cursor <- if (busy) "watch" else ""
  try({
    tkconfigure(.con$top, cursor = cursor)
    tkconfigure(.con$text, cursor = if (busy) "watch" else "xterm")
    tcl("update", "idletasks")
  }, silent = TRUE)
  invisible()
}

.cat_print_warnings <- function(ws) {
  if (!length(ws)) return(invisible())
  fmt <- function(w) {
    call <- conditionCall(w)
    if (is.null(call)) conditionMessage(w)
    else paste0("In ", paste(deparse(call, nlines = 1L), collapse = ""), " : ",
                conditionMessage(w))
  }
  if (length(ws) == 1) {
    message("Warning message:\n", fmt(ws[[1]]))
  } else {
    n <- min(length(ws), 50L)
    message(if (length(ws) <= 10) "Warning messages:" else
      sprintf("There were %d warnings (showing the first %d):", length(ws), n))
    for (k in seq_len(n)) message(k, ": ", fmt(ws[[k]]))
  }
  invisible()
}

# Evaluates parsed expressions in the global environment the way the R
# top level does: visible values are printed, errors are reported and
# warnings are shown at the end.
.cat_eval_exprs <- function(exprs) {
  ws <- list()
  silent <- FALSE
  for (e in exprs) {
    ok <- tryCatch(
      withCallingHandlers({
        res <- withVisible(eval(e, envir = .GlobalEnv))
        if (res$visible) print(res$value)
        TRUE
      },
      warning = function(w) {
        # deprecation notices of current packages mean nothing to CAT users
        if (inherits(w, c("lifecycle_warning_deprecated", "deprecatedWarning")))
          invokeRestart("muffleWarning")
        if (getOption("warn", 0) >= 1) return()
        ws[[length(ws) + 1]] <<- w
        invokeRestart("muffleWarning")
      },
      # CAT scripts stop silently with options(show.error.messages = FALSE);
      # the option is restored by on.exit() before the error is handled here
      error = function(err) silent <<- identical(getOption("show.error.messages"), FALSE)),
      error = function(err) {
        if (!silent) .cat_report_error(err)
        FALSE
      },
      interrupt = function(i) {
        message("\nInterrupted")
        FALSE
      })
    if (!isTRUE(ok)) break
  }
  .cat_print_warnings(ws)
  options(show.error.messages = TRUE)
  try(.gd_refresh_all(), silent = TRUE)
  invisible()
}

.cat_run_text <- function(code, echo = TRUE) {
  if (.con$busy) {
    tkbell()
    return(invisible(FALSE))
  }
  .cat_set_busy(TRUE)
  on.exit({
    .cat_flush_output()
    .cat_set_busy(FALSE)
  })
  if (echo) {
    .cat_flush_output()
    w <- .con$text
    .Tcl("set ::catcon::output 1")
    tkdelete(w, "inputStart", "end-1c")
    tkinsert(w, "end", paste0(code, "\n"), "input")
    .Tcl("set ::catcon::output 0")
    .cat_prompt_reset()
  }
  exprs <- tryCatch(parse(text = code, keep.source = FALSE),
                    error = function(e) { .cat_report_error(e); NULL })
  if (!is.null(exprs)) .cat_eval_exprs(exprs)
  invisible(TRUE)
}

# after an echoed command the old prompt line becomes part of the output
.cat_prompt_reset <- function() {
  w <- .con$text
  tkmark.set(w, "promptStart", "end-1c")
  tkmark.set(w, "inputStart", "end-1c")
}

.cat_submit <- function() {
  w <- .con$text
  if (.con$busy) {
    tkbell()
    return(invisible())
  }
  line <- tclvalue(tkget(w, "inputStart", "end-1c"))
  .Tcl("set ::catcon::output 1")
  tkmark.set(w, "insert", "end")
  tkinsert(w, "end", "\n")
  tktag.add(w, "input", "inputStart", "end-1c")
  .Tcl("set ::catcon::output 0")
  .cat_prompt_reset()
  if (nzchar(trimws(line))) {
    .con$history <- c(.con$history[.con$history != line], line)
  }
  .con$hpos <- length(.con$history) + 1L
  .con$buffer <- c(.con$buffer, line)
  code <- paste(.con$buffer, collapse = "\n")
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) e)
  if (inherits(exprs, "error") &&
      grepl("unexpected end of input|INCOMPLETE_STRING", conditionMessage(exprs))) {
    .cat_prompt(continuation = TRUE)
    return(invisible())
  }
  .con$buffer <- character(0)
  if (inherits(exprs, "error")) {
    .cat_report_error(exprs)
  } else if (length(exprs)) {
    .cat_set_busy(TRUE)
    .cat_eval_exprs(exprs)
    .cat_set_busy(FALSE)
  }
  .cat_flush_output()
  .cat_prompt()
  invisible()
}

.cat_history <- function(step) {
  h <- .con$history
  if (!length(h)) return(invisible())
  pos <- min(max(.con$hpos + step, 1L), length(h) + 1L)
  .con$hpos <- pos
  w <- .con$text
  tkdelete(w, "inputStart", "end-1c")
  if (pos <= length(h)) tkinsert(w, "end", h[pos])
  tkmark.set(w, "insert", "end")
  tksee(w, "insert")
  invisible()
}

# Runs a menu action as if it had been typed at the console (as the Windows
# R-GUI does), then shows a new prompt.
.cat_menu_action <- function(action) {
  if (.con$busy) {
    tkbell()
    return(invisible())
  }
  pending <- tclvalue(tkget(.con$text, "inputStart", "end-1c"))
  if (length(.con$buffer)) .con$buffer <- character(0)
  .cat_run_text(action, echo = TRUE)
  .cat_prompt()
  if (nzchar(pending)) tkinsert(.con$text, "end", pending)
  invisible()
}

# ---------------------------------------------------------------------------
# menus: Windows R-GUI API used by mylib/menu.r
# ---------------------------------------------------------------------------

.cat_menu_widget <- function(path) {
  m <- .con$menus[[path]]
  if (!is.null(m)) return(m)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (length(parts) == 1) {
    m <- tkmenu(.con$top, tearoff = FALSE)
    .con$menus[[path]] <- m
    .con$tops <- c(.con$tops, path)
    .cat_add_bar_label(path, m)
  } else {
    parent_path <- paste(parts[-length(parts)], collapse = "/")
    pm <- .cat_menu_widget(parent_path)
    m <- tkmenu(pm, tearoff = FALSE)
    tkadd(pm, "cascade", label = parts[length(parts)], menu = m)
    .con$menus[[path]] <- m
  }
  m
}

winMenuAdd <- function(menuname) {
  .cat_menu_widget(menuname)
  invisible()
}

winMenuAddItem <- function(menuname, itemname, action) {
  m <- .cat_menu_widget(menuname)
  if (identical(itemname, "-")) {
    tkadd(m, "separator")
  } else if (nzchar(itemname)) {
    act <- action
    tkadd(m, "command", label = itemname,
          command = function() .cat_menu_action(act))
  }
  invisible()
}

winMenuDel <- function(menuname) {
  m <- .con$menus[[menuname]]
  if (!is.null(m)) {
    lab <- .con$labels[[menuname]]
    if (!is.null(lab)) tkdestroy(lab)
    tkdestroy(m)
    keep <- !startsWith(names(.con$menus), menuname)
    .con$menus <- .con$menus[keep]
    .con$tops <- setdiff(.con$tops, menuname)
    .con$labels[[menuname]] <- NULL
    .cat_layout_bar()
  }
  invisible()
}

winMenuDelItem <- function(menuname, itemname) {
  m <- .con$menus[[menuname]]
  if (!is.null(m)) try(tkdelete(m, itemname), silent = TRUE)
  invisible()
}

winMenuNames <- function() names(.con$menus)

winMenuItems <- function(menuname) {
  m <- .con$menus[[menuname]]
  if (is.null(m)) return(character(0))
  last <- as.integer(tclvalue(tkindex(m, "end")))
  if (is.na(last)) return(character(0))
  labs <- vapply(0:last, function(k)
    tryCatch(tclvalue(tkentrycget(m, k, "-label")), error = function(e) ""), "")
  labs[nzchar(labs)]
}

# menu bar inside the window (always visible, wraps on narrow windows like
# the R-GUI menu bar does)
.cat_add_bar_label <- function(name, menu) {
  bar <- .con$bar
  lab <- tklabel(bar, text = name, padx = 7, pady = 3, font = "TkMenuFont",
                 bg = .con$colors$bar, fg = .con$colors$barfg)
  tkbind(lab, "<Enter>", function() tkconfigure(lab, bg = .con$colors$hl, fg = .con$colors$hlfg))
  tkbind(lab, "<Leave>", function() tkconfigure(lab, bg = .con$colors$bar, fg = .con$colors$barfg))
  tkbind(lab, "<ButtonPress-1>", function() {
    x <- as.integer(tkwinfo("rootx", lab))
    y <- as.integer(tkwinfo("rooty", lab)) + as.integer(tkwinfo("height", lab))
    tkpopup(menu, x, y)
    tkconfigure(lab, bg = .con$colors$bar, fg = .con$colors$barfg)
  })
  .con$labels[[name]] <- lab
  .cat_layout_bar()
}

.cat_layout_bar <- function() {
  bar <- .con$bar
  width <- as.integer(tkwinfo("width", bar))
  if (is.na(width) || width < 50) width <- 900L
  x <- 0L; y <- 0L; rowh <- 0L
  for (name in .con$tops) {
    lab <- .con$labels[[name]]
    if (is.null(lab)) next
    lw <- as.integer(tkwinfo("reqwidth", lab))
    lh <- as.integer(tkwinfo("reqheight", lab))
    if (x > 0 && x + lw > width) { y <- y + rowh; x <- 0L; rowh <- 0L }
    tkplace(lab, x = x, y = y)
    x <- x + lw
    rowh <- max(rowh, lh)
  }
  height <- y + rowh
  if (as.integer(tclvalue(tkcget(bar, "-height"))) != height) tkconfigure(bar, height = height)
  invisible()
}

tkpopup <- function(menu, x, y) tcl("tk_popup", menu, x, y)

# ---------------------------------------------------------------------------
# window
# ---------------------------------------------------------------------------

.cat_tcl_setup <- function() {
  .Tcl('
namespace eval ::catcon { variable output 0 }
# the text widget is read-only except for the input line after the prompt
proc ::catcon::wrap {w} {
  rename $w ::catcon::_$w
  proc ::$w {cmd args} [string map [list %W $w] {
    if {!$::catcon::output} {
      switch -- $cmd {
        insert {
          if {[::catcon::_%W compare [lindex $args 0] < inputStart]} {
            ::catcon::_%W mark set insert end
            set args [lreplace $args 0 0 end]
          }
        }
        delete - replace {
          set i1 [lindex $args 0]
          if {[::catcon::_%W compare $i1 < inputStart]} {
            if {[llength $args] > 1 && [::catcon::_%W compare [lindex $args 1] > inputStart]} {
              set args [lreplace $args 0 0 inputStart]
            } else {
              return
            }
          }
        }
      }
    }
    uplevel 1 [list ::catcon::_%W $cmd {*}$args]
  }]
}
')
}

.cat_open_url_or_file <- function(path) {
  system2("open", shQuote(path), wait = FALSE)
}

.cat_build_window <- function() {
  .cat_tcl_setup()
  # Tk on macOS opens its own Tcl console when started without a terminal
  try(.Tcl("catch {console hide}"), silent = TRUE)
  tcl("wm", "withdraw", ".")
  dark <- tryCatch(tclvalue(.Tcl("::tk::unsupported::MacWindowStyle isdark .")) == "1",
                   error = function(e) FALSE)
  .con$colors <- list(
    bar = if (dark) "#3a3a3c" else "#ececec", barfg = if (dark) "#f2f2f2" else "black",
    hl = "#2f6fdc", hlfg = "white")

  top <- tktoplevel()
  .con$top <- top
  options(CAT.mainwindow = top)
  tkwm.title(top, "CAT - Chemometric Agile Tool")
  tkwm.geometry(top, getOption("CAT.geometry", "980x640"))
  tkwm.minsize(top, 520, 300)

  bar <- tkframe(top, bg = .con$colors$bar)
  .con$bar <- bar
  tkpack(bar, side = "top", fill = "x")
  sep <- ttkseparator(top, orient = "horizontal")
  tkpack(sep, side = "top", fill = "x")
  tkbind(bar, "<Configure>", function() .cat_layout_bar())

  frm <- ttkframe(top)
  tkpack(frm, side = "top", fill = "both", expand = TRUE)
  font <- getOption("CAT.font", "Menlo 12")
  txt <- tktext(frm, wrap = "char", undo = FALSE, font = font,
                bg = "white", fg = "navy", insertbackground = "black",
                highlightthickness = 0, borderwidth = 0, padx = 6, pady = 4,
                selectbackground = "#b4d5fe", selectforeground = "black")
  sb <- ttkscrollbar(frm, orient = "vertical", command = function(...) tkyview(txt, ...))
  tkconfigure(txt, yscrollcommand = function(...) tkset(sb, ...))
  tkpack(sb, side = "right", fill = "y")
  tkpack(txt, side = "left", fill = "both", expand = TRUE)
  .con$text <- txt
  # colours of the CAT Rconsole file: navy output, red user input
  tktag.configure(txt, "output", foreground = "navy")
  tktag.configure(txt, "prompt", foreground = "red")
  tktag.configure(txt, "input", foreground = "red")
  tktag.configure(txt, "error", foreground = "darkred")
  tkmark.set(txt, "promptStart", "end-1c")
  tkmark.set(txt, "inputStart", "end-1c")
  tcl("::catcon::wrap", txt$ID)

  submit <- .Tcl.callback(function() .cat_submit(), txt$env)
  up <- .Tcl.callback(function() .cat_history(-1L), txt$env)
  down <- .Tcl.callback(function() .cat_history(1L), txt$env)
  id <- txt$ID
  .Tcl(sprintf("bind %s <Return> {%s; break}", id, submit))
  .Tcl(sprintf("bind %s <KP_Enter> {%s; break}", id, submit))
  .Tcl(sprintf("bind %s <Up> {%s; break}", id, up))
  .Tcl(sprintf("bind %s <Down> {%s; break}", id, down))
  .Tcl(sprintf("bind %s <Home> {%s mark set insert inputStart; break}", id, id))
  .Tcl(sprintf("bind %s <Control-a> {%s mark set insert inputStart; break}", id, id))
  .Tcl(sprintf("bind %s <Command-l> {::catcon::clear %s; break}", id, id))
  .Tcl(sprintf("bind %s <Control-l> {::catcon::clear %s; break}", id, id))
  .Tcl('proc ::catcon::clear {w} {
          set ::catcon::output 1
          ::catcon::_$w delete 1.0 promptStart
          set ::catcon::output 0 }')
  .cat_native_menus(top)
  tcl("wm", "protocol", top, "WM_DELETE_WINDOW", function() .cat_quit())
  tkfocus(txt)
  invisible(top)
}

.cat_native_menus <- function(top) {
  mb <- tkmenu(top)
  tkconfigure(top, menu = mb)
  # a menu named <menubar>.apple becomes the macOS application menu
  .Tcl(sprintf("menu %s.apple -tearoff 0", mb$ID))
  .Tcl(sprintf("%s add cascade -menu %s.apple", mb$ID, mb$ID))
  .Tcl(sprintf("%s.apple add command -label {About CAT} -command {%s}", mb$ID,
               .Tcl.callback(function() .cat_about(), mb$env)))

  file <- tkmenu(mb, tearoff = FALSE)
  tkadd(mb, "cascade", label = "File", menu = file)
  tkadd(file, "command", label = "Source R Script...", accelerator = "Command-O",
        command = function() .cat_source_dialog())
  tkadd(file, "separator")
  tkadd(file, "command", label = "Load Workspace...",
        command = function() .cat_load_workspace_dialog())
  tkadd(file, "command", label = "Save Workspace...", accelerator = "Command-S",
        command = function() .cat_save_workspace_dialog())
  tkadd(file, "separator")
  tkadd(file, "command", label = "Change Working Directory...",
        command = function() .cat_chdir_dialog())
  tkadd(file, "command", label = "Show Working Directory in Finder",
        command = function() .cat_open_url_or_file(getwd()))
  tkadd(file, "separator")
  tkadd(file, "command", label = "Save Console Text...",
        command = function() .cat_save_console())
  tkadd(file, "command", label = "Close All Plots",
        command = function() .cat_menu_action("graphics.off()"))

  edit <- tkmenu(mb, tearoff = FALSE)
  tkadd(mb, "cascade", label = "Edit", menu = edit)
  tkadd(edit, "command", label = "Copy", accelerator = "Command-C",
        command = function() tcl("event", "generate", tcl("focus"), "<<Copy>>"))
  tkadd(edit, "command", label = "Paste", accelerator = "Command-V",
        command = function() tcl("event", "generate", tcl("focus"), "<<Paste>>"))
  tkadd(edit, "command", label = "Select All", accelerator = "Command-A",
        command = function() tcl("event", "generate", tcl("focus"), "<<SelectAll>>"))
  tkadd(edit, "separator")
  tkadd(edit, "command", label = "Clear Console", accelerator = "Command-L",
        command = function() .Tcl(sprintf("::catcon::clear %s", .con$text$ID)))

  win <- .Tcl(sprintf("menu %s.window -tearoff 0", mb$ID))
  .Tcl(sprintf("%s add cascade -label Window -menu %s.window", mb$ID, mb$ID))
  .Tcl(sprintf("%s.window add command -label {CAT Console} -command {%s}", mb$ID,
               .Tcl.callback(function() { tkwm.deiconify(top); tkraise(top); tkfocus(.con$text) },
                             mb$env)))

  help <- .Tcl(sprintf("menu %s.help -tearoff 0", mb$ID))
  .Tcl(sprintf("%s add cascade -label Help -menu %s.help", mb$ID, mb$ID))
  hcb <- function(f) .Tcl.callback(f, mb$env)
  .Tcl(sprintf("%s.help add command -label {CAT Instructions (English)} -command {%s}", mb$ID,
               hcb(function() .cat_open_url_or_file(file.path(script.dir, "Instructions.pdf")))))
  .Tcl(sprintf("%s.help add command -label {CAT Istruzioni (Italiano)} -command {%s}", mb$ID,
               hcb(function() .cat_open_url_or_file(file.path(script.dir, "Istruzioni.pdf")))))
  .Tcl(sprintf("%s.help add command -label {CAT web site} -command {%s}", mb$ID,
               hcb(function() browseURL("https://gruppochemiometria.it/index.php/software"))))
  .Tcl(sprintf("%s.help add separator", mb$ID))
  .Tcl(sprintf("%s.help add command -label {R Help (HTML)} -command {%s}", mb$ID,
               hcb(function() .cat_menu_action("help.start()"))))

  # application level handlers of Tk on macOS
  .Tcl(sprintf("proc ::tk::mac::Quit {} {%s}", .Tcl.callback(function() .cat_quit(), mb$env)))
  .Tcl(sprintf("proc ::tk::mac::ShowPreferences {} {}"))
  .Tcl(sprintf("proc ::tk::mac::ReopenApplication {} {wm deiconify %s; raise %s}", top$ID, top$ID))
  tkbind(top, "<Command-o>", function() .cat_source_dialog())
  tkbind(top, "<Command-s>", function() .cat_save_workspace_dialog())
  invisible()
}

.cat_about <- function() {
  tk_messageBox(type = "ok", caption = "About CAT",
                message = paste0("CAT - Chemometric Agile Tool\n",
                                 "Gruppo di Chemiometria (Società Chimica Italiana)\n\n",
                                 "R scripts by Camillo Melzi and Gianmarco Polotti\n",
                                 "Chemometric consultancy by Riccardo Leardi\n\n",
                                 "macOS edition running on ", R.version.string))
}

.cat_source_dialog <- function() {
  f <- tclvalue(tkgetOpenFile(title = "Source R script",
                              filetypes = "{{R scripts} {.R .r}} {{All files} *}"))
  if (nzchar(f)) .cat_menu_action(sprintf("source(%s)", deparse(f)))
}

.cat_load_workspace_dialog <- function() {
  f <- tclvalue(tkgetOpenFile(title = "Load workspace",
                              filetypes = "{{R workspace} {.RData .rda}} {{All files} *}"))
  if (nzchar(f)) .cat_menu_action(sprintf("load(%s)", deparse(f)))
}

.cat_save_workspace_dialog <- function() {
  f <- tclvalue(tkgetSaveFile(title = "Save workspace", initialfile = ".RData",
                              defaultextension = ".RData",
                              filetypes = "{{R workspace} {.RData}}"))
  if (nzchar(f)) .cat_menu_action(sprintf("save.image(%s)", deparse(f)))
}

.cat_chdir_dialog <- function() {
  d <- tclvalue(tkchooseDirectory(title = "Choose working directory", initialdir = getwd()))
  if (nzchar(d)) {
    .cat_menu_action(sprintf("setwd(%s)", deparse(d)))
    assign("working.dir", getwd(), envir = .GlobalEnv)
  }
}

.cat_save_console <- function() {
  f <- tclvalue(tkgetSaveFile(title = "Save console text", initialfile = "CAT console.txt",
                              defaultextension = ".txt"))
  if (nzchar(f)) writeLines(tclvalue(tkget(.con$text, "1.0", "end-1c")), f, useBytes = TRUE)
}

# ---------------------------------------------------------------------------
# quitting (asks to save the workspace like the R-GUI)
# ---------------------------------------------------------------------------

.cat_quit <- function(save = "default", status = 0) {
  if (.con$busy && .gtk$level > 0) {
    tkbell()
    return(invisible())
  }
  if (identical(save, "default") || identical(save, "ask")) {
    ans <- tclvalue(tkmessageBox(parent = .con$top, type = "yesnocancel", icon = "question",
                                 title = "Question", default = "yes",
                                 message = "Save workspace image?"))
    if (ans == "cancel") return(invisible())
    save <- if (ans == "yes") "yes" else "no"
  }
  if (identical(save, "yes")) {
    ok <- tryCatch({
      save.image(file.path(getOption("CAT.datadir"), ".RData"))
      TRUE
    }, error = function(e) {
      tk_messageBox(type = "ok", icon = "error", caption = "CAT",
                    message = paste("The workspace could not be saved:", conditionMessage(e)))
      FALSE
    })
    if (!ok) return(invisible())
  }
  try(savehistory_cat(), silent = TRUE)
  try(graphics.off(), silent = TRUE)
  try(sink(type = "message"), silent = TRUE)
  try(sink(), silent = TRUE)
  base::quit(save = "no", status = status, runLast = TRUE)
}

savehistory_cat <- function() {
  h <- tail(.con$history, 500)
  if (length(h)) writeLines(h, file.path(getOption("CAT.datadir"), ".Rhistory"))
}

loadhistory_cat <- function() {
  f <- file.path(getOption("CAT.datadir"), ".Rhistory")
  if (file.exists(f)) {
    .con$history <- readLines(f, warn = FALSE)
    .con$hpos <- length(.con$history) + 1L
  }
}
