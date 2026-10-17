# Purpose   : macOS replacements for the Windows-only R functions and for the
#             packages used by CAT that are no longer installable (RGtk2,
#             RGtk2Extras, cairoDevice, playwith, rggobi) or not needed in full
#             (chemometrics, StatDA, rPref).
#             Everything defined here is attached to the search path as
#             "CAT:compat", in front of the attached packages.
# Licence   : GPL-3 (same as CAT)

# ---------------------------------------------------------------------------
# library()/require(): packages replaced by this compatibility layer are
# "loaded" silently, everything else is loaded normally.
# ---------------------------------------------------------------------------

.cat_replaced_packages <- c("RGtk2", "RGtk2Extras", "cairoDevice", "playwith",
                            "rggobi", "chemometrics", "StatDA", "rPref",
                            "gWidgets", "gWidgetsRGtk2")

# packages attached on Windows together with the replaced ones (their
# "Depends"): scripts use functions from them without loading them
.cat_replaced_depends <- list(
  chemometrics = c("class", "e1071", "gclus", "lars", "MASS", "mclust", "nnet",
                   "pcaPP", "pls", "rpart", "robustbase"),
  playwith = c("lattice", "grid"),
  StatDA = "methods")

.cat_attach_replaced <- function(pkg) {
  for (d in .cat_replaced_depends[[pkg]]) {
    if (!(paste0("package:", d) %in% search()) && requireNamespace(d, quietly = TRUE))
      suppressPackageStartupMessages(base::library(d, character.only = TRUE,
                                                   warn.conflicts = FALSE))
  }
  .cat_compat_to_front()
}

.cat_pkg_name <- function(expr, character.only, value) {
  if (character.only) return(as.character(value))
  if (is.character(expr)) return(expr)
  if (is.name(expr)) return(as.character(expr))
  NA_character_
}

library <- function(package, help, pos = 2, lib.loc = NULL, character.only = FALSE,
                    logical.return = FALSE, warn.conflicts, quietly = FALSE,
                    verbose = getOption("verbose"), ...) {
  if (!missing(package)) {
    pkg <- .cat_pkg_name(substitute(package), character.only,
                         if (character.only) package else NULL)
    if (!is.na(pkg) && pkg %in% .cat_replaced_packages) {
      .cat_attach_replaced(pkg)
      return(if (logical.return) invisible(TRUE) else invisible(.packages()))
    }
  }
  mc <- match.call()
  mc[[1]] <- quote(base::library)
  res <- eval(mc, parent.frame())
  .cat_compat_to_front()
  invisible(res)
}

require <- function(package, lib.loc = NULL, quietly = FALSE, warn.conflicts,
                    character.only = FALSE) {
  pkg <- .cat_pkg_name(substitute(package), character.only,
                       if (character.only) package else NULL)
  if (!is.na(pkg) && pkg %in% .cat_replaced_packages) {
    .cat_attach_replaced(pkg)
    return(invisible(TRUE))
  }
  mc <- match.call()
  mc[[1]] <- quote(base::require)
  res <- eval(mc, parent.frame())
  .cat_compat_to_front()
  invisible(res)
}

# keeps the compatibility functions in front of packages attached later
# (e.g. readxl::read_excel must not mask the version defined here)
.cat_compat_to_front <- function() {
  sp <- search()
  if (identical(sp[2], "CAT:compat") && sum(sp == "CAT:compat") == 1) return(invisible())
  while (!is.na(pos <- match("CAT:compat", search()))) detach(pos = pos)
  attach(.cat_ns, pos = 2L, name = "CAT:compat", warn.conflicts = FALSE)
  invisible()
}

# source(): the menu refers to some scripts with a different letter case
# than the file name (harmless on Windows, fatal on case-sensitive disks)
source <- function(file, ...) {
  if (is.character(file) && length(file) == 1 && !file.exists(file) &&
      !grepl("^(https?|ftp)://", file)) {
    dir <- dirname(file)
    cand <- list.files(dir, all.files = TRUE)
    hit <- cand[tolower(cand) == tolower(basename(file))]
    if (length(hit)) file <- file.path(dir, hit[1])
  }
  base::source(file, ...)
}

# ---------------------------------------------------------------------------
# R 3 -> R 4 differences relied upon by the CAT scripts
# ---------------------------------------------------------------------------

# Since R 4.0 class(<matrix>) is c("matrix", "array") and since R 4.2 an if()
# condition of length 2 is an error: CAT (written for R 3) tests
# class(x) == "matrix". Scripts see the R 3 result; packages are not affected.
class <- function(x) {
  cl <- base::class(x)
  if (length(cl) == 2L && cl[1L] == "matrix" && cl[2L] == "array") "matrix" else cl
}

# Before R 4.0 character vectors became factors in data frames by default
# (stringsAsFactors = TRUE). Several CAT scripts rely on it (e.g. levels() of a
# column of class labels), so the scripts get the old default back.
data.frame <- function(..., stringsAsFactors = TRUE)
  base::data.frame(..., stringsAsFactors = stringsAsFactors)
cbind.data.frame <- function(..., deparse.level = 1)
  data.frame(..., check.names = FALSE)
as.data.frame <- function(x, ...) {
  if ((is.character(x) || (is.list(x) && !is.data.frame(x))) &&
      !("stringsAsFactors" %in% names(list(...))))
    base::as.data.frame(x, ..., stringsAsFactors = TRUE)
  else base::as.data.frame(x, ...)
}
read.table <- function(file, ..., stringsAsFactors = TRUE)
  utils::read.table(file, ..., stringsAsFactors = stringsAsFactors)
read.csv <- function(file, ..., stringsAsFactors = TRUE)
  utils::read.csv(file, ..., stringsAsFactors = stringsAsFactors)
read.csv2 <- function(file, ..., stringsAsFactors = TRUE)
  utils::read.csv2(file, ..., stringsAsFactors = stringsAsFactors)
read.delim <- function(file, ..., stringsAsFactors = TRUE)
  utils::read.delim(file, ..., stringsAsFactors = stringsAsFactors)

# gdata (attached by mylib/menu.r) masks utils::object.size with a deprecated
# copy that warns at every call
object.size <- utils::object.size

# ---------------------------------------------------------------------------
# Windows-only functions
# ---------------------------------------------------------------------------

win.graph <- function(width, height, pointsize = 12) {
  args <- list(pointsize = pointsize)
  if (!missing(width)) args$width <- width
  if (!missing(height)) args$height <- height
  do.call(dev.new, args)
}
windows <- function(width, height, pointsize = 12, ...) {
  args <- list(pointsize = pointsize)
  if (!missing(width)) args$width <- width
  if (!missing(height)) args$height <- height
  do.call(dev.new, args)
}
bringToTop <- function(which = dev.cur(), stay = FALSE) invisible()
arrangeWindows <- function(...) invisible()
msgWindow <- function(...) invisible()
flush.console <- function() {
  utils::flush.console()
  if (exists(".cat_flush_output", mode = "function")) try(.cat_flush_output(), silent = TRUE)
  invisible()
}
memory.limit <- function(size = NA) Inf
memory.size <- function(max = FALSE) NA

winProgressBar <- function(title = "R progress bar", label = "", min = 0, max = 1,
                           initial = 0, width = 300) {
  tkProgressBar(title = title, label = label, min = min, max = max,
                initial = initial, width = width)
}
setWinProgressBar <- function(pb, value, title = NULL, label = NULL) {
  old <- setTkProgressBar(pb, value, title = title, label = label)
  tcl("update", "idletasks")
  invisible(old)
}
getWinProgressBar <- function(pb) getTkProgressBar(pb)

shell <- function(cmd, shell, flag = "/c", intern = FALSE, wait = TRUE,
                  translate = FALSE, mustWork = FALSE, ...) {
  if (length(cmd) == 1 && file.exists(cmd)) {
    system2("open", shQuote(cmd), wait = FALSE)
    return(invisible(0L))
  }
  system(cmd, intern = intern, wait = wait)
}
shell.exec <- function(file) {
  system2("open", shQuote(file), wait = FALSE)
  invisible()
}

file.choose <- function(new = FALSE) {
  f <- if (new) tclvalue(tkgetSaveFile(title = "Select file", initialdir = getwd()))
       else tclvalue(tkgetOpenFile(title = "Select file", initialdir = getwd()))
  if (!nzchar(f)) stop("file choice cancelled")
  f
}

choose.files <- function(default = "", caption = "Select files", multi = TRUE,
                         filters = NULL, index = 1) {
  tk_choose.files(default = default, caption = caption, multi = multi)
}

choose.dir <- function(default = "", caption = "Select folder") {
  d <- tclvalue(tkchooseDirectory(title = caption,
                                  initialdir = if (nzchar(default)) default else getwd()))
  if (nzchar(d)) d else NA_character_
}

# file.edit() opens a terminal editor outside R.app: use the default text editor
file.edit <- function(..., title = file, editor = NULL, fileEncoding = "") {
  file <- c(...)
  for (f in file) {
    if (!file.exists(f)) file.create(f)
    system2("open", c("-t", shQuote(normalizePath(f))), wait = FALSE)
  }
  invisible()
}

readClipboard <- function(format = 1, raw = FALSE) {
  x <- system2("pbpaste", stdout = TRUE)
  if (raw) charToRaw(paste(x, collapse = "\n")) else x
}
writeClipboard <- function(str, format = 1) {
  con <- pipe("pbcopy", "w")
  on.exit(close(con))
  writeLines(as.character(str), con)
  invisible(TRUE)
}

# message boxes: show the plots drawn so far first
tk_messageBox <- function(type = c("ok", "okcancel", "yesno", "yesnocancel",
                                   "retrycancel", "abortretryignore"),
                          message, caption = "", default = "", ...) {
  try(.gd_refresh_all(), silent = TRUE)
  type <- match.arg(type)
  tcltk::tk_messageBox(type = type, message = message, caption = caption,
                       default = default, ...)
}

q <- function(save = "default", status = 0, runLast = TRUE) .cat_quit(save, status)
quit <- q

# readxl: old versions (used by CAT on Windows) returned plain data frames,
# the scripts index them as such (tibbles behave differently)
# (old readxl also kept column names as they are, without "New names:" notes)
.cat_name_repair <- function(nms) {
  empty <- is.na(nms) | nms == ""
  nms[empty] <- paste0("...", which(empty))
  nms
}
read_excel <- function(path, sheet = NULL, range = NULL, col_names = TRUE, ...,
                       .name_repair = .cat_name_repair)
  as.data.frame(readxl::read_excel(path, sheet = sheet, range = range,
                                   col_names = col_names, ..., .name_repair = .name_repair))
read_xls <- function(path, ..., .name_repair = .cat_name_repair)
  as.data.frame(readxl::read_xls(path, ..., .name_repair = .name_repair))
read_xlsx <- function(path, ..., .name_repair = .cat_name_repair)
  as.data.frame(readxl::read_xlsx(path, ..., .name_repair = .name_repair))
