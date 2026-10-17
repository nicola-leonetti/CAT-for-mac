# Purpose   : start-up of CAT on macOS (replaces the Windows shortcut that
#             started Rgui.exe in C:\CAT\working and the .First() function
#             stored in the CAT workspace).
#             Environment variables (set by the CAT.app launcher):
#               CAT_HOME  folder containing mylib/, myscript/, working/, mac/
#               CAT_DATA  working folder of the user (default ~/Documents/CAT)
# Licence   : GPL-3 (same as CAT)

local({
  home <- Sys.getenv("CAT_HOME")
  if (!nzchar(home)) {
    this <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA)
    home <- if (!is.na(this)) dirname(dirname(this)) else getwd()
  }
  home <- normalizePath(home)
  data <- Sys.getenv("CAT_DATA")
  if (!nzchar(data)) data <- file.path(path.expand("~"), "Documents", "CAT")
  dir.create(data, showWarnings = FALSE, recursive = TRUE)
  data <- normalizePath(data)

  # first start: copy the example data sets shipped with CAT
  first <- !file.exists(file.path(data, ".cat-examples-copied"))
  if (first) {
    ex <- list.files(file.path(home, "working"), full.names = TRUE)
    file.copy(ex, data, overwrite = FALSE)
    file.create(file.path(data, ".cat-examples-copied"))
  }

  options(CAT.home = home, CAT.datadir = data, help_type = "html",
          
          keep.source = FALSE, width = 100)

  # compatibility layer, attached in front of the packages ("CAT:compat")
  ns <- new.env(parent = globalenv())
  assign(".cat_ns", ns, envir = ns)
  for (f in c("gtk_shim.R", "compat.R", "device.R", "console.R", "dfedit.R", "pkgs.R"))
    sys.source(file.path(home, "mac", f), envir = ns, keep.source = FALSE)
  attach(ns, pos = 2L, name = "CAT:compat", warn.conflicts = FALSE)
  options(device = ns$cat_device)
  s3 <- rbind(c("$", "GtkShim"), c("[<-", "GtkShim"), c("[", "GtkShim"),
              c("print", "GtkShim"), c("dim", "RGtkDataFrame"),
              c("dimnames", "RGtkDataFrame"), c("as.data.frame", "RGtkDataFrame"),
              c("[", "RGtkDataFrame"), c("[<-", "RGtkDataFrame"), c("*", "cat_pref"))
  for (k in seq_len(nrow(s3)))
    registerS3method(s3[k, 1], s3[k, 2], get(paste0(s3[k, 1], ".", s3[k, 2]), envir = ns),
                     envir = baseenv())

  ns$.cat_build_window()
  log <- Sys.getenv("CAT_CONSOLE_LOG")
  ns$.cat_start_output(if (nzchar(log)) log else file.path(tempdir(), "cat-console.txt"))

  cat(R.version.string, " -- \"", R.version$nickname, "\"\n", sep = "")
  cat("Copyright (C) ", R.version$year, " The R Foundation for Statistical Computing\n",
      "Platform: ", R.version$platform, "\n\n",
      "R is free software and comes with ABSOLUTELY NO WARRANTY.\n",
      "Type 'license()' or 'licence()' for distribution details.\n\n", sep = "")

  setwd(data)
  assign("working.dir", data, envir = .GlobalEnv)
  assign("base.dir", paste0(home, "/"), envir = .GlobalEnv)
  assign("lib.dir", file.path(home, "mylib"), envir = .GlobalEnv)
  assign("script.dir", file.path(home, "myscript"), envir = .GlobalEnv)

  ws <- file.path(data, ".RData")
  if (file.exists(ws)) {
    ok <- tryCatch({ load(ws, envir = .GlobalEnv); TRUE },
                   error = function(e) { message("The workspace could not be restored: ",
                                                 conditionMessage(e)); FALSE })
    if (ok) {
      # directories saved by a previous session (possibly on Windows) are stale
      assign("working.dir", data, envir = .GlobalEnv)
      assign("base.dir", paste0(home, "/"), envir = .GlobalEnv)
      assign("lib.dir", file.path(home, "mylib"), envir = .GlobalEnv)
      assign("script.dir", file.path(home, "myscript"), envir = .GlobalEnv)
      if (exists(".First", envir = .GlobalEnv, inherits = FALSE)) rm(".First", envir = .GlobalEnv)
      cat("[Previously saved workspace restored]\n\n")
    }
  }
  ns$loadhistory_cat()

  # what .First() did on Windows: load every script found in mylib/
  for (nm in list.files(file.path(home, "mylib"), pattern = "\\.[RrSsQq]$")) {
    tryCatch(suppressPackageStartupMessages(
               ns$source(file.path(home, "mylib", nm), local = FALSE, encoding = "latin1")),
             error = function(e) ns$.cat_report_error(e))
  }
  setwd(data)
  print("CAT (Chemometric Agile Tool) is now correctly loaded")
  print("R scripts by Camillo Melzi and Gianmarco Polotti")
  print("Chemometric consultancy by Riccardo Leardi")
  cat("\nmacOS edition: working folder is", data, "\n\n")
  if (first) cat("The example data sets have been copied to the working folder.\n\n")

  ns$.cat_flush_output()
  ns$.cat_prompt()
  ns$.cat_poll()
  tkwm.deiconify(ns$.con$top)
  tkraise(ns$.con$top)
  # automated tests: run an R file once the window is ready
  auto <- Sys.getenv("CAT_AUTORUN")
  if (nzchar(auto)) tcl("after", 500, function() base::source(auto, local = FALSE))
  invisible()
})

# the R session lives as long as the CAT window
tkwait.window(getOption("CAT.mainwindow"))
