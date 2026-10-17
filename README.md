# CAT for macOS

macOS edition of **CAT – Chemometric Agile Tool** by the Gruppo di Chemiometria
(Società Chimica Italiana), <https://gruppochemiometria.it/index.php/software>.

CAT for Windows ships as `setup_CAT.exe`, which installs a 32-bit R 3.1 together
with the CAT R scripts. Its graphical interface is the Windows R GUI (`Rgui.exe`):
menus added with `winMenuAdd()`, dialog boxes written with RGtk2, interactive
exploration with GGobi. None of these work on a Mac. This project runs the
**same CAT scripts** (unchanged but for one bug fix) on a current 64-bit R (4.5,
native on Apple silicon and Intel) with the same interface:

| Windows CAT                                    | macOS edition                                                         |
|------------------------------------------------|-----------------------------------------------------------------------|
| R GUI console with the CAT menus               | CAT window: same menus, R console (navy output, red input)            |
| RGtk2 dialog boxes (`mylib/gvfib.r`)           | the same code, running on a small RGtk2-compatible layer over Tcl/Tk  |
| RGtk2Extras data editor (Copy and Paste, Modify)| spreadsheet editor, ⌘V pastes data copied from Excel                 |
| R graphics windows (Save as / Copy)            | plot windows with File menu: Save as PDF/PNG/JPEG/TIFF, Copy          |
| GGobi (Exploration menu)                       | loon: linked, brushable scatter plots (+ inspector window)            |
| playwith (Plot Editing ▸ 2D plot editing)      | plot editor: title, labels, ranges, point and font size, Save         |
| `C:\CAT\working`                               | `~/Documents/CAT` (example data sets copied there at first start)     |

## Install

Requirements: macOS 11 (Big Sur) or later, Apple silicon (M1…) or Intel, an
internet connection during the installation (about 600 MB are downloaded) and
about 2 GB of disk space. No administrator rights, no XQuartz, no Xcode, no
Homebrew, no R installation are needed: the installer brings its own R.

Open the Terminal (Applications ▸ Utilities ▸ Terminal) and run:

```sh
cd ~/Desktop/cat-for-mac      # the folder containing install.sh
./install.sh
```

The installer downloads R 4.5 and the packages used by CAT from conda-forge
(prebuilt for macOS) into `~/Library/CAT/R`, and creates
`~/Applications/CAT.app`. It takes a few minutes. Then start CAT from the
Finder (your home folder ▸ Applications), Launchpad or Spotlight (⌘-space,
type "CAT"); you can drag it to the Dock.

Options: `--dest /Applications` to put the app elsewhere, `--app-only` to rebuild
only the app (e.g. after updating the scripts), `--uninstall` to remove the app
and its R (your data in `~/Documents/CAT` is kept).

## Install on another Mac

1. On a Mac that has this folder, create the package (26 MB; the Windows
   installer is not included because it is not needed):

   ```sh
   cd ~/Desktop/cat-for-mac
   ./make_package.sh            # creates ../CAT-for-mac.zip next to the folder
   ```

   (Alternatively copy the whole `cat-for-mac` folder, e.g. on a USB stick.)
2. Copy `CAT-for-mac.zip` to the other Mac (AirDrop, USB stick, e-mail, cloud
   drive…) and double-click it in the Finder: this creates a `cat-for-mac`
   folder (in Downloads, if it was downloaded).
3. On the other Mac open the Terminal (Applications ▸ Utilities ▸ Terminal),
   type `cd ` (with a space), drag the `cat-for-mac` folder from the Finder
   into the Terminal window, press Return, then run:

   ```sh
   ./install.sh
   ```

   If the Terminal answers `permission denied`, use `bash install.sh`.
4. When it prints `Done`, start CAT from Launchpad or Spotlight. The
   `cat-for-mac` folder and the zip file can then be deleted.

Every user account that wants to use CAT runs `install.sh` once (everything is
installed in the user's home folder). To install for all users instead, an
administrator can run `./install.sh --dest /Applications`; R is still installed
per user in `~/Library/CAT`, so the other users must run
`./install.sh --dest /Applications` as well (or simply install in their own
`~/Applications`).

Troubleshooting:

* *"CAT cannot be opened because Apple cannot check it…"*: the installer removes
  the download quarantine, so this should not happen; if it does, right-click
  CAT.app ▸ Open ▸ Open (once), or System Settings ▸ Privacy & Security ▸
  Open Anyway.
* *"R for CAT was not found"*: the installation of R did not complete (e.g. the
  internet connection dropped); run `./install.sh` again.
* Anything else: look at `~/Library/Logs/CAT.log` (R messages of the last
  session; the previous one is in `CAT.previous.log`).

## Using CAT

Everything works as described in the CAT manuals (Help menu, and the "Help" item
of each CAT menu). Differences from Windows:

* The CAT menus are in the bar at the top of the CAT window (they wrap onto two
  lines on narrow windows). The macOS menu bar holds File (source scripts,
  load/save workspace, change working folder), Edit and Help.
* When you quit, CAT asks whether to save the workspace, like R on Windows. It is
  saved as `~/Documents/CAT/.RData` and restored at the next start. A workspace
  saved by CAT on Windows can be loaded with File ▸ Load Workspace.
* Plot windows: File menu (or right click) to save as PDF/PNG/JPEG/TIFF or copy
  to the clipboard. For publication-quality figures use Save as PDF (the
  window itself shows a screen-resolution image). Where R asks for mouse input
  (`locator()`, `identify()`, zoom) click in the plot window; Esc finishes.
* Plot Editing ▸ 2D plot editing opens a small editor next to the plot: title,
  axis labels, ranges, point and font size, Zoom (click two corners), Identify
  (click on points to label them), Save as.
* Data Handling ▸ Load ▸ Copy and Paste: copy the cells in Excel, click the cell
  in the CAT data editor and press ⌘V. CAT asks whether the first row/column
  contain names. Numbers with decimal comma (Italian Excel) are recognised.
* Exploration ▸ Load Dataset opens loon: a scatter plot matrix (up to 6
  variables) or a scatter plot with selectable axes. Drag to select points; the
  Loon Inspector window changes colours, sizes, etc. Exploration ▸ Save Picture
  saves PDF, PNG or BMP.
* Errors and the R log of the last session are in `~/Library/Logs/CAT.log`.

## How it works

```
CAT/                     the CAT application folder (as installed in C:\CAT)
  myscript/, mylib/      CAT R scripts, unchanged except for the fixes listed below
  working/               example data sets
  mac/                   macOS layer
    main.R               start-up (replaces .First() of the Windows workspace)
    console.R            CAT window: console + winMenuAdd()/winMenuAddItem()
    gtk_shim.R           RGtk2 API (gtkWindow, gtkTable, gSignalConnect, ...) on Tcl/Tk
    device.R             graphics windows
    dfedit.R             data editor (RGtk2Extras::dfedit)
    compat.R             Windows-only functions (winProgressBar, shell, file.choose, ...)
    pkgs.R               rggobi, playwith, rPref and StatDA functions used by CAT
    launcher.sh          executable of CAT.app
install.sh               installer
make_package.sh          creates CAT-for-mac.zip to install on another Mac
tools/                   to update CAT/ from a newer setup_CAT.exe
```

The Windows installer `setup_CAT.exe` is not part of the repository; it is only
needed to update CAT/ to a new CAT release (see below) and can be downloaded
from <https://gruppochemiometria.it/index.php/software>.

Notes for maintainers:

* R's Quartz graphics windows cannot be used together with Tcl/Tk on macOS: after
  a Quartz window is closed, the next Tk window crashes R (a long-standing
  problem). Plots are therefore drawn on an off-screen recording device and
  shown in Tk windows (`device.R`).
* On Windows `library(chemometrics)` also attached MASS, pls, robustbase,
  e1071, ... and some scripts rely on this; `compat.R` attaches them too.
* The scripts were written for R 3.1. For the code they run, `compat.R` restores
  the two R 3 behaviours they depend on: `class(<matrix>)` is `"matrix"` (in R 4
  it is `c("matrix", "array")`, which breaks `if (class(x) == "matrix")`) and
  `data.frame()`/`read.table()` convert text to factors (`stringsAsFactors`).
  Packages are not affected.
* Debugging: CAT's R output goes to `~/Library/Logs/CAT.log`. To test a change
  without clicking, set `CAT_AUTORUN` to an R file: it is sourced inside the
  running app once the CAT window is up (`main.R`), e.g.
  `CAT_AUTORUN=/path/test.R ~/Applications/CAT.app/Contents/MacOS/CAT`.
  R must be started `--interactive` with `main.R` read from standard input
  (as `launcher.sh` does), not with `-f` or Rscript.
* Fixes to the original scripts (they were bugs on Windows too):
  `mylib/gvfib.r`, `inpboxcrrr()` referred to a non-existent `inpboxrrr()`, which
  made Exploration ▸ Save Picture unusable.

### Updating to a new CAT release

```sh
tools/update_from_installer.sh ~/Downloads/setup_CAT.exe   # replaces CAT/myscript, mylib, working
                                                            # and re-applies the fix above
./install.sh --app-only
```

The Windows installer is an Inno Setup 6.4.3 package; the script builds
innoextract with a small patch (`tools/innoextract-inno-6.4.3.patch`) to read it.

## Licence

CAT is distributed by its authors under the GNU GPL 3 (`CAT/gpl_3.0.txt`); the
macOS layer is distributed under the same licence. `edaplot()` comes from the
StatDA package (P. Filzmoser, GPL ≥ 3).
