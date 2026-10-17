#!/bin/bash
# Installs CAT (Chemometric Agile Tool) on macOS.
#
#   ./install.sh                  install CAT.app in ~/Applications
#   ./install.sh --dest /Applications
#   ./install.sh --app-only       rebuild CAT.app only (keeps the installed R)
#   ./install.sh --uninstall      remove CAT.app and its R (keeps your data)
#
# What gets installed (no administrator rights needed):
#   ~/Applications/CAT.app                      the application (CAT scripts)
#   ~/Library/CAT/R                             R 4.5 + packages (conda-forge)
#   ~/Documents/CAT                             working folder (created at first start)
#
# R is installed with micromamba from conda-forge, which provides ready-made
# macOS builds of R, of the packages used by CAT and of Tcl/Tk with native
# macOS windows (no XQuartz needed).

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
DEST="$HOME/Applications"
# R's start-up scripts do not quote paths: R must live in a folder without spaces
SUPPORT="${CAT_SUPPORT:-$HOME/Library/CAT}"
case "$SUPPORT" in
  *" "*) SUPPORT="/Users/Shared/CAT-$(id -u)" ;;
esac
APP_ONLY=0
UNINSTALL=0

while [ $# -gt 0 ]; do
  case "$1" in
    --dest) DEST="$2"; shift 2 ;;
    --app-only) APP_ONLY=1; shift ;;
    --uninstall) UNINSTALL=1; shift ;;
    -h|--help) sed -n '2,17p' "$0"; exit 0 ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
  esac
done

APP="$DEST/CAT.app"
R_ENV="$SUPPORT/R"

say() { printf '\n==> %s\n' "$*"; }

if [ "$UNINSTALL" = 1 ]; then
  say "Removing $APP and $SUPPORT (your data in ~/Documents/CAT is kept)"
  rm -rf "$APP" "$SUPPORT" "$HOME/Library/Application Support/CAT"   # (location of early versions)
  exit 0
fi

[ "$(uname -s)" = Darwin ] || { echo "This installer is for macOS." >&2; exit 1; }
case "$(uname -m)" in
  arm64)  PLATFORM=osx-arm64 ;;
  x86_64) PLATFORM=osx-64 ;;
  *) echo "Unsupported processor: $(uname -m)" >&2; exit 1 ;;
esac

# --------------------------------------------------------------------------
# R and packages
# --------------------------------------------------------------------------

CONDA_PACKAGES=(
  "r-base=4.5"
  r-tcltk2 r-loon            # dialogs, tables, interactive exploration (GGobi replacement)
  r-readxl r-gdata r-stringr # data loading and handling
  r-gplots r-lattice r-latticeextra r-ggplot2 r-hmisc
  r-mass r-pls r-matrixcalc r-expm r-pracma r-robustbase r-sp r-rcpp
  bioconductor-pcamethods
  # attached on Windows by the old "chemometrics" package and used by the scripts
  r-class r-e1071 r-gclus r-lars r-mclust r-nnet r-pcapp r-rpart
)
CRAN_PACKAGES=(zoom)          # pure R, not on conda-forge

install_r() {
  local mamba
  if command -v micromamba >/dev/null 2>&1; then
    mamba="$(command -v micromamba)"
  else
    say "Downloading micromamba (package manager used to install R)"
    local tmp
    tmp="$(mktemp -d)"
    curl -fsSL "https://micro.mamba.pm/api/micromamba/$PLATFORM/latest" | tar -xj -C "$tmp" bin/micromamba
    mamba="$tmp/bin/micromamba"
  fi
  export MAMBA_ROOT_PREFIX="$SUPPORT/mamba"
  say "Installing R and the packages used by CAT in $R_ENV (this takes a few minutes)"
  rm -rf "$R_ENV"
  "$mamba" create --yes --quiet --prefix "$R_ENV" \
    --channel conda-forge --channel bioconda --override-channels \
    "${CONDA_PACKAGES[@]}"
  "$mamba" clean --all --yes --quiet >/dev/null 2>&1 || true
  rm -rf "$MAMBA_ROOT_PREFIX/pkgs"

  say "Installing CRAN packages: ${CRAN_PACKAGES[*]}"
  local pkgs
  pkgs="$(printf '"%s",' "${CRAN_PACKAGES[@]}")"
  "$R_ENV/bin/Rscript" -e "install.packages(c(${pkgs%,}), repos = 'https://cloud.r-project.org', type = 'source', quiet = TRUE)"
}

check_r() {
  say "Checking the R installation"
  "$R_ENV/bin/Rscript" -e '
    pk <- c("tcltk", "tcltk2", "loon", "readxl", "gdata", "stringr", "gplots", "lattice",
            "latticeExtra", "ggplot2", "Hmisc", "MASS", "pls", "matrixcalc", "expm",
            "pracma", "robustbase", "sp", "Rcpp", "pcaMethods", "zoom", "class", "e1071",
            "gclus", "lars", "mclust", "nnet", "pcaPP", "rpart")
    ok <- vapply(pk, requireNamespace, logical(1), quietly = TRUE)
    if (!all(ok)) stop("missing packages: ", paste(pk[!ok], collapse = ", "))
    if (!capabilities("tcltk") || !capabilities("aqua")) stop("R was built without Tcl/Tk or Quartz")
    cat(R.version.string, "- all", length(pk), "packages available\n")'
}

mkdir -p "$SUPPORT/library"
if [ "$APP_ONLY" = 0 ] || [ ! -x "$R_ENV/bin/R" ]; then
  install_r
fi
check_r

# --------------------------------------------------------------------------
# CAT.app
# --------------------------------------------------------------------------

say "Building $APP"
mkdir -p "$DEST"
STAGE="$(mktemp -d)/CAT.app"
mkdir -p "$STAGE/Contents/MacOS" "$STAGE/Contents/Resources/CAT"
cp -R "$HERE/CAT/myscript" "$HERE/CAT/mylib" "$HERE/CAT/working" "$HERE/CAT/mac" \
      "$HERE/CAT/gpl_3.0.txt" "$STAGE/Contents/Resources/CAT/"
cp "$HERE/CAT/mac/launcher.sh" "$STAGE/Contents/MacOS/CAT"
printf '%s\n' "$SUPPORT" > "$STAGE/Contents/Resources/support-location"
chmod +x "$STAGE/Contents/MacOS/CAT"
/usr/bin/sips -s format icns "$HERE/CAT/cat.ico" --out "$STAGE/Contents/Resources/CAT.icns" >/dev/null 2>&1 || true
VERSION="$(sed -n 's/^#define MyAppVersion "\(.*\)"/\1/p' "$HERE/CAT/CAT.iss" 2>/dev/null || true)"
cat > "$STAGE/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>                <string>CAT</string>
  <key>CFBundleDisplayName</key>         <string>CAT</string>
  <key>CFBundleIdentifier</key>          <string>it.gruppochemiometria.cat</string>
  <key>CFBundleVersion</key>             <string>${VERSION:-1.0}</string>
  <key>CFBundleShortVersionString</key>  <string>${VERSION:-1.0}</string>
  <key>CFBundleExecutable</key>          <string>CAT</string>
  <key>CFBundleIconFile</key>            <string>CAT.icns</string>
  <key>CFBundlePackageType</key>         <string>APPL</string>
  <key>LSMinimumSystemVersion</key>      <string>11.0</string>
  <key>NSHighResolutionCapable</key>     <true/>
  <key>NSHumanReadableCopyright</key>    <string>Gruppo di Chemiometria - GPL 3</string>
</dict>
</plist>
PLIST
rm -rf "$APP"
mv "$STAGE" "$APP"
# files copied from a downloaded/AirDropped folder carry the quarantine flag,
# which would make Gatekeeper refuse to open the (unsigned) application
/usr/bin/xattr -dr com.apple.quarantine "$APP" 2>/dev/null || true
/usr/bin/codesign --force --deep --sign - "$APP" >/dev/null 2>&1 || true
# make the Finder pick up the icon
touch "$APP"

say "Done. Start CAT from $APP (your working folder is ~/Documents/CAT)."
echo "    If something goes wrong, the log is in ~/Library/Logs/CAT.log"
