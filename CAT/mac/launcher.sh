#!/bin/bash
# CAT.app launcher (Contents/MacOS/CAT): starts R with the CAT window.
# R and the packages live in ~/Library/CAT/R (created by install.sh, which
# records the location in Resources/support-location); the CAT scripts are
# inside the application bundle.

APP_CONTENTS="$(cd "$(dirname "$0")/.." && pwd)"
# where install.sh put R
SUPPORT="${CAT_SUPPORT:-}"
if [ -z "$SUPPORT" ] && [ -f "$APP_CONTENTS/Resources/support-location" ]; then
  SUPPORT="$(head -n 1 "$APP_CONTENTS/Resources/support-location")"
fi
SUPPORT="${SUPPORT:-$HOME/Library/CAT}"
R_ENV="$SUPPORT/R"
LOG_DIR="$HOME/Library/Logs"
LOG="$LOG_DIR/CAT.log"

export CAT_HOME="$APP_CONTENTS/Resources/CAT"
export CAT_DATA="${CAT_DATA:-$HOME/Documents/CAT}"

if [ ! -x "$R_ENV/bin/R" ]; then
  /usr/bin/osascript -e "display alert \"CAT cannot start\" message \"R for CAT was not found in $SUPPORT. Please run install.sh again.\" as critical" >/dev/null 2>&1
  exit 1
fi

# applications started from the Finder get no locale: use the one of macOS
if [ -z "$LANG" ] || [ "$LANG" = "C" ]; then
  loc="$(/usr/bin/defaults read -g AppleLocale 2>/dev/null | sed 's/@.*//')"
  case "$loc" in
    ??_??) export LANG="$loc.UTF-8" ;;
    *)     export LANG="en_US.UTF-8" ;;
  esac
fi
export LC_ALL="$LANG"
# R, not the shell, decides which packages to use
unset R_HOME R_LIBS R_LIBS_USER R_PROFILE_USER R_ENVIRON_USER
export R_LIBS_USER="$SUPPORT/library"   # extra packages installed by the user from CAT

mkdir -p "$LOG_DIR"
[ -f "$LOG" ] && mv -f "$LOG" "$LOG_DIR/CAT.previous.log"

# R reads its first command from standard input (R must run in interactive
# mode for graphics and dialogs, which rules out Rscript)
exec "$R_ENV/bin/R" --no-echo --no-save --no-restore --interactive \
  < "$CAT_HOME/mac/start.R" > "$LOG" 2>&1
