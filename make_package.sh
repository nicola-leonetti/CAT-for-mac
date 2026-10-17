#!/bin/bash
# Creates CAT-for-mac.zip, the file to copy to another Mac to install CAT there
# (see "Install on another Mac" in README.md). The Windows installer and build
# leftovers are left out.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
NAME="$(basename "$HERE")"
OUT="${1:-$HERE/../CAT-for-mac.zip}"
rm -f "$OUT"
cd "$HERE/.."
/usr/bin/zip -r -q -X "$OUT" "$NAME" \
  -x "$NAME/setup_CAT.exe" "$NAME/tools/bin/*" "$NAME/.git/*" "*/.DS_Store" "$NAME/CAT-for-mac.zip"
echo "Created $(cd "$(dirname "$OUT")" && pwd)/$(basename "$OUT") ($(du -h "$OUT" | cut -f1))"
