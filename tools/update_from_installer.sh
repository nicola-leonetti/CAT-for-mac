#!/bin/bash
# Updates the CAT scripts in ../CAT from a new Windows installer (setup_CAT.exe)
# published at https://gruppochemiometria.it/index.php/software
#
#   tools/update_from_installer.sh path/to/setup_CAT.exe
#
# The installer is an Inno Setup 6.4.3 package. The released innoextract (1.9)
# cannot read it, so a patched innoextract is built from source first (needs
# Homebrew with cmake, boost and xz, and the Xcode command line tools).
# Only myscript/, mylib/, working/ and the version file are replaced: the macOS
# layer (CAT/mac) is kept. Then run ./install.sh --app-only to rebuild CAT.app.

set -euo pipefail
HERE="$(cd "$(dirname "$0")/.." && pwd)"
EXE="${1:?usage: $0 setup_CAT.exe}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

IE="$HERE/tools/bin/innoextract"
if [ ! -x "$IE" ]; then
  echo "==> Building innoextract with Inno Setup 6.4.3 support"
  brew list cmake boost xz >/dev/null 2>&1 || brew install cmake boost xz
  git clone -q https://github.com/dscharrer/innoextract.git "$WORK/ie"
  git -C "$WORK/ie" checkout -q "$(cat "$HERE/tools/innoextract-base-commit.txt")"
  git -C "$WORK/ie" apply "$HERE/tools/innoextract-inno-6.4.3.patch"
  # some Xcode installations ship without C++ headers in the default SDK path
  SDK="$(xcrun --show-sdk-path 2>/dev/null || echo /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk)"
  cmake -S "$WORK/ie" -B "$WORK/ie/build" -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_PREFIX_PATH="$(brew --prefix)" -DCMAKE_OSX_SYSROOT="$SDK" \
    -DCMAKE_CXX_FLAGS="-isystem $SDK/usr/include/c++/v1" \
    -Diconv_LIBRARY="$SDK/usr/lib/libiconv.tbd" -Diconv_INCLUDE_DIR="$SDK/usr/include" >/dev/null
  cmake --build "$WORK/ie/build" -j 8 >/dev/null
  mkdir -p "$HERE/tools/bin"
  cp "$WORK/ie/build/innoextract" "$IE"
fi

echo "==> Extracting $EXE"
"$IE" -q -d "$WORK/out" "$EXE"
APPDIR="$WORK/out/app"
[ -d "$APPDIR/myscript" ] || { echo "Unexpected installer layout" >&2; exit 1; }

for d in myscript mylib working; do
  rm -rf "$HERE/CAT/$d"
  cp -R "$APPDIR/$d" "$HERE/CAT/$d"
done
rm -f "$HERE/CAT/working/.RData" "$HERE/CAT/working/.Rhistory"
# bug fix (see README): inpboxcrrr() referred to a non-existent inpboxrrr()
LC_ALL=C sed -i '' 's/environment(inpboxrrr)/environment(inpboxcrrr)/g' "$HERE/CAT/mylib/gvfib.r"
cp "$APPDIR/CAT.iss" "$APPDIR/cat.ico" "$APPDIR/gpl_3.0.txt" "$HERE/CAT/" 2>/dev/null || true
echo "==> CAT updated to version: $(sed -n 's/^#define MyAppVersion "\(.*\)"/\1/p' "$HERE/CAT/CAT.iss")"
echo "    Now run ./install.sh --app-only"
