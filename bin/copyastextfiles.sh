#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -eq 0 ]; then
  echo "Usage: copyastextcontents file1 [file2 ...]" >&2
  exit 1
fi

tmp="$(mktemp)"

for src in "$@"; do
  if [ ! -f "$src" ]; then
    echo "Skipping '$src' (not a regular file)" >&2
    continue
  fi

  {
    echo "These are contents of $src"
    echo '```'
    cat "$src"
    echo
    echo '```'
    echo
  } >> "$tmp"
done

pbcopy < "$tmp"
rm -f "$tmp"

echo "Copied formatted file contents to clipboard."
