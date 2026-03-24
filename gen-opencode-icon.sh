#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_PATH=""
MAIN_TEXT=""
BADGE_TEXT=""

MAIN_FONT_COLOR="#111111"
BADGE_FONT_COLOR="#ffffff"
BADGE_BG_COLOR="#666666"
IMAGE_BG_COLOR="none"

usage() {
	cat <<'EOF'
Usage: ./gen-opencode-icon.sh [options]

Generates a 128x128 icon.

The script renders a large main label in the top area and a full-width badge pinned
to the bottom. Colors are optional; text and output path are required.

Defaults:
  main font color:  #111111
  badge font color: #ffffff
  badge bg color:   #666666
  image bg color:   none

Options:
  --output-path PATH        Output file path (required)
  --main-text TEXT          Main text content (required)
  --badge-text TEXT         Badge text content (required)
  --main-font-color COLOR   Main text color for "zxc"
  --badge-font-color COLOR  Badge text color for "demo"
  --badge-bg-color COLOR    Badge background color
  --image-bg-color COLOR    Image background color
  -h, --help                Show this help

Notes:
  - Relative output paths are resolved from the current working directory.
  - Parent directories for the output path are created automatically.
  - Requires ImageMagick with the `magick` command available.

Example:
  ./gen-opencode-icon.sh \
    --output-path "$SCRIPT_DIR/assets/opencode-icon.png" \
    --main-text 'zxc' \
    --badge-text 'demo' \
    --main-font-color '#222222' \
    --badge-font-color '#ffffff' \
    --badge-bg-color '#1f7a5a' \
    --image-bg-color '#f5f3ee'
EOF
}

while [[ $# -gt 0 ]]; do
	case "$1" in
	--output-path)
		OUTPUT_PATH="$2"
		shift 2
		;;
	--main-text)
		MAIN_TEXT="$2"
		shift 2
		;;
	--badge-text)
		BADGE_TEXT="$2"
		shift 2
		;;
	--main-font-color)
		MAIN_FONT_COLOR="$2"
		shift 2
		;;
	--badge-font-color)
		BADGE_FONT_COLOR="$2"
		shift 2
		;;
	--badge-bg-color)
		BADGE_BG_COLOR="$2"
		shift 2
		;;
	--image-bg-color)
		IMAGE_BG_COLOR="$2"
		shift 2
		;;
	-h | --help)
		usage
		exit 0
		;;
	*)
		printf 'Unknown option: %s\n\n' "$1" >&2
		usage >&2
		exit 1
		;;
	esac
done

if [[ -z "$OUTPUT_PATH" ]]; then
	printf 'Missing required option: --output-path\n\n' >&2
	usage >&2
	exit 1
fi

if [[ -z "$MAIN_TEXT" ]]; then
	printf 'Missing required option: --main-text\n\n' >&2
	usage >&2
	exit 1
fi

if [[ -z "$BADGE_TEXT" ]]; then
	printf 'Missing required option: --badge-text\n\n' >&2
	usage >&2
	exit 1
fi

if ! command -v magick >/dev/null 2>&1; then
	printf 'ImageMagick is required. Install it so the `magick` command is available.\n' >&2
	exit 1
fi

if [[ "$OUTPUT_PATH" != /* ]]; then
	OUTPUT_PATH="$PWD/$OUTPUT_PATH"
fi

mkdir -p "$(dirname -- "$OUTPUT_PATH")"

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

main_label="$tmp_dir/main.png"
badge_label="$tmp_dir/badge.png"

magick -background none -fill "$MAIN_FONT_COLOR" -font Helvetica \
	-pointsize 45 label:"$MAIN_TEXT" "$main_label"

magick -size 128x60 xc:"$BADGE_BG_COLOR" -fill "$BADGE_FONT_COLOR" -font Helvetica \
	-gravity center -pointsize 40 -annotate +0+0 "$BADGE_TEXT" \
	-alpha set -channel A -evaluate set 100% +channel \
	\( +clone -alpha extract -draw 'roundrectangle 0,0 %[fx:w-1],%[fx:h-1] 8,8' \) \
	-compose CopyOpacity -composite "$badge_label"

magick -size 128x128 xc:"$IMAGE_BG_COLOR" \
	\( "$main_label" -resize 128x68 \) \
	-gravity center -geometry +0-30 -composite \
	"$badge_label" -gravity south -geometry +0+0 -composite \
	"$OUTPUT_PATH"

printf 'Created %s\n' "$OUTPUT_PATH"
