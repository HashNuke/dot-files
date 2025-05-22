#!/bin/bash

# Usage check
if [ $# -lt 1 ]; then
  echo "Usage: $0 input_file [output_directory]"
  exit 1
fi

INPUT="$1"
BASENAME=$(basename "$INPUT")
EXT="${BASENAME##*.}"
NAME="${BASENAME%.*}"

# Use provided output directory, or default to input file's directory
if [ -n "$2" ]; then
  OUTDIR="$2"
else
  OUTDIR=$(dirname "$INPUT")
fi

# Ensure output directory exists
mkdir -p "$OUTDIR"

OUTPUT="${OUTDIR}/${NAME}_clean.${EXT}"

# Run ffmpeg
ffmpeg -i "$INPUT" -map_metadata -1 -update 1 -frames:v 1 "$OUTPUT"
