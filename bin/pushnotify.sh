#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: pushnotify [MESSAGE...]
       echo "MESSAGE" | pushnotify

Send a Pushover notification.

Arguments:
  MESSAGE            Notification text. If omitted, reads from stdin.

Options:
  -h, --help         Show this help message and exit.

Environment:
  PUSHOVER_APP_TOKEN Pushover application token.
  PUSHOVER_USER_KEY  Pushover user or group key.
EOF
}

if [ $# -eq 1 ]; then
  case "$1" in
    -h|--help)
      usage
      exit 0
      ;;
  esac
fi

# Ensure env vars exist
: "${PUSHOVER_APP_TOKEN:?Missing PUSHOVER_APP_TOKEN}"
: "${PUSHOVER_USER_KEY:?Missing PUSHOVER_USER_KEY}"

# Get message: arg > stdin
if [ $# -gt 0 ]; then
  msg="$*"
else
  msg="$(cat)"
fi

# Fail if empty
[ -n "$msg" ] || { echo "No message provided"; exit 1; }

# Send
curl -sS -X POST https://api.pushover.net/1/messages.json \
  -d "token=$PUSHOVER_APP_TOKEN" \
  -d "user=$PUSHOVER_USER_KEY" \
  --data-urlencode "message=$msg"
