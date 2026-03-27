#!/usr/bin/env bash
set -euo pipefail

# Ensure env vars exist
: "${PUSHER_APP_TOKEN:?Missing PUSHER_APP_TOKEN}"
: "${PUSHER_USER_KEY:?Missing PUSHER_USER_KEY}"

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
  -d "token=$PUSHER_APP_TOKEN" \
  -d "user=$PUSHER_USER_KEY" \
  --data-urlencode "message=$msg"
