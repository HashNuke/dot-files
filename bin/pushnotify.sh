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

# Send. Keep the response so we can distinguish a successful Pushover request
# from a curl failure. The marker makes the HTTP status unambiguous even when
# the response body contains newlines.
response=''
if response="$(curl -sS -X POST https://api.pushover.net/1/messages.json \
  -d "token=$PUSHOVER_APP_TOKEN" \
  -d "user=$PUSHOVER_USER_KEY" \
  --data-urlencode "message=$msg" \
  --write-out $'\n__PUSHNOTIFY_HTTP_STATUS__%{http_code}')"; then
  curl_status=0
else
  curl_status=$?
fi

http_marker=$'\n__PUSHNOTIFY_HTTP_STATUS__'
http_status="${response##*"$http_marker"}"
body="${response%"$http_marker$http_status"}"

# Pushover can acknowledge the message even if curl reports a local transfer
# error after receiving the response. Trust the API acknowledgement in that
# case; otherwise preserve the transport failure for the caller.
if [[ "$http_status" == "200" && "$body" =~ \"status\"[[:space:]]*:[[:space:]]*1 ]]; then
  exit 0
fi

if [ "$curl_status" -ne 0 ]; then
  echo "pushnotify: curl failed (exit $curl_status)" >&2
  exit "$curl_status"
fi

echo "pushnotify: Pushover rejected the notification (HTTP $http_status): $body" >&2
exit 1
