#!/usr/bin/env bash
set -euo pipefail

GEMINI_DIR="$HOME/.gemini"
PROFILE_DIR="$HOME/.config/gemini-profiles"
FILES=(google_accounts.json installation_id oauth_creds.json user_id)

ensure_profile_dir() {
    local uid="$1"
    mkdir -p "$PROFILE_DIR/$uid"
    for f in "${FILES[@]}"; do
        if [[ -f "$GEMINI_DIR/$f" ]]; then
            cp "$GEMINI_DIR/$f" "$PROFILE_DIR/$uid/"
        fi
    done
}

rotate_profile() {
    # 1. Identify current user ID
    if [[ ! -f "$GEMINI_DIR/user_id" ]]; then
        echo "No user_id found in $GEMINI_DIR"
        exit 1
    fi
    read -r current_id < "$GEMINI_DIR/user_id"

    # 2. Ensure current profile is saved
    ensure_profile_dir "$current_id"

    # 3. Get list of profile IDs sorted
    mapfile -t ids < <(find "$PROFILE_DIR" -mindepth 1 -maxdepth 1 -type d -printf "%f\n" | sort)
    if [[ ${#ids[@]} -eq 0 ]]; then
        echo "No profiles found in $PROFILE_DIR"
        exit 1
    fi

    # 4. Find the next ID in order
    next_id=""
    for i in "${!ids[@]}"; do
        if [[ "${ids[$i]}" == "$current_id" ]]; then
            next_index=$(( (i + 1) % ${#ids[@]} ))
            next_id="${ids[$next_index]}"
            break
        fi
    done

    if [[ -z "$next_id" ]]; then
        echo "No next profile found."
        exit 1
    fi

    echo "Switching profile: $current_id -> $next_id"

    # 5. Copy files into ~/.gemini
    for f in "${FILES[@]}"; do
        src="$PROFILE_DIR/$next_id/$f"
        if [[ -f "$src" ]]; then
            cp "$src" "$GEMINI_DIR/$f"
        else
            echo "Warning: $src not found"
        fi
    done

    # 6. Confirm feedback
    echo "Now using profile: $next_id"
}

status_profile() {
    local acct_file="$GEMINI_DIR/google_accounts.json"
    if [[ ! -f "$acct_file" ]]; then
        echo "No google_accounts.json found"
        exit 1
    fi
    active=$(jq -r '.active' "$acct_file")
    echo "Active profile: $active"
}

backup_profile() {
    if [[ ! -f "$GEMINI_DIR/user_id" ]]; then
        echo "No user_id found in $GEMINI_DIR"
        exit 1
    fi
    read -r current_id < "$GEMINI_DIR/user_id"
    echo "Backing up profile: $current_id"
    ensure_profile_dir "$current_id"
    echo "Backup completed for $current_id"
}

cmd="${1:-}"
case "$cmd" in
    rotate)
        rotate_profile
        ;;
    status)
        status_profile
        ;;
    backup)
        backup_profile
        ;;
    *)
        echo "Usage: $0 {rotate|status|backup}"
        exit 1
        ;;
esac
