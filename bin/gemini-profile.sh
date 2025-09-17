#!/usr/bin/env bash
set -euo pipefail

GEMINI_DIR="$HOME/.gemini"
PROFILE_DIR="$HOME/.config/gemini-profiles"
# Exclude user_id + installation_id
FILES=(google_accounts.json oauth_creds.json)

# Convert email to safe dir name
email_to_dir() {
    echo "$1" | tr '@' '-' | sed 's/-/---/1'
}

# Read active email from google_accounts.json
get_active_email() {
    local acct_file="$GEMINI_DIR/google_accounts.json"
    if [[ ! -f "$acct_file" ]]; then
        echo "No google_accounts.json found in $GEMINI_DIR" >&2
        return 1
    fi
    jq -r '.active' "$acct_file"
}

# Ensure profile dir exists and back up
ensure_profile_dir() {
    local email="$1"
    local dir
    dir=$(email_to_dir "$email")
    mkdir -p "$PROFILE_DIR/$dir"

    # Force "old" to [] before backing up
    jq '.old=[]' "$GEMINI_DIR/google_accounts.json" > "$PROFILE_DIR/$dir/google_accounts.json"

    for f in "${FILES[@]}"; do
        [[ "$f" == "google_accounts.json" ]] && continue
        if [[ -f "$GEMINI_DIR/$f" ]]; then
            cp "$GEMINI_DIR/$f" "$PROFILE_DIR/$dir/"
        fi
    done
}

backup_profile() {
    local mode="${1:-strict}"

    local email
    email=$(get_active_email) || { [[ "$mode" == "strict" ]] && exit 1 || return 1; }

    if [[ ! -f "$GEMINI_DIR/oauth_creds.json" ]]; then
        if [[ "$mode" == "strict" ]]; then
            echo "Error: oauth_creds.json missing in $GEMINI_DIR, cannot back up"
            exit 1
        else
            echo "Skipping backup: oauth_creds.json missing in $GEMINI_DIR"
            return 0
        fi
    fi

    echo "Backing up profile: $email"
    ensure_profile_dir "$email"
    echo "Backup completed for $email"
}

rotate_profile() {
    local email
    email=$(get_active_email) || exit 1
    local current_dir
    current_dir=$(email_to_dir "$email")

    # Save current profile (lenient mode, so it won’t bail if creds missing)
    backup_profile lenient || true

    ids=()
    while IFS= read -r dir; do
        ids+=("$(basename "$dir")")
    done < <(find "$PROFILE_DIR" -mindepth 1 -maxdepth 1 -type d | sort)

    if [[ ${#ids[@]} -eq 0 ]]; then
        echo "No profiles found in $PROFILE_DIR"
        exit 1
    fi

    next_id=""
    for i in "${!ids[@]}"; do
        if [[ "${ids[$i]}" == "$current_dir" ]]; then
            next_index=$(( (i + 1) % ${#ids[@]} ))
            next_id="${ids[$next_index]}"
            break
        fi
    done

    [[ -z "$next_id" ]] && { echo "No next profile found."; exit 1; }

    echo "Switching profile: $email -> $next_id"

    # 🚨 Remove user_id and installation_id before rotation
    rm -f "$GEMINI_DIR/user_id" "$GEMINI_DIR/installation_id"

    for f in "${FILES[@]}"; do
        src="$PROFILE_DIR/$next_id/$f"
        if [[ -f "$src" ]]; then
            cp "$src" "$GEMINI_DIR/$f"
        else
            echo "Warning: $src not found"
        fi
    done

    echo "Now using profile: $next_id"
}

restore_profile() {
    local email
    email=$(get_active_email) || exit 1
    local dir
    dir=$(email_to_dir "$email")

    profile_dir="$PROFILE_DIR/$dir"
    if [[ ! -d "$profile_dir" ]]; then
        echo "No backup found for profile: $email"
        exit 1
    fi

    echo "Restoring profile: $email"

    # 🚨 Remove user_id and installation_id before restoring
    rm -f "$GEMINI_DIR/user_id" "$GEMINI_DIR/installation_id"

    for f in "${FILES[@]}"; do
        src="$profile_dir/$f"
        if [[ -f "$src" ]]; then
            cp "$src" "$GEMINI_DIR/$f"
        else
            echo "Warning: $src not found in backup for $email"
        fi
    done
    echo "Restore completed for $email"
}

status_profile() {
    local email
    email=$(get_active_email) || exit 1
    echo "Active profile: $email"
}

list_profiles() {
    ids=()
    while IFS= read -r dir; do
        ids+=("$(basename "$dir")")
    done < <(find "$PROFILE_DIR" -mindepth 1 -maxdepth 1 -type d | sort)

    if [[ ${#ids[@]} -eq 0 ]]; then
        echo "No profiles found in $PROFILE_DIR"
        exit 0
    fi

    echo "Available profiles:"
    for id in "${ids[@]}"; do
        acct_file="$PROFILE_DIR/$id/google_accounts.json"
        if [[ -f "$acct_file" ]]; then
            email=$(jq -r '.active' "$acct_file" 2>/dev/null || echo "unknown")
        else
            email="missing google_accounts.json"
        fi
        echo "$id - $email"
    done
}

cmd="${1:-}"
case "$cmd" in
    rotate)  rotate_profile ;;
    restore) restore_profile ;;
    status)  status_profile ;;
    backup)  backup_profile strict ;;
    list)    list_profiles ;;
    *) echo "Usage: $0 {rotate|restore|status|backup|list}"; exit 1 ;;
esac
