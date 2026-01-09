#!/usr/bin/env bash
set -euo pipefail

CODEX_DIR="$HOME/.codex"
PROFILE_DIR="$HOME/.config/codex-profiles"
# Main file to backup/rotate
FILES=(auth.json)

# Convert account ID to safe dir name
account_id_to_dir() {
    echo "$1" | tr '/' '-' | sed 's/[^a-zA-Z0-9-]/-/g'
}

# Read active account ID from auth.json
get_active_account_id() {
    local auth_file="$CODEX_DIR/auth.json"
    if [[ ! -f "$auth_file" ]]; then
        echo "No auth.json found in $CODEX_DIR" >&2
        return 1
    fi
    jq -r '.tokens.account_id' "$auth_file"
}

resolve_profile_name_for_backup() {
    local account_id="$1"
    local provided_name="${2:-}"
    local dir
    dir=$(account_id_to_dir "$account_id")

    if [[ -n "$provided_name" ]]; then
        echo "$provided_name"
        return
    fi

    local profile_name_file="$PROFILE_DIR/$dir/profile_name"
    if [[ -f "$profile_name_file" ]]; then
        cat "$profile_name_file"
        return
    fi

    echo "$account_id"
}

# Ensure profile dir exists and back up
ensure_profile_dir() {
    local account_id="$1"
    local profile_name="${2:-$account_id}"
    local dir
    dir=$(account_id_to_dir "$account_id")
    mkdir -p "$PROFILE_DIR/$dir"

    for f in "${FILES[@]}"; do
        if [[ -f "$CODEX_DIR/$f" ]]; then
            cp "$CODEX_DIR/$f" "$PROFILE_DIR/$dir/"
        fi
    done

    # Store profile name for reference
    echo "$profile_name" > "$PROFILE_DIR/$dir/profile_name"
}

backup_profile() {
    local profile_name="${1:-}"
    local mode="${2:-strict}"

    local account_id
    account_id=$(get_active_account_id) || { [[ "$mode" == "strict" ]] && exit 1 || return 1; }

    if [[ ! -f "$CODEX_DIR/auth.json" ]]; then
        if [[ "$mode" == "strict" ]]; then
            echo "Error: auth.json missing in $CODEX_DIR, cannot back up"
            exit 1
        else
            echo "Skipping backup: auth.json missing in $CODEX_DIR"
            return 0
        fi
    fi

    profile_name=$(resolve_profile_name_for_backup "$account_id" "$profile_name")

    echo "Backing up profile: $profile_name (account: $account_id)"
    ensure_profile_dir "$account_id" "$profile_name"
    echo "Backup completed for $profile_name"
}

rotate_profile() {
    local account_id
    account_id=$(get_active_account_id) || exit 1
    local current_dir
    current_dir=$(account_id_to_dir "$account_id")

    # Save current profile (lenient mode, so it won't bail if auth missing)
    # Use account_id as profile name for rotation
    backup_profile "$account_id" lenient || true

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

    echo "Switching profile: $account_id -> $next_id"

    for f in "${FILES[@]}"; do
        src="$PROFILE_DIR/$next_id/$f"
        if [[ -f "$src" ]]; then
            cp "$src" "$CODEX_DIR/$f"
        else
            echo "Warning: $src not found"
        fi
    done

    echo "Now using profile: $next_id"
}

restore_profile() {
    local profile_name="${1:-}"

    if [[ -z "$profile_name" ]]; then
        echo "Error: Profile name is required for restore" >&2
        echo "Usage: $0 restore <profile_name>" >&2
        exit 1
    fi

    # Find profile by name
    local profile_dir=""
    while IFS= read -r dir; do
        if [[ -f "$dir/profile_name" ]] && [[ "$(cat "$dir/profile_name")" == "$profile_name" ]]; then
            profile_dir="$dir"
            break
        fi
    done < <(find "$PROFILE_DIR" -mindepth 1 -maxdepth 1 -type d)

    if [[ -z "$profile_dir" ]]; then
        echo "No backup found for profile: $profile_name"
        exit 1
    fi

    echo "Restoring profile: $profile_name"

    for f in "${FILES[@]}"; do
        src="$profile_dir/$f"
        if [[ -f "$src" ]]; then
            cp "$src" "$CODEX_DIR/$f"
        else
            echo "Warning: $src not found in backup for $profile_name"
        fi
    done
    echo "Restore completed for $profile_name"
}

status_profile() {
    local account_id
    account_id=$(get_active_account_id) || exit 1
    echo "Active account ID: $account_id"
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
        auth_file="$PROFILE_DIR/$id/auth.json"
        profile_name_file="$PROFILE_DIR/$id/profile_name"

        if [[ -f "$profile_name_file" ]]; then
            profile_name=$(cat "$profile_name_file")
        else
            profile_name="unnamed"
        fi

        if [[ -f "$auth_file" ]]; then
            account_id=$(jq -r '.tokens.account_id' "$auth_file" 2>/dev/null || echo "unknown")
        else
            account_id="missing auth.json"
        fi
        echo "$id - $profile_name (account: $account_id)"
    done
}

cmd="${1:-}"
case "$cmd" in
    rotate)  rotate_profile ;;
    restore) restore_profile "${2:-}" ;;
    status)  status_profile ;;
    backup)  backup_profile "${2:-}" strict ;;
    list)    list_profiles ;;
    *) echo "Usage: $0 {rotate|restore|status|backup|list} [profile_name]"; exit 1 ;;
esac
