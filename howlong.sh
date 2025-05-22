#!/bin/bash

# This script calculates and prints the first commit date and total days of effort in the current Git repository.

# Ensure we are in a git repository
if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
  echo "Error: Not inside a git repository. Please run this script from within a git repo."
  exit 1
fi

# Get the first commit date
first_commit_date=$(git log --reverse --date=short --pretty=format:"%ad" | head -n 1)

# Check if there is any commit
if [ -z "$first_commit_date" ]; then
  echo "Error: No commits found in this repository."
  exit 1
fi

# Calculate the days since the first commit
first_commit_days=$(( ($(date +%s) - $(date -j -f "%Y-%m-%d" "$first_commit_date" +%s)) / 86400 ))

# Calculate total days of effort
days_of_effort=$(git log --date=short --pretty=format:"%ad" | sort -u | wc -l | xargs)

# Print the results
echo ""
echo "🏁 First commit $first_commit_days days ago"
echo "👷 $days_of_effort days of effort. Make money yet?"
echo ""
