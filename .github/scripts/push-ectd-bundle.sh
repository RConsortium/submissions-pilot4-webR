#!/usr/bin/env bash
set -euo pipefail # strict mode
readonly SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
function l { # Log a message to the terminal.
    echo
    echo -e "[$SCRIPT_NAME] ${1:-}"
}

# Set up file paths
ECTD_REPO_DIR=submissions-pilot4-webR-to-fda

# Navigate to clone of ECTD repo
cd "./${ECTD_REPO_DIR}"
echo "Open root of ECTD GitHub Repo"

# Commit updated bundle to repository
git add -A .
git config user.name github-actions
git config user.email github-actions@github.com
git commit -am "GH Action: Update ECTD bundle of application"
git push --set-upstream origin main

echo "Updated ECTD bundle successfully to repository"
