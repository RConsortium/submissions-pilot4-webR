#!/usr/bin/env bash
set -euo pipefail # strict mode
readonly SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
function l { # Log a message to the terminal.
    echo
    echo -e "[$SCRIPT_NAME] ${1:-}"
}

# ECTD file to copy from source repo
ECTD_BUNDLE_DIR=ectd_bundle
ECTD_BUNDLE_FILE=r4app.zip

if [ -f "${ECTD_BUNDLE_DIR}/${ECTD_BUNDLE_FILE}" ]; then
  echo "Copying ${ECTD_BUNDLE_DIR}/${ECTD_BUNDLE_FILE}"
  if [ ! -f "$DESTINATION_DIR" ]; then
    echo "Create new directory ${DESTINATION_DIR}"
    mkdir -p "${DESTINATION_DIR}"
  fi
  cp "${ECTD_BUNDLE_DIR}/${ECTD_BUNDLE_FILE}" "${DESTINATION_DIR}/${ECTD_BUNDLE_FILE}"
fi

echo "ECTD Bundle file copied to ${DESTINATION_DIR}/${ECTD_BUNDLE_FILE}"