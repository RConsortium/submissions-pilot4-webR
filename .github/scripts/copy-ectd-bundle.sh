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
ADRG_SOURCE_DIR=adrg
ADRG_SOURCE_FILE=adrg-quarto-pdf.pdf
ADRG_DEST_FILE=adrg.pdf
README_SOURCE_DIR=ectd_readme
README_SOURCE_FILE=README.md
README_DEST_FILE=README.md

if [ -f "${ADRG_SOURCE_DIR}/${ADRG_SOURCE_FILE}" ]; then
  echo "Copying ${ADRG_SOURCE_DIR}/${ADRG_SOURCE_FILE}"
  if [ ! -f "$ADRG_DESTINATION_DIR" ]; then
    echo "Create new directory ${ADRG_DESTINATION_DIR}"
    mkdir -p "${ADRG_DESTINATION_DIR}"
  fi
  cp "${ADRG_SOURCE_DIR}/${ADRG_SOURCE_FILE}" "${ADRG_DESTINATION_DIR}/${ADRG_DEST_FILE}"
fi

echo "ADRG copied to ${ADRG_DESTINATION_DIR}/${ADRG_DEST_FILE}"

if [ -f "${README_SOURCE_DIR}/${README_SOURCE_FILE}" ]; then
  echo "Copying ${README_SOURCE_DIR}/${README_SOURCE_FILE}"
  cp "${README_SOURCE_DIR}/${README_SOURCE_FILE}" "${README_DEST_FILE}"
fi

echo "README copied to ${README_DEST_FILE}"

# if [ -f "${ECTD_BUNDLE_DIR}/${ECTD_BUNDLE_FILE}" ]; then
#   echo "Copying ${ECTD_BUNDLE_DIR}/${ECTD_BUNDLE_FILE}"
#   if [ ! -f "$DESTINATION_DIR" ]; then
#     echo "Create new directory ${DESTINATION_DIR}"
#     mkdir -p "${DESTINATION_DIR}"
#   fi
#   cp "${ECTD_BUNDLE_DIR}/${ECTD_BUNDLE_FILE}" "${DESTINATION_DIR}/${ECTD_BUNDLE_FILE}"
# fi

# echo "ECTD Bundle file copied to ${DESTINATION_DIR}/${ECTD_BUNDLE_FILE}"