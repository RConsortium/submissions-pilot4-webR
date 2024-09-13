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
LETTER_SOURCE_DIR=cover-letter
LETTER_SOURCE_FILE=cover-letter.pdf
LETTER_DEST_FILE=cover-letter.pdf

# Copy ADRG (PDF version)
if [ -f "${ADRG_SOURCE_DIR}/${ADRG_SOURCE_FILE}" ]; then
  echo "Copying ${ADRG_SOURCE_DIR}/${ADRG_SOURCE_FILE}"
  if [ ! -f "$ADRG_DESTINATION_DIR" ]; then
    echo "Create new directory ${ADRG_DESTINATION_DIR}"
    mkdir -p "${ADRG_DESTINATION_DIR}"
  fi
  cp "${ADRG_SOURCE_DIR}/${ADRG_SOURCE_FILE}" "${ADRG_DESTINATION_DIR}/${ADRG_DEST_FILE}"
fi

echo "ADRG copied to ${ADRG_DESTINATION_DIR}/${ADRG_DEST_FILE}"

# Copy README for ectd repository
if [ -f "${README_SOURCE_DIR}/${README_SOURCE_FILE}" ]; then
  echo "Copying ${README_SOURCE_DIR}/${README_SOURCE_FILE}"
  if [ ! -f "$README_DESTINATION_DIR" ]; then
    echo "Create new directory ${README_DESTINATION_DIR}"
    mkdir -p "${README_DESTINATION_DIR}"
  fi
  cp "${README_SOURCE_DIR}/${README_SOURCE_FILE}" "${README_DESTINATION_DIR}/${README_DEST_FILE}"
fi

echo "README copied to ${README_DESTINATION_DIR}/${README_DEST_FILE}"

# Copy cover letter (PDF version)
if [ -f "${LETTER_SOURCE_DIR}/${LETTER_SOURCE_FILE}" ]; then
  echo "Copying ${LETTER_SOURCE_DIR}/${LETTER_SOURCE_FILE}"
  if [ ! -f "$LETTER_DESTINATION_DIR" ]; then
    echo "Create new directory ${LETTER_DESTINATION_DIR}"
    mkdir -p "${LETTER_DESTINATION_DIR}"
  fi
  cp "${LETTER_SOURCE_DIR}/${LETTER_SOURCE_FILE}" "${LETTER_DESTINATION_DIR}/${LETTER_DEST_FILE}"
fi

echo "Cover letter copied to ${LETTER_DESTINATION_DIR}/${LETTER_DEST_FILE}"

# if [ -f "${ECTD_BUNDLE_DIR}/${ECTD_BUNDLE_FILE}" ]; then
#   echo "Copying ${ECTD_BUNDLE_DIR}/${ECTD_BUNDLE_FILE}"
#   if [ ! -f "$DESTINATION_DIR" ]; then
#     echo "Create new directory ${DESTINATION_DIR}"
#     mkdir -p "${DESTINATION_DIR}"
#   fi
#   cp "${ECTD_BUNDLE_DIR}/${ECTD_BUNDLE_FILE}" "${DESTINATION_DIR}/${ECTD_BUNDLE_FILE}"
# fi

# echo "ECTD Bundle file copied to ${DESTINATION_DIR}/${ECTD_BUNDLE_FILE}"