#!/usr/bin/env bash
set -euo pipefail # strict mode
readonly SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
function l { # Log a message to the terminal.
    echo
    echo -e "[$SCRIPT_NAME] ${1:-}"
}

# Environment variables representing directory structure
# - defined in specific GitHub Action step
#DESTINATION_PROGRAMS_DIR=submissions-pilot4-webR-to-fda/m5/datasets/rconsortiumpilot4/analysis/adam/programs
#DESTINATION_DATASETS_DIR=submissions-pilot4-webR-to-fda/m5/datasets/rconsortiumpilot4/analysis/adam/datasets
#ADRG_DESTINATION_DIR=submissions-pilot4-webR-to-fda/m5/datasets/rconsortiumpilot4/analysis/adam/datasets
#README_DESTINATION_DIR=submissions-pilot4-webR-to-fda
#LETTER_DESTINATION_DIR: submissions-pilot4-webR-to-fda/m1/us
#ECTD file to copy from source repo

# Variables representing files/directories
ECTD_BUNDLE_DIR=ectd_bundle
ECTD_PKGLITE_FILE=pilot4_webR_pkglite.txt
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
DATASETS_SOURCE_DIR=app/www/adam

# Copy XPT datasets and metadata files
if [ -f "${DATASETS_SOURCE_DIR}" ]; then
  echo "Copy XPT data and metadata files"
  if [ ! -f "${DESTINATION_DATASETS_DIR}" ]; then
    echo "Create new directory ${DESTINATION_DATASETS_DIR}"
    mkdir -p "${DESTINATION_DATASETS_DIR}"
  fi
  cp "${DATASETS_SOURCE_DIR}/*" "${DESTINATION_DATASETS_DIR}/."
fi

# Copy pkglite file
if [ -f "${ECTD_PKGLITE_FILE}" ]; then
  echo "Copy pkglite file"
  if [ ! -f "${DESTINATION_PROGRAMS_DIR}" ]; then
    echo "Create new directory ${DESTINATION_PROGRAMS_DIR}"
    mkdir -p "${DESTINATION_PROGRAMS_DIR}"
  fi
  cp "${ECTD_PKGLITE_FILE}" "${DESTINATION_PROGRAMS_DIR}/."
fi

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
