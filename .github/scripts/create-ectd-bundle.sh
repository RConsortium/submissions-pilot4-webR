#!/usr/bin/env bash
set -euo pipefail # strict mode
readonly SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
function l { # Log a message to the terminal.
    echo
    echo -e "[$SCRIPT_NAME] ${1:-}"
}

# Define file and directory paths
PKGLITE_SOURCE_DIR=dev
PKGLITE_APP_SOURCE_FILE=pilot4_webR_pkglite.txt
PKGLITE_UTILS_FILE=pkglite_utils.r
ECTD_BUNDLE_DIR=submissions-pilot4-webR-to-fda
ECTD_LETTER_DIR=${ECTD_BUNDLE_DIR}/m1/us
ECTD_ROOT_DIR=${ECTD_BUNDLE_DIR}/m5/datasets/rconsortiumpilot4webR/analysis/adam
ECTD_DATASETS_DIR=${ECTD_BUNDLE_DIR}/m5/datasets/rconsortiumpilot4webR/analysis/adam/datasets
ECTD_PROGRAMS_DIR=${ECTD_BUNDLE_DIR}/m5/datasets/rconsortiumpilot4webR/analysis/adam/programs
ADRG_SOURCE_DIR=adrg
ADRG_SOURCE_FILE=adrg-quarto-pdf.pdf
ADRG_DESTINATION_DIR=${ECTD_DATASETS_DIR}
ADRG_DEST_FILE=adrg.pdf
README_SOURCE_DIR=ectd_readme
README_SOURCE_FILE=README.md
README_DESTINATION_DIR=${ECTD_BUNDLE_DIR}
README_DEST_FILE=README.md
LETTER_SOURCE_DIR=cover-letter
LETTER_SOURCE_FILE=cover-letter.pdf
LETTER_DESTINATION_DIR=${ECTD_LETTER_DIR}
LETTER_DEST_FILE=cover-letter.pdf
DATASETS_SOURCE_DIR=datasets

# Create directory structure for ectd bundle
mkdir -p "${ECTD_LETTER_DIR}"
mkdir -p "${ECTD_PROGRAMS_DIR}"

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

# Copy datasets
if [ -d "$DATASETS_SOURCE_DIR" ]; then
  echo "Copying ${DATASETS_SOURCE_DIR}"
  cp -r "${DATASETS_SOURCE_DIR}" "${ECTD_ROOT_DIR}"
fi

# Copy pilot 4 app pkglite file
if [ -f "${PKGLITE_SOURCE_DIR}/${PKGLITE_APP_SOURCE_FILE}" ]; then
  echo "Copying ${PKGLITE_SOURCE_DIR}/${PKGLITE_APP_SOURCE_FILE}"
  cp "${PKGLITE_SOURCE_DIR}/${PKGLITE_APP_SOURCE_FILE}" "${ECTD_PROGRAMS_DIR}/${PKGLITE_APP_SOURCE_FILE}"
fi

echo "App pkglite file copied to ${ECTD_PROGRAMS_DIR}/${PKGLITE_APP_SOURCE_FILE}"

# Copy pilot 4 pkglite utils R script
if [ -f "${PKGLITE_UTILS_FILE}" ]; then
  echo "Copying ${PKGLITE_UTILS_FILE}"
  cp "${PKGLITE_UTILS_FILE}" "${ECTD_PROGRAMS_DIR}/${PKGLITE_UTILS_FILE}"
fi

echo "Utils pkglite R file copied to ${ECTD_PROGRAMS_DIR}/${PKGLITE_UTILS_FILE}"

