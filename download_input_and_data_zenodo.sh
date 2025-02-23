#!/bin/bash
# download_data.sh
# This script downloads and extracts the input.zip and data.zip files
# from Zenodo if their target directories ("input" and "data") do not exist.

# Zenodo download URLs (using the file download links)
INPUT_URL="https://zenodo.org/record/14914046/files/input.zip?download=1"
DATA_URL="https://zenodo.org/record/14913925/files/data.zip?download=1"

# Download and extract input.zip if directory "input" doesn't exist
if [ ! -d "input" ]; then
    echo "Downloading input.zip..."
    curl -L "$INPUT_URL" -o input.zip
    echo "Extracting input.zip "
    unzip input.zip -d .
    rm input.zip
else
    echo "Directory 'input' already exists; skipping download of input.zip."
fi

# Download and extract data.zip if directory "data" doesn't exist
if [ ! -d "data" ]; then
    echo "Downloading data.zip..."
    curl -L "$DATA_URL" -o data.zip
    echo "Extracting data.zip"
    unzip data.zip -d .
    rm data.zip
else
    echo "Directory 'data' already exists; skipping download of data.zip."
fi
