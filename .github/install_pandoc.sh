#!/bin/bash -xe

PANDOC_FILE_NAME="pandoc-3.2.1-1-amd64.deb"
wget "https://github.com/jgm/pandoc/releases/download/3.2.1/${PANDOC_FILE_NAME}"
sudo dpkg -i "${PANDOC_FILE_NAME}"
pandoc --version
