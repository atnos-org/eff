#!/bin/bash -xe

wget https://github.com/jgm/pandoc/releases/download/3.1.5/pandoc-3.1.5-1-amd64.deb
sudo dpkg -i pandoc-3.1.5-1-amd64.deb
pandoc --version
