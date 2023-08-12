#!/bin/bash -xe

wget https://github.com/jgm/pandoc/releases/download/3.1.6.1/pandoc-3.1.6.1-1-amd64.deb
sudo dpkg -i pandoc-3.1.6.1-1-amd64.deb
pandoc --version
