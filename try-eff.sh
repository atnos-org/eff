#!/bin/sh
test -e ~/.coursier/cr || (mkdir -p ~/.coursier && wget -q -O ~/.coursier/cr https://git.io/vgvpD && chmod +x ~/.coursier/cr)
CLASSPATH="$(~/.coursier/cr fetch -q -p \
  \
  org.atnos:eff_2.12:4.0.1 \
  com.lihaoyi:ammonite_2.12.1:0.8.1 \
  \
)" java ammonite.Main --predef 'import org.atnos.eff._, all._, syntax.all._'
