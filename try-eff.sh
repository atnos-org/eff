#!/bin/sh
test -e ~/.coursier/cr || (mkdir -p ~/.coursier && wget -q -O ~/.coursier/cr https://git.io/vgvpD && chmod +x ~/.coursier/cr)
~/.coursier/cr launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.13.2:2.1.1 \
  org.atnos:eff_2.13:5.11.0 \
  -- --predef-code 'import org.atnos.eff._, all._, syntax.all._' < /dev/tty
