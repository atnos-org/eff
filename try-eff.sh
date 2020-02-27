#!/bin/sh
test -e ~/.coursier/cr || (mkdir -p ~/.coursier && wget -q -O ~/.coursier/cr https://git.io/vgvpD && chmod +x ~/.coursier/cr)
~/.coursier/cr launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.13.1:2.0.4 \
  org.atnos:eff_2.13:5.7.0 \
  -- --predef-code 'import org.atnos.eff._, all._, syntax.all._' < /dev/tty
