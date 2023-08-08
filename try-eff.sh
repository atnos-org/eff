#!/bin/sh
test -e ~/.coursier/cs || (mkdir -p ~/.coursier && wget -q -O ~/.coursier/cs https://git.io/coursier-cli-"$(uname | tr LD ld)" && chmod +x ~/.coursier/cs)
~/.coursier/cs launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_3.1.2:2.5.3 \
  org.atnos:eff_3:7.0.0 \
  -- --predef-code 'import org.atnos.eff._, all._, syntax.all._' < /dev/tty
