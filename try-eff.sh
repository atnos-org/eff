#!/bin/sh
test -e ~/.coursier/cs || (mkdir -p ~/.coursier && wget -q -O ~/.coursier/cs https://git.io/coursier-cli-"$(uname | tr LD ld)" && chmod +x ~/.coursier/cs)
~/.coursier/cs launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_3.6.3:3.0.2 \
  org.atnos:eff_2.13:7.0.5 \
  -- --predef-code 'import org.atnos.eff._, all._, syntax.all._' < /dev/tty
