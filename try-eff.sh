#!/bin/sh
test -e ~/.coursier/cs || (mkdir -p ~/.coursier && wget -q -O ~/.coursier/cs https://git.io/coursier-cli-"$(uname | tr LD ld)" && chmod +x ~/.coursier/cs)
~/.coursier/cs launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.13.13:3.0.0-M1 \
  org.atnos:eff_2.13:7.0.1 \
  -- --predef-code 'import org.atnos.eff._, all._, syntax.all._' < /dev/tty
