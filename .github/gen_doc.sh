#!/bin/bash -xe

sbt -v \
  "set aggregate := false" \
  "testOnly *index* -- html html.search html.toc html.nostats html.outdir target/specs2-reports/site all"
