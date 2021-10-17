#! /bin/sh

# Setup Scala CI
# https://diamantidis.github.io/2020/05/17/ci-with-github-actions-for-scala-project

set -o errexit
set -o nounset
set -o xtrace

touch build.sbt
touch .scalafmt.conf
sbt about scalafmtCheckAll scalafmtSbtCheck 'scalafix --check'
sbt clean coverage coverageReport test

