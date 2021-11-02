#! /bin/sh

# Setup Scala CI
# https://diamantidis.github.io/2020/05/17/ci-with-github-actions-for-scala-project

set -o errexit
set -o nounset
set -o xtrace


#sbt about scalafmtCheckAll scalafmtSbtCheck 'scalafix --check'  #FiXME
sbt scalafmtAll
sbt test
#sbt clean compile "testOnly "
sbt clean coverage coverageReport test
