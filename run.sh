#! /bin/sh

# Setup Scala CI
# https://diamantidis.github.io/2020/05/17/ci-with-github-actions-for-scala-project

set -o errexit
set -o nounset
set -o xtrace


#sbt about scalafmtCheckAll scalafmtSbtCheck 'scalafix --check'  #FiXME
sbt scalafmtAll
sbt clean compile "testOnly LogarithmOfPowersOfTwoTest BitmaskNextWithConstantPopcountNtzTest BitmaskThermometerFromCountTest BitmaskThermometerToRightmost0BitTest BitmaskThermometerToRightmost1BitTest BitmaskTurnOffTrailing1BitsTest BitmaskTurnOnTrailing0BitsTest"
#sbt clean coverage coverageReport test
