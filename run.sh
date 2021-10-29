#! /bin/sh

# Setup Scala CI
# https://diamantidis.github.io/2020/05/17/ci-with-github-actions-for-scala-project

set -o errexit
set -o nounset
set -o xtrace


#sbt about scalafmtCheckAll scalafmtSbtCheck 'scalafix --check'  #FiXME
sbt scalafmtAll
sbt clean compile "testOnly MuxStrTest CarryInBinTest AddressDecoderArithmeticTest AddressDecoderStaticTest AddressTranslatorArithmeticTest AddressTranslatorStaticTest Bitmask0BitAtRightmost1BitTest Bitmask1BitAtRightmost0BitTest BitmaskIsolateRightmost1BitTest BitmaskNextwithConstantPopcountTest"
sbt clean coverage coverageReport test