BigInt("41", 16)
BigInt("0E", 16)
BigInt("DE", 16)
BigInt("5A", 16)

import lib.simSupport._

val ba = Array[BigInt](65, 14, 222, 90)
ba.reduce(_^_)