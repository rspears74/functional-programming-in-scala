package com.rspears.state

trait RNG {
  def nextInt: (Int, RNG)
}
