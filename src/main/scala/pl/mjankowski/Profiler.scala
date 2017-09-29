/*
 * Copyright (c) IMS Health 2016
 * Authors:
 *   Oskar Kapala <okapala@pl.imshealth.com>
 *   Szymon Kuryło <skurylo@pl.imshealth.com>
 *   Maciej Jankowski <mjankowski@pl.imshealth.com>
 *
 * This is auto-generated information. Do not edit manually, use https://gitlab.imshealth.com/autocoding/copyright-generator.git instead.
 */

package pl.mjankowski

object Profiler{

  def profile[T](methodName: String)(op:  => T): T = {
    val start = System.currentTimeMillis()

    val ret = op
    val end = System.currentTimeMillis()
    val elapsed = end - start
    val elapsedSec = elapsed / 1000
    val elapsedMin = elapsedSec / 60
    println(s"Execution of $methodName took ${elapsed} milliseconds, ${elapsedSec} seconds, ${elapsedMin} minutes" )
    ret
  }

}
