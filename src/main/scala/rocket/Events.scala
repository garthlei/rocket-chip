// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.util._
import freechips.rocketchip.util.property

trait EventSets {
  val eventSets: Seq[Any]
  def maskEventSelector(eventSel: UInt): UInt
  protected def decode(counter: UInt): (UInt, UInt) = {
    require(eventSets.size <= (1 << maxEventSetIdBits))
    require(eventSetIdBits > 0)
    (counter(eventSetIdBits-1, 0), counter >> maxEventSetIdBits)
  }
  def evaluate(eventSel: UInt): UInt
  def cover(): Unit
  protected def eventSetIdBits = log2Ceil(eventSets.size)
  protected def maxEventSetIdBits = 8
  require(eventSetIdBits <= maxEventSetIdBits)
}

class EventSet(val gate: (UInt, UInt) => Bool, val events: Seq[(String, () => Bool)]) {
  def size = events.size
  val hits = WireDefault(VecInit(Seq.fill(size)(false.B)))
  def check(mask: UInt) = {
    hits := events.map(_._2())
    gate(mask, hits.asUInt)
  }
  def dump(): Unit = {
    for (((name, _), i) <- events.zipWithIndex)
      when (check(1.U << i)) { printf(s"Event $name\n") }
  }
  def withCovers: Unit = {
    events.zipWithIndex.foreach {
      case ((name, func), i) => property.cover(gate((1.U << i), (func() << i)), name)
    }
  }
}

class ScalarEventSets(val eventSets: Seq[EventSet]) extends EventSets {
  def maskEventSelector(eventSel: UInt): UInt = {
    // allow full associativity between counters and event sets (for now?)
    val setMask = (BigInt(1) << eventSetIdBits) - 1
    val maskMask = ((BigInt(1) << eventSets.map(_.size).max) - 1) << maxEventSetIdBits
    eventSel & (setMask | maskMask).U
  }

  def evaluate(eventSel: UInt): UInt = {
    val (set, mask) = decode(eventSel)
    val sets = for (e <- eventSets) yield {
      require(e.hits.getWidth <= mask.getWidth, s"too many events ${e.hits.getWidth} wider than mask ${mask.getWidth}")
      e check mask
    }
    sets(set)
  }

  def cover() = eventSets.foreach { _.withCovers }
}

class SuperscalarEventSets(val eventSets: Seq[(Seq[EventSet], (UInt, UInt) => UInt)])
    extends EventSets {
  def maskEventSelector(eventSel: UInt): UInt = {
    // allow full associativity between counters and event sets (for now?)
    val setMask = (BigInt(1) << eventSetIdBits) - 1
    val maskMask =
      ((BigInt(1) << (eventSets map { case (s, _) => if (s.isEmpty) 0 else s.head.size }).max) - 1) << maxEventSetIdBits
    eventSel & (setMask | maskMask).U
  }

  def evaluate(eventSel: UInt): UInt = {
    val (set, mask) = decode(eventSel)
    val sets = for ((sets, reducer) <- eventSets) yield {
      sets.map { set =>
        require(set.hits.getWidth <= mask.getWidth, s"too many events ${set.hits.getWidth} wider than mask ${mask.getWidth}")
        set.check(mask)
      }.reduce(reducer)
    }
    val zeroPadded = sets.padTo(1 << eventSetIdBits, 0.U)
    zeroPadded(set)
  }

  def toScalarEventSets: ScalarEventSets = new ScalarEventSets(eventSets.map(_._1.head))

  def cover(): Unit = { eventSets.foreach(_._1.foreach(_.withCovers)) }

  require(eventSets.forall(s => s._1.forall(_.size == s._1.head.size)))
}
