package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("demux example") {
    val in, c1, c2, out1, out2, out3, out4 = new Wire
    val c = List(c1, c2)
    val out = List(out1, out2, out3, out4)
    demux(in, c, out)
    
    probe("out1", out1)
    probe("out2", out2)
    probe("out3", out3)
    probe("out4", out4)
    
    in.setSignal(true)
    c1.setSignal(true)
    c2.setSignal(true)
    run
    
    assert(out1.getSignal === true, "demux 1")
  }
    
}
