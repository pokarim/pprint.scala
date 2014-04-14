package com.pokarim.pprint.tests

import org.specs2.mutable._ 
import scala.Console.withOut
import java.io.Writer
import java.io.ByteArrayOutputStream
import com.pokarim.pprint._

class PPrintSpec extends Specification {

  def getOutput(f: => Unit):String = {
	val out = new ByteArrayOutputStream();
	withOut(out){f}
	out.toString()
  }

  def getOutputWithoutColor(f: => Unit):String = {
	val out = new ByteArrayOutputStream();
	withOut(out){enableColor.withValue(false){f}}
	out.toString()
  }

  "The 'Hello world' string" should {

	"getOutput" in {
	  getOutput(println("xx")) === "xx\n"
	  getOutput(print("yy")) === "yy"
	  getOutput(print("z\nz")) === "z\nz"
	}

	val ListOfMap01 = List(Map(1->2), Map(1->2), Map(1->2),
						   Map(1->2), Map(1->2), Map(1->2),Map(1->2))

	val ListOfMap01Str = """List(Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2),
     Map(1 -> 2))"""

    "pprn" in {
	  getOutputWithoutColor(pprn(List(1,2,3))) ===
		"List(1, 2, 3)\n"
	  getOutputWithoutColor(pprn(ListOfMap01)) === ListOfMap01Str ++ "\n"
	}

    "toPrettyString" in {
	  enableColor.withValue(false){toPrettyString(List(1, 2, 3))} === 
		"List(1, 2, 3)"
	  enableColor.withValue(false){toPrettyString(ListOfMap01)} === 
		ListOfMap01Str
	}
  }
}
